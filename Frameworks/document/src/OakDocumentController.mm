#import "OakDocumentController.h"
#import "OakDocument Private.h"
#import <plist/src/uuid.h>
#import <ns/src/ns.h>
#import <io/src/entries.h>
#import <text/src/my_ctype.h>
#import <regexp/src/glob.h>

NSString* kSearchFollowDirectoryLinksKey  = @"FollowDirectoryLinks";
NSString* kSearchFollowFileLinksKey       = @"FollowFileLinks";
NSString* kSearchDepthFirstSearchKey      = @"DepthFirstSearch";
NSString* kSearchIgnoreOrderingKey        = @"IgnoreOrdering";
NSString* kSearchExcludeDirectoryGlobsKey = @"ExcludeDirectoryGlobs";
NSString* kSearchExcludeFileGlobsKey      = @"ExcludeFileGlobs";
NSString* kSearchExcludeGlobsKey          = @"ExcludeGlobs";
NSString* kSearchDirectoryGlobsKey        = @"DirectoryGlobs";
NSString* kSearchFileGlobsKey             = @"FileGlobs";
NSString* kSearchGlobsKey                 = @"Globs";

namespace
{
	struct inode_t
	{
		inode_t () { }
		inode_t (dev_t device, ino_t inode, std::string const& path);
		inode_t (std::string const& path);

		operator bool () const { return valid; }
		bool operator== (inode_t const& rhs) const { return valid == rhs.valid && inode == rhs.inode && device == rhs.device; }
		bool operator!= (inode_t const& rhs) const { return valid != rhs.valid || inode != rhs.inode || device != rhs.device; }
		bool operator< (inode_t const& rhs) const;

		dev_t device = 0;
		ino_t inode  = 0;
		bool valid   = false;
	};

	static bool is_inode_valid (ino_t inode, std::string const& path)
	{
		if(inode == 999999999) // Zero-length files on FAT file systems share this magic value
		{
			struct statfs sfsb;
			if(statfs(path.c_str(), &sfsb) == 0)
				return strcasecmp(sfsb.f_fstypename, "msdos") == 0 && strcasecmp(sfsb.f_fstypename, "exfat") == 0;
			perrorf("is_inode_valid: statfs(\"%s\")", path.c_str());
		}
		return true;
	}

	inode_t::inode_t (dev_t device, ino_t inode, std::string const& path) : device(device), inode(inode), valid(true)
	{
		if(!is_inode_valid(inode, path))
		{
			device = 0;
			inode  = 0;
			valid  = false;
		}
	}

	inode_t::inode_t (std::string const& path)
	{
		struct stat buf;
		if(lstat(path.c_str(), &buf) == 0)
		{
			if(is_inode_valid(buf.st_ino, path))
			{
				device = buf.st_dev;
				inode  = buf.st_ino;
				valid  = true;
			}
		}
	}

	bool inode_t::operator< (inode_t const& rhs) const
	{
		return std::make_tuple(valid ? 1 : 0, inode, device) < std::make_tuple(rhs.valid ? 1 : 0, rhs.inode, rhs.device);
	}

	struct record_t
	{
		oak::uuid_t uuid;
		std::string path;
		inode_t inode;
		__weak OakDocument* document;
	};

	typedef std::shared_ptr<record_t> record_ptr;
}

@interface OakDocumentController ()
{
	std::mutex                        _lock;
	std::map<oak::uuid_t, record_ptr> _documents_by_uuid;
	std::map<std::string, record_ptr> _documents_by_path;
	std::map<inode_t, record_ptr>     _documents_by_inode;

	NSMutableDictionary* _rankedPaths;
	NSMutableDictionary* _rankedUUIDs;
	NSUInteger _lastLRURank;
	NSTimer* _saveRankedPathsTimer;
}
@end

@implementation OakDocumentController
+ (instancetype)sharedInstance
{
	static OakDocumentController* sharedInstance = [self new];
	return sharedInstance;
}

- (OakDocument*)untitledDocument
{
	return [self documentWithPath:nil];
}

- (OakDocument*)documentWithPath:(NSString*)aPath
{
	std::lock_guard<std::mutex> lock(_lock);
	OakDocument* doc;

	if(aPath)
	{
		auto pathIter = _documents_by_path.find(to_s(aPath));
		if(pathIter != _documents_by_path.end())
			doc = pathIter->second->document;

		if(!doc)
		{
			auto inodeIter = _documents_by_inode.find(inode_t(to_s(aPath)));
			if(inodeIter != _documents_by_inode.end())
				doc = inodeIter->second->document;
		}
	}

	if(!doc)
	{
		doc = [[OakDocument alloc] initWithPath:aPath];
		[self internalAddDocument:doc];
	}
	return doc;
}

- (OakDocument*)findDocumentWithIdentifier:(NSUUID*)anUUID
{
	std::lock_guard<std::mutex> lock(_lock);
	auto it = _documents_by_uuid.find(to_s(anUUID));
	if(it != _documents_by_uuid.end())
		return it->second->document;
	return nil;
}

- (void)register:(OakDocument*)aDocument
{
	std::lock_guard<std::mutex> lock(_lock);
	[self internalAddDocument:aDocument];
}

- (void)unregister:(OakDocument*)aDocument
{
	std::lock_guard<std::mutex> lock(_lock);
	[self internalRemoveDocument:aDocument];
}

- (void)update:(OakDocument*)aDocument
{
	std::lock_guard<std::mutex> lock(_lock);
	[self internalRemoveDocument:aDocument];
	[self internalAddDocument:aDocument];
}

- (NSUInteger)firstAvailableUntitledCount
{
	std::lock_guard<std::mutex> lock(_lock);

	std::set<NSUInteger> reserved;
	for(auto const& pair : _documents_by_uuid)
	{
		if(OakDocument* doc = pair.second->document)
		{
			if(!doc.path && !doc.customName)
				reserved.insert(doc.untitledCount);
		}
	}

	NSUInteger available = 1;
	while(reserved.find(available) != reserved.end())
		++available;
	return available;
}

- (NSArray<OakDocument*>*)documents
{
	std::lock_guard<std::mutex> lock(_lock);

	NSMutableArray<OakDocument*>* res = [NSMutableArray array];
	for(auto pair : _documents_by_uuid)
	{
		if(OakDocument* doc = pair.second->document)
			[res addObject:doc];
	}
	return res;
}

- (NSArray<OakDocument*>*)openDocuments
{
	NSArray<OakDocument*>* array = [self.documents filteredArrayUsingPredicate:[NSPredicate predicateWithFormat:@"isOpen == YES"]];
	return [array sortedArrayUsingComparator:^NSComparisonResult(OakDocument* lhs, OakDocument* rhs){
		if(!lhs.path && !rhs.path)
			return lhs.untitledCount < rhs.untitledCount ? NSOrderedAscending : (lhs.untitledCount > rhs.untitledCount ? NSOrderedDescending : NSOrderedSame);
		else if(lhs.path && rhs.path)
			return [lhs.path localizedCompare:rhs.path];
		else if(lhs.path)
			return NSOrderedDescending;
		else
			return NSOrderedAscending;
	}];
}

- (NSArray<OakDocument*>*)openDocumentsInDirectory:(NSString*)aDirectory
{
	NSArray<OakDocument*>* array = [self.documents filteredArrayUsingPredicate:[NSPredicate predicateWithFormat:@"path BEGINSWITH %@ OR directory BEGINSWITH %@", aDirectory, aDirectory]];
	return [array sortedArrayUsingComparator:^NSComparisonResult(OakDocument* lhs, OakDocument* rhs){
		if(lhs.untitledCount != rhs.untitledCount)
			return lhs.untitledCount < rhs.untitledCount ? NSOrderedAscending : NSOrderedDescending;
		return [lhs.displayName localizedCompare:rhs.displayName];
	}];
}

- (NSArray<OakDocument*>*)untitledDocumentsInDirectory:(NSString*)aDirectory
{
	return [[self openDocumentsInDirectory:aDirectory] filteredArrayUsingPredicate:[NSPredicate predicateWithFormat:@"path == NULL"]];
}

// =====================
// = Window Controller =
// =====================

- (void)showDocument:(OakDocument*)aDocument
{
	[self showDocument:aDocument andSelect:text::range_t::undefined inProject:nil bringToFront:YES];
}

- (void)showDocument:(OakDocument*)aDocument inProject:(NSUUID*)identifier bringToFront:(BOOL)bringToFront
{
	[self showDocument:aDocument andSelect:text::range_t::undefined inProject:identifier bringToFront:bringToFront];
}

// ======================
// = Last Recently Used =
// ======================

- (void)setupRankedPaths
{
	if(_rankedPaths)
		return;

	_rankedPaths = [NSMutableDictionary dictionary];
	_rankedUUIDs = [NSMutableDictionary dictionary];

	NSArray* paths = [NSUserDefaults.standardUserDefaults stringArrayForKey:@"LRUDocumentPaths"];

	// LEGACY format used by 2.0-beta.12.11 and earlier
	if(!paths)
	{
		NSDictionary* dictionary = [NSUserDefaults.standardUserDefaults dictionaryForKey:@"LRUDocumentPaths"];
		paths = dictionary[@"paths"];
	}

	for(NSString* path in [paths reverseObjectEnumerator])
		_rankedPaths[path] = @(++_lastLRURank);
}

- (void)saveRankedPathsTimerDidFire:(NSTimer*)aTimer
{
	_saveRankedPathsTimer = nil;

	std::map<NSInteger, NSString*> ordered;
	for(NSString* path in _rankedPaths)
		ordered.emplace(-[_rankedPaths[path] intValue], path);
	NSMutableArray* array = [NSMutableArray array];
	for(auto const& pair : ordered)
	{
		[array addObject:pair.second];
		if(array.count == 50)
			break;
	}
	[NSUserDefaults.standardUserDefaults setObject:array forKey:@"LRUDocumentPaths"];
}

- (NSInteger)lruRankForDocument:(OakDocument*)aDocument
{
	[self setupRankedPaths];
	return aDocument.path ? [_rankedPaths[aDocument.path] intValue] : [_rankedUUIDs[aDocument.identifier] intValue];
}

- (void)didTouchDocument:(OakDocument*)aDocument
{
	if(!aDocument)
		return;

	[self setupRankedPaths];
	if(aDocument.path)
			_rankedPaths[aDocument.path] = @(++_lastLRURank);
	else	_rankedUUIDs[aDocument.identifier] = @(++_lastLRURank);

	[_saveRankedPathsTimer invalidate];
	_saveRankedPathsTimer = [NSTimer scheduledTimerWithTimeInterval:2 target:self selector:@selector(saveRankedPathsTimerDidFire:) userInfo:nil repeats:NO];
}

// ======================
// = Directory Scanning =
// ======================

- (void)enumerateDocumentsAtPath:(NSString*)aDirectory options:(NSDictionary*)someOptions usingBlock:(void(^)(OakDocument* document, BOOL* stop))block;
{
	[self enumerateDocumentsAtPaths:@[ aDirectory ] options:someOptions usingBlock:block];
}

- (void)enumerateDocumentsAtPaths:(NSArray*)items options:(NSDictionary*)someOptions usingBlock:(void(^)(OakDocument* document, BOOL* stop))block
{
	BOOL stop = NO;

	BOOL followDirectoryLinks = [someOptions[kSearchFollowDirectoryLinksKey] boolValue];
	BOOL followFileLinks      = [someOptions[kSearchFollowFileLinksKey] boolValue] || !someOptions[kSearchFollowFileLinksKey];
	BOOL depthFirst           = [someOptions[kSearchDepthFirstSearchKey] boolValue];
	BOOL ignoreOrdering       = [someOptions[kSearchIgnoreOrderingKey] boolValue];

	static std::vector<std::pair<NSString*, size_t>> const map = {
		{ kSearchExcludeDirectoryGlobsKey, path::kPathItemDirectory | path::kPathItemExclude },
		{ kSearchExcludeFileGlobsKey,      path::kPathItemFile      | path::kPathItemExclude },
		{ kSearchExcludeGlobsKey,          path::kPathItemAny       | path::kPathItemExclude },
		{ kSearchDirectoryGlobsKey,        path::kPathItemDirectory                          },
		{ kSearchFileGlobsKey,             path::kPathItemFile                               },
		{ kSearchGlobsKey,                 path::kPathItemAny                                },
	};

	path::glob_list_t globs;
	for(auto const& pair : map)
	{
		for(NSString* glob in someOptions[pair.first])
			globs.add_glob(to_s(glob), pair.second);
	}

	std::set<std::pair<dev_t, ino_t>> didScan;
	std::deque<std::string> dirs;
	std::vector<std::string> links;

	for(NSString* item in items)
	{
		struct stat buf;
		if(lstat([item fileSystemRepresentation], &buf) != -1)
		{
			if(S_ISDIR(buf.st_mode) && didScan.emplace(buf.st_dev, buf.st_ino).second)
				dirs.push_back(to_s(item));
			else if(S_ISLNK(buf.st_mode))
				links.push_back(to_s(item));
			else if(S_ISREG(buf.st_mode) && didScan.emplace(buf.st_dev, buf.st_ino).second)
			{
				block([OakDocument documentWithPath:item], &stop);
				if(stop)
					break;
			}
		}
		else
		{
			perrorf("OakDocumentController: lstat(\"%s\")", [item fileSystemRepresentation]);
		}
	}

	NSMutableSet<NSUUID*>* didSee = [NSMutableSet set];
	for(std::string const& dir : dirs)
	{
		NSArray* documents = ignoreOrdering ? [self openDocumentsInDirectory:to_ns(dir)] : [self untitledDocumentsInDirectory:to_ns(dir)];
		for(OakDocument* document in documents)
		{
			if([didSee containsObject:document.identifier])
				continue;
			[didSee addObject:document.identifier];

			if(document.path)
			{
				std::string const path = to_s(document.path);
				if(globs.exclude(path, path::kPathItemFile))
					continue;

				struct stat buf;
				if(lstat([document.path fileSystemRepresentation], &buf) == -1)
				{
					perrorf("lstat(\"%s\")", [document.path fileSystemRepresentation]);
					continue;
				}

				if(didScan.emplace(buf.st_dev, buf.st_ino).second == false)
					continue;
			}

			block(document, &stop);
			if(stop)
				return;
		}
	}

	while(stop == NO && !dirs.empty())
	{
		std::string dir = dirs.front();
		dirs.pop_front();

		struct stat buf;
		if(lstat(dir.c_str(), &buf) == -1) // get st_dev so we don’t need to stat each path entry (unless it is a symbolic link)
		{
			perrorf("OakDocumentController: lstat(\"%s\")", dir.c_str());
			continue;
		}

		ASSERT(S_ISDIR(buf.st_mode) || S_ISLNK(buf.st_mode));

		std::vector<std::string> newDirs;
		std::set<std::string, text::less_t> files;
		for(auto const& it : path::entries(dir))
		{
			std::string const& path = path::join(dir, it->d_name);
			if(it->d_type == DT_DIR)
			{
				if(globs.exclude(path, path::kPathItemDirectory))
					continue;

				if(didScan.emplace(buf.st_dev, it->d_ino).second)
					newDirs.push_back(path);
			}
			else if(it->d_type == DT_REG)
			{
				if(globs.exclude(path, path::kPathItemFile))
					continue;

				if(didScan.emplace(buf.st_dev, it->d_ino).second)
					files.emplace(path);
			}
			else if(it->d_type == DT_LNK && (followDirectoryLinks || followFileLinks))
			{
				links.push_back(path); // handle later since link may point to another device plus if link is “local” and will be seen later, we reported the local path rather than this link
			}
		}

		std::sort(newDirs.begin(), newDirs.end(), text::less_t());
		dirs.insert(depthFirst ? dirs.begin() : dirs.end(), newDirs.begin(), newDirs.end());

		if(dirs.empty())
		{
			for(auto const& link : links)
			{
				std::string const path = path::resolve(link);
				if(lstat(path.c_str(), &buf) != -1)
				{
					if(S_ISDIR(buf.st_mode) && followDirectoryLinks && didScan.emplace(buf.st_dev, buf.st_ino).second)
					{
						if(globs.exclude(path, path::kPathItemDirectory))
							continue;
						dirs.push_back(path);
					}
					else if(S_ISREG(buf.st_mode) && followFileLinks)
					{
						if(globs.exclude(path, path::kPathItemFile))
							continue;

						if(didScan.emplace(buf.st_dev, buf.st_ino).second)
							files.emplace(path);
					}
				}
				else
				{
					perrorf("OakDocumentController: path::resolve(\"%s\") → lstat(\"%s\")", link.c_str(), path.c_str());
				}
			}
			links.clear();
		}

		for(auto const& file : files)
		{
			block([OakDocument documentWithPath:to_ns(file)], &stop);
			if(stop)
				break;
		}
	}
}

// ===================================================
// = Mutex must be locked when calling the following =
// ===================================================

- (void)internalAddDocument:(OakDocument*)doc
{
	auto r = std::make_shared<record_t>();
	r->uuid     = to_s(doc.identifier);
	r->path     = to_s(doc.path);
	r->inode    = inode_t(r->path);
	r->document = doc;

	ASSERT(_documents_by_uuid.find(r->uuid) == _documents_by_uuid.end());
	_documents_by_uuid.emplace(r->uuid, r);

	if(r->path != NULL_STR)
		_documents_by_path[r->path] = r;

	if(r->inode)
		_documents_by_inode[r->inode] = r;
}

- (void)internalRemoveDocument:(OakDocument*)doc
{
	oak::uuid_t const uuid = to_s(doc.identifier);
	auto it = _documents_by_uuid.find(uuid);
	ASSERT(it != _documents_by_uuid.end());
	if(it != _documents_by_uuid.end())
	{
		if(it->second->inode)
		{
			auto inodeIter = _documents_by_inode.find(it->second->inode);
			if(inodeIter != _documents_by_inode.end() && inodeIter->second->uuid == uuid)
				_documents_by_inode.erase(inodeIter);
		}

		if(it->second->path != NULL_STR)
		{
			auto pathIter = _documents_by_path.find(it->second->path);
			if(pathIter != _documents_by_path.end() && pathIter->second->uuid == uuid)
				_documents_by_path.erase(pathIter);
		}

		_documents_by_uuid.erase(it);
	}
}
@end
