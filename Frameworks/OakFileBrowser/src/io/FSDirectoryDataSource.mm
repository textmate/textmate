#import "FSDirectoryDataSource.h"
#import "FSItem.h"
#import <OakFoundation/NSString Additions.h>
#import <OakAppKit/OakFileIconImage.h>
#import <OakAppKit/OakFileManager.h>
#import <Preferences/Keys.h>
#import <io/entries.h>
#import <io/events.h>
#import <io/path.h>
#import <regexp/glob.h>
#import <regexp/format_string.h>
#import <settings/settings.h>
#import <scm/scm.h>
#import <text/encode.h>
#import <ns/ns.h>
#import <oak/debug.h>

OAK_DEBUG_VAR(FileBrowser_DSDirectory);

struct tracking_t;

@interface FSFileItem : FSItem
{
	std::unique_ptr<tracking_t> _tracking;
}
@property (nonatomic) dev_t device;
@property (nonatomic) ino_t inode;
@property (nonatomic) BOOL missing;
- (void)internalNeedsReload;
@end

@interface FSDirectoryDataSource ()
{
	OBJC_WATCH_LEAKS(FSDirectoryDataSource);
}
@property (nonatomic) NSUInteger dataSourceOptions;
@end

struct tracking_t : fs::event_callback_t
{
	tracking_t (FSDirectoryDataSource* dataSource, FSFileItem* item, std::string const& path) : _data_source(dataSource), _item(item), _path(path)
	{
		_scm_info = scm::info(_path);
		if(_scm_info)
		{
			_scm_info->add_callback(^(scm::info_t const& info){

				std::set<std::string> pathsShown, pathsDeleted, pathsMissingOnDisk;
				for(FSFileItem* item in _item.children)
				{
					if(item.missing)
						pathsMissingOnDisk.insert(to_s(item.url.path));
					pathsShown.insert(to_s(item.url.path));
				}

				for(auto pair : info.status())
				{
					if(pair.second == scm::status::deleted && _path == path::parent(pair.first))
						pathsDeleted.insert(pair.first);
				}

				if(std::includes(pathsShown.begin(), pathsShown.end(), pathsDeleted.begin(), pathsDeleted.end()) && std::includes(pathsDeleted.begin(), pathsDeleted.end(), pathsMissingOnDisk.begin(), pathsMissingOnDisk.end()))
				{
					for(FSFileItem* item in _item.children)
					{
						scm::status::type newStatus = info.status(to_s([item.url path]));
						if(newStatus != item.scmStatus)
						{
							item.scmStatus = newStatus;
							[[NSNotificationCenter defaultCenter] postNotificationName:FSItemDidReloadNotification object:_data_source userInfo:@{ @"item" : item }];
						}
					}
				}
				else
				{
					[_item internalNeedsReload];
				}
			});
		}

		fs::watch(_path, this);
	}

	~tracking_t ()
	{
		fs::unwatch(_path, this);
	}

	void did_change (std::string const& path, std::string const& observedPath, uint64_t eventId, bool recursive)
	{
		if(_path == path)
			[_item internalNeedsReload];
	}

	__weak FSDirectoryDataSource* _data_source;
	__weak FSFileItem* _item;

	std::string const _path;
	scm::info_ptr _scm_info;
	struct timespec _last_modified = { };
};

@implementation FSFileItem
- (void)dealloc
{
	[[NSNotificationCenter defaultCenter] removeObserver:self];
}

- (BOOL)isEqual:(FSFileItem*)otherObject
{
	return [super isEqual:otherObject] && otherObject.missing == _missing;
}

- (void)loadChildren:(FSDirectoryDataSource*)dataSource completionHandler:(void(^)(NSArray*))block
{
	struct fs_item_t
	{
		fs_item_t (dev_t device, dirent const* entry, std::string const& path) : device(device), inode(entry->d_fileno), path(path), target(NULL_STR), label(0), is_directory(false), is_link(false), treat_as_directory(false), sort_as_directory(false)
		{
			label = path::label_index(path);

			if(entry->d_type == DT_LNK)
			{
				std::string const resolved = path::resolve_head(path);
				uint32_t const flags = path::info(resolved);

				is_link           = true;
				sort_as_directory = (flags & (path::flag::directory|path::flag::package)) == path::flag::directory;
				target            = to_s([[NSURL fileURLWithPath:[NSString stringWithCxxString:resolved] isDirectory:path::flag::directory|path::flag::package] absoluteString]);
			}
			else if(entry->d_type == DT_DIR)
			{
				uint32_t const flags = path::info(path);

				is_directory       = true;
				sort_as_directory  = !(flags & path::flag::package);
				treat_as_directory = !(flags & (path::flag::package|path::flag::hidden_volume));
				target             = to_s([[NSURL fileURLWithPath:[NSString stringWithCxxString:path] isDirectory:path::flag::directory|path::flag::package] absoluteString]);

				if(path::extension(path) == ".xcodeproj")
					target = "xcodeproj://localhost" + encode::url_part(path, "/") + "/";
			}
			else if(entry->d_type == DT_REG)
			{
				if(path::extension(path) == ".savedSearch")
					target = "search://localhost" + encode::url_part(path, "/") + "/";
			}
		}

		dev_t device;
		ino_t inode;
		std::string path;
		std::string target;
		size_t label;
		bool is_directory;
		bool is_link;
		bool treat_as_directory;
		bool sort_as_directory;
	};

	std::string const dir = [[self.url path] fileSystemRepresentation];
	if(!_tracking)
	{
		_tracking.reset(new tracking_t(dataSource, self, dir));

		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(applicationDidBecomeActive:) name:NSApplicationDidBecomeActiveNotification object:NSApp];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(fileManagerDidChangeContentsOfDirectory:) name:OakFileManagerDidChangeContentsOfDirectory object:nil];
	}

	bool allowExpandingLinks = [[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsAllowExpandingLinksKey];
	bool includeHidden       = (dataSource.dataSourceOptions & kFSDataSourceOptionIncludeHidden) == kFSDataSourceOptionIncludeHidden;

	path::glob_list_t globs;
	if(!includeHidden)
	{
		settings_t const& settings = settings_for_path(NULL_STR, "", dir);

		globs.add_exclude_glob(settings.get(kSettingsExcludeDirectoriesInBrowserKey), path::kPathItemDirectory);
		globs.add_exclude_glob(settings.get(kSettingsExcludeDirectoriesKey),          path::kPathItemDirectory);
		globs.add_exclude_glob(settings.get(kSettingsExcludeFilesInBrowserKey),       path::kPathItemFile);
		globs.add_exclude_glob(settings.get(kSettingsExcludeFilesKey),                path::kPathItemFile);
		globs.add_exclude_glob(settings.get(kSettingsExcludeInBrowserKey),            path::kPathItemAny);
		globs.add_exclude_glob(settings.get(kSettingsExcludeKey),                     path::kPathItemAny);

		globs.add_include_glob(settings.get(kSettingsIncludeDirectoriesInBrowserKey), path::kPathItemDirectory);
		globs.add_include_glob(settings.get(kSettingsIncludeDirectoriesKey),          path::kPathItemDirectory);
		globs.add_include_glob(settings.get(kSettingsIncludeFilesInBrowserKey),       path::kPathItemFile);
		globs.add_include_glob(settings.get(kSettingsIncludeFilesKey),                path::kPathItemFile);
		globs.add_include_glob(settings.get(kSettingsIncludeInBrowserKey),            path::kPathItemAny);
		globs.add_include_glob(settings.get(kSettingsIncludeKey),                     path::kPathItemAny);
	}

	dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_LOW, 0), ^{
		struct stat buf;
		if(stat(dir.c_str(), &buf) == 0)
		{
			std::vector<fs_item_t> newItems;
			for(auto entry : path::entries(dir))
			{
				std::string const path = path::join(dir, entry->d_name);
				if(!includeHidden && (globs.exclude(path, entry->d_type == DT_DIR ? path::kPathItemDirectory : path::kPathItemFile) || (path::info(path) & path::flag::hidden)))
					continue;
				newItems.emplace_back(buf.st_dev, entry, path);
			}

			dispatch_async(dispatch_get_main_queue(), ^{
				std::map< std::pair<dev_t, ino_t>, FSFileItem* > existingItems;
				for(FSFileItem* item in self.children)
					existingItems.emplace(std::make_pair(item.device, item.inode), item);

				std::set<std::string> pathsOnDisk;
				scm::info_ptr scmInfo = _tracking ? _tracking->_scm_info : scm::info_ptr();

				NSMutableArray* array = [NSMutableArray array];
				for(auto const& fsItem : newItems)
				{
					FSFileItem* item;
					auto it = existingItems.find(std::make_pair(fsItem.device, fsItem.inode));
					if(it != existingItems.end())
					{
						item = it->second;
						existingItems.erase(it);
					}
					else
					{
						item = [FSFileItem new];
						item.device = fsItem.device;
						item.inode  = fsItem.inode;
					}

					OakFileIconImage* image = [[OakFileIconImage alloc] initWithSize:NSMakeSize(16, 16)];
					image.path      = [NSString stringWithCxxString:fsItem.path];
					image.directory = fsItem.is_directory || (fsItem.is_link && fsItem.sort_as_directory);
					image.alias     = fsItem.is_link;
					if(scmInfo)
						image.scmStatus = scmInfo->status(fsItem.path);

					item.url          = [NSURL fileURLWithPath:[NSString stringWithCxxString:fsItem.path] isDirectory:fsItem.is_directory];
					item.displayName  = [NSString stringWithCxxString:path::display_name(fsItem.path)];
					item.icon         = image;
					item.labelIndex   = fsItem.label;
					item.sortAsFolder = fsItem.sort_as_directory;
					item.leaf         = !fsItem.treat_as_directory;
					item.link         = fsItem.is_link;
					item.target       = fsItem.target != NULL_STR ? [NSURL URLWithString:[NSString stringWithCxxString:fsItem.target]] : nil;

					if(allowExpandingLinks && fsItem.is_link && fsItem.sort_as_directory && item.leaf)
						item.leaf = NO;

					[array addObject:item];
					pathsOnDisk.insert(fsItem.path);
				}

				if(scmInfo)
				{
					for(auto pair : scmInfo->status())
					{
						if(!(pair.second & scm::status::deleted) || dir != path::parent(pair.first) || pathsOnDisk.find(pair.first) != pathsOnDisk.end())
							continue;

						OakFileIconImage* image = [[OakFileIconImage alloc] initWithSize:NSMakeSize(16, 16)];
						image.path      = [NSString stringWithCxxString:pair.first];
						image.exists    = NO;
						image.scmStatus = pair.second;

						FSFileItem* item = [FSFileItem new];
						item.url         = [NSURL fileURLWithPath:[NSString stringWithCxxString:pair.first] isDirectory:NO];
						item.displayName = [NSString stringWithCxxString:path::name(pair.first)];
						item.icon        = image;
						item.leaf        = YES;
						item.missing     = YES;

						[array addObject:item];
					}
				}

				if(_tracking)
					_tracking->_last_modified = buf.st_mtimespec;

				block([FSDataSource sortArray:array usingOptions:dataSource.dataSourceOptions]);
			});
		}
	});
}

- (void)unloadChildren:(FSDirectoryDataSource*)dataSource
{
	[[NSNotificationCenter defaultCenter] removeObserver:self];
	_tracking.reset();
	self.children = nil;
}

- (void)applicationDidBecomeActive:(NSNotification*)aNotification
{
	std::string const dir = [[self.url path] fileSystemRepresentation];
	struct timespec lastModified = _tracking->_last_modified;

	dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_LOW, 0), ^{
		struct stat buf;
		if(stat(dir.c_str(), &buf) == 0 && (buf.st_mtimespec.tv_sec != lastModified.tv_sec || buf.st_mtimespec.tv_nsec != lastModified.tv_nsec))
		{
			dispatch_async(dispatch_get_main_queue(), ^{
				[self internalNeedsReload];
			});
		}
	});
}

- (void)fileManagerDidChangeContentsOfDirectory:(NSNotification*)aNotification
{
	NSDictionary* userInfo = [aNotification userInfo];
	NSString* dir = userInfo[OakFileManagerPathKey];
	if([dir isEqualToString:[self.url path]])
		[self internalNeedsReload];
}

- (void)internalNeedsReload
{
	if(_tracking)
		[[NSNotificationCenter defaultCenter] postNotificationName:FSItemDidReloadNotification object:_tracking->_data_source userInfo:@{ @"item" : self }];
}
@end

@implementation FSDirectoryDataSource
- (id)initWithURL:(NSURL*)anURL options:(NSUInteger)someOptions
{
	if((self = [super init]))
	{
		self.dataSourceOptions = someOptions;
		self.rootItem = [FSFileItem itemWithURL:anURL];
	}
	return self;
}

- (void)reloadItem:(FSItem*)anItem completionHandler:(void(^)(NSArray*))block
{
	[((FSFileItem*)anItem) loadChildren:self completionHandler:block];
}

- (BOOL)unloadItem:(FSItem*)anItem
{
	D(DBF_FileBrowser_DSDirectory, bug("%s\n", [[[anItem url] path] UTF8String]););
	[((FSFileItem*)anItem) unloadChildren:self];
	return YES;
}
@end
