#import "FSSCMDataSource.h"
#import "FSItem.h"
#import <scm/scm.h>
#import <OakAppKit/NSImage Additions.h>
#import <OakFoundation/NSString Additions.h>
#import <io/path.h>
#import <text/encode.h>
#import <text/format.h>
#import <oak/oak.h>

static NSImage* SCMFolderIcon ()
{
	NSImage* res = [[NSWorkspace sharedWorkspace] iconForFileType:NSFileTypeForHFSTypeCode(kGenericFolderIcon)];
	res.size = NSMakeSize(16, 16);
	return res;
}

static NSURL* URLAppend (NSURL* base, NSString* relativePath)
{
	return [NSURL URLWithString:[[NSURL URLWithString:relativePath relativeToURL:base] absoluteString]];
}

static NSArray* convert (std::map<std::string, scm::status::type> const& pathsMap, std::string const& wcPath, NSArray* oldItems, bool hideSCMBadge = false)
{
	NSMutableDictionary* cache = [NSMutableDictionary new];
	for(FSItem* item in oldItems)
		cache[item.url] = item;

	std::vector<std::string> paths;
	std::transform(pathsMap.begin(), pathsMap.end(), back_inserter(paths), [](std::pair<std::string, scm::status::type> const& p){ return p.first; });

	auto parents = path::disambiguate(paths);
	auto parent = parents.begin();

	NSMutableArray* res = [NSMutableArray array];
	for(auto const& pair : pathsMap)
	{
		NSURL* url = [NSURL fileURLWithPath:[NSString stringWithCxxString:pair.first]];
		FSItem* item = cache[url];
		if(!item)
			item = [FSItem itemWithURL:url];

		item.displayName = [NSString stringWithCxxString:path::display_name(pair.first, *parent++)];
		item.target      = url;
		item.labelIndex  = path::label_index(pair.first);
		item.toolTip     = [NSString stringWithCxxString:path::relative_to(pair.first, wcPath)];
		item.leaf        = YES;
		item.scmStatus   = hideSCMBadge ? scm::status::none : pair.second;
		item.missing     = pair.second == scm::status::deleted;

		[res addObject:item];
	}
	return res;
}

@interface FSSCMItem : FSItem
@property (nonatomic, weak) FSDataSource* dataSource;
@property (nonatomic) SEL selector;
@end

@implementation FSSCMItem
- (id)initWithURL:(NSURL*)anURL dataSource:(FSDataSource*)aDataSource selector:(SEL)aSelector
{
	if((self = [super initWithURL:anURL]))
	{
		_dataSource = aDataSource;
		_selector   = aSelector;
	}
	return self;
}

- (void)loadChildren:(FSDataSource*)dataSource completionHandler:(void(^)(NSArray*))block
{
	auto fn = (NSArray*(*)(id, SEL, NSArray*))[_dataSource methodForSelector:_selector];
	block(fn(_dataSource, _selector, self.children));
}
@end

@interface FSSCMDataSource ()
{
	OBJC_WATCH_LEAKS(FSSCMDataSource);
	NSURL* _url;
	NSUInteger _options;
	scm::info_ptr _scmInfo;
	NSMapTable* _items;
}
@end

@implementation FSSCMDataSource
+ (NSURL*)scmURLWithPath:(NSString*)aPath
{
	NSURL* url;
	if(scm::scm_enabled_for_path([aPath fileSystemRepresentation]))
	{
		std::string root = scm::root_for_path([aPath fileSystemRepresentation]);
		if(root != NULL_STR)
			url = [NSURL URLWithString:[NSString stringWithCxxString:"scm://localhost" + encode::url_part(root, "/") + "/"]];
		else	url = [NSURL URLWithString:[@"scm://locahost" stringByAppendingString:[aPath stringByAppendingString:@"?status=unversioned"]]];
	}
	else
	{
		url = [NSURL URLWithString:[@"scm://locahost" stringByAppendingString:[aPath stringByAppendingString:@"?status=disabled"]]];
	}
	return url;
}

- (id)initWithURL:(NSURL*)anURL options:(NSUInteger)someOptions
{
	if((self = [super init]))
	{
		_url     = anURL;
		_options = someOptions;
		_scmInfo = scm::info([[anURL path] fileSystemRepresentation]);
		_items   = [NSMapTable strongToWeakObjectsMapTable];

		NSArray* query = [[anURL query] componentsSeparatedByString:@"="];
		self.rootItem = [self itemOfType:[query lastObject] ?: @"all"];

		if(_scmInfo)
		{
			__weak FSSCMDataSource* weakSelf = self;
			_scmInfo->add_callback(^(scm::info_t const&){
				[weakSelf postReloadNotification];
			});
		}
	}
	return self;
}

- (void)postReloadNotification
{
	for(id key in _items)
	{
		if(FSItem* item = [_items objectForKey:key])
		{
			if(item.children)
				[[NSNotificationCenter defaultCenter] postNotificationName:FSItemDidReloadNotification object:self userInfo:@{ @"item" : item }];
		}
	}
}

- (NSArray*)expandedURLs
{
	return @[ URLAppend(_url, @"?status=unstaged"), URLAppend(_url, @"?status=untracked") ];
}

- (NSArray*)unstagedItems:(NSArray*)oldItems
{
	std::map<std::string, scm::status::type> unstagedPaths;
	for(auto pair : _scmInfo->status())
	{
		if(pair.second & (scm::status::modified|scm::status::added|scm::status::deleted|scm::status::conflicted|scm::status::unversioned))
		{
			if(!(pair.second & scm::status::unversioned))
				unstagedPaths.insert(pair);
		}
	}

	if(!_scmInfo->tracks_directories())
	{
		std::vector<std::string> parents;

		std::string child = NULL_STR;
		for(auto it = unstagedPaths.rbegin(); it != unstagedPaths.rend(); ++it)
		{
			if(path::is_child(child, it->first))
					parents.push_back(it->first);
			else	child = it->first;
		}

		for(auto const& path : parents)
			unstagedPaths.erase(path);
	}

	return [FSDataSource sortArray:convert(unstagedPaths, _scmInfo->root_path(), oldItems) usingOptions:_options];
}

- (NSArray*)untrackedItems:(NSArray*)oldItems
{
	std::map<std::string, scm::status::type> untrackedPaths;
	for(auto pair : _scmInfo->status())
	{
		if(pair.second & (scm::status::modified|scm::status::added|scm::status::deleted|scm::status::conflicted|scm::status::unversioned))
		{
			if(pair.second & scm::status::unversioned)
				untrackedPaths.insert(pair);
		}
	}

	if(!_scmInfo->tracks_directories())
	{
		std::vector<std::string> children;

		std::string parent = NULL_STR;
		for(auto const& pair : untrackedPaths)
		{
			if(path::is_child(pair.first, parent))
					children.push_back(pair.first);
			else	parent = pair.first;
		}

		for(auto const& path : children)
			untrackedPaths.erase(path);
	}

	return [FSDataSource sortArray:convert(untrackedPaths, _scmInfo->root_path(), oldItems, true) usingOptions:_options];
}

- (NSArray*)rootItems:(NSArray*)oldItems
{
	return @[ [self itemOfType:@"unstaged"], [self itemOfType:@"untracked"] ];
}

- (FSItem*)itemOfType:(NSString*)type
{
	FSItem* res = nil;
	if([type isEqualToString:@"all"])
	{
		NSString* name = [[NSFileManager defaultManager] displayNameAtPath:[_url path]];
		if(!_scmInfo->dry())
		{
			auto const vars   = _scmInfo->scm_variables();
			auto const branch = vars.find("TM_SCM_BRANCH");
			if(branch != vars.end())
				name = [NSString stringWithFormat:@"%@ (%@)", name, [NSString stringWithCxxString:branch->second]];
		}

		res = [[FSSCMItem alloc] initWithURL:URLAppend(_url, @"?status=all") dataSource:self selector:@selector(rootItems:)];
		res.icon        = [NSImage imageNamed:@"SCMTemplate" inSameBundleAsClass:[self class]];
		res.displayName = name;
		res.group       = YES;
	}
	else if([type isEqualToString:@"unstaged"])
	{
		res = [[FSSCMItem alloc] initWithURL:URLAppend(_url, @"?status=unstaged") dataSource:self selector:@selector(unstagedItems:)];
		res.icon        = SCMFolderIcon();
		res.displayName = @"Uncommitted Changes";
		res.group       = YES;

		[_items setObject:res forKey:res.url];
	}
	else if([type isEqualToString:@"untracked"])
	{
		res = [[FSSCMItem alloc] initWithURL:URLAppend(_url, @"?status=untracked") dataSource:self selector:@selector(untrackedItems:)];
		res.icon        = SCMFolderIcon();
		res.displayName = @"Untracked Items";
		res.group       = YES;

		[_items setObject:res forKey:res.url];
	}
	else
	{
		res             = [FSItem itemWithURL:_url];
		res.icon        = [NSImage imageNamed:@"SCMTemplate" inSameBundleAsClass:[self class]];
		res.displayName = [NSString stringWithFormat:@"%@ (%@)", [[NSFileManager defaultManager] displayNameAtPath:[_url path]], type];
	}
	return res;
}

- (void)reloadItem:(FSItem*)anItem completionHandler:(void(^)(NSArray*))block
{
	if([anItem respondsToSelector:@selector(loadChildren:completionHandler:)])
		[(FSSCMItem*)anItem loadChildren:self completionHandler:block];
}

- (BOOL)unloadItem:(FSItem*)anItem
{
	anItem.children = nil;
	return YES;
}
@end
