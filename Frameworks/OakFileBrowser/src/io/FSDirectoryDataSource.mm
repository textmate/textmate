#import "FSDirectoryDataSource.h"
#import "FSItem.h"
#import <OakFoundation/NSArray Additions.h>
#import <OakFoundation/NSString Additions.h>
#import <OakAppKit/OakFileIconImage.h>
#import <OakAppKit/OakFileManager.h>
#import <Preferences/Keys.h>
#import <oak/server.h>
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

@interface FSFileItem : FSItem
@property (nonatomic) dev_t device;
@property (nonatomic) ino_t inode;
@end

@implementation FSFileItem
@end

struct item_record_t;
typedef std::shared_ptr<item_record_t> item_record_ptr;

@interface FSDirectoryDataSource ()
{
	OBJC_WATCH_LEAKS(FSDirectoryDataSource);
	std::map<std::string, item_record_ptr> visibleItems;
}
@property (nonatomic) NSUInteger dataSourceOptions;
- (void)setLastModified:(struct timespec)lastModified forPath:(std::string const&)aPath;
- (void)internalReloadItem:(FSItem*)anItem requested:(BOOL)flag;
- (void)lostItems:(NSArray*)someItems;
@end

struct item_record_t : fs::event_callback_t
{
	item_record_t (FSDirectoryDataSource* dataSource, FSItem* item)
	{
		_data_source = dataSource;
		_item        = (FSFileItem*)item;
		_path        = to_s([item.url path]);
		_scm_info    = scm::info(_path);

		if(_scm_info)
		{
			_scm_info->add_callback(^(scm::info_t const& info){
				if(!_item.children)
					return;

				std::set<std::string> pathsShown, pathsDeleted, pathsMissingOnDisk;
				for(FSFileItem* item in _item.children)
				{
					if(!item.icon.exists)
						pathsMissingOnDisk.insert(to_s(item.icon.path));
					pathsShown.insert(to_s(item.icon.path));
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
						if(newStatus != item.icon.scmStatus)
						{
							item.icon.scmStatus = newStatus;
							[[NSNotificationCenter defaultCenter] postNotificationName:FSItemDidReloadNotification object:_data_source userInfo:@{ @"item" : item }];
						}
					}
				}
				else
				{
					reload(false);
				}
			});
		}

		fs::watch(_path, this);
	}

	~item_record_t ()
	{
		fs::unwatch(_path, this);
	}

	void reload (bool reloadWasRequested = false)
	{
		internal_reload(reloadWasRequested);
	}

	void set_last_modified (struct timespec lastModified) { _last_modified = lastModified; }
	struct timespec last_modified () const                { return _last_modified; }

private:
	void did_change (std::string const& path, std::string const& observedPath, uint64_t eventId, bool recursive)
	{
		if(_path == path)
			reload(false);
	}

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
				target            = "file://localhost" + encode::url_part(resolved, "/") + ((flags & (path::flag::directory|path::flag::package)) ? "/" : "");
			}
			else if(entry->d_type == DT_DIR)
			{
				uint32_t const flags = path::info(path);

				is_directory       = true;
				sort_as_directory  = !(flags & path::flag::package);
				treat_as_directory = !(flags & (path::flag::package|path::flag::hidden_volume));
				target             = "file://localhost" + encode::url_part(path, "/") + "/";

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

	static void add_item_and_children (FSItem* item, NSMutableArray* array)
	{
		[array addObject:item];
		if(item.leaf)
			return;
		for(FSItem* child in item.children)
			add_item_and_children(child, array);
	}

	static void async_reload (FSDirectoryDataSource* dataSource, FSItem* rootItem, scm::weak_info_ptr weakSCMInfo, bool reloadWasRequested)
	{
		BOOL allowExpandingLinks = [[[NSUserDefaults standardUserDefaults] objectForKey:kUserDefaultsAllowExpandingLinksKey] boolValue];

		std::string const dir = to_s([rootItem.url path]);
		bool includeHidden = (dataSource.dataSourceOptions & kFSDataSourceOptionIncludeHidden) == kFSDataSourceOptionIncludeHidden;

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
					for(FSFileItem* item in rootItem.children)
						existingItems.emplace(std::make_pair(item.device, item.inode), item);

					std::set<std::string> pathsOnDisk;
					scm::info_ptr scmInfo = weakSCMInfo.lock();

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
						item.name         = [NSString stringWithCxxString:path::display_name(fsItem.path)];
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
							item.url  = [NSURL fileURLWithPath:[NSString stringWithCxxString:pair.first] isDirectory:NO];
							item.name = [NSString stringWithCxxString:path::name(pair.first)];
							item.icon = image;
							item.leaf = YES;

							[array addObject:item];
						}
					}

					NSMutableArray* lostItems = [NSMutableArray array];
					for(auto const& pair : existingItems)
						add_item_and_children(pair.second, lostItems);
					[dataSource lostItems:lostItems];
					[dataSource setLastModified:buf.st_mtimespec forPath:dir];

					[[NSNotificationCenter defaultCenter] postNotificationName:FSItemDidReloadNotification object:dataSource userInfo:@{ @"item" : rootItem, @"children" : [FSDataSource sortArray:array usingOptions:dataSource.dataSourceOptions], @"recursive" : @YES, @"requested" : @(reloadWasRequested) }];
				});
			}
		});
	}

	void internal_reload (bool reloadWasRequested)
	{
		async_reload(_data_source, _item, _scm_info, reloadWasRequested);
	}

	__weak FSDirectoryDataSource* _data_source;
	FSFileItem* _item;
	std::string _path;
	struct timespec _last_modified;
	scm::info_ptr _scm_info;
};

@implementation FSDirectoryDataSource
- (id)initWithURL:(NSURL*)anURL options:(NSUInteger)someOptions
{
	if((self = [super init]))
	{
		self.dataSourceOptions = someOptions;
		self.rootItem = [FSFileItem itemWithURL:anURL];

		[self internalReloadItem:self.rootItem requested:NO];

		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(applicationDidBecomeActive:) name:NSApplicationDidBecomeActiveNotification object:NSApp];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(fileManagerDidChangeContentsOfDirectory:) name:OakFileManagerDidChangeContentsOfDirectory object:nil];
	}
	return self;
}

- (void)dealloc
{
	[[NSNotificationCenter defaultCenter] removeObserver:self];
}

- (void)applicationDidBecomeActive:(NSNotification*)aNotification
{
	std::vector<std::pair<std::string, struct timespec>> paths;
	std::transform(visibleItems.begin(), visibleItems.end(), back_inserter(paths), [](std::pair<std::string, item_record_ptr> const& p){ return std::make_pair(p.first, p.second->last_modified()); });
	dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_LOW, 0), ^{
		std::vector<std::string> modified;
		for(auto pair : paths)
		{
			struct stat buf;
			if(stat(pair.first.c_str(), &buf) == 0 && (buf.st_mtimespec.tv_sec != pair.second.tv_sec || buf.st_mtimespec.tv_nsec != pair.second.tv_nsec))
				modified.push_back(pair.first);
		}

		if(!modified.empty())
		{
			dispatch_async(dispatch_get_main_queue(), ^{
				for(auto path : modified)
				{
					auto it = visibleItems.find(path);
					if(it != visibleItems.end())
						it->second->reload(false);
				}
			});
		}
	});
}

- (void)fileManagerDidChangeContentsOfDirectory:(NSNotification*)aNotification
{
	NSDictionary* userInfo = [aNotification userInfo];
	NSString* dir = userInfo[OakFileManagerPathKey];
	auto it = visibleItems.find(to_s(dir));
	if(it != visibleItems.end())
		it->second->reload(false);
}

- (void)lostItems:(NSArray*)someItems
{
	for(FSItem* item in someItems)
		visibleItems.erase(to_s([item.url path]));
}

- (void)setLastModified:(struct timespec)lastModified forPath:(std::string const&)aPath
{
	auto it = visibleItems.find(aPath);
	if(it != visibleItems.end())
		it->second->set_last_modified(lastModified);
}

- (void)internalReloadItem:(FSItem*)anItem requested:(BOOL)flag
{
	D(DBF_FileBrowser_DSDirectory, bug("%s %s\n", [[[anItem url] path] UTF8String], BSTR(flag)););
	std::string const path = to_s([anItem.url path]);
	auto record = visibleItems.find(path);
	if(record == visibleItems.end())
		record = visibleItems.emplace(path, std::make_shared<item_record_t>(self, anItem)).first;
	record->second->reload(flag);
}

- (BOOL)reloadItem:(FSItem*)anItem
{
	D(DBF_FileBrowser_DSDirectory, bug("%s\n", [[[anItem url] path] UTF8String]););
	[self internalReloadItem:anItem requested:YES];
	return YES;
}

- (BOOL)unloadItem:(FSItem*)anItem
{
	D(DBF_FileBrowser_DSDirectory, bug("%s\n", [[[anItem url] path] UTF8String]););
	if(visibleItems.find(to_s([anItem.url path])) == visibleItems.end())
		NSLog(@"%s %@, item not loaded", sel_getName(_cmd), anItem);

	NSMutableArray* children = [NSMutableArray arrayWithObject:anItem];
	for(NSUInteger i = 0; i < children.count; ++i)
	{
		for(FSItem* item in [children[i] children])
		{
			if(item.children)
				[children addObject:item];
		}
	}

	[self lostItems:children];
	anItem.children = nil;
	return YES;
}
@end
