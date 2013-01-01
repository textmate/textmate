#import "FSDirectoryDataSource.h"
#import "FSItem.h"
#import <OakFoundation/NSArray Additions.h>
#import <OakFoundation/NSString Additions.h>
#import <OakAppKit/OakFileIconImage.h>
#import <oak/server.h>
#import <io/entries.h>
#import <io/path.h>
#import <regexp/glob.h>
#import <regexp/format_string.h>
#import <settings/settings.h>
#import <scm/scm.h>
#import <text/encode.h>
#import <ns/ns.h>
#import <oak/debug.h>

OAK_DEBUG_VAR(FileBrowser_DSDirectory);

@interface FSDirectoryDataSource ()
@property (nonatomic, retain) NSMutableDictionary* visible;
- (void)internalReloadItem:(FSItem*)anItem requested:(BOOL)flag;
- (void)lostItems:(NSArray*)someItems;
@end

// TODO ask SCM about missing items

@interface FSFileItem : FSItem
{
	std::pair<dev_t, ino_t> key;
}
@property (nonatomic, assign) std::pair<dev_t, ino_t> const& key;
@end

@implementation FSFileItem
- (std::pair<dev_t, ino_t> const&)key                 { return key; }
- (void)setKey:(std::pair<dev_t, ino_t> const&)newKey { key = newKey; }
@end

namespace
{
	struct fs_item_t
	{
		fs_item_t (dev_t device, dirent const* entry, std::string const& path) : device(device), inode(entry->d_fileno), path(path), target(NULL_STR), label(0), is_directory(false), treat_as_directory(false), sort_as_directory(false)
		{
			label = path::label_index(path);

			if(entry->d_type == DT_LNK)
			{
				std::string const resolved = path::resolve_head(path);
				uint32_t const flags = path::info(resolved);

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

		bool operator< (fs_item_t const& rhs) const
		{
			return device < rhs.device || device == rhs.device && inode < rhs.inode;
		}

		dev_t device;
		ino_t inode;
		std::string path;
		std::string target;
		size_t label;
		bool is_directory;
		bool treat_as_directory;
		bool sort_as_directory;
	};

	struct scanner_t
	{
		scanner_t (FSDirectoryDataSource* dataSource, FSItem* item, NSUInteger options, bool requestedReload);
		~scanner_t ();

		struct request_t
		{
			request_t (std::string const& path = NULL_STR, NSUInteger options = 0) : _path(path), _options(options)
			{
				settings_t const& settings = settings_for_path(NULL_STR, "", path);

				_globs.add_exclude_glob(settings.get(kSettingsExcludeDirectoriesInBrowserKey), path::kPathItemDirectory);
				_globs.add_exclude_glob(settings.get(kSettingsExcludeDirectoriesKey),          path::kPathItemDirectory);
				_globs.add_exclude_glob(settings.get(kSettingsExcludeFilesInBrowserKey),       path::kPathItemFile);
				_globs.add_exclude_glob(settings.get(kSettingsExcludeFilesKey),                path::kPathItemFile);
				_globs.add_exclude_glob(settings.get(kSettingsExcludeInBrowserKey),            path::kPathItemAny);
				_globs.add_exclude_glob(settings.get(kSettingsExcludeKey),                     path::kPathItemAny);

				_globs.add_include_glob(settings.get(kSettingsIncludeDirectoriesInBrowserKey), path::kPathItemDirectory);
				_globs.add_include_glob(settings.get(kSettingsIncludeDirectoriesKey),          path::kPathItemDirectory);
				_globs.add_include_glob(settings.get(kSettingsIncludeFilesInBrowserKey),       path::kPathItemFile);
				_globs.add_include_glob(settings.get(kSettingsIncludeFilesKey),                path::kPathItemFile);
				_globs.add_include_glob(settings.get(kSettingsIncludeInBrowserKey),            path::kPathItemAny);
				_globs.add_include_glob(settings.get(kSettingsIncludeKey),                     path::kPathItemAny);
			}

			bool exclude_path (std::string const& path, bool isDirectory) const
			{
				if(_options & kFSDataSourceOptionIncludeHidden)
					return false;
				return (path::info(path) & path::flag::hidden) || _globs.exclude(path);
			}

			std::string _path;
			NSUInteger _options;

		private:
			path::glob_list_t _globs;
		};

		static std::vector<fs_item_t> handle_request (request_t const& dir);
		void handle_reply (std::vector<fs_item_t> const& pathEntries);

	private:
		FSDirectoryDataSource* _data_source;
		FSItem* _item;
		NSUInteger _options;
		bool _requested_reload;
		size_t _client_key;
	};

	static oak::server_t<scanner_t>& server ()
	{
		static oak::server_t<scanner_t> instance;
		return instance;
	}

	scanner_t::scanner_t (FSDirectoryDataSource* dataSource, FSItem* item, NSUInteger options, bool requestedReload) : _options(options), _requested_reload(requestedReload)
	{
		D(DBF_FileBrowser_DSDirectory, bug("%s\n", [[[item url] path] UTF8String]););

		_data_source = [dataSource retain];
		_item        = [item retain];
		_client_key  = server().register_client(this);
		server().send_request(_client_key, request_t([[item.url path] fileSystemRepresentation], options));
	}

	scanner_t::~scanner_t ()
	{
		server().unregister_client(_client_key);
		[_item release];
      [_data_source release];
	}

	std::vector<fs_item_t> scanner_t::handle_request (request_t const& request)
	{
		std::string const& dir = request._path;

		std::vector<fs_item_t> res;
		struct stat buf;
		if(stat(dir.c_str(), &buf) == 0)
		{
			citerate(entry, path::entries(dir))
			{
				std::string const path = path::join(dir, (*entry)->d_name);
				if(request.exclude_path(path, (*entry)->d_type == DT_DIR))
					continue;
				res.push_back(fs_item_t(buf.st_dev, *entry, path));
			}
		}
		return res;
	}

	static void add_item_and_children (FSItem* item, NSMutableArray* array)
	{
		[array addObject:item];
		if(item.leaf)
			return;
		for(FSItem* child in item.children)
			add_item_and_children(child, array);
	}

	void scanner_t::handle_reply (std::vector<fs_item_t> const& items)
	{
		D(DBF_FileBrowser_DSDirectory, bug("%s\n", [[[_item url] path] UTF8String]););

		std::map< std::pair<dev_t, ino_t>, FSFileItem* > existingItems;
		for(FSFileItem* item in _item.children)
			existingItems.insert(std::make_pair(std::make_pair(item.key.first, item.key.second), item));

		NSMutableArray* array = [NSMutableArray array];
		iterate(fsItem, items)
		{
			NSURL* url = [NSURL fileURLWithPath:[NSString stringWithCxxString:fsItem->path] isDirectory:fsItem->is_directory];

			FSFileItem* item;
			auto key = std::make_pair(fsItem->device, fsItem->inode);
			auto it = existingItems.find(key);
			if(it != existingItems.end())
			{
				item = it->second;
				item.url  = url;
				item.icon = [OakFileIconImage fileIconImageWithPath:[url path] size:NSMakeSize(16, 16)];
				item.name = [NSString stringWithCxxString:path::display_name([[url path] fileSystemRepresentation])];

				existingItems.erase(it);
			}
			else
			{
				item = (FSFileItem*)[FSFileItem itemWithURL:url];
				item.key = key;
			}

			item.labelIndex   = fsItem->label;
			item.sortAsFolder = fsItem->sort_as_directory;
			item.leaf         = !fsItem->treat_as_directory;
			item.target       = fsItem->target != NULL_STR ? [NSURL URLWithString:[NSString stringWithCxxString:fsItem->target]] : nil;

			[array addObject:item];
		}

		NSMutableArray* lostItems = [NSMutableArray array];
		iterate(pair, existingItems)
			add_item_and_children(pair->second, lostItems);
		[_data_source lostItems:lostItems];

		[[NSNotificationCenter defaultCenter] postNotificationName:FSItemDidReloadNotification object:_data_source userInfo:@{ @"item" : _item, @"children" : [FSDataSource sortArray:array usingOptions:_options], @"recursive" : @YES, @"requested" : @(_requested_reload) }];

		delete this;
	}
}

static void ensure_callback (scm::callback_t* cb, std::string const& path, std::map<std::string, scm::info_ptr>& drivers, std::map<std::string, size_t>& refCounts)
{
	if(scm::info_ptr const& info = scm::info(path))
	{
		if(drivers.insert(std::make_pair(path, info)).second == false)
			return; // we already added a callback to this path

		if(++refCounts[info->path()] == 1)
		{
			D(DBF_FileBrowser_DSDirectory, bug("add to %s (%s)\n", info->path().c_str(), path.c_str()););
			info->add_callback(cb);
		}
		else
		{
			D(DBF_FileBrowser_DSDirectory, bug("+++ to %s (%s)\n", info->path().c_str(), path.c_str()););
		}
	}
}

static void remove_callbacks (scm::callback_t* cb, std::string const& path, std::map<std::string, scm::info_ptr>& drivers, std::map<std::string, size_t>& refCounts)
{
	for(auto it = drivers.find(path); it != drivers.end() && it->first.find(path) == 0;)
	{
		if(--refCounts[it->second->path()] == 0)
		{
			D(DBF_FileBrowser_DSDirectory, bug("rem from %s (%s)\n", it->second->path().c_str(), it->first.c_str()););
			it->second->remove_callback(cb);
		}
		else
		{
			D(DBF_FileBrowser_DSDirectory, bug("--- from %s (%s)\n", it->second->path().c_str(), it->first.c_str()););
		}
		drivers.erase(it++);
	}
}

@implementation FSDirectoryDataSource
- (void)fsEvent:(std::string const&)aPath
{
	D(DBF_FileBrowser_DSDirectory, bug("%s\n", aPath.c_str()););
	if(FSItem* item = self.visible[[NSString stringWithCxxString:aPath]])
		new scanner_t(self, item, dataSourceOptions, false);
}

- (void)lostItems:(NSArray*)someItems
{
	NSMutableArray* toDelete = [NSMutableArray array];
	for(NSString* path in self.visible)
	{
		if([someItems containsObject:self.visible[path]])
			[toDelete addObject:path];
	}

	for(NSString* path in toDelete)
		[self.visible removeObjectForKey:path];
}

- (id)initWithURL:(NSURL*)anURL options:(NSUInteger)someOptions
{
	if((self = [super init]))
	{
		dataSourceOptions = someOptions;
		self.visible  = [NSMutableDictionary dictionary];
		self.rootItem = [FSFileItem itemWithURL:anURL];

		struct event_callback_t : fs::event_callback_t
		{
			event_callback_t (FSDirectoryDataSource* self) : _self(self) { }

			void did_change (std::string const& path, std::string const& observedPath, uint64_t eventId, bool recursive)
			{
				[_self fsEvent:path];
			}

		private:
			FSDirectoryDataSource* _self;
		};

		callback = new event_callback_t(self);
		fs::watch([[self.rootItem.url path] fileSystemRepresentation], callback);

		struct scm_callback_t : scm::callback_t
		{
			scm_callback_t (FSDirectoryDataSource* self) : _self(self) { }

			void status_changed (scm::info_t const& info, std::set<std::string> const& changedPaths)
			{
				// FIXME only notify about the actual changed item
				std::set<std::string> paths;
				iterate(path, changedPaths)
					paths.insert(path::parent(*path));
				iterate(path, paths)
					[_self fsEvent:*path];
			}

		private:
			FSDirectoryDataSource* _self;
		};

		scmCallback = new scm_callback_t(self);

		[self internalReloadItem:self.rootItem requested:NO];
	}
	return self;
}

- (void)dealloc
{
	iterate(pair, scmDrivers)
	{
		if(--scmReferenceCounts[pair->second->path()] == 0)
			pair->second->remove_callback(scmCallback);
	}
	delete scmCallback;

	DB(iterate(pair, scmReferenceCounts) ASSERT_EQ(pair->second, 0););

	fs::unwatch([[self.rootItem.url path] fileSystemRepresentation], callback);
	delete callback;
	self.visible = nil;
	[super dealloc];
}

- (void)internalReloadItem:(FSItem*)anItem requested:(BOOL)flag
{
	D(DBF_FileBrowser_DSDirectory, bug("%s %s\n", [[[anItem url] path] UTF8String], BSTR(flag)););
	self.visible[[anItem.url path]] = anItem;
	new scanner_t(self, anItem, dataSourceOptions, flag);
	ensure_callback(scmCallback, [[anItem.url path] fileSystemRepresentation], scmDrivers, scmReferenceCounts);
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
	remove_callbacks(scmCallback, [[anItem.url path] fileSystemRepresentation], scmDrivers, scmReferenceCounts);
	[self.visible removeObjectForKey:[anItem.url path]];
	anItem.children = nil;
	return YES;
}
@end
