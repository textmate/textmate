#import "InstallBundleItems.h"
#import "BundlesManager.h"
#import <OakFoundation/NSString Additions.h>
#import <bundles/bundles.h>
#import <text/ctype.h>
#import <regexp/format_string.h>
#import <io/io.h>
#import <ns/ns.h>

static std::map<std::string, bundles::item_ptr> installed_items ()
{
	std::map<std::string, bundles::item_ptr> res;
	for(auto const& item : bundles::query(bundles::kFieldAny, NULL_STR, scope::wildcard, bundles::kItemTypeAny, oak::uuid_t(), false, true))
	{
		for(auto const& path : item->paths())
			res.emplace(path, item);
	}
	return res;
}

void InstallBundleItems (NSArray* itemPaths)
{
	struct info_t
	{
		info_t (std::string const& path, std::string const& name, oak::uuid_t const& uuid, bool isBundle, bundles::item_ptr installed = bundles::item_ptr()) : path(path), name(name), uuid(uuid), is_bundle(isBundle), installed(installed) { }

		std::string path;
		std::string name;
		oak::uuid_t uuid;
		bool is_bundle;
		bundles::item_ptr installed;
	};

	std::map<std::string, bundles::item_ptr> const installedItems = installed_items();
	std::vector<info_t> installed, toInstall, delta, malformed;

	for(NSString* path in itemPaths)
	{
		bool isDelta;
		std::string bundleName;
		oak::uuid_t bundleUUID;
		bundles::item_ptr installedItem;

		bool isBundle                       = [[[path pathExtension] lowercaseString] isEqualToString:@"tmbundle"];
		std::string const loadPath          = isBundle ? path::join(to_s(path), "info.plist") : to_s(path);
		plist::dictionary_t const infoPlist = plist::load(loadPath);

		auto it = installedItems.find(loadPath);
		if(it != installedItems.end())
			installedItem = it->second;

		if(plist::get_key_path(infoPlist, "isDelta", isDelta) && isDelta)
		{
			delta.push_back(info_t(to_s(path), NULL_STR, oak::uuid_t(), isBundle, installedItem));
		}
		else if(plist::get_key_path(infoPlist, "name", bundleName) && plist::get_key_path(infoPlist, "uuid", bundleUUID))
		{
			if(installedItem)
					installed.push_back(info_t(to_s(path), bundleName, bundleUUID, isBundle, installedItem));
			else	toInstall.push_back(info_t(to_s(path), bundleName, bundleUUID, isBundle, installedItem));
		}
		else
		{
			malformed.push_back(info_t(to_s(path), NULL_STR, oak::uuid_t(), isBundle, installedItem));
		}
	}

	for(auto const& info : delta)
	{
		char const* type = info.is_bundle ? "bundle" : "bundle item";
		std::string const name = path::name(path::strip_extension(info.path));
		std::string const title = text::format("The %s “%s” could not be installed because it is in delta format.", type, name.c_str());
		NSRunAlertPanel([NSString stringWithCxxString:title], @"Contact the author of this %s to get a properly exported version.", @"OK", nil, nil, type);
	}

	for(auto const& info : malformed)
	{
		char const* type = info.is_bundle ? "bundle" : "bundle item";
		std::string const name = path::name(path::strip_extension(info.path));
		std::string const title = text::format("The %s “%s” could not be installed because it is malformed.", type, name.c_str());
		NSRunAlertPanel([NSString stringWithCxxString:title], @"The %s lacks mandatory keys in its property list file.", @"OK", nil, nil, type);
	}

	for(auto const& info : installed)
	{
		char const* type = info.is_bundle ? "bundle" : "bundle item";
		std::string const name = info.name;
		std::string const title = text::format("The %s “%s” is already installed.", type, name.c_str());
		int choice = NSRunAlertPanel([NSString stringWithCxxString:title], @"You can edit the installed %s to inspect it.", @"OK", @"Edit", nil, type);
		if(choice == NSAlertAlternateReturn) // "Edit"
			[NSApp sendAction:@selector(editBundleItemWithUUIDString:) to:nil from:[NSString stringWithCxxString:info.uuid]];
	}

	std::set<std::string> pathsToReload;
	for(auto const& info : toInstall)
	{
		if(info.is_bundle)
		{
			int choice = NSRunAlertPanel([NSString stringWithFormat:@"Would you like to install the “%@” bundle?", [NSString stringWithCxxString:info.name]], @"Installing a bundle adds new functionality to TextMate.", @"Install", @"Cancel", nil);
			if(choice == NSAlertDefaultReturn) // "Install"
			{
				std::string const installDir = path::join(path::home(), "Library/Application Support/Avian/Pristine Copy/Bundles");
				if(path::make_dir(installDir))
				{
					std::string const installPath = path::unique(path::join(installDir, path::name(info.path)));
					if(path::copy(info.path, installPath))
					{
						pathsToReload.insert(installDir);
						fprintf(stderr, "installed bundle at: %s\n", installPath.c_str());
						continue;
					}
				}
				fprintf(stderr, "failed to install bundle: %s\n", info.path.c_str());
			}
		}
		else
		{
			bundles::item_ptr bundle;
			if([[BundlesManager sharedInstance] findBundleForInstall:&bundle])
			{
				static struct { std::string extension; std::string directory; } DirectoryMap[] =
				{
					{ ".tmCommand",     "Commands"     },
					{ ".tmDragCommand", "DragCommands" },
					{ ".tmMacro",       "Macros"       },
					{ ".tmPreferences", "Preferences"  },
					{ ".tmSnippet",     "Snippets"     },
					{ ".tmLanguage",    "Syntaxes"     },
					{ ".tmProxy",       "Proxies"      },
					{ ".tmTheme",       "Themes"       },
				};

				if(bundle->local() || bundle->save())
				{
					std::string dest = path::parent(bundle->paths().front());
					for(auto const& iter : DirectoryMap)
					{
						if(path::extension(info.path) == iter.extension)
						{
							dest = path::join(dest, iter.directory);
							if(path::make_dir(dest))
							{
								dest = path::join(dest, path::name(info.path));
								pathsToReload.insert(dest);
								dest = path::unique(dest);
								if(path::copy(info.path, dest))
										break;
								else	fprintf(stderr, "error: copy(‘%s’, ‘%s’)\n", info.path.c_str(), dest.c_str());
							}
							else
							{
								fprintf(stderr, "error: makedir(‘%s’)\n", dest.c_str());
							}
						}
					}
				}
			}
		}
	}

	for(auto path : pathsToReload)
		[[BundlesManager sharedInstance] reloadPath:[NSString stringWithCxxString:path]];
}
