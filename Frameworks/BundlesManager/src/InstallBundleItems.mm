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
	citerate(item, bundles::query(bundles::kFieldAny, NULL_STR, scope::wildcard, bundles::kItemTypeAny, oak::uuid_t(), false, true))
	{
		citerate(path, (*item)->paths())
			res.insert(std::make_pair(*path, *item));
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

	iterate(info, delta)
	{
		char const* type = info->is_bundle ? "bundle" : "bundle item";
		std::string const name = path::name(path::strip_extension(info->path));
		std::string const title = text::format("The %s “%s” could not be installed because it is in delta format.", type, name.c_str());
		NSRunAlertPanel([NSString stringWithCxxString:title], @"Contact the author of this %s to get a properly exported version.", @"OK", nil, nil, type);
	}

	iterate(info, malformed)
	{
		char const* type = info->is_bundle ? "bundle" : "bundle item";
		std::string const name = path::name(path::strip_extension(info->path));
		std::string const title = text::format("The %s “%s” could not be installed because it is malformed.", type, name.c_str());
		NSRunAlertPanel([NSString stringWithCxxString:title], @"The %s lacks mandatory keys in its property list file.", @"OK", nil, nil, type);
	}

	iterate(info, installed)
	{
		char const* type = info->is_bundle ? "bundle" : "bundle item";
		std::string const name = info->name;
		std::string const title = text::format("The %s “%s” is already installed.", type, name.c_str());
		int choice = NSRunAlertPanel([NSString stringWithCxxString:title], @"You can edit the installed %s to inspect it.", @"OK", @"Edit", nil, type);
		if(choice == NSAlertAlternateReturn) // "Edit"
			[NSApp sendAction:@selector(editBundleItemWithUUIDString:) to:nil from:[NSString stringWithCxxString:info->uuid]];
	}

	std::set<std::string> pathsToReload;
	iterate(info, toInstall)
	{
		if(info->is_bundle)
		{
			int choice = NSRunAlertPanel([NSString stringWithFormat:@"Would you like to install the “%@” bundle?", [NSString stringWithCxxString:info->name]], @"Installing a bundle adds new functionality to TextMate.", @"Install", @"Cancel", nil);
			if(choice == NSAlertDefaultReturn) // "Install"
			{
				std::string const installDir = path::join(path::home(), "Library/Application Support/Avian/Pristine Copy/Bundles");
				if(path::make_dir(installDir))
				{
					std::string const installPath = path::unique(path::join(installDir, path::name(info->path)));
					if(path::copy(info->path, installPath))
					{
						pathsToReload.insert(installDir);
						fprintf(stderr, "installed bundle at: %s\n", installPath.c_str());
						continue;
					}
				}
				fprintf(stderr, "failed to install bundle: %s\n", info->path.c_str());
			}
		}
		else
		{
			oak::uuid_t defaultBundle;
			if(path::extension(info->path) == ".tmTheme")
			{
				citerate(item, bundles::query(bundles::kFieldAny, NULL_STR, scope::wildcard, bundles::kItemTypeBundle, "A4380B27-F366-4C70-A542-B00D26ED997E"))
					defaultBundle = (*item)->uuid();
			}

			std::map<std::string, std::string> vars;
			vars.insert(std::make_pair("TM_FULLNAME", path::passwd_entry()->pw_gecos ?: "John Doe"));
			std::string personalBundleName = format_string::expand("${TM_FULLNAME/^(\\S+).*$/$1/}’s Bundle", vars);
			// std::string personalBundleName = format_string::expand("${TM_FULLNAME/^(\\S+).*$/$1/}’s Bundle", std::map<std::string, std::string>{ { "TM_FULLNAME", path::passwd_entry()->pw_gecos ?: "John Doe" } });
			if(!defaultBundle)
			{
				citerate(item, bundles::query(bundles::kFieldName, personalBundleName, scope::wildcard, bundles::kItemTypeBundle))
					defaultBundle = (*item)->uuid();
			}

			NSPopUpButton* bundleChooser = [[NSPopUpButton alloc] initWithFrame:NSZeroRect pullsDown:NO];
			[bundleChooser.menu removeAllItems];
			[bundleChooser.menu addItemWithTitle:@"Create new bundle…" action:NULL keyEquivalent:@""];
			[bundleChooser.menu addItem:[NSMenuItem separatorItem]];

			std::multimap<std::string, bundles::item_ptr, text::less_t> ordered;
			citerate(item, bundles::query(bundles::kFieldAny, NULL_STR, scope::wildcard, bundles::kItemTypeBundle))
				ordered.insert(std::make_pair((*item)->name(), *item));
			NSMenuItem* selectedItem = nil;
			iterate(pair, ordered)
			{
				NSMenuItem* menuItem = [bundleChooser.menu addItemWithTitle:[NSString stringWithCxxString:pair->first] action:NULL keyEquivalent:@""];
				[menuItem setRepresentedObject:[NSString stringWithCxxString:to_s(pair->second->uuid())]];
				if(defaultBundle && defaultBundle == pair->second->uuid())
					selectedItem = menuItem;
			}
			if(selectedItem)
				[bundleChooser selectItem:selectedItem];

			[bundleChooser sizeToFit];
			NSRect frame = [bundleChooser frame];
			if(NSWidth(frame) > 200)
				[bundleChooser setFrameSize:NSMakeSize(200, NSHeight(frame))];

			NSAlert* alert = [NSAlert alertWithMessageText:[NSString stringWithFormat:@"Would you like to install the “%@” bundle item?", [NSString stringWithCxxString:info->name]] defaultButton:@"Install" alternateButton:@"Cancel" otherButton:nil informativeTextWithFormat:@"Installing a bundle item adds new functionality to TextMate."];
			[alert setAccessoryView:bundleChooser];
			if([alert runModal] == NSAlertDefaultReturn) // "Install"
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

				if(NSString* bundleUUID = [[bundleChooser selectedItem] representedObject])
				{
					citerate(item, bundles::query(bundles::kFieldAny, NULL_STR, scope::wildcard, bundles::kItemTypeBundle, to_s(bundleUUID)))
					{
						if((*item)->local() || (*item)->save())
						{
							std::string dest = path::parent((*item)->paths().front());
							iterate(iter, DirectoryMap)
							{
								if(path::extension(info->path) == iter->extension)
								{
									dest = path::join(dest, iter->directory);
									if(path::make_dir(dest))
									{
										dest = path::join(dest, path::name(info->path));
										pathsToReload.insert(dest);
										dest = path::unique(dest);
										if(path::copy(info->path, dest))
												break;
										else	fprintf(stderr, "error: copy(‘%s’, ‘%s’)\n", info->path.c_str(), dest.c_str());
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
				else
				{
					NSRunAlertPanel(@"Creating bundles as part of installing bundle items is not yet supported.", @"You can create a new bundle in the bundle editor via File → New (⌘N) and then install this item again, selecting your newly created bundle.", @"OK", nil, nil);
				}
			}
		}
	}

	for(auto path : pathsToReload)
		[[BundlesManager sharedInstance] reloadPath:[NSString stringWithCxxString:path]];
}
