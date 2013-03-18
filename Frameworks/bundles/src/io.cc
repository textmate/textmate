#include "index.h"
#include "locations.h"
#include "query.h" // set_index
#include "fsevents/fs_controller.h"
#include <plist/delta.h>
#include <regexp/glob.h>
#include <text/case.h>

namespace bundles
{
	static std::string const kFieldChangedItems = "changed";
	static std::string const kFieldMainMenu     = "mainMenu";
	static std::string const kSeparatorString   = "------------------------------------";

	// ===================
	// = Index Functions =
	// ===================

	static fs::node_t resolve (std::string& cwd, fs::node_t node, std::map<std::string, fs::node_t> const& heads)
	{
		while(node.type() == fs::node_t::kNodeTypeLink)
		{
			std::map<std::string, fs::node_t>::const_iterator it = heads.find(cwd);
			if(it == heads.end())
				break;
			node = it->second;
			cwd = node.real_path(path::parent(cwd));
		}
		return node;
	}

	static std::vector<oak::uuid_t> to_menu (plist::array_t const& uuids)
	{
		std::vector<oak::uuid_t> res;
		iterate(uuid, uuids)
		{
			if(std::string const* str = boost::get<std::string>(&*uuid))
				res.push_back(*str == kSeparatorString ? kSeparatorUUID : oak::uuid_t(*str));
		}
		return res;
	}

	namespace
	{
		struct delta_item_t
		{
			delta_item_t (bundles::item_ptr item, plist::dictionary_t const& plist) : item(item), plist(plist) { }

			bundles::item_ptr item;
			plist::dictionary_t plist;
		};

		struct property_cache_t
		{
			property_cache_t (std::string const& cacheFile) : _cache_file(cacheFile), _dirty(false)
			{
				_cache = plist::load(_cache_file);

				int32_t version;
				if(!plist::get_key_path(_cache, "version", version) || version != 2)
					_cache = plist::dictionary_t();
			}

			~property_cache_t ()
			{
				std::vector<std::string> knownPaths, deadPaths;
				std::transform(_cache.begin(), _cache.end(), back_inserter(knownPaths), [](std::pair<std::string, plist::any_t> const& p){ return p.first; });
				std::set_difference(knownPaths.begin(), knownPaths.end(), _accessed_paths.begin(), _accessed_paths.end(), back_inserter(deadPaths));
				if(!deadPaths.empty())
				{
					iterate(path, deadPaths)
						_cache.erase(*path);
					_dirty = true;
				}

				if(_dirty)
				{
					_cache["version"] = 2;
					plist::save(_cache_file, _cache);
				}
			}

			plist::dictionary_t const& get (std::string const& path, time_t modified)
			{
				_accessed_paths.insert(path);

				plist::dictionary_t::const_iterator cachedNode = _cache.find(path);
				if(cachedNode != _cache.end() && boost::get<plist::dictionary_t>(&cachedNode->second))
				{
					plist::dictionary_t const& node = boost::get<plist::dictionary_t>(cachedNode->second);
					oak::date_t cacheDate;
					if(plist::get_key_path(node, "modified", cacheDate) && cacheDate == oak::date_t(modified))
					{
						plist::dictionary_t::const_iterator res = node.find("values");
						if(res != node.end() && boost::get<plist::dictionary_t>(&res->second))
							return boost::get<plist::dictionary_t>(res->second);
						fprintf(stderr, "error looking for ‘%s’ in cache\n", path.c_str());
					}
				}

				plist::dictionary_t cacheEntry;
				cacheEntry["modified"] = oak::date_t(modified);
				cacheEntry["values"]   = prune_dictionary(plist::load(path));
				_cache[path] = cacheEntry;
				_dirty       = true;

				return get(path, modified);
			}

			void erase (std::string const& path)
			{
				_cache.erase(path);
			}

		private:
			static plist::dictionary_t prune_dictionary (plist::dictionary_t const& plist)
			{
				static std::set<std::string> const DesiredKeys = { kFieldName, kFieldKeyEquivalent, kFieldTabTrigger, kFieldScopeSelector, kFieldSemanticClass, kFieldContentMatch, kFieldGrammarFirstLineMatch, kFieldGrammarScope, kFieldGrammarInjectionSelector, kFieldDropExtension, kFieldGrammarExtension, kFieldSettingName, kFieldHideFromUser, kFieldIsDeleted, kFieldIsDisabled, kFieldRequiredItems, kFieldUUID, kFieldMainMenu, kFieldIsDelta, kFieldChangedItems };

				plist::dictionary_t res;
				citerate(pair, plist)
				{
					if(DesiredKeys.find(pair->first) == DesiredKeys.end() && pair->first.find(kFieldSettingName) != 0)
						continue;

					if(pair->first == kFieldSettingName)
					{
						if(plist::dictionary_t const* dictionary = boost::get<plist::dictionary_t>(&pair->second))
						{
							plist::array_t settings;
							iterate(settingsPair, *dictionary)
								settings.push_back(settingsPair->first);
							res.insert(std::make_pair(pair->first, settings));
						}
					}
					else if(pair->first == kFieldChangedItems)
					{
						if(plist::dictionary_t const* dictionary = boost::get<plist::dictionary_t>(&pair->second))
							res.insert(std::make_pair(pair->first, prune_dictionary(*dictionary)));
					}
					else
					{
						res.insert(*pair);
					}
				}
				return res;
			}

			std::string _cache_file;
			plist::dictionary_t _cache;
			std::set<std::string> _accessed_paths;
			bool _dirty;
		};
	}

	static void traverse (std::map<std::string, fs::node_t> const& heads, property_cache_t& plistCache)
	{
		std::vector<item_ptr> items(1, item_t::menu_item_separator());
		std::map< oak::uuid_t, std::vector<oak::uuid_t> > menus;

		std::map<oak::uuid_t, delta_item_t> deltaItems;
		std::set<oak::uuid_t> loadedItems;

		bool local = true;
		iterate(path, locations())
		{
			std::string cwd = path::join(*path, "Bundles");
			std::map<std::string, fs::node_t>::const_iterator node = heads.find(cwd);
			if(node == heads.end() || node->second.type() == fs::node_t::kNodeTypeMissing)
			{
				local = false;
				continue;
			}

			fs::node_t bundlesNode = resolve(cwd, node->second, heads);
			if(bundlesNode.type() != fs::node_t::kNodeTypeDirectory || !bundlesNode.entries())
			{
				fprintf(stderr, "*** no bundles for path ‘%s’\n", bundlesNode.real_path(cwd).c_str());
				continue;
			}

			citerate(fsBundle, *bundlesNode.entries())
			{
				if(text::lowercase(path::extension(fsBundle->name())) != ".tmbundle")
				{
					fprintf(stderr, "not a bundle: %s\n", fsBundle->name().c_str());
					continue;
				}

				std::string bundlePath = fsBundle->real_path(cwd);
				fs::node_t bundleNode = resolve(bundlePath, *fsBundle, heads);
				if(bundleNode.type() != fs::node_t::kNodeTypeDirectory)
				{
					fprintf(stderr, "not a directory: %s (%s)\n", bundleNode.name().c_str(), bundleNode.path(cwd).c_str());
					continue;
				}

				item_ptr bundle;
				std::set<oak::uuid_t> hiddenItems;
				citerate(infoPlist, *bundleNode.entries())
				{
					if(infoPlist->name() != "info.plist")
						continue;

					std::string infoPlistPath = infoPlist->real_path(bundlePath);
					fs::node_t infoPlistNode = resolve(infoPlistPath, *infoPlist, heads);
					if(infoPlistNode.type() != fs::node_t::kNodeTypeFile)
						break;

					oak::uuid_t bundleUUID;
					plist::dictionary_t plist = plistCache.get(infoPlistPath, infoPlistNode.modified());
					if(!plist::get_key_path(plist, kFieldUUID, bundleUUID))
						break;

					bundle.reset(new item_t(bundleUUID, item_ptr(), kItemTypeBundle, local));
					bundle->add_path(infoPlistPath);

					bool isDelta = false;
					if(plist::get_key_path(plist, kFieldIsDelta, isDelta) && isDelta)
					{
						deltaItems.insert(std::make_pair(bundleUUID, delta_item_t(bundle, plist)));
						break;
					}

					std::map<oak::uuid_t, delta_item_t>::iterator deltaItem = deltaItems.find(bundleUUID);
					if(deltaItem != deltaItems.end())
					{
						bundle = deltaItem->second.item;
						bundle->add_path(infoPlistPath);

						std::vector<plist::dictionary_t> plists;
						plists.push_back(deltaItem->second.plist);
						plists.push_back(plist);
						plist = plist::merge_delta(plists);
						deltaItems.erase(deltaItem);
					}
					else if(loadedItems.find(bundleUUID) != loadedItems.end())
					{
						// fprintf(stderr, "skip ‘%s’ (already loaded)\n", infoPlistPath.c_str());
						break;
					}

					bundle->initialize(plist);
					items.push_back(bundle);

					// ====================
					// = Load Bundle Menu =
					// ====================

					plist::array_t mainMenu;
					plist::get_key_path(plist, "mainMenu.items", mainMenu);
					menus.insert(std::make_pair(bundleUUID, to_menu(mainMenu)));

					plist::dictionary_t subMenus;
					plist::get_key_path(plist, "mainMenu.submenus", subMenus);
					iterate(submenuIter, subMenus)
					{
						std::string name;
						plist::array_t uuids;
						if(plist::get_key_path(submenuIter->second, kFieldName, name) && plist::get_key_path(submenuIter->second, "items", uuids))
						{
							item_ptr item(new item_t(submenuIter->first, bundle, kItemTypeMenu));
							item->set_name(name);
							items.push_back(item);
							menus.insert(std::make_pair(submenuIter->first, to_menu(uuids)));
						}
					}

					plist::array_t uuids;
					plist::get_key_path(plist, "mainMenu.excludedItems", uuids);
					iterate(uuid, uuids)
					{
						if(std::string const* str = boost::get<std::string>(&*uuid))
							hiddenItems.insert(*str);
					}

					break;
				}

				if(!bundle)
				{
					fprintf(stderr, "no bundle: %s\n", bundlePath.c_str());
					continue;
				}

				citerate(dir, *bundleNode.entries())
				{
					static struct { std::string name; std::string extension; kind_t kind; } const dirs[] =
					{
						{ "Commands",     ".tmCommand",     kItemTypeCommand     },
						{ "DragCommands", ".tmDragCommand", kItemTypeDragCommand },
						{ "Macros",       ".tmMacro",       kItemTypeMacro       },
						{ "Preferences",  ".tmPreferences", kItemTypeSettings    },
						{ "Snippets",     ".tmSnippet",     kItemTypeSnippet     },
						{ "Syntaxes",     ".tmLanguage",    kItemTypeGrammar     },
						{ "Proxies",      ".tmProxy",       kItemTypeProxy       },
						{ "Themes",       ".tmTheme",       kItemTypeTheme       },
					};

					for(size_t i = 0; i < sizeofA(dirs); ++i)
					{
						if(dir->name() != dirs[i].name)
							continue;

						std::string dirPath = dir->real_path(bundlePath);
						fs::node_t dirNode = resolve(dirPath, *dir, heads);
						if(dirNode.type() != fs::node_t::kNodeTypeDirectory)
							break;

						std::string const extensions[] = { dirs[i].extension, ".tmDelta", ".plist" };
						citerate(fsItem, *dirNode.entries())
						{
							if(!oak::contains(std::begin(extensions), std::end(extensions), path::extension(fsItem->name())))
							{
								fprintf(stderr, "wrong item type: %s\n", fsItem->name().c_str());
								continue;
							}

							std::string itemPath = fsItem->real_path(dirPath);
							fs::node_t itemNode = resolve(itemPath, *fsItem, heads);
							if(itemNode.type() != fs::node_t::kNodeTypeFile)
								continue;

							plist::dictionary_t plist = plistCache.get(itemPath, itemNode.modified());
							oak::uuid_t uuid;
							if(!plist::get_key_path(plist, kFieldUUID, uuid))
								continue;

							if(loadedItems.find(uuid) != loadedItems.end())
							{
								// fprintf(stderr, "skip ‘%s’ (already loaded)\n", itemPath.c_str());
								continue;
							}

							item_ptr item(new item_t(uuid, bundle, dirs[i].kind, local));
							item->add_path(itemPath);

							bool isDelta = false;
							if(plist::get_key_path(plist, kFieldIsDelta, isDelta) && isDelta)
							{
								deltaItems.insert(std::make_pair(uuid, delta_item_t(item, plist)));
								continue;
							}

							std::map<oak::uuid_t, delta_item_t>::iterator deltaItem = deltaItems.find(uuid);
							if(deltaItem != deltaItems.end())
							{
								item = deltaItem->second.item;
								item->add_path(itemPath);

								std::vector<plist::dictionary_t> plists;
								plists.push_back(deltaItem->second.plist);
								plists.push_back(plist);
								plist = plist::merge_delta(plists);
								deltaItems.erase(deltaItem);
							}
							else if(loadedItems.find(bundle->uuid()) != loadedItems.end())
							{
								// fprintf(stderr, "skip ‘%s’ (bundle already loaded from more local path)\n", itemPath.c_str());
								continue;
							}

							if(hiddenItems.find(item->uuid()) != hiddenItems.end())
								plist.insert(std::make_pair(kFieldHideFromUser, true));

							item->initialize(plist);
							items.push_back(item);

							loadedItems.insert(uuid);
						}

						break;
					}
				}

				if(deltaItems.find(bundle->uuid()) == deltaItems.end())
					loadedItems.insert(bundle->uuid());
			}

			local = false;
		}

		for(ssize_t i = items.size(); i-- > 0; )
		{
			item_ptr item = items[i];
			if(deltaItems.find(item->bundle_uuid()) != deltaItems.end())
			{
				fprintf(stderr, "Warning: Bundle item ‘%s’ at path %s has no (non-delta) parent\n", item->name().c_str(), text::join(item->paths(), ", ").c_str());
				items.erase(items.begin() + i);
			}
		}

		set_index(items, menus);
	}

	void build_index (std::string const& cacheDir)
	{
		static path::glob_t const dirGlob  = "*.tm[Bb]undle{,/{Commands,DragCommands,Macros,Preferences,Proxies,Snippets,Syntaxes,Themes}}";
		static path::glob_t const fileGlob = "*.tm[Bb]undle/{info.plist,Commands/*.{plist,tmCommand,tmDelta},DragCommands/*.{plist,tmDragCommand,tmDelta},Macros/*.{plist,tmMacro,tmDelta},Preferences/*.{plist,tmPreferences,tmDelta},Proxies/*.{tmProxy,tmDelta},Snippets/*.{plist,tmSnippet,tmDelta},Syntaxes/*.{plist,tmLanguage,tmDelta},Themes/*.{plist,tmTheme,tmDelta}}";

		struct callback_t : fs::callback_t
		{
			callback_t (std::string const& cacheFile) : _cache_file(cacheFile) { }
			void did_change (std::map<std::string, fs::node_t> const& heads, std::map< std::string, std::vector<std::string> > const& changes)
			{
				property_cache_t plistCache(_cache_file);
				for(auto pair : changes)
				{
					for(auto path : pair.second)
						plistCache.erase(path);
				}

				traverse(heads, plistCache);
			}

		private:
			std::string _cache_file;
		};

		std::set<std::string> rootPaths;
		iterate(path, bundles::locations())
			rootPaths.insert(path::join(*path, "Bundles"));

		std::string const fsTreeCacheFile = path::join(cacheDir == NULL_STR ? "/tmp" : cacheDir, "FSNodes.plist");
		std::string const plistCacheFile  = path::join(cacheDir == NULL_STR ? "/tmp" : cacheDir, "PropertyValues.plist");

		static fs::watch_info_ptr info = fs::watch_paths(rootPaths, new callback_t(plistCacheFile), dirGlob, fileGlob, fsTreeCacheFile);
	}

} /* bundles */