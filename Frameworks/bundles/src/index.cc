#include "index.h"
#include "locations.h"
#include "fsevents/fs_controller.h"
#include "query.h" // set_index
#include <plist/delta.h>
#include <text/case.h>
#include <regexp/glob.h>
#include <regexp/format_string.h>

namespace bundles
{
	int kItemTypeMenuTypes = kItemTypeCommand|kItemTypeMacro|kItemTypeSnippet|kItemTypeProxy;
	int kItemTypeAny       = -1;

	std::string const kFieldUUID                     = "uuid";
	std::string const kFieldName                     = "name";

	std::string const kFieldIsDelta                  = "isDelta";
	std::string const kFieldIsDisabled               = "isDisabled";
	std::string const kFieldIsDeleted                = "isDeleted";
	std::string const kFieldHideFromUser             = "hideFromUser";

	std::string const kFieldKeyEquivalent            = "keyEquivalent";
	std::string const kFieldTabTrigger               = "tabTrigger";
	std::string const kFieldScopeSelector            = "scope";

	std::string const kFieldSemanticClass            = "semanticClass";
	std::string const kFieldContentMatch             = "contentMatch";
	std::string const kFieldDropExtension            = "draggedFileExtensions"; // array
	std::string const kFieldGrammarExtension         = "fileTypes";             // array
	std::string const kFieldGrammarFirstLineMatch    = "firstLineMatch";
	std::string const kFieldGrammarScope             = "scopeName";
	std::string const kFieldGrammarInjectionSelector = "injectionSelector";
	std::string const kFieldSettingName              = "settings";              // dictionary

	std::string const kFieldRequiredItems            = "require";
	std::string const kFieldDeletedItems             = "deleted";
	std::string const kFieldChangedItems             = "changed";
	std::string const kFieldMainMenu                 = "mainMenu";

	std::string const kFieldAny                      = NULL_STR;

	std::string const kSeparatorString               = "------------------------------------";
	oak::uuid_t const kSeparatorUUID                 = "AB21B655-D3BE-4EAD-82C9-E8CFF02B2913";

	// ==========
	// = item_t =
	// ==========

	item_t::item_t (oak::uuid_t const& uuid, item_ptr bundleItem, kind_t kind, bool local) : _deleted(false), _disabled(false), _hidden_from_user(false), _local(local), _uuid(uuid), _bundle(bundleItem), _kind(kind), _full_name(NULL_STR)
	{
	}

	item_ptr item_t::menu_item_separator ()
	{
		static item_ptr item(new item_t(kSeparatorUUID, item_ptr(), kItemTypeMenuItemSeparator));
		return item;
	}

	void item_t::add_path (std::string const& path)
	{
		_paths.push_back(path);
	}

	bool item_t::initialize (plist::dictionary_t const& plist)
	{
		bool isDelta = false;
		if(plist::get_key_path(plist, kFieldIsDelta, isDelta) && isDelta)
			return false; // this should never happen

		// reset
		_disabled         = false;
		_deleted          = false;
		_hidden_from_user = false;
		_scope_selector   = scope::selector_t();
		_fields.clear();
		_required_bundles.clear();
		_required_executables.clear();

		plist::get_key_path(plist, kFieldIsDisabled,   _disabled);
		plist::get_key_path(plist, kFieldIsDeleted,    _deleted);
		plist::get_key_path(plist, kFieldHideFromUser, _hidden_from_user);

		iterate(pair, plist)
		{
			static std::string const stringKeys[]     = { kFieldName, kFieldKeyEquivalent, kFieldTabTrigger, kFieldScopeSelector, kFieldSemanticClass, kFieldContentMatch, kFieldGrammarFirstLineMatch, kFieldGrammarScope, kFieldGrammarInjectionSelector };
			static std::string const arrayKeys[]      = { kFieldDropExtension, kFieldGrammarExtension };
			static std::string const dictionaryKeys[] = { kFieldSettingName };

			if(pair->first == kFieldScopeSelector)
			{
				if(std::string const* str = boost::get<std::string>(&pair->second))
					_scope_selector = *str;
			}
			else if(oak::contains(beginof(stringKeys), endof(stringKeys), pair->first))
			{
				if(std::string const* str = boost::get<std::string>(&pair->second))
					_fields.insert(std::make_pair(pair->first, *str));
			}
			else if(oak::contains(beginof(arrayKeys), endof(arrayKeys), pair->first))
			{
				if(plist::array_t const* array = boost::get<plist::array_t>(&pair->second))
				{
					iterate(any, *array)
					{
						if(std::string const* str = boost::get<std::string>(&*any))
							_fields.insert(std::make_pair(pair->first, *str));
					}
				}
			}
			else if(oak::contains(beginof(dictionaryKeys), endof(dictionaryKeys), pair->first))
			{
				if(plist::dictionary_t const* dictionary = boost::get<plist::dictionary_t>(&pair->second))
				{
					iterate(dictPair, *dictionary)
						_fields.insert(std::make_pair(pair->first, dictPair->first));
				}
			}
		}

		plist::array_t require;
		if(plist::get_key_path(plist, kFieldRequiredItems, require))
		{
			iterate(it, require)
			{
				std::string name;
				oak::uuid_t uuid;
				if(plist::get_key_path(*it, kFieldName, name) && plist::get_key_path(*it, kFieldUUID, uuid))
					_required_bundles.push_back(required_bundle_t(name, uuid));
			}
		}

		return true;
	}

	std::string const& item_t::name () const
	{
		static std::string const separator = "~";
		if(_kind == kItemTypeMenuItemSeparator)
			return separator;

		std::multimap<std::string, std::string>::const_iterator it = _fields.find(kFieldName);
		static std::string const untitled = "untitled";
		return it == _fields.end() ? untitled : it->second;
	}

	void item_t::set_name (std::string const& newName)
	{
		_fields.erase(_fields.lower_bound(kFieldName), _fields.upper_bound(kFieldName));
		_fields.insert(std::make_pair(kFieldName, newName));
		_full_name = NULL_STR; // FIXME should setup a new full name based on menu nesting…
	}

	std::string const& item_t::full_name () const
	{
		if(_full_name == NULL_STR)
			_full_name = name() + (_kind == kItemTypeBundle || !bundle() ? "" : " — " + bundle()->name());
		return _full_name;
	}

	void item_t::set_full_name (std::string const& newFullName)
	{
		_full_name = newFullName;
	}

	oak::uuid_t const& item_t::uuid () const
	{
		return _uuid;
	}

	oak::uuid_t item_t::bundle_uuid () const
	{
		return _bundle ? _bundle->uuid() : (_kind == kItemTypeBundle ? _uuid : oak::uuid_t());
	}

	scope::selector_t const& item_t::scope_selector () const
	{
		return _scope_selector;
	}

	item_ptr item_t::bundle () const
	{
		return _bundle;
	}

	kind_t item_t::kind () const
	{
		return _kind;
	}

	std::string item_t::support_path () const
	{
		if(_kind != kItemTypeBundle)
			return bundle()->support_path();

		iterate(path, _paths)
		{
			std::string supportPath = path::join(*path, "../Support");
			if(path::exists(supportPath))
				return supportPath;
		}
		return NULL_STR;
	}

	std::map<std::string, std::string> item_t::environment (std::map<std::string, std::string> base) const
	{
		if(_kind != kItemTypeBundle)
		{
			base = bundle()->environment(base);

			base["TM_BUNDLE_ITEM_NAME"] = name();
			base["TM_BUNDLE_ITEM_UUID"] = to_s(uuid());
		}
		else
		{
			std::string const& bundleSupportPath = support_path();
			if(bundleSupportPath != NULL_STR)
				base["TM_BUNDLE_SUPPORT"] = bundleSupportPath;
		}

		iterate(require, _required_bundles)
		{
			auto bundles = query(kFieldName, require->_name, scope::wildcard, kItemTypeBundle, require->_uuid);
			if(bundles.size() == 1)
			{
				std::map<std::string, std::string> vars;
				vars.insert(std::make_pair("name", require->_name));
				base[format_string::expand("TM_${name/.*/\\U${0/[^a-zA-Z]+/_/g}/}_BUNDLE_SUPPORT", vars)] = (bundles.back())->support_path();
			}
			else
			{
				fprintf(stderr, "*** %s: unable to find required bundle: %s / %s\n", full_name().c_str(), require->_name.c_str(), to_s(require->_uuid).c_str());
			}
		}

		return base;
	}

	plist::dictionary_t const& item_t::plist () const
	{
		if(!_plist)
		{
			static plist::dictionary_t const fallback;
			switch(_paths.size())
			{
				case 0: return fallback;
				case 1: _plist.reset(new plist::dictionary_t(plist::load(_paths.back()))); break;

				default:
				{
					std::vector<plist::dictionary_t> plists;
					iterate(path, _paths)
						plists.push_back(plist::load(*path));
					_plist.reset(new plist::dictionary_t(plist::merge_delta(plists)));
				}
				break;
			}
		}
		return *_plist;
	}

	void item_t::set_plist (plist::dictionary_t const& plist, bool shouldInitialize)
	{
		_plist.reset(new plist::dictionary_t(plist));
		if(shouldInitialize)
			initialize(plist);
	}

	std::vector<std::string> item_t::values_for_field (std::string const& field) const
	{
		std::vector<std::string> res;
		std::transform(_fields.lower_bound(field), _fields.upper_bound(field), back_inserter(res), [](std::pair<std::string, std::string> const& p){ return p.second; });
		return res;
	}

	std::string const& item_t::value_for_field (std::string const& field) const
	{
		static std::string const fallback = NULL_STR;
		std::multimap<std::string, std::string>::const_iterator it = _fields.find(field);
		return it == _fields.end() ? fallback : it->second;
	}

	bool item_t::does_match (std::string const& field, std::string const& value, scope::context_t const& scope, int kind, oak::uuid_t const& bundle, double* rank)
	{
		bool match = true;
		if(field != kFieldAny)
		{
			match = false;
			foreach(pair, _fields.lower_bound(field), _fields.upper_bound(field))
				match = match || pair->second == value || (field == kFieldSemanticClass && pair->second.size() > value.size() && pair->second.find(value) == 0 && pair->second[value.size()] == '.');
		}

		match = match && (scope == scope::wildcard || _scope_selector.does_match(scope, rank));
		match = match && (_kind & kind) == _kind;
		match = match && (!bundle || bundle == bundle_uuid());
		return match;
	}

	bool item_t::save (bool useDeltaIfNonLocal)
	{
		static struct path_format_t { kind_t type; char const* format; } const PathFormats[] =
		{
			{ kItemTypeBundle,         "%s.tmbundle/info.plist"        },
			{ kItemTypeCommand,        "Commands/%s.tmCommand"         },
			{ kItemTypeDragCommand,    "DragCommands/%s.tmDragCommand" },
			{ kItemTypeMacro,          "Macros/%s.tmMacro"             },
			{ kItemTypeSettings,       "Preferences/%s.tmPreferences"  },
			{ kItemTypeSnippet,        "Snippets/%s.tmSnippet"         },
			{ kItemTypeGrammar,        "Syntaxes/%s.tmLanguage"        },
			{ kItemTypeProxy,          "Proxies/%s.tmProxy"            },
			{ kItemTypeTheme,          "Themes/%s.tmTheme"             },
		};

		std::string destPath = NULL_STR;
		if(_local)
		{
			destPath = _paths.front();
		}
		else
		{
			std::string location = NULL_STR;
			if(_kind == kItemTypeBundle)
			{
				location = path::join(locations().front(), "Bundles");
			}
			else
			{
				if(!bundle()->local())
				{
					if(!bundle()->save()) // create local bundle
						return false;
				}
				location = path::parent(bundle()->_paths.front());
			}

			for(size_t i = 0; i < sizeofA(PathFormats) && destPath == NULL_STR; ++i)
			{
				if(PathFormats[i].type == _kind)
				{
					std::string base = name();
					std::replace(base.begin(), base.end(), '/', ':');
					std::replace(base.begin(), base.end(), '.', '_');
					destPath = path::unique(path::join(location, text::format(PathFormats[i].format, base.c_str())));
				}
			}
		}

		plist::dictionary_t newPlist = plist();
		bool saveAsDelta = useDeltaIfNonLocal && (!_local && !_paths.empty() || _paths.size() > 1);
		if(saveAsDelta)
		{
			plist::dictionary_t oldPlist = plist::load(_paths[_local ? 1 : 0]);
			newPlist = plist::create_delta(oldPlist, newPlist);
		}

		if(!plist::save(destPath, newPlist, plist::kPlistFormatXML))
			return fprintf(stderr, "failed to save ‘%s’\n", destPath.c_str()), false;

		if(!_local)
		{
			_local = true;
			if(saveAsDelta)
					_paths.insert(_paths.begin(), destPath);
			else	_paths = std::vector<std::string>(1, destPath);
		}

		return true;
	}

	bool item_t::move_to_trash ()
	{
		bool res = true;
		if(_local && _paths.size() == 1)
		{
			res = path::move_to_trash(_paths.front()) != NULL_STR;
		}
		else if(!_paths.empty())
		{
			plist::dictionary_t dict = plist();
			dict[kFieldIsDeleted] = true;
			set_plist(dict, false);

			res = save();
		}

		_deleted = true;
		return res;
	}

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
					plist::save(_cache_file, _cache);
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

		private:
			static plist::dictionary_t prune_dictionary (plist::dictionary_t const& plist)
			{
				static std::string const DesiredKeysArray[] = { kFieldName, kFieldKeyEquivalent, kFieldTabTrigger, kFieldScopeSelector, kFieldSemanticClass, kFieldContentMatch, kFieldGrammarFirstLineMatch, kFieldGrammarScope, kFieldGrammarInjectionSelector, kFieldDropExtension, kFieldGrammarExtension, kFieldSettingName, kFieldHideFromUser, kFieldIsDeleted, kFieldIsDisabled, kFieldRequiredItems, kFieldUUID, kFieldMainMenu, kFieldIsDelta, kFieldDeletedItems, kFieldChangedItems };
				static std::set<std::string> const DesiredKeys(beginof(DesiredKeysArray), endof(DesiredKeysArray));

				plist::dictionary_t res;
				citerate(pair, plist)
				{
					if(DesiredKeys.find(pair->first) == DesiredKeys.end())
						continue;

					if(pair->first == kFieldSettingName)
					{
						if(plist::dictionary_t const* dictionary = boost::get<plist::dictionary_t>(&pair->second))
						{
							plist::dictionary_t prunedDict;
							iterate(settingsPair, *dictionary)
								prunedDict.insert(std::make_pair(settingsPair->first, true));
							res.insert(std::make_pair(pair->first, prunedDict));
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

	void item_t::traverse (std::map<std::string, fs::node_t> const& heads, std::string const& cacheFile)
	{
		property_cache_t plistCache(cacheFile);

		std::vector<item_ptr> items(1, item_t::menu_item_separator());
		std::map< oak::uuid_t, std::vector<oak::uuid_t> > menus;

		std::map<oak::uuid_t, delta_item_t> deltaItems;
		std::set< std::pair<oak::uuid_t, oak::uuid_t> > hiddenItems, deletedItems;
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

					plist::array_t uuids;
					plist::get_key_path(plist, kFieldDeletedItems, uuids);
					iterate(uuid, uuids)
					{
						if(std::string const* str = boost::get<std::string>(&*uuid))
							deletedItems.insert(std::make_pair(*str, bundleUUID));
					}

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
							item->_uuid = submenuIter->first;
							item->_fields.insert(std::make_pair(kFieldName, name));
							items.push_back(item);
							menus.insert(std::make_pair(submenuIter->first, to_menu(uuids)));
						}
					}

					plist::get_key_path(plist, "mainMenu.excludedItems", uuids);
					iterate(uuid, uuids)
					{
						if(std::string const* str = boost::get<std::string>(&*uuid))
							hiddenItems.insert(std::make_pair(*str, bundleUUID));
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
							if(!oak::contains(beginof(extensions), endof(extensions), path::extension(fsItem->name())))
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
				fprintf(stderr, "Orphaned item: ‘%s’\n", item->name().c_str());
				items.erase(items.begin() + i);
			}
			else
			{
				item->_hidden_from_user = hiddenItems.find(std::make_pair(item->uuid(), item->bundle_uuid())) != hiddenItems.end() ?: item->_hidden_from_user;
				item->_deleted          = deletedItems.find(std::make_pair(item->uuid(), item->bundle_uuid())) != deletedItems.end() ?: item->_deleted;
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
				item_t::traverse(heads, _cache_file);
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
