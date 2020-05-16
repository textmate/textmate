#include "item.h"
#include "query.h"     // required by item_t::environment
#include "locations.h" // required by item_t::save
#include <plist/delta.h>
#include <io/path.h>
#include <text/format.h>
#include <text/parse.h>
#include <text/trim.h>
#include <regexp/format_string.h>

namespace bundles
{
	int kItemTypeMenuTypes = kItemTypeCommand|kItemTypeMacro|kItemTypeSnippet|kItemTypeProxy;
	int kItemTypeMost      = ~(kItemTypeSettings|kItemTypeBundle|kItemTypeMenu|kItemTypeMenuItemSeparator|kItemTypeUnknown);
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

	std::string const kFieldAny                      = NULL_STR;

	oak::uuid_t const kSeparatorUUID                 = "AB21B655-D3BE-4EAD-82C9-E8CFF02B2913";

	// ==========
	// = item_t =
	// ==========

	item_t::item_t (oak::uuid_t const& uuid, item_ptr bundleItem, kind_t kind, bool local) : _deleted(false), _disabled(false), _hidden_from_user(false), _local(local), _uuid(uuid), _bundle(bundleItem), _kind(kind)
	{
	}

	item_ptr item_t::menu_item_separator ()
	{
		static item_ptr item = std::make_shared<item_t>(kSeparatorUUID, item_ptr(), kItemTypeMenuItemSeparator);
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

		for(auto const& pair : plist)
		{
			static std::set<std::string> const stringKeys     = { kFieldName, kFieldKeyEquivalent, kFieldTabTrigger, kFieldScopeSelector, kFieldSemanticClass, kFieldContentMatch, kFieldGrammarFirstLineMatch, kFieldGrammarScope, kFieldGrammarInjectionSelector };
			static std::set<std::string> const arrayKeys      = { kFieldDropExtension, kFieldGrammarExtension };

			if(pair.first == kFieldScopeSelector)
			{
				if(std::string const* str = boost::get<std::string>(&pair.second))
					_scope_selector = *str;
			}
			else if(stringKeys.find(pair.first) != stringKeys.end())
			{
				if(std::string const* str = boost::get<std::string>(&pair.second))
				{
					if(pair.first == kFieldSemanticClass)
					{
						for(std::string const& value : text::split(*str, ","))
							_fields.emplace(pair.first, text::trim(value));
					}
					else
					{
						_fields.emplace(pair.first, *str);
					}
				}
			}
			else if(arrayKeys.find(pair.first) != arrayKeys.end())
			{
				if(plist::array_t const* array = boost::get<plist::array_t>(&pair.second))
				{
					for(auto const& any : *array)
					{
						if(std::string const* str = boost::get<std::string>(&any))
							_fields.emplace(pair.first, *str);
					}
				}
			}
			else if(pair.first == kFieldSettingName)
			{
				// initialize from a tmSettings file
				if(plist::dictionary_t const* dictionary = boost::get<plist::dictionary_t>(&pair.second))
				{
					for(auto const& dictPair : *dictionary)
						_fields.emplace(pair.first, dictPair.first);
				}
				else if(plist::array_t const* array = boost::get<plist::array_t>(&pair.second))
				{
					for(auto const& any : *array) // initialize from cache
					{
						if(std::string const* str = boost::get<std::string>(&any))
							_fields.emplace(pair.first, *str);
					}
				}
			}
			else if(pair.first == kFieldIsDisabled)
			{
				_disabled = plist::is_true(pair.second);
			}
			else if(pair.first == kFieldIsDeleted)
			{
				_deleted = plist::is_true(pair.second);
			}
			else if(pair.first == kFieldHideFromUser)
			{
				_hidden_from_user = plist::is_true(pair.second);
			}
			else if(pair.first == kFieldRequiredItems && boost::get<plist::array_t>(&pair.second))
			{
				for(auto const& dict : boost::get<plist::array_t>(pair.second))
				{
					std::string name;
					oak::uuid_t uuid;
					if(plist::get_key_path(dict, kFieldName, name) && plist::get_key_path(dict, kFieldUUID, uuid))
						_required_bundles.emplace_back(name, uuid);
				}
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
		_fields.emplace(kFieldName, newName);
	}

	std::string item_t::name_with_bundle () const
	{
		return name() + (bundle() ? " — " + bundle()->name() : "");
	}

	oak::uuid_t const& item_t::parent_menu () const
	{
		return _parent_menu;
	}

	void item_t::set_parent_menu (oak::uuid_t const& newParentMenu)
	{
		_parent_menu = newParentMenu;
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

		for(auto const& path : _paths)
		{
			std::string supportPath = path::join(path, "../Support");
			if(path::exists(supportPath))
				return supportPath;
		}
		return NULL_STR;
	}

	std::map<std::string, std::string> item_t::bundle_variables () const
	{
		std::map<std::string, std::string> base;
		if(_kind != kItemTypeBundle)
		{
			base << bundle()->bundle_variables();

			base["TM_BUNDLE_ITEM_NAME"] = name();
			base["TM_BUNDLE_ITEM_UUID"] = to_s(uuid());
		}
		else
		{
			std::string const& bundleSupportPath = support_path();
			if(bundleSupportPath != NULL_STR)
				base["TM_BUNDLE_SUPPORT"] = bundleSupportPath;
		}

		for(auto require : _required_bundles)
		{
			auto bundles = query(kFieldName, require._name, scope::wildcard, kItemTypeBundle, require._uuid);
			if(bundles.size() == 1)
					base[format_string::expand("TM_${name/.*/\\U${0/[^a-zA-Z]+/_/g}/}_BUNDLE_SUPPORT", std::map<std::string, std::string>{ { "name", require._name } })] = (bundles.back())->support_path();
			else	os_log_error(OS_LOG_DEFAULT, "%{public}s: unable to find required bundle: %{public}s / %{public}s", name_with_bundle().c_str(), require._name.c_str(), to_s(require._uuid).c_str());
		}

		return base;
	}

	plist::dictionary_t const& item_t::plist () const
	{
		std::lock_guard<std::mutex> lock(_plist_mutex);
		if(!_plist)
		{
			static plist::dictionary_t const fallback;
			switch(_paths.size())
			{
				case 0: return fallback;
				case 1: _plist = std::make_shared<plist::dictionary_t>(plist::load(_paths.back())); break;

				default:
				{
					std::vector<plist::dictionary_t> plists;
					for(auto const& path : _paths)
						plists.push_back(plist::load(path));
					_plist = std::make_shared<plist::dictionary_t>(plist::merge_delta(plists));
				}
				break;
			}
		}
		return *_plist;
	}

	void item_t::set_plist (plist::dictionary_t const& plist, bool shouldInitialize)
	{
		std::lock_guard<std::mutex> lock(_plist_mutex);
		_plist = std::make_shared<plist::dictionary_t>(plist);
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

	std::optional<double> item_t::does_match (std::string const& field, std::string const& value, scope::context_t const& scope, int kind, oak::uuid_t const& bundle)
	{
		bool match = true;
		if(field != kFieldAny)
		{
			match = false;
			foreach(pair, _fields.lower_bound(field), _fields.upper_bound(field))
				match = match || pair->second == value || (field == kFieldSemanticClass && pair->second.size() > value.size() && pair->second.find(value) == 0 && pair->second[value.size()] == '.');
		}

		match = match && (_kind & kind) == _kind;
		match = match && (!bundle || bundle == bundle_uuid());
		return match ? (scope == scope::wildcard ? 1 : _scope_selector.does_match(scope)) : std::optional<double>();
	}

	plist::dictionary_t erase_false_values (plist::dictionary_t const& plist)
	{
		plist::dictionary_t res;
		for(auto const& pair : plist)
		{
			if(!boost::get<bool>(&pair.second) || boost::get<bool>(pair.second))
				res.insert(res.end(), pair);
		}
		return res;
	}

	static std::string path_for_kind (std::string const& folder, std::string name, kind_t kind)
	{
		static struct path_format_t { kind_t type; char const* format; } const PathFormats[] =
		{
			{ kItemTypeBundle,         "%s.tmbundle"                   },
			{ kItemTypeCommand,        "Commands/%s.tmCommand"         },
			{ kItemTypeDragCommand,    "DragCommands/%s.tmDragCommand" },
			{ kItemTypeMacro,          "Macros/%s.tmMacro"             },
			{ kItemTypeSettings,       "Preferences/%s.tmPreferences"  },
			{ kItemTypeSnippet,        "Snippets/%s.tmSnippet"         },
			{ kItemTypeGrammar,        "Syntaxes/%s.tmLanguage"        },
			{ kItemTypeProxy,          "Proxies/%s.tmProxy"            },
			{ kItemTypeTheme,          "Themes/%s.tmTheme"             },
		};

		for(auto const& pathFormat : PathFormats)
		{
			if(pathFormat.type == kind)
			{
				std::replace(name.begin(), name.end(), '/', ':');
				std::replace(name.begin(), name.end(), '.', '_');

				std::string path = path::unique(path::join(folder, text::format(pathFormat.format, name.c_str())));
				if(kind == kItemTypeBundle)
					path = path::join(path, "info.plist");

				return path;
			}
		}
		return NULL_STR;
	}

	bool item_t::save (bool useDeltaIfNonLocal)
	{
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
			destPath = path_for_kind(location, name(), _kind);
		}

		plist::dictionary_t newPlist = erase_false_values(plist());
		bool saveAsDelta = useDeltaIfNonLocal && (!_local && !_paths.empty() || _paths.size() > 1);
		if(saveAsDelta)
		{
			plist::dictionary_t oldPlist = erase_false_values(plist::load(_paths[_local ? 1 : 0]));
			if(oldPlist == newPlist && _kind != kItemTypeBundle)
			{
				if(unlink(destPath.c_str()) == 0 || errno == ENOENT)
				{
					if(_local)
					{
						_local = false;
						if(_paths.size() > 1)
							_paths.erase(_paths.begin(), ++_paths.begin());
					}
					return true;
				}
			}

			newPlist = plist::create_delta(oldPlist, newPlist);
		}

		if(!plist::save(destPath, newPlist, plist::kPlistFormatXML))
			return os_log_error(OS_LOG_DEFAULT, "Failed to save ‘%{public}s’", destPath.c_str()), false;

		if(!_local)
		{
			_local = true;
			destPath = path::resolve(destPath);
			if(saveAsDelta)
					_paths.insert(_paths.begin(), destPath);
			else	_paths = std::vector<std::string>(1, destPath);
		}

		return true;
	}

	bool item_t::save_to (std::string const& folder)
	{
		std::string const path = _kind == kItemTypeBundle ? path::join(folder, "info.plist") : path_for_kind(folder, name(), _kind);
		if(!plist::save(path, erase_false_values(plist()), plist::kPlistFormatXML))
			return os_log_error(OS_LOG_DEFAULT, "Failed to save ‘%{public}s’", path.c_str()), false;
		return true;
	}

	bool item_t::move_to_trash ()
	{
		bool res = true;
		if(_local && _paths.size() == 1)
		{
			std::string path = _paths.front();
			if(_kind == kItemTypeBundle)
				path = path::parent(path);
			res = path::move_to_trash(path) != NULL_STR;
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

} /* bundles */
