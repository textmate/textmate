#include "item.h"
#include "query.h"     // required by item_t::environment
#include "locations.h" // required by item_t::save
#include <plist/delta.h>
#include <io/path.h>
#include <text/format.h>
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

	std::string const kFieldAny                      = NULL_STR;

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
			static std::set<std::string> const stringKeys     = { kFieldName, kFieldKeyEquivalent, kFieldTabTrigger, kFieldScopeSelector, kFieldSemanticClass, kFieldContentMatch, kFieldGrammarFirstLineMatch, kFieldGrammarScope, kFieldGrammarInjectionSelector };
			static std::set<std::string> const arrayKeys      = { kFieldDropExtension, kFieldGrammarExtension };

			if(pair->first == kFieldScopeSelector)
			{
				if(std::string const* str = boost::get<std::string>(&pair->second))
					_scope_selector = *str;
			}
			else if(stringKeys.find(pair->first) != stringKeys.end())
			{
				if(std::string const* str = boost::get<std::string>(&pair->second))
					_fields.insert(std::make_pair(pair->first, *str));
			}
			else if(arrayKeys.find(pair->first) != arrayKeys.end())
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
			else if(pair->first == kFieldSettingName)
			{
				// initialize from a tmSettings file
				if(plist::dictionary_t const* dictionary = boost::get<plist::dictionary_t>(&pair->second))
				{
					iterate(dictPair, *dictionary)
						_fields.insert(std::make_pair(pair->first, dictPair->first));
				}
				else if(plist::array_t const* array = boost::get<plist::array_t>(&pair->second))
				{
					iterate(any, *array) // initialize from cache
					{
						if(std::string const* str = boost::get<std::string>(&*any))
							_fields.insert(std::make_pair(pair->first, *str));
					}
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
					_required_bundles.emplace_back(name, uuid);
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
			else	fprintf(stderr, "*** %s: unable to find required bundle: %s / %s\n", full_name().c_str(), require._name.c_str(), to_s(require._uuid).c_str());
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
					if(_kind == kItemTypeBundle)
						destPath = path::join(destPath, "info.plist");
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
			destPath = path::resolve(destPath);
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
