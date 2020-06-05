#ifndef BUNDLES_ITEM_H_H2GEVOXK
#define BUNDLES_ITEM_H_H2GEVOXK

#include <plist/plist.h>
#include <scope/scope.h>

namespace fs { struct node_t; }

namespace bundles
{
	enum kind_t { kItemTypeCommand = 1, kItemTypeDragCommand = 2, kItemTypeGrammar = 4, kItemTypeMacro = 8, kItemTypeSettings = 16, kItemTypeSnippet = 32, kItemTypeProxy = 64, kItemTypeTheme = 128, kItemTypeBundle = 256, kItemTypeMenu = 512, kItemTypeMenuItemSeparator = 1024, kItemTypeUnknown = 2048 };

	extern int kItemTypeMenuTypes;
	extern int kItemTypeMost;
	extern int kItemTypeAny;

	extern std::string const kFieldName;
	extern std::string const kFieldUUID;
	extern std::string const kFieldIsDisabled;
	extern std::string const kFieldHideFromUser;

	extern std::string const kFieldKeyEquivalent;
	extern std::string const kFieldTabTrigger;
	extern std::string const kFieldScopeSelector;

	extern std::string const kFieldSemanticClass;
	extern std::string const kFieldContentMatch;
	extern std::string const kFieldDropExtension;
	extern std::string const kFieldGrammarExtension;
	extern std::string const kFieldGrammarFirstLineMatch;
	extern std::string const kFieldGrammarScope;
	extern std::string const kFieldGrammarInjectionSelector;
	extern std::string const kFieldSettingName;

	extern std::string const kFieldAny;

	extern std::string const kFieldIsDelta;
	extern std::string const kFieldIsDeleted;
	extern std::string const kFieldRequiredItems;
	extern oak::uuid_t const kSeparatorUUID;

	struct item_t;
	typedef std::shared_ptr<item_t> item_ptr;

	struct item_t
	{
		std::string const& name () const;
		void set_name (std::string const& newName);
		std::string name_with_bundle () const;
		oak::uuid_t const& parent_menu () const;
		void set_parent_menu (oak::uuid_t const& uuid);
		oak::uuid_t const& uuid () const;
		oak::uuid_t bundle_uuid () const;
		scope::selector_t const& scope_selector () const;
		std::map<std::string, std::string> bundle_variables () const;
		plist::dictionary_t const& plist () const;
		kind_t kind () const;

		bool disabled () const         { return _disabled; }
		bool hidden_from_user () const { return _hidden_from_user; }
		bool deleted () const          { return _deleted; }
		bool local () const            { return _local; }

		item_ptr bundle () const;
		std::string support_path () const;
		std::vector<item_ptr> menu (bool includeDisabledItems = false) const;
		std::vector<std::string> const& paths () const { return _paths; }

		std::vector<std::string> values_for_field (std::string const& field) const;
		std::string const& value_for_field (std::string const& field) const;

		bool save (bool useDeltaIfNonLocal = true);
		bool save_to (std::string const& folder);
		bool move_to_trash ();

		item_t (oak::uuid_t const& uuid, item_ptr bundleItem, kind_t kind, bool local = false);
		static item_ptr menu_item_separator ();
		void add_path (std::string const& path);
		bool initialize (plist::dictionary_t const& plist);
		void set_plist (plist::dictionary_t const& plist, bool shouldInitialize = true);
		std::optional<double> does_match (std::string const& field, std::string const& value, scope::context_t const& scope, int kind, oak::uuid_t const& bundle);

	private:
		struct required_bundle_t
		{
			required_bundle_t (std::string const& name, oak::uuid_t const& uuid) : _name(name), _uuid(uuid) { }

			std::string _name;
			oak::uuid_t _uuid;
		};

		struct required_executable_t
		{
			std::string _executable; // e.g. ‘git’
			std::string _variable;   // e.g. ‘TM_GIT’
			std::string _help;       // e.g. ‘Install from MacPorts: sudo port install git-core’
		};

		bool _deleted;
		bool _disabled;
		bool _hidden_from_user; // not shown in menus
		bool _local; // if in ~/Library/…

		oak::uuid_t _uuid;
		item_ptr _bundle;
		kind_t _kind;

		scope::selector_t _scope_selector;

		std::vector<std::string> _paths;
		std::multimap<std::string, std::string> _fields;

		mutable std::shared_ptr<plist::dictionary_t> _plist;
		mutable std::mutex _plist_mutex;
		std::vector<required_bundle_t> _required_bundles;
		std::vector<required_executable_t> _required_executables;
		oak::uuid_t _parent_menu;
	};

} /* bundles */

#endif /* end of include guard: BUNDLES_ITEM_H_H2GEVOXK */
