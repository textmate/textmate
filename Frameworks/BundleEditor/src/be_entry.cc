#include "be_entry.h"
#include <text/src/my_ctype.h>
#include <io/src/entries.h>
#include <io/src/path.h>

namespace be
{
	std::vector<entry_ptr> entry_t::kNoChildren(1, entry_ptr());

	std::vector<entry_ptr> const& entry_t::children () const
	{
		static std::vector<entry_ptr> const EmptyVector;
		return setup_children() ? *_children : EmptyVector;
	}

	struct file_entry_t : entry_t
	{
		file_entry_t (std::string const& name, std::string const& path) : entry_t(name, path) { }
	};

	struct bundle_item_entry_t : entry_t
	{
		bundle_item_entry_t (bundles::item_ptr item) : entry_t(item) { }
	};

	struct directory_entry_t : file_entry_t
	{
		directory_entry_t (std::string const& name, std::string const& path) : file_entry_t(name, path) { }

		std::vector<entry_ptr> entries () const
		{
			std::vector<entry_ptr> res;
			if(_path == NULL_STR)
				return res;

			std::multimap<std::string, entry_ptr, text::less_t> directories, files;
			for(auto const& entry : path::entries(_path))
			{
				std::string const path = path::join(_path, entry->d_name);
				std::string const displayName = path::display_name(path);
				if(entry->d_type == DT_DIR)
					directories.emplace(displayName, std::make_shared<directory_entry_t>(displayName, path));
				else if(entry->d_type == DT_REG || entry->d_type == DT_LNK)
					files.emplace(displayName, std::make_shared<file_entry_t>(displayName, path));
			}

			std::transform(directories.begin(), directories.end(), back_inserter(res), [](std::pair<std::string, entry_ptr> const& p){ return p.second; });
			std::transform(files.begin(), files.end(), back_inserter(res), [](std::pair<std::string, entry_ptr> const& p){ return p.second; });
			return res;
		}
	};

	struct menu_entry_t : entry_t
	{
		menu_entry_t (std::string const& name, bundles::item_ptr menuItem) : entry_t(menuItem, name) { }

		std::string identifier () const
		{
			return _name;
		}

		std::vector<entry_ptr> entries () const
		{
			std::vector<entry_ptr> res;
			for(auto const& item : _item->menu(true))
			{
				if(item->kind() == bundles::kItemTypeMenu)
						res.push_back(std::make_shared<menu_entry_t>(item->name(), item));
				else	res.push_back(std::make_shared<bundle_item_entry_t>(item));
			}
			return res;
		}
	};

	struct group_entry_t : entry_t
	{
		group_entry_t (std::string const& name, bundles::item_ptr bundle, bundles::kind_t kind) : entry_t(name), _bundle(bundle), _kind(kind) { }

		std::vector<entry_ptr> entries () const
		{
			std::multimap<std::string, bundles::item_ptr, text::less_t> ordered;
			for(auto const& item : bundles::query(bundles::kFieldAny, NULL_STR, scope::wildcard, _kind, _bundle->uuid(), false, true))
				ordered.emplace(item->name(), item);

			std::vector<entry_ptr> res;
			for(auto const& pair : ordered)
				res.push_back(std::make_shared<bundle_item_entry_t>(pair.second));
			return res;
		}

	private:
		bundles::item_ptr _bundle;
		bundles::kind_t _kind;
	};

	struct other_entry_t : entry_t
	{
		other_entry_t (std::string const& name, bundles::item_ptr bundle) : entry_t(name), _bundle(bundle) { }

		std::vector<entry_ptr> entries () const
		{
			std::multimap<std::string, bundles::item_ptr, text::less_t> ordered;
			for(auto const& item : bundles::query(bundles::kFieldAny, NULL_STR, scope::wildcard, bundles::kItemTypeMenuTypes, _bundle->uuid(), false, true))
			{
				if(item->hidden_from_user())
					ordered.emplace(item->name(), item);
			}

			std::vector<entry_ptr> res;
			for(auto const& pair : ordered)
				res.push_back(std::make_shared<bundle_item_entry_t>(pair.second));
			return res;
		}

	private:
		bundles::item_ptr _bundle;
	};

	struct bundle_entry_t : entry_t
	{
		bundle_entry_t (bundles::item_ptr item) : entry_t(item) { }

		std::vector<entry_ptr> entries () const
		{
			std::vector<entry_ptr> res;
			res.push_back(std::make_shared<menu_entry_t>("Menu Actions",       _item));
			res.push_back(std::make_shared<other_entry_t>("Other Actions",     _item));
			res.push_back(std::make_shared<group_entry_t>("File Drop Actions", _item, bundles::kItemTypeDragCommand));
			res.push_back(std::make_shared<group_entry_t>("Language Grammars", _item, bundles::kItemTypeGrammar));
			res.push_back(std::make_shared<group_entry_t>("Settings",          _item, bundles::kItemTypeSettings));
			res.push_back(std::make_shared<directory_entry_t>("Support", _item->support_path()));
			// res.push_back(std::make_shared<group_entry_t>("Templates",         _item, bundles::kItemTypeTemplates));
			res.push_back(std::make_shared<group_entry_t>("Themes",            _item, bundles::kItemTypeTheme));
			return res;
		}
	};

	struct bundles_entry_t : entry_t
	{
		bundles_entry_t () : entry_t("Bundles") { }

		std::vector<entry_ptr> entries () const
		{
			std::multimap<std::string, bundles::item_ptr, text::less_t> ordered;
			for(auto const& item : bundles::query(bundles::kFieldAny, NULL_STR, scope::wildcard, bundles::kItemTypeBundle, oak::uuid_t(), false, true))
				ordered.emplace(item->name(), item);

			std::vector<entry_ptr> res;
			for(auto const& pair : ordered)
				res.push_back(std::make_shared<bundle_entry_t>(pair.second));
			return res;
		}
	};

	entry_ptr bundle_entries ()
	{
		return std::make_shared<bundles_entry_t>();
	}

} /* be */
