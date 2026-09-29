#ifndef BE_ENTRY_H_HB65VO7S
#define BE_ENTRY_H_HB65VO7S

#include <bundles/src/bundles.h>
#include <io/src/path.h>

namespace be
{
	struct entry_t;
	typedef std::shared_ptr<entry_t> entry_ptr;

	struct entry_t
	{
		entry_t (bundles::item_ptr item, std::string const& name = NULL_STR) : _item(item), _name(name), _path(NULL_STR) { }
		entry_t (std::string const& name, std::string const& path = NULL_STR) : _name(name), _path(path) { }
		virtual ~entry_t () { }

		std::string name () const                        { return _name != NULL_STR ? _name : (_path != NULL_STR ? path::display_name(_path) : (_item ? _item->name() : NULL_STR)); }
		bundles::item_ptr represented_item () const      { return _item; }
		std::string represented_path () const            { return _path; }
		bool disabled () const                           { return _item ? (_item->kind() == bundles::kItemTypeMenuItemSeparator ? true : _item->disabled()) : false; }
		bool has_children () const                       { return setup_children(); }
		std::vector<entry_ptr> const& children () const;

		virtual std::string identifier () const          { return _item ? to_s(_item->uuid()) : (_path != NULL_STR ? _path : _name); }

	protected:
		virtual std::vector<entry_ptr> entries () const  { return kNoChildren; }

		bundles::item_ptr _item;
		std::string _name;
		std::string _path;

	private:
		static std::vector<entry_ptr> kNoChildren;
		mutable std::shared_ptr< std::vector<entry_ptr> > _children;

		bool setup_children () const
		{
			if(!_children)
				_children.reset(new std::vector<entry_ptr>(entries()));
			return *_children != kNoChildren;
		}
	};

	entry_ptr bundle_entries ();

} /* be */

#endif /* end of include guard: BE_ENTRY_H_HB65VO7S */
