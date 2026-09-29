#ifndef TEST_BUNDLE_INDEX_H_FIMSPZJT
#define TEST_BUNDLE_INDEX_H_FIMSPZJT

#include <bundles/src/bundles.h>
#include <plist/ascii.h>

namespace test
{
	struct bundle_index_t
	{
		bundle_index_t ()
		{
			_bundle = add(bundles::kItemTypeBundle, "{ name = 'Fixtures Bundle'; }");
		}

		bundles::item_ptr add (bundles::kind_t itemKind, plist::dictionary_t const& plist)
		{
			oak::uuid_t uuid;
			if(!plist::get_key_path(plist, bundles::kFieldUUID, uuid))
				uuid.generate();

			auto item = std::make_shared<bundles::item_t>(uuid, itemKind == bundles::kItemTypeBundle ? bundles::item_ptr() : _bundle, itemKind);
			item->set_plist(plist);
			_items.push_back(item);

			return item;
		}

		bundles::item_ptr add (bundles::kind_t itemKind, std::string const& plistString)
		{
			return add(itemKind, boost::get<plist::dictionary_t>(plist::parse_ascii(plistString)));
		}

		bool commit () const
		{
			return bundles::set_index(_items);
		}

	private:
		bundles::item_ptr _bundle;
		std::vector<bundles::item_ptr> _items;
	};

} /* test */

#endif /* end of include guard: TEST_BUNDLE_INDEX_H_FIMSPZJT */
