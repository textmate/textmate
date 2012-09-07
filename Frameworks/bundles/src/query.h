#ifndef BUNDLES_QUERY_H_7L9NPR0I
#define BUNDLES_QUERY_H_7L9NPR0I

#include "index.h"

namespace bundles
{
	PUBLIC bool set_index (std::vector<item_ptr> const& items, std::map< oak::uuid_t, std::vector<oak::uuid_t> > const& menus);
	inline bool set_index (std::vector<item_ptr> const& items) { return set_index(items, std::map< oak::uuid_t, std::vector<oak::uuid_t> >()); }

	PUBLIC std::vector<item_ptr> query (std::string const& field, std::string const& value, scope::context_t const& scope = scope::wildcard, int kind = kItemTypeCommand|kItemTypeDragCommand|kItemTypeGrammar|kItemTypeMacro|kItemTypeSnippet|kItemTypeProxy|kItemTypeTheme, oak::uuid_t const& bundle = oak::uuid_t(), bool filter = true, bool includeDisabledItems = false);
	PUBLIC std::vector<item_ptr> items_for_proxy (item_ptr proxyItem, scope::context_t const& scope = scope::wildcard, int kind = kItemTypeCommand|kItemTypeDragCommand|kItemTypeGrammar|kItemTypeMacro|kItemTypeSnippet|kItemTypeProxy|kItemTypeTheme, oak::uuid_t const& bundle = oak::uuid_t(), bool filter = true, bool includeDisabledItems = false);
	PUBLIC item_ptr lookup (oak::uuid_t const& uuid);
	PUBLIC std::string name_with_selection (item_ptr const& item, bool hasSelection);
	PUBLIC std::string full_name_with_selection (item_ptr const& item, bool hasSelection);
	PUBLIC std::string key_equivalent (item_ptr const& item);

} /* bundles */

#endif /* end of include guard: BUNDLES_QUERY_H_7L9NPR0I */
