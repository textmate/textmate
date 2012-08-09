#ifndef BUNDLES_WRAPPERS_H_IFW4TEJN
#define BUNDLES_WRAPPERS_H_IFW4TEJN

#include <oak/misc.h>
#include <scope/scope.h>
#include <plist/plist.h>

namespace bundles
{
	struct item_t;
	typedef std::tr1::shared_ptr<item_t> item_ptr;

	typedef std::map<std::string, std::string> string_map_t; // kludge to support empty map as a default argument
	PUBLIC std::map<std::string, std::string> environment (scope::context_t const& scope, std::map<std::string, std::string> const& base = string_map_t());
	PUBLIC plist::any_t value_for_setting (std::string const& setting, scope::context_t const& scope, item_ptr* match = NULL);

	PUBLIC std::vector<item_ptr> grammars_for_path (std::string const& path);
	PUBLIC std::vector<item_ptr> drag_commands_for_path (std::string const& ext, scope::context_t const& scope);

} /* bundles */

#endif /* end of include guard: BUNDLES_WRAPPERS_H_IFW4TEJN */
