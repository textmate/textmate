#ifndef BUNDLES_WRAPPERS_H_IFW4TEJN
#define BUNDLES_WRAPPERS_H_IFW4TEJN

#include <scope/src/scope.h>
#include <plist/src/plist.h>

namespace bundles
{
	struct item_t;
	typedef std::shared_ptr<item_t> item_ptr;

	struct required_command_t
	{
		required_command_t (std::string const& command = NULL_STR, std::string const& moreInfoUrl = NULL_STR, std::string const& variable = NULL_STR, std::vector<std::string> const& locations = std::vector<std::string>());

		std::string command;
		std::string more_info_url;
		std::string variable;
		std::vector<std::string> locations;
	};

	bool missing_requirement (item_ptr const& item, std::map<std::string, std::string>& environment, required_command_t* failedRequirement);

	std::vector< std::pair<std::string, std::string> > shell_variables (item_ptr const& item);
	std::map<std::string, std::string> scope_variables (std::map<std::string, std::string> const& base, scope::context_t const& scope = scope::context_t());
	plist::any_t value_for_setting (std::string const& setting, scope::context_t const& scope, item_ptr* match = NULL);

	std::vector<item_ptr> drag_commands_for_path (std::string const& ext, scope::context_t const& scope);

} /* bundles */

#endif /* end of include guard: BUNDLES_WRAPPERS_H_IFW4TEJN */
