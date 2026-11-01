#ifndef THEME_EDITOR_RULE_MATCHING_H
#define THEME_EDITOR_RULE_MATCHING_H

#include <plist/plist.h>
#include <scope/scope.h>

namespace theme_editor
{
	inline std::optional<size_t> matching_rule (plist::array_t const& rules, scope::scope_t const& scope)
	{
		if(scope.empty())
			return std::nullopt;
		std::optional<size_t> result;
		double bestRank = -1;
		for(size_t index = 0; index < rules.size(); ++index)
		{
			auto rule = plist::get<plist::dictionary_t>(&rules[index]);
			if(!rule)
				continue;
			std::string selector;
			plist::get_key_path(*rule, "scope", selector);
			if(auto rank = scope::selector_t(selector).does_match(scope))
			{
				// Match theme_t's specificity ordering and later-rule tie breaking.
				if(*rank >= bestRank)
				{
					bestRank = *rank;
					result = index;
				}
			}
		}
		return result;
	}
}

#endif
