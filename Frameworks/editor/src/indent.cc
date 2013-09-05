#include "indent.h"
#include <bundles/bundles.h>

OAK_DEBUG_VAR(Indent);

namespace indent
{
	std::map<indent::pattern_type, regexp::pattern_t> patterns_for_scope (scope::context_t const& scope)
	{
		D(DBF_Indent, bug("scope: %s\n", to_s(scope).c_str()););
		std::map<indent::pattern_type, regexp::pattern_t> res;

		static std::map<std::string, indent::pattern_type> const map =
		{
			{ "increaseIndentPattern", indent::pattern_type::kIncrease     },
			{ "decreaseIndentPattern", indent::pattern_type::kDecrease     },
			{ "indentNextLinePattern", indent::pattern_type::kIncreaseNext },
			{ "unIndentedLinePattern", indent::pattern_type::kIgnore       },
			{ "zeroIndentPattern",     indent::pattern_type::kZeroIndent   },
		};

		for(auto pair : map)
		{
			plist::any_t const& plist = bundles::value_for_setting(pair.first, scope);
			if(std::string const* value = boost::get<std::string>(&plist))
			{
				D(DBF_Indent, bug("%s = %s\n", pair.first.c_str(), value->c_str()););
				res.emplace(pair.second, *value);
			}
		}
		return res;
	}

} /* indent */