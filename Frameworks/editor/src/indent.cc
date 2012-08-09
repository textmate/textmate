#include "indent.h"
#include <bundles/bundles.h>

OAK_DEBUG_VAR(Indent);

namespace indent
{
	patterns_t patterns_for_scope (scope::context_t const& scope)
	{
		D(DBF_Indent, bug("scope: %s\n", to_s(scope).c_str()););

		static std::string const settings[] = { "increaseIndentPattern", "decreaseIndentPattern", "indentNextLinePattern", "unIndentedLinePattern" };
		patterns_t res;
		iterate(it, settings)
		{
			plist::any_t const& plist = bundles::value_for_setting(*it, scope);
			if(std::string const* value = boost::get<std::string>(&plist))
			{
				D(DBF_Indent, bug("%s = %s\n", it->c_str(), value->c_str()););
				res.array[it - settings] = *value;
			}
		}
		return res;
	}

} /* indent */