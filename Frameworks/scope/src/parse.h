#ifndef SCOPE_PARSE_H_MMVW7L5U
#define SCOPE_PARSE_H_MMVW7L5U

#include "types.h"

namespace scope
{
	namespace parse
	{
		char const* scope (char const* first, char const* last, scope::types::scope_t& scope);
		char const* path (char const* first, char const* last, scope::types::path_t& path);
		char const* selector (char const* first, char const* last, scope::types::selector_t& selector);
	
	} /* parse */ 

} /* scope */ 

#endif /* end of include guard: SCOPE_PARSE_H_MMVW7L5U */
