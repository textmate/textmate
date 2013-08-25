#ifndef GRAMMAR_TYPES_H_4M8CRK03
#define GRAMMAR_TYPES_H_4M8CRK03

#include <scope/scope.h>
#include <oak/misc.h>
#include <oak/debug.h>

namespace parse
{
	struct stack_t;
	typedef std::shared_ptr<stack_t> stack_ptr;

	PUBLIC stack_ptr parse (char const* first, char const* last, stack_ptr stack, std::map<size_t, scope::scope_t>& scopes, bool firstLine);
	PUBLIC bool equal (stack_ptr lhs, stack_ptr rhs);

} /* parse */ 

#endif /* end of include guard: GRAMMAR_TYPES_H_4M8CRK03 */
