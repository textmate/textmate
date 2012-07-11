#include "compile.h"
#include "types.h"
#include <list>
#include <sstream>
// Addition trick: use one bit for each "path" + 1 for _overflow_
// investigate op
// pure  const& : after addition trick, immediately fetch result.
// pure | : immediately fetch result, no addition trick needed.
// - is it worth it to use a negate mask?
// pure ' ' : after addition trick, scope can be matched.

// expression
void build (scope::types::expression_t const& expression, scope::compile::analyze_t& analyzer, bool negate)
{
	bool expr_neg = expression.op == scope::types::expression_t::op_t::op_minus || expression.negate;
	negate = expr_neg ^ negate; // + + = +, - - = +, + - = -
	
	expression.selector->build(analyzer, negate);		
}

//composite
void build (scope::types::composite_t const& composite, scope::compile::analyze_t& analyzer, bool negate)
{
	iterate(iter, composite.expressions)
	{
		build(*iter, analyzer, negate);
	}
}

// selector
void build (scope::types::selector_t const& selector, scope::compile::analyze_t& analyzer, bool negate)
{
	iterate(iter, selector.composites)
	{
		build(*iter, analyzer, negate);
	}
}

//path
void build (scope::types::path_t const& path, scope::compile::analyze_t& analyzer, bool negate)
{
	scope::compile::scopesx* p = negate?  &analyzer.not_paths: &analyzer.or_paths;
	
	iterate(iter, path.scopes)
	{
		p->push_back(std::vector<std::string>());
		//scope::compile::analyze_t* wc = &analyzer;

		iterate(iter2, iter->atoms)
		{
			//wc = &wc->path[*iter2];
			p->back().push_back(*iter2);
		}

	}	
		
}
//group
void build (scope::types::group_t const& group, scope::compile::analyze_t& analyzer, bool negate)
{
	build(group.selector, analyzer, negate);
}
//filter
void build (scope::types::filter_t const& filter, scope::compile::analyze_t& analyzer, bool negate)
{
	filter.build(analyzer, negate);
}

void scope::types::path_t::build (compile::analyze_t& analyzer, bool negate) const           { ::build(*this, analyzer, negate); }
void scope::types::group_t::build (compile::analyze_t& analyzer, bool negate) const          { ::build(*this, analyzer, negate); }
void scope::types::filter_t::build (compile::analyze_t& analyzer, bool negate) const         { ::build(*this, analyzer, negate); }
