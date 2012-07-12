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
void build (scope::types::expression_t const& expression, scope::compile::analyze_t& analyzer, scope::types::side_t right_side, bool negate)
{
	bool expr_neg = expression.op == scope::types::expression_t::op_t::op_minus || expression.negate;
	negate = expr_neg ^ negate; // + + = +, - - = +, + - = -
	
	expression.selector->build(analyzer, right_side, negate);
}

//composite
void build (scope::types::composite_t const& composite, scope::compile::analyze_t& analyzer, scope::types::side_t right_side, bool negate)
{
	iterate(iter, composite.expressions)
	{
		build(*iter, analyzer, right_side, negate);
	}
}

// selector
void build (scope::types::selector_t const& selector, scope::compile::analyze_t& analyzer, scope::types::side_t right_side, bool negate)
{
	iterate(iter, selector.composites)
	{
		build(*iter, analyzer, right_side, negate);
	}
}

scope::compile::scopesx* choose(bool negate, scope::compile::analyze_t::paths_t& path)
{
	return negate? &path.not_paths: &path.or_paths;
}

//path
void build (scope::types::path_t const& path, scope::compile::analyze_t& analyzer, scope::types::side_t right_side, bool negate)
{
	std::vector<scope::compile::scopesx*> affected;
	if(right_side == scope::types::side_t::right) 
	{
		affected.push_back(choose(negate, analyzer.right));
		analyzer.needs_right = true;
	}
	else if(right_side  == scope::types::side_t::both) 
	{
		affected.push_back(choose(negate, analyzer.left));
		affected.push_back(choose(negate, analyzer.right));
		analyzer.needs_right = true;
	}
	else
	{
		affected.push_back(choose(negate, analyzer.left));
	}

	iterate(iter, path.scopes)
	{
		iterate(a, affected)
		  (*a)->push_back(std::vector<std::string>());
		//scope::compile::analyze_t* wc = &analyzer;

		iterate(iter2, iter->atoms)
		{
			//wc = &wc->path[*iter2];
			iterate(a, affected)
			  (*a)->back().push_back(*iter2);
		}

	}	
		
}
//group
void build (scope::types::group_t const& group, scope::compile::analyze_t& analyzer, scope::types::side_t right_side, bool negate)
{
	build(group.selector, analyzer, right_side, negate);
}
//filter
void build (scope::types::filter_t const& filter, scope::compile::analyze_t& analyzer, scope::types::side_t right_side, bool negate)
{
	//
	filter.build(analyzer, filter.filter, negate);
}

void scope::types::path_t::build (compile::analyze_t& analyzer, scope::types::side_t right_side, bool negate) const           { ::build(*this, analyzer, right_side, negate); }
void scope::types::group_t::build (compile::analyze_t& analyzer, scope::types::side_t right_side, bool negate) const          { ::build(*this, analyzer, right_side, negate); }
void scope::types::filter_t::build (compile::analyze_t& analyzer, scope::types::side_t right_side, bool negate) const         { ::build(*this, analyzer, right_side, negate); }
