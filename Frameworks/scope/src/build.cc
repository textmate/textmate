#include "compile.h"
#include "types.h"
// Addition trick: use one bit for each "path" + 1 for _overflow_
// investigate op
// pure  const& : after addition trick, immediately fetch result.
// pure | : immediately fetch result, no addition trick needed.
// - is it worth it to use a negate mask?
// pure ' ' : after addition trick, scope can be matched.


// expression
void build (scope::types::expression_t const& expression, scope::compile::analyze_t& root)
{
	if(expression.op != scope::types::expression_t::op_t::op_minus && !expression.negate)
		expression.selector->build(root);		
}

//composite
void build (scope::types::composite_t const& composite, scope::compile::analyze_t& root)
{
	std::vector<scope::types::expression_t>::const_iterator iter = composite.expressions.begin();
	while(iter != composite.expressions.end())
	{
		build(*iter, root);
	}
}

// selector
void build (scope::types::selector_t const& selector, scope::compile::analyze_t& root)
{
	std::vector<scope::types::composite_t>::const_iterator iter = selector.composites.begin();
	while(iter != selector.composites.end())
	{
		build(*iter, root);
	}
}

//path
void build (scope::types::path_t const& path, scope::compile::analyze_t& root)
{
	std::vector<scope::types::scope_t>::const_iterator iter = path.scopes.begin();
	while(iter != path.scopes.end())
	{
		scope::compile::analyze_t* wc;
		std::vector<scope::types::atom_t>::const_iterator iter2 = iter->atoms.begin();
		while(iter2 != iter->atoms.end())
		{
			wc = &root.path[*iter2];
		}
	}	
		
}
//group
void build (scope::types::group_t const& group, scope::compile::analyze_t& root)
{
	build(group.selector, root);
}
//filter
void build (scope::types::filter_t const& filter, scope::compile::analyze_t& root)
{
	filter.build(root);
}


template<typename T>
const scope::compile::compiled_t<T> build (const std::vector<T> const const& list)
{
	scope::compile::analyze_t root;
	typename std::vector<T>::const_iterator iter = list.begin();
	while(iter != list.end())
	{
		std::vector<scope::types::composite_t>::const_iterator iter2 = iter->composites.begin();
		while(iter2 != iter->composites.end())
		{
			build(*iter2, root);
			iter2++;
		}
		iter++;
	}
}


void scope::types::path_t::build (compile::analyze_t& root) const           { ::build(*this, root); }
void scope::types::group_t::build (compile::analyze_t& root) const          { ::build(*this, root); }
void scope::types::filter_t::build (compile::analyze_t& root) const         { ::build(*this, root); }

/*
	atom:         «string» | '*' // atom_any
	scope:        «atom» ('.' «atom»)*
	path:         '^'? «scope» ('>'? «scope»)* '$'?
	group:        '(' «selector» ')'
	filter:       ("L:"|"R:"|"B:") («group» | «path») //selector
	expression:   '-'? («filter» | «group» | «path») // selector negate
	composite:    «expression» ([|&-] «expression»)*
	selector:     «composite» (',' «composite»)*
*/

/*
struct compiled_styles_t
{
	compiled_styles_t (std::vector<decomposed_style_t> const const& styles);
	decomposed_style_t const const& decomposed_style_for_scope () const;
private:
	struct stop_t;
	struct stop_t
	{
		std::map<scope::types::atom_t, stop_t> _stops;
		decomposed_style_t* style;
		long* affected_rules;
	}
	stop_t root;
	int capacity;
	long* mutator;
	long* mask;
	long* rules;
	std::vector<decomposed_style_t> candidates;
}

*/
