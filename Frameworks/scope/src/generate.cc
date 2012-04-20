#include "compressed.h"
#include "compile.h"
#include "types.h"
#include <oak/oak.h>

// expression
scope::compressed::expression_t generate (scope::types::expression_t const& expression, scope::compile::analyze_t const& root)
{	
	scope::compressed::expression_t xexpression(expression.op);
	xexpression.negate = expression.negate;
	xexpression.selector = expression.selector->generate(root);
	return xexpression;		
}

//composite
scope::compressed::composite_t generate (scope::types::composite_t const& composite, scope::compile::analyze_t const& root)
{
	
	scope::compressed::composite_t xcomposite;
	iterate(iter, composite.expressions)
	{
		auto expr = generate(*iter, root);
		xcomposite.expressions.push_back(expr);
	}
	return xcomposite;
}

// selector
scope::compressed::selector_t generate (scope::types::selector_t const& selector, scope::compile::analyze_t const& root)
{
	scope::compressed::selector_t xselector;
	iterate(iter, selector.composites)
	{
		xselector.composites.push_back(generate(*iter, root));
	}
	return xselector;
}

//path
scope::compressed::any_ptr generate (scope::types::path_t const& path, scope::compile::analyze_t const& root)
{	
	scope::compressed::path_t* ptr = new scope::compressed::path_t();
	ptr->anchor_to_bol = path.anchor_to_bol;
	ptr->anchor_to_eol = path.anchor_to_eol;
	
	iterate(iter, path.scopes)
	{
		const scope::compile::analyze_t* wc = &root;
		int number = 0;
		iterate(iter2, iter->atoms)
		{
			assert(wc->path.find(*iter2) != wc->path.end());
			wc = &wc->path.at(*iter2);
			number++;
		}
		scope::compressed::scope_t xscope;
		xscope.data = wc->hash;
		xscope.mask = wc->mask;
		xscope.number = number;
		ptr->scopes.push_back(xscope);
		//generate
	}
	scope::compressed::any_ptr xpath = scope::compressed::any_ptr(ptr);
	return xpath;
}
//group
scope::compressed::any_ptr generate (scope::types::group_t const& group, scope::compile::analyze_t const& root)
{
	scope::compressed::group_t* ptr = new scope::compressed::group_t();
	ptr->selector = generate(group.selector, root);

 	scope::compressed::any_ptr xgroup = scope::compressed::any_ptr( ptr);
	return xgroup;
}
//filter
scope::compressed::any_ptr generate (scope::types::filter_t const& filter, scope::compile::analyze_t const& root)
{
	scope::compressed::filter_t* ptr = new scope::compressed::filter_t();
	ptr->filter = scope::compressed::filter_t::side_t((char)filter.filter);
	ptr->selector = filter.selector->generate(root);
 	
	scope::compressed::any_ptr xfilter = scope::compressed::any_ptr(ptr);
	return xfilter;
}

scope::compressed::any_ptr scope::types::path_t::generate (compile::analyze_t const& root) const
{ return ::generate(*this, root); }
scope::compressed::any_ptr scope::types::group_t::generate (compile::analyze_t const& root) const         
{ return ::generate(*this, root); }
scope::compressed::any_ptr scope::types::filter_t::generate (compile::analyze_t const& root) const
{ return ::generate(*this, root); }

void scope::compile::compiler_t::compress (const scope::selector_t& selector, int rule_id, int composite_index)
{
	scope::compile::sub_rule_t sr;
	auto ptr = generate(selector.selector->composites[composite_index], root);
	sr.composite = scope::compile::sub_rule_t::composite_ptr(new scope::compressed::composite_t(ptr));
	sr.rule_id = rule_id;
	_expressions.push_back(sr);
}
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
