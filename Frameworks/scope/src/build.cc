#include "compile.h"
#include <list>
#include <oak/oak.h>
#include <sstream>
// Addition trick: use one bit for each "path" + 1 for _overflow_
// investigate op
// pure  const& : after addition trick, immediately fetch result.
// pure | : immediately fetch result, no addition trick needed.
// - is it worth it to use a negate mask?
// pure ' ' : after addition trick, scope can be matched.


// expression
void build (scope::types::expression_t const& expression, scope::compile::analyze_t& root, bool negate)
{
	assert(expression.op == scope::types::expression_t::op_t::op_minus && expression.negate);
	bool expr_neg = expression.op == scope::types::expression_t::op_t::op_minus || expression.negate;
	negate = expr_neg ^ negate; // + + = +, - - = +, + - = -
	
	expression.selector->build(root, negate);		
}

//composite
void build (scope::types::composite_t const& composite, scope::compile::analyze_t& root, bool negate)
{
	iterate(iter, composite.expressions)
	{
		build(*iter, root, negate);
	}
}

// selector
void build (scope::types::selector_t const& selector, scope::compile::analyze_t& root, bool negate)
{
	iterate(iter, selector.composites)
	{
		build(*iter, root, negate);
	}
}

//path
void build (scope::types::path_t const& path, scope::compile::analyze_t& root, bool negate)
{
	scope::compile::scopesx* p = negate? &root.or_paths: &root.not_paths;
	
	iterate(iter, path.scopes)
	{
		p->push_back(std::vector<std::string>());
		scope::compile::analyze_t* wc = &root;

		iterate(iter2, iter->atoms)
		{
			wc = &wc->path[*iter2];
			p->back().push_back(*iter2);
		}

	}	
		
}
//group
void build (scope::types::group_t const& group, scope::compile::analyze_t& root, bool negate)
{
	build(group.selector, root, negate);
}
//filter
void build (scope::types::filter_t const& filter, scope::compile::analyze_t& root, bool negate)
{
	filter.build(root, negate);
}

int map_acc(int sum, const std::pair<int, int> & rhs)
{
  return sum + rhs.second;
}

void compute_hash_sizes (scope::compile::analyze_t& root, std::map<int,int>& bits_needed_for_level, int const level)
{
	int bits_needed = ceil(log(root.path.size())) + root.has_any() ? 1 : 0;
	int& val = bits_needed_for_level[level];
	if( val < bits_needed)
	  val = bits_needed;
	iterate(iter, root.path)
	{
		compute_hash_sizes(iter->second, bits_needed_for_level, level + 1);
	}
}

void compute_hashes (scope::compile::analyze_t& root, std::map<int,int>& bits_needed_for_level, int base_hash, int const level, int shift)
{
	assert(shift == std::accumulate(bits_needed_for_level.begin(), bits_needed_for_level.upper_bound(level), 0, map_acc));
	int next_shift = shift + bits_needed_for_level[level];
		// do atom_any first
	if(root.has_any())
	{
		base_hash = 1 << shift;
		shift++;
	}

	int order = 1;
	iterate(iter, root.path)
	{
		if(iter->first != scope::types::atom_any)
		{
			base_hash = order << shift;
			compute_hashes(iter->second, bits_needed_for_level, base_hash|(order<<shift), level + 1, next_shift);
			order++;
		}
	}
}

void scope::compile::analyze_t::compress()
{
	// räkna ut barn per nivå.
	// räkna ut antal bitar som behövs
	// kolla om any är satt, i så fall lägg till en bit
	
	// we use a map, since [] constructs a default element
	std::map<int, int> bits_needed_for_level;
	compute_hash_sizes(*this, bits_needed_for_level, 0);
	const int sum = std::accumulate(bits_needed_for_level.begin(), bits_needed_for_level.end(), 0, map_acc);
	
}

bool scope::compile::analyze_t::has_any()
{
	return path.find(scope::types::atom_any) != path.end();
}

void scope::compile::analyze_t::clear()
{
	or_paths.clear();
	not_paths.clear();
}

std::string scope::compile::analyze_t::to_s (int indent) const {
	std::string res = std::string(indent, ' ') +"<";
   std::stringstream ss;//create a stringstream
	ss << std::string(indent, ' ') + "simple=";
   iterate(number, simple) {
		ss << *number;//add number to the stream
		ss << ", ";
	}
	ss << "\n" << std::string(indent, ' ') + " multi_part=";
   iterate(number, multi_part) {
		ss << "["<<number->first << ", " << number->second << "]";//add number to the stream
		ss << ", ";
	}
	
	res += ss.str();
	iterate(it, path)
		res += std::string(indent, ' ') + it->first+"= "+ it->second.to_s(indent + 1);
	res += std::string(indent, ' ') + ">";
	return res;
}


scope::compile::analyze_t* traverse (scope::compile::analyze_t* wc, scope::compile::scopex& scope)
{
	iterate(atom, scope)
		wc = &wc->path[*atom];
	return wc;
}

void set_sub_rule (scope::compile::analyze_t& root, scope::compile::scopesx& scopes, int rule_id, int sub_rule_id)
{
	scope::compile::analyze_t* wc = &root;	
	iterate(o, scopes)
	{
		wc = traverse(wc, *o);
		wc->multi_part[sub_rule_id]=rule_id;				
		wc = &root;
	}
}
template<typename T>
const scope::compile::compiled_t<T> build (const std::vector<T> const& rules)
{
	typedef std::vector<scope::compile::scopesx> compositesx;
	typedef std::list<compositesx> rulesx;
	scope::compile::analyze_t root;
	int rule_id = 0;
	int sub_rule_id=0;
	iterate(iter, rules)
	{
		iterate(iter2, iter->composites)
		{
			// for every composite we want to know if it is simple i.e. just one non-negative path
			// or multi-part
			// after we know this, we can traverse the tree again, this time setting rule_ids
			build(*iter2, root, false);
			if(root.or_paths.size() > 1 || root.not_paths.size() > 0)
			{
				set_sub_rule(root, root.or_paths, rule_id, sub_rule_id);
				set_sub_rule(root, root.not_paths, rule_id, sub_rule_id);				
				sub_rule_id++;				
			} else {
				assert(root.or_paths == 1);
				scope::compile::analyze_t* wc = &root;
				iterate(o, root.or_paths)
				{
					wc = traverse(wc, *o);
				}
				wc->simple.insert(rule_id);
			}
			root.clear();
			iter2++;
		}
		
		iter++;
		rule_id++;
	}
	root.compress();
}


void scope::types::path_t::build (compile::analyze_t& root, bool negate) const           { ::build(*this, root, negate); }
void scope::types::group_t::build (compile::analyze_t& root, bool negate) const          { ::build(*this, root, negate); }
void scope::types::filter_t::build (compile::analyze_t& root, bool negate) const         { ::build(*this, root, negate); }

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
