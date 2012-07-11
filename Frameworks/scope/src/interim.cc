#include "compile.h"
#include "types.h"
#include "build.h"
int map_acc(int sum, const std::pair<int, int> & rhs)
{
  return sum + rhs.second;
}

void compute_hash_sizes (scope::compile::interim_t& root, std::map<int,int>& bits_needed_for_level, int const level)
{
	// size + 1 (since we want to recognize the empty case)
	// conviently log(0 + 1) = 0 which handles the zero children case
	// when root.has_any()== true -> root.path.size() > 0
	int bits_needed = int(ceil(log(root.path.size() + 1 - (root.has_any() ? 1 : 0))) + (root.has_any() ? 1 : 0));
	
	int& val = bits_needed_for_level[level];
	if( val < bits_needed) // max
	  val = bits_needed;
	iterate(iter, root.path)
		compute_hash_sizes(iter->second, bits_needed_for_level, level + 1);
}

void compute_hashes (scope::compile::interim_t& root, std::map<int,int>& bits_needed_for_level, int const level = 0, int shift = 0)
{
	int next_shift = shift + bits_needed_for_level[level];
	int base_hash = 0;
	int temp = floor( pow(2, bits_needed_for_level[level])) - 1; // create mask
	int base_mask = (temp << shift) | root.mask;

		// do atom_any first
	if(root.has_any())
	{
		base_hash = 1 << shift;
		root.path[scope::types::atom_any].mask = root.mask | ( 1 << shift) ;
		shift++; // add one to offset
	}

	int order = 1;
	iterate(iter, root.path)
	{
		iter->second.hash = root.hash | base_hash;
		// the atom_any child should always be xxxx1 so don't include that
		if(iter->first != scope::types::atom_any)
		{
			iter->second.hash |= order << shift;
			iter->second.mask = base_mask;
			order++;
		}
		compute_hashes(iter->second, bits_needed_for_level, level + 1, next_shift);
	}
}

void scope::compile::interim_t::calculate_bit_fields()
{
	// räkna ut barn per nivå.
	// räkna ut antal bitar som behövs
	// kolla om any är satt, i så fall lägg till en bit
	
	// we use a map, since [] constructs a default element
	std::map<int, int> bits_needed_for_level;
	compute_hash_sizes(*this, bits_needed_for_level, 0);
	const int sum = std::accumulate(bits_needed_for_level.begin(), bits_needed_for_level.end(), 0, map_acc);
	hash = 0;
	mask = 0;
	compute_hashes(*this, bits_needed_for_level);
}

bool scope::compile::interim_t::has_any()
{
	return path.find(scope::types::atom_any) != path.end();
}

std::string scope::compile::interim_t::to_s (int indent) const {
	std::string res = "<";
   std::stringstream ss;//create a stringstream
	ss << "hash=" << std::hex << hash << std::dec << std::endl; 
	ss << std::string(indent, ' ') << " mask=" << std::hex << mask << std::endl;
	ss << std::dec << std::string(indent, ' ') + " simple=(";
   iterate(number, simple) {
		ss << *number;//add number to the stream
		ss << ", ";
	}
	ss << ")\n" << std::string(indent, ' ') + " multi_part=(";
   iterate(number, multi_part) {
		ss << "["<<number->first << ", " << number->second << "]";//add number to the stream
		ss << ", ";
	}
	
	res += ss.str() + ")\n";
	iterate(it, path)
		res += " " + std::string(indent, ' ') +"\""+ it->first+"\"= "+ it->second.to_s(indent + 5 +it->first.size());
	res += std::string(indent, ' ') + ">\n";
	return res;
}


scope::compile::interim_t* traverse (scope::compile::interim_t* wc, scope::compile::scopex& scope)
{
	iterate(atom, scope) {
		wc = &wc->path[*atom];
	}
	return wc;
}

void set_sub_rule (scope::compile::interim_t& root, scope::compile::scopesx& scopes, int rule_id, int sub_rule_id)
{
	scope::compile::interim_t* wc = &root;	
	iterate(o, scopes)
	{		
		wc = traverse(wc, *o);

		wc->multi_part[sub_rule_id]=rule_id;				
		wc = &root;
	}
}

void propagate(scope::compile::interim_t& child, scope::compile::interim_t& any)
{
	iterate(c_any, any.path)
	{
		child.path[c_any->first];
		iterate(c_child, child.path)
			propagate(c_child->second, c_any->second);
	}
	child.simple.insert(any.simple.begin(), any.simple.end());
	child.multi_part.insert(any.multi_part.begin(), any.multi_part.end());
	
}
// expand scopes containing * into those that do not. e.g foo.* and foo.markdown, since foo.markdown is a subpart of foo.*, it too needs to behave like one
void scope::compile::compiler_t::expand_wildcards (interim_t& root)
{
	iterate(child, root.path)
		expand_wildcards(child->second);
	if(root.has_any())
	{
		iterate(child, root.path)
		{
			if(child->first != scope::types::atom_any ) 
				propagate(child->second, root.path[scope::types::atom_any]);
		}
	}
}

void scope::compile::compiler_t::graph ( const scope::selector_t& selector, int& rule_id, int& sub_rule_id)
{
	size_t index = 0;
	if(!selector.selector)
	{
		// rules without selectors always match and needs to be treated as having rank = 0
		root.simple.insert(rule_id);
		
		return;
	}
	iterate(iter2, selector.selector->composites)
	{
		// for every composite we want to know if it is simple i.e. just one non-negative path
		// or multi-part
		// after we know this, we can traverse the tree again, this time setting rule_ids
		build(*iter2, _analyzer, false);
		/*
		printf("or_path size=%d ", _analyzer.or_paths.size());
		printf("not_path size=%d ", _analyzer.not_paths.size());
		
		iterate(d1, _analyzer.or_paths)
		{
			printf("or_path");
			iterate(d2, *d1)
				printf("%s.", d2->c_str());
		}
		*/
		// multi part
		if(_analyzer.or_paths.size() > 1 || _analyzer.not_paths.size() > 0)
		{
			set_sub_rule(root, _analyzer.or_paths, rule_id, sub_rule_id);
			set_sub_rule(root, _analyzer.not_paths, rule_id, sub_rule_id);				
			sub_rule_mapping.insert(std::make_pair(rule_id, index));
			sub_rule_id++;
		// simple case				
		} else {
			assert(__analyzer.or_paths.size() == 1);
			scope::compile::interim_t* wc = &root;
			iterate(o, _analyzer.or_paths)
			{
				wc = traverse(wc, *o);
			}
			wc->simple.insert(rule_id);
		}  
		_analyzer.clear();
		index++;
	}
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
