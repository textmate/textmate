#include "compile.h"
#include "compressed.h"
#include "types.h"

scope::compile::compressor_t& scope::compile::compressor_t::setup(analyze_t const& analyze, compressor_t& compressor) {
	size_t sz = sizeof(bits_t)*CHAR_BIT;
	// set the bit for each sub_rule that is affected by this scope
	iterate(o, analyze.multi_part)
		compressor.possible[o->first/sz] = 1L << (o->first%sz);
	iterate(o, analyze.simple)
		compressor.simple.push_back(*o);
	compressor.match = analyze.simple.size() > 0;
	compressor.hash = analyze.hash;
	iterate(a, analyze.path)
	{
		auto t = compressor.path.insert(std::make_pair(a->first,compressor_t(compressor.possible.size())));
		setup(a->second, t.first->second);
	}
	return compressor;
}

const scope::compile::compressor_t* next(std::string const& str, const scope::compile::compressor_t& path) {
	typedef std::map<std::string, scope::compile::compressor_t::compressor_t>::const_iterator iterator;
	iterator it = path.path.find(str);
	iterator last = path.path.end();
	if(it != last) 
		return &it->second;
	assert('*' == 42);
	assert(*scope::types::atom_any.c_str() == 42);
	assert(*scope::types::atom_any.c_str() < 'a');
	
	//if(path.path.begin() != last && *path.path.begin()->first.c_str() == *scope::types::atom_any.c_str()) 
	if(path.path.size() > 0 && *path.path.begin()->first.c_str() == *scope::types::atom_any.c_str())
		return &path.path.begin()->second;
	
	return NULL; 
}

std::map<int, double> scope::compile::match (scope::context_t const& scope, const scope::compile::compressor_t& compressor, const std::vector<scope::compile::sub_rule_t>& expressions, size_t backing_size)
{
	// TODO can palette be cleared and reused?
	static std::vector<scope::compile::bits_t> palette(backing_size);
	palette.assign(backing_size, 0); // clear
	std::vector<scope::types::scope_t>& path = scope.left.path->scopes;
	scope::compressed::path_t xpath;
	std::map<int, double> ruleToRank; // should this be a vector, and ignore zero values?
	size_t s = path.size();
	int power = 0;
	while(s--)
	{
		const scope::compile::compressor_t* comp = &compressor;
		int j = 0;
		int sz = path[s].atoms.size();
		power += sz;

		while(const scope::compile::compressor_t* current = next(path[s].atoms[j], *comp))
		{
			j++;
			comp = current;
			if(comp->match)
			{
				// calculate score
				double score = 0;
				for(size_t k = 0; k < j; ++k)
					score += 1 / pow(2, power - k);
				
				iterate(id, comp->simple) {		
   		 		double& rank = ruleToRank[*id];
					rank = std::max(rank, score);
				}
			}
		}
		for(int i = 0; i < comp->possible.size();i++)
			palette[i] |= comp->possible[i];
		scope::compressed::scope_t xscope(comp->hash, 0, sz,false);
		xpath.scopes.push_back(xscope);
			
	}

	for(size_t index = 0; index < palette.size(); index++) {	
		while(int sub_rule_id = ffs(palette.at(index))) {
			int real_index = sub_rule_id - 1 + index*(sizeof(scope::compile::bits_t)*CHAR_BIT);
			double score;
			//printf("palette %zu %llu id= %d;", index, palette[index], expressions.at(real_index).rule_id);
			
			if(expressions[real_index].composite->does_match(xpath, xpath, &score)) {
				size_t r_id = expressions.at(real_index).rule_id;
				double& rank = ruleToRank[r_id];				
				rank = std::max(rank, score);
			}
			palette[index] &= ~(1<<sub_rule_id-1);
		}
	}
	return ruleToRank;
}
