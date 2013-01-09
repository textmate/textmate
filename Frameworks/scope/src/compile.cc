#include "compile.h"
#include "compressed.h"
#include "types.h"

scope::compile::compressor_t::compressor_t (interim_t const& interim, size_t sz): possible(sz), path(
			boost::make_transform_iterator(interim.path.begin(), converter(sz)),
				boost::make_transform_iterator(interim.path.end(), converter(sz))
			) 
{
	size_t block_sz = sizeof(bits_t)*CHAR_BIT;
	// set the bit for each sub_rule that is affected by this scope
	iterate(o, interim.multi_part)
		possible[o->first/block_sz] = 1L << (o->first%block_sz);
	iterate(o, interim.simple)
		simple.push_back(*o);
	match = interim.simple.size() > 0;
	hash = interim.hash;
	needs_right = interim.needs_right;
}

static scope::compile::compressor_unique_ptr EMPTY;
scope::compile::compressor_unique_ptr const& scope::compile::compressor_t::next(std::string const& str) const{
	typedef map_type::const_iterator iterator;

	iterator it = this->path.find(str);
	iterator last = this->path.end();
	if(it != last) 
		return it->second;

	// We use the fact that '*' is sorted before alphabetical letters 
	if(this->path.size() > 0 && this->path.begin()->first == scope::types::atom_any)
		return this->path.begin()->second;
	
	return EMPTY; 
}
