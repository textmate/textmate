class bitvector
{
	const size_t WORD_BIT = sizeof(scope::compile::bits_t)*CHAR_BIT;
	typedef unsigned long long bits_t;
	std::vector<bits_t> backing;
	bitvector(size_t n): backing(n/WORD_BIT + (n % WORD_BIT > 0 ? 1 :0)){}
	
};