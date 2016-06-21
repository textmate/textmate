#ifndef TEXT_HEXDUMP_RLPHB9XL
#define TEXT_HEXDUMP_RLPHB9XL

namespace text
{
	template <typename T, typename _OutputIter>
	_OutputIter int_to_hex (T value, _OutputIter out, size_t width)
	{
		while(width--)
		{
			T bits = (value & (0xF << (4*width))) >> (4*width);
			*out++ = bits > 9 ? ('A' + bits-10) : ('0' + bits);
		}
		return out;
	}

	template <typename _Iter, typename _OutputIter>
	_OutputIter hex_dump (_Iter it, _Iter const& last, _OutputIter out)
	{
		int hex = 4, cols = 16;
		for(int row = 0; it != last; ++row)
		{
			if(row != 0)
				*out++ = '\n';

			std::vector<char> number, ascii;
			for(int i = 0; i < cols && it != last; ++i, ++it)
			{
				int_to_hex(*it, back_inserter(number), 2);
				number.push_back(' ');
				ascii.push_back(*it < 0x7F && isprint(*it) ? *it : '.');
			}

			std::vector<char> v(hex + 1 + 4*cols, ' ');
			int_to_hex(row * cols, v.begin(), hex);
			std::copy(number.begin(), number.end(), v.begin() + (hex + 1));
			std::copy(ascii.begin(), ascii.end(), v.begin() + (hex + 1 + 3*cols));

			out = std::copy(v.begin(), v.end(), out);
		}
		return out;
	}

	template <typename _InputIter>
	std::string to_hex (_InputIter const& first, _InputIter const& last)
	{
		std::string str = "";
		hex_dump(first, last, back_inserter(str));
		return str;
	}

} /* text */

#endif /* end of include guard: TEXT_HEXDUMP_RLPHB9XL */
