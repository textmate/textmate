#ifndef TEXT_PARSE_H_4CKIHQHS
#define TEXT_PARSE_H_4CKIHQHS

namespace text
{
	template <typename _InputIter>
	std::vector< std::pair<_InputIter, _InputIter> > to_lines (_InputIter first, _InputIter last)
	{
		_InputIter eol(first);
		std::vector<_InputIter> lines(1, first);
		do {
			eol = std::find(lines.back(), last, '\n');
			if(eol != last)
			{
				_InputIter dummy(eol);
				lines.push_back(++dummy);
			}
			else
			{
				lines.push_back(eol);
			}
		} while(eol != last);

		std::vector< std::pair<_InputIter, _InputIter> > res;
		for(size_t i = 1; i < lines.size(); ++i)
			res.emplace_back(lines[i-1], lines[i]);
		return res;
	}

	std::vector<std::string> split (std::string const& str, std::string const& sep = ", ");
	std::vector<size_t> soft_breaks (std::string const& str, size_t width, size_t tabSize, size_t prefixSize = 0);

} /* text */

#endif /* end of include guard: TEXT_PARSE_H_4CKIHQHS */
