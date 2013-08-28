#ifndef PARSER_BASE_H_ELAFLOAQ
#define PARSER_BASE_H_ELAFLOAQ

struct parser_base_t
{
	parser_base_t (std::string const& str);

	bool parse_char (char const* ch);
	bool parse_chars (char const* chars, std::string& res);
	bool parse_int (size_t& res);
	bool parse_until (char const* stopChars, std::string& res);

	size_t bytes_parsed () const { return it - first; }

protected:
	char const* first;
	char const* it;
	char const* last;
};

#endif /* end of include guard: PARSER_BASE_H_ELAFLOAQ */
