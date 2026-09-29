#include "classification.h"
#include "my_ctype.h"
#include "utf8.h"

namespace text
{
	bool is_word_char (std::string const& str)
	{
		return text::is_word_char(utf8::to_ch(str));
	}

	bool is_whitespace (std::string const& str)
	{
		return text::is_space(utf8::to_ch(str));
	}

} /* text */
