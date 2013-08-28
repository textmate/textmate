#ifndef FORMAT_STRING_H_X8KPCX9K
#define FORMAT_STRING_H_X8KPCX9K

#include "parser_fwd.h"
#include "regexp.h"
#include <oak/debug.h>

namespace format_string
{
	struct PUBLIC format_string_t
	{
		WATCH_LEAKS(format_string::format_string_t);

		format_string_t (char const* str = "", char const* stopChars = "")   { init(str, stopChars); }
		format_string_t (std::string const& str, char const* stopChars = "") { init(str, stopChars); }
		
		format_string_t (parser::nodes_t const& nodes);
		std::string expand (std::map<std::string, std::string> const& variables) const;

		size_t length () const { return _length; }

	private:
		friend std::string replace (std::string const& src, regexp::pattern_t const& ptrn, format_string_t const& format, bool repeat, std::map<std::string, std::string> const& variables);
		friend std::set<std::string> get_variables (format_string_t const&);

		void init (std::string const& str, char const* stopChars);
		std::shared_ptr<parser::nodes_t> nodes;
		size_t _length = 0;
	};

	PUBLIC std::string replace (std::string const& src, regexp::pattern_t const& ptrn, format_string_t const& format, bool repeat = true, std::map<std::string, std::string> const& variables = std::map<std::string, std::string>());
	PUBLIC std::string expand (std::string const& format, std::map<std::string, std::string> const& variables = std::map<std::string, std::string>());
	PUBLIC std::string escape (std::string const& format);

} /* format_string */

#endif /* end of include guard: FORMAT_STRING_H_X8KPCX9K */
