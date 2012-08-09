#ifndef FORMAT_STRING_H_X8KPCX9K
#define FORMAT_STRING_H_X8KPCX9K

#include "parser_fwd.h"
#include "regexp.h"
#include <oak/debug.h>

namespace format_string
{
	typedef std::map<std::string, std::string> string_map_t;

	struct PUBLIC format_string_t
	{
		WATCH_LEAKS(format_string::format_string_t);

		format_string_t (char const* str = "")   { init(str); }
		format_string_t (std::string const& str) { init(str); }
		
		format_string_t (parser::nodes_t const& nodes);
		std::string expand (string_map_t const& variables) const;

	private:
		friend std::string replace (std::string const& src, regexp::pattern_t const& ptrn, format_string_t const& format, bool repeat, string_map_t const& variables);
		friend std::set<std::string> get_variables (format_string_t const&);

		void init (std::string const& str);
		std::tr1::shared_ptr<parser::nodes_t> nodes;
	};

	PUBLIC std::string replace (std::string const& src, regexp::pattern_t const& ptrn, format_string_t const& format, bool repeat = true, string_map_t const& variables = string_map_t());
	PUBLIC std::string expand (std::string const& format, string_map_t const& variables = string_map_t());

} /* format_string */

#endif /* end of include guard: FORMAT_STRING_H_X8KPCX9K */
