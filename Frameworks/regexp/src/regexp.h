#ifndef ONIG_REGEXP_H_UMTUKY6I
#define ONIG_REGEXP_H_UMTUKY6I

#include <Onigmo/oniguruma.h>
#include <oak/debug.h>

#define ONIG_OPTION_NOTGPOS (ONIG_OPTION_MAXBIT << 1)

namespace regexp
{
	typedef std::shared_ptr<regex_t> regex_ptr;
	typedef std::shared_ptr<OnigRegion> region_ptr;

	struct match_t;
	struct pattern_t;

	struct PUBLIC match_t
	{
		WATCH_LEAKS(regexp::match_t);
	private:
		region_ptr region;
		regex_ptr compiled_pattern;
		char const* buf;

		mutable std::shared_ptr< std::map<std::string, std::string> > captured_variables;
		mutable std::shared_ptr< std::multimap<std::string, std::pair<size_t, size_t> > > captured_indices;

		friend match_t search (pattern_t const& ptrn, char const* first, char const* last, char const* from, char const* to, OnigOptionType options);
		match_t (region_ptr const& region, regex_ptr const& compiled_pattern, char const* buf) : region(region), compiled_pattern(compiled_pattern), buf(buf) { }

	public:
		match_t () : buf(NULL) { }

		int size () const						{ return region ? region->num_regs : 0; }
		bool empty (int i = 0) const		{ return begin(i) == end(i); }

		bool did_match (int i = 0) const	{ return i < size() && region->beg[i] != -1; }

		explicit operator bool () const	{ return did_match(); }

		char const* buffer () const		{ return buf; }

		size_t begin () const				{ return did_match() ? (size_t)region->beg[0] : SIZE_T_MAX; }
		size_t end () const					{ return did_match() ? (size_t)region->end[0] : SIZE_T_MAX; }
		size_t begin (int i) const			{ return did_match(i) ? (size_t)region->beg[i] : end(); }
		size_t end (int i) const			{ return did_match(i) ? (size_t)region->end[i] : end(); }

		std::map<std::string, std::string> const& captures () const;
		std::multimap<std::string, std::pair<size_t, size_t> > const& capture_indices () const;
		std::string operator[] (size_t i) const;
	};

	struct PUBLIC pattern_t
	{
		WATCH_LEAKS(regexp::pattern_t);
	private:
		regex_ptr compiled_pattern;
		std::string pattern_string;
		void init (std::string const& pattern, OnigOptionType options);

		friend match_t search (pattern_t const& ptrn, char const* first, char const* last, char const* from, char const* to, OnigOptionType options);
		regex_ptr get () const { return compiled_pattern; }
	public:
		pattern_t () : pattern_string("(?=un)initialized") { }
		pattern_t (char const* pattern, OnigOptionType options = ONIG_OPTION_NONE);
		pattern_t (std::string const& pattern, OnigOptionType options = ONIG_OPTION_NONE);
		pattern_t (std::string const& pattern, std::string const& str_options);
		explicit operator bool () const { return compiled_pattern ? true : false; }

		bool operator== (pattern_t const& rhs) const { return pattern_string == rhs.pattern_string; }
		bool operator!= (pattern_t const& rhs) const { return !(*this == rhs); }

		friend std::string to_s (pattern_t const& ptrn);
	};

	inline std::string to_s (pattern_t const& ptrn) { return ptrn.pattern_string; }

	PUBLIC std::string validate (std::string const& ptrn);
	PUBLIC match_t search (pattern_t const& ptrn, char const* first, char const* last, char const* from = NULL, char const* to = NULL, OnigOptionType options = ONIG_OPTION_NONE);
	PUBLIC match_t search (pattern_t const& ptrn, std::string const& str);

} /* regexp */

#endif /* end of include guard: ONIG_REGEXP_H_UMTUKY6I */
