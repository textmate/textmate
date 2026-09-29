#include "regexp.h"
#include "private.h"

#include <oak/oak.h>
#include <text/src/format.h>

namespace regexp
{
	std::string escape (std::string ptrn)
	{
		char const* const specialChars = "[]{}()|*.\\?+^$#";

		auto last = ptrn.size();
		while(last > 0)
		{
			last = ptrn.find_last_of(specialChars, last-1);
			if(last == std::string::npos)
				break;
			ptrn.insert(last, "\\");
		}
		return ptrn;
	}

	// =============
	// = pattern_t =
	// =============

	void pattern_t::init (std::string const& pattern, OnigOptionType options)
	{
		OnigRegex tmp = nullptr;

		OnigErrorInfo einfo;
		if((options & ONIG_OPTION_DONT_CAPTURE_GROUP) == 0)
			options |= ONIG_OPTION_CAPTURE_GROUP;
		int r = onig_new(&tmp, (OnigUChar const*)pattern.data(), (OnigUChar const*)pattern.data() + pattern.size(), options, ONIG_ENCODING_UTF8, ONIG_SYNTAX_DEFAULT, &einfo);
		if(r == ONIG_NORMAL)
		{
			compiled_pattern.reset(tmp, onig_free);
		}
		else
		{
			OnigUChar s[ONIG_MAX_ERROR_MESSAGE_LEN];
			onig_error_code_to_str(s, r, &einfo);
			os_log_error(OS_LOG_DEFAULT, "pattern_t: %{public}s (%{public}s)", s, pattern.c_str());

			if(tmp)
				onig_free(tmp);
		}
	}

	pattern_t::pattern_t (char const* pattern, OnigOptionType options) : pattern_string(pattern)
	{
		init(pattern, options);
	}

	pattern_t::pattern_t (std::string const& pattern, OnigOptionType options) : pattern_string(pattern)
	{
		init(pattern, options);
	}

	pattern_t::pattern_t (std::string const& pattern, std::string const& str_options) : pattern_string(pattern)
	{
		OnigOptionType options = ONIG_OPTION_NONE;
		for(auto const& it : str_options)
		{
			switch(it)
			{
				case 'e': options |= ONIG_OPTION_EXTEND;     break;
				// case 'g': options |= ONIG_OPTION_REPEAT;     break;
				case 'i': options |= ONIG_OPTION_IGNORECASE; break;
				case 'm': options |= ONIG_OPTION_MULTILINE;  break;
				case 's': options |= ONIG_OPTION_SINGLELINE; break;
			}
		}
		init(pattern, options);
	}

	// ===========
	// = match_t =
	// ===========

	std::map<std::string, std::string> const& match_t::captures () const
	{
		if(!captured_variables)
			captured_variables.reset(new std::map<std::string, std::string>(extract_captures((OnigUChar const*)buffer(), region.get(), compiled_pattern.get())));
		return *captured_variables;
	}

	std::multimap<std::string, std::pair<size_t, size_t> > const& match_t::capture_indices () const
	{
		struct helper_t
		{
			static int main (OnigUChar const* name, OnigUChar const* name_end, int len, int* list, OnigRegex pattern, void* udata)
			{
				match_t const& m = *((match_t const*)udata);
				for(int* it = list; it != list + len; ++it)
				{
					if(m.did_match(*it))
						m.captured_indices->insert(std::make_pair(std::string(name, name_end), std::make_pair(m.begin(*it), m.end(*it))));
				}
				return 0;
			}
		};

		if(!captured_indices)
		{
			captured_indices = std::make_shared<std::multimap<std::string, std::pair<size_t, size_t>>>();
			for(size_t i = 0; i < size(); ++i)
			{
				if(did_match(i))
					captured_indices->insert(std::make_pair(std::to_string(i), std::make_pair(begin(i), end(i))));
			}
			onig_foreach_name(compiled_pattern.get(), &helper_t::main, (void*)this);
		}
		return *captured_indices;
	}

	std::string match_t::operator[] (size_t i) const
	{
		return did_match(i) ? std::string(buffer() + begin(i), buffer() + end(i)) : NULL_STR;
	}

	// ============
	// = Matching =
	// ============

	match_t search (pattern_t const& ptrn, char const* first, char const* last, char const* from, char const* to, OnigOptionType options)
	{
		if(ptrn)
		{
			char const* gpos = (options & ONIG_OPTION_NOTGPOS) ? nullptr : (from ?: first);
			options &= ~ONIG_OPTION_NOTGPOS;

			struct helper_t { static void region_free (OnigRegion* r) { onig_region_free(r, 1); } };
			regexp::region_ptr region(onig_region_new(), &helper_t::region_free);
			if(ONIG_MISMATCH != onig_search_gpos(ptrn.get().get(), (OnigUChar const*)first, (OnigUChar const*)last, (OnigUChar*)gpos, (OnigUChar const*)(from ?: first), (OnigUChar const*)(to ?: last), region.get(), options))
				return match_t(region, ptrn.get(), first);
		}
		return match_t();
	}

	match_t search (pattern_t const& ptrn, std::string const& str)
	{
		return search(ptrn, str.data(), str.data() + str.size());
	}

	// =====================
	// = Syntax validation =
	// =====================

	std::string validate (std::string const& pattern)
	{
		OnigErrorInfo einfo;
		OnigRegex tmp = nullptr;
		int r = onig_new(&tmp, (OnigUChar const*)pattern.data(), (OnigUChar const*)pattern.data() + pattern.size(), ONIG_OPTION_CAPTURE_GROUP, ONIG_ENCODING_UTF8, ONIG_SYNTAX_DEFAULT, &einfo);
		if(tmp)
			onig_free(tmp);

		std::string error = NULL_STR;
		if(r != ONIG_NORMAL)
		{
			OnigUChar s[ONIG_MAX_ERROR_MESSAGE_LEN];
			onig_error_code_to_str(s, r, &einfo);
			error = std::string(s, s + strlen((char const*)s));
		}
		return error;
	}
} /* regexp */
