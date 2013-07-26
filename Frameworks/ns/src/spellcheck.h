#ifndef NS_SPELLCHECK_H_Y012GDZ5
#define NS_SPELLCHECK_H_Y012GDZ5

#include <oak/misc.h>

namespace ns
{
	struct range_t
	{
		range_t (size_t first, size_t last) : first(first), last(last) { }
		size_t first, last;
	};

	struct spelling_tag_t
	{
		spelling_tag_t () : _helper(new helper_t) { }
		operator long int () const { return _helper->tag(); }

	private:
		struct PUBLIC helper_t
		{
			~helper_t ();
			long int tag ();

		private:
			long int _tag;
			bool _did_setup = false;
		};

		std::shared_ptr<helper_t> _helper;
	};

	PUBLIC std::vector<ns::range_t> spellcheck (char const* first, char const* last, std::string const& language = "en", spelling_tag_t const& tag = spelling_tag_t());
	PUBLIC bool is_misspelled (char const* first, char const* last, std::string const& language = "en", spelling_tag_t const& tag = spelling_tag_t());
	inline bool is_misspelled (std::string const& str, std::string const& language = "en", spelling_tag_t const& tag = spelling_tag_t()) { return is_misspelled(str.data(), str.data() + str.size(), language, tag); }
	
} /* ns */

#endif /* end of include guard: NS_SPELLCHECK_H_Y012GDZ5 */
