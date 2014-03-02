#include "snippets.h"
#include <oak/oak.h>

namespace ng
{
	void snippet_controller_t::push (snippet::snippet_t const& snippet, ng::range_t const& range)
	{
		if(stack.empty())
			anchor = range.min().index;
		stack.push(snippet, snippet::range_t(range.first.index - anchor, range.last.index - anchor));
	}

	std::vector< std::pair<ng::range_t, std::string> > snippet_controller_t::replace (size_t from, size_t to, std::string const& replacement)
	{
		if(stack.empty())
			return std::vector< std::pair<ng::range_t, std::string> >(1, std::make_pair(ng::range_t(from, to), replacement));

		std::vector< std::pair<ng::range_t, std::string> > res;
		for(auto const& pair : stack.replace(snippet::range_t(from - anchor, to - anchor), replacement))
			res.push_back(std::make_pair(ng::range_t(pair.first.from.offset + anchor, pair.first.to.offset + anchor), pair.second));
		return res;
	}

	ng::range_t snippet_controller_t::current () const
	{
		snippet::range_t const& range = stack.current();
		return ng::range_t(range.from.offset + anchor, range.to.offset + anchor, false, false, range.from.offset != range.to.offset ? true : false);
	}

	std::vector<std::string> const& snippet_controller_t::choices () const
	{
		return stack.choices();
	}

	void snippet_controller_t::drop_for_pos (size_t pos)
	{
		if(!stack.empty())
			stack.drop_for_pos(pos - anchor);
	}

} /* ng */
