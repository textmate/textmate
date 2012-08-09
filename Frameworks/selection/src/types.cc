#include "types.h"
#include <text/format.h>

namespace ng
{
	std::string to_s (index_t const& index)
	{
		return index ? (index.carry ? text::format("%zu:%zu", index.index, index.carry) : text::format("%zu", index.index)) : "«undefined»";
	}

	std::string to_s (range_t const& range)
	{
		return "[" + to_s(range.min()) + (range.empty() ? "" : "-" + to_s(range.max())) + "]";
	}

	std::string to_s (ranges_t const& ranges)
	{
		std::vector<std::string> v;
		iterate(range, ranges)
			v.push_back(to_s(*range));
		return v.empty() ? "(empty)" : text::join(v, "&");
	}

} /* ng */
