#ifndef RANKER_KFO7JS5A
#define RANKER_KFO7JS5A

#include <oak/oak.h>

namespace oak
{
	std::string normalize_filter (std::string const& filter);
	double rank (std::string const& filter, std::string const& candidate, std::vector< std::pair<size_t, size_t> >* out = NULL);
}

#endif /* end of include guard: RANKER_KFO7JS5A */
