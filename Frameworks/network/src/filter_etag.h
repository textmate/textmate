#ifndef FILTER_ETAG_H_TKYP6FQQ
#define FILTER_ETAG_H_TKYP6FQQ

#include "download.h" // filter_t

struct etag_t : filter_t
{
	bool receive_header (std::string const& header, std::string const& value)
	{
		if(header == "etag")
			etag = value;
		return true;
	}

	std::string etag = NULL_STR;
};

#endif /* end of include guard: FILTER_ETAG_H_TKYP6FQQ */
