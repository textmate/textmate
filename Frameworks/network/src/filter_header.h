#ifndef FILTER_HEADER_H_494C7357
#define FILTER_HEADER_H_494C7357

#include "download.h" // filter_t

namespace network
{
	struct header_t : filter_t
	{
		header_t (std::string const& header, std::string const& defaultValue = NULL_STR) : _header(header), _value(defaultValue) { }

		bool receive_header (std::string const& header, std::string const& value)
		{
			if(header == _header)
				_value = value;
			return true;
		}

		std::string const& value () const
		{
			return _value;
		}

	private:
		std::string _header;
		std::string _value;
	};

} /* network */

#endif /* end of include guard: FILTER_HEADER_H_494C7357 */
