#include "bytes.h"

#include <boost/crc.hpp>

namespace io
{
	bytes_t::bytes_t (size_t size) : _dispose(true)
	{
		_bytes = new char[size];
		_size  = size;
	}

	bytes_t::bytes_t (std::string const& str) : _dispose(true)
	{
		_bytes = new char[_size = str.size()];
		memcpy(_bytes, str.data(), _size);
	}

	bytes_t::bytes_t (char const* bytes, size_t size, bool dispose) : _bytes((char*)bytes), _size(size), _dispose(dispose)
	{
	}

	bytes_t::~bytes_t ()
	{
		if(_dispose)
			delete[] _bytes;
	}

	void bytes_t::set_string (std::string const& str)
	{
		if(_dispose)
			delete[] _bytes;
		_bytes   = new char[_size = str.size()];
		_dispose = true;
		memcpy(_bytes, str.data(), _size);
	}

	uint32_t bytes_t::crc32 () const
	{
		boost::crc_32_type result;
		result.process_bytes(_bytes, _size);
		return result.checksum();
	}

} /* io */
