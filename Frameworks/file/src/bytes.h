#ifndef BYTES_H_ZGQQH1SH
#define BYTES_H_ZGQQH1SH

#include <cstddef>
#include <string>
#include <cstdint>    // for uint32_t
#include <memory>     // for std::shared_ptr

namespace io
{
	struct bytes_t
	{
		bytes_t (size_t size);
		bytes_t (std::string const& str);
		bytes_t (char const* bytes, size_t size, bool dispose = true);
		~bytes_t ();

		char* get ()               { return _bytes; };
		char* begin ()             { return _bytes; };
		char* end ()               { return _bytes + _size; };
		char const* get () const   { return _bytes; };
		char const* begin () const { return _bytes; };
		char const* end () const   { return _bytes + _size; };
		size_t size () const       { return _size; }
		void resize (size_t size)  { _size = size; }

		void set_string (std::string const& str);
		uint32_t crc32 () const;

	private:
		char* _bytes;
		size_t _size;
		bool _dispose;
	};

	typedef std::shared_ptr<bytes_t> bytes_ptr;

} /* io */

#endif /* end of include guard: BYTES_H_ZGQQH1SH */
