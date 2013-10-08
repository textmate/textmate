#ifndef READER_H_KYZ00D5F
#define READER_H_KYZ00D5F

#include "bytes.h"
#include "encoding.h"

namespace file
{
	struct PUBLIC reader_t
	{
		reader_t (std::string const& path);
		~reader_t();
		io::bytes_ptr next ();
		encoding::type encoding () const;

	private:
		void set_charset (std::string const& charset);
		void io_error (char const* msg);

		std::string _path;
		int _fd;
		iconv_t _cd = (iconv_t)-1;
		std::string _spillover;
		encoding::type _encoding;
	};

	PUBLIC std::string read_utf8 (std::string const& path, std::string* charset = nullptr);

} /* file */

#endif /* end of include guard: READER_H_KYZ00D5F */
