#ifndef FILE_ENCODING_H_CM7CPD1M
#define FILE_ENCODING_H_CM7CPD1M

#include "bytes.h"

PUBLIC extern std::string const kCharsetNoEncoding;
PUBLIC extern std::string const kCharsetASCII;
PUBLIC extern std::string const kCharsetUTF8;
PUBLIC extern std::string const kCharsetUTF16BE;
PUBLIC extern std::string const kCharsetUTF16LE;
PUBLIC extern std::string const kCharsetUTF32BE;
PUBLIC extern std::string const kCharsetUTF32LE;
PUBLIC extern std::string const kCharsetUnknown;

namespace encoding
{
	PUBLIC io::bytes_ptr convert (io::bytes_ptr content, std::string const& from, std::string const& to);

} /* encoding */

#endif /* end of include guard: FILE_ENCODING_H_CM7CPD1M */
