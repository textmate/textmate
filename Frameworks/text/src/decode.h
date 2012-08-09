#ifndef DECODE_H_53UK6QRW
#define DECODE_H_53UK6QRW

#include <oak/misc.h>

namespace decode
{
	PUBLIC std::string base32 (std::string const& src);
	PUBLIC std::string base64 (std::string const& src);
	PUBLIC std::string rot13 (std::string src);
	PUBLIC std::string entities (std::string const& src);
	PUBLIC std::string url_part (std::string const& src);

} /* decode */

#endif /* end of include guard: DECODE_H_53UK6QRW */
