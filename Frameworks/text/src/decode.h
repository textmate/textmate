#ifndef DECODE_H_53UK6QRW
#define DECODE_H_53UK6QRW

namespace decode
{
	std::string base32 (std::string const& src);
	std::string base64 (std::string const& src);
	std::string rot13 (std::string src);
	std::string entities (std::string const& src);
	std::string url_part (std::string const& src);

} /* decode */

#endif /* end of include guard: DECODE_H_53UK6QRW */
