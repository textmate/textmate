#include "encoding.h"
#include "constants.h"
#include <plist/plist.h>
#include <io/path.h>
#include <text/utf8.h>
#include <cf/cf.h>
#include <oak/oak.h>
#include <oak/debug.h>

OAK_DEBUG_VAR(File_Charset);

std::string const kCharsetNoEncoding = NULL_STR;
std::string const kCharsetASCII      = "ASCII";
std::string const kCharsetUTF8       = "UTF-8";
std::string const kCharsetUTF16BE    = "UTF-16BE";
std::string const kCharsetUTF16LE    = "UTF-16LE";
std::string const kCharsetUTF32BE    = "UTF-32BE";
std::string const kCharsetUTF32LE    = "UTF-32LE";
std::string const kCharsetUnknown    = "UNKNOWN";

namespace encoding
{
	bool type::supports_byte_order_mark (std::string const& charset)
	{
		static std::set<std::string> const Encodings = { kCharsetUTF8, kCharsetUTF16BE, kCharsetUTF16LE, kCharsetUTF32BE, kCharsetUTF32LE };
		return Encodings.find(charset) != Encodings.end();
	}

	io::bytes_ptr convert (io::bytes_ptr content, std::string const& from, std::string const& to)
	{
		io::bytes_ptr res;
		if(from == to)
			return to == kCharsetUTF8 && !utf8::is_valid(content->begin(), content->end()) ? res : content;

		iconv_t cd = iconv_open(to.c_str(), from.c_str());
		if(cd == (iconv_t)(-1))
			return res;

		std::string buffer(1024, ' ');
		size_t buffer_contains = 0;

		char const* first = content->begin();
		char const* last  = content->end();
		while(first != last)
		{
			if(buffer.size() - buffer_contains < 256)
				buffer.resize(buffer.size() * 2);

			char* dst      = &buffer[buffer_contains];
			size_t dstSize = buffer.size() - buffer_contains;
			size_t srcSize = last - first;

			size_t rc = iconv(cd, (char**)&first, &srcSize, &dst, &dstSize);
			if(rc == (size_t)(-1) && errno != E2BIG && (errno != EINVAL || buffer.size() - buffer_contains - dstSize == 0))
				break;
			D(DBF_File_Charset, bug("did decode %zu bytes\n", buffer.size() - buffer_contains - dstSize););

			buffer_contains += buffer.size() - buffer_contains - dstSize;
		}

		iconv_close(cd);

		if(first == last)
		{
			ASSERT(to != kCharsetUTF8 || utf8::is_valid(buffer.begin(), buffer.end()));
			buffer.resize(buffer_contains);
			res = std::make_shared<io::bytes_t>(buffer);
		}

		return res;
	}

} /* encoding */