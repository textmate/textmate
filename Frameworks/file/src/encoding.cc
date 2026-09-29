#include "encoding.h"
#include "constants.h"
#include <plist/src/plist.h>
#include <io/src/path.h>
#include <text/src/utf8.h>
#include <text/src/transcode.h>
#include <cf/src/cf.h>
#include <oak/oak.h>
#include <oak/debug.h>

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
	io::bytes_ptr convert (io::bytes_ptr content, std::string const& from, std::string const& to)
	{
		io::bytes_ptr res;
		if(from == to)
			return to == kCharsetUTF8 && !utf8::is_valid(content->begin(), content->end()) ? res : content;

		if(auto transcode = text::transcode_t(from, to))
		{
			std::string buffer;
			transcode(transcode(content->begin(), content->end(), back_inserter(buffer)));
			if(transcode.invalid_count() == 0)
				res = std::make_shared<io::bytes_t>(buffer);
		}

		return res;
	}

} /* encoding */
