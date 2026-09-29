#ifndef FILE_ENCODING_H_CM7CPD1M
#define FILE_ENCODING_H_CM7CPD1M

#include "bytes.h"
#include <oak/oak.h>
#include <text/src/case.h>

extern std::string const kCharsetNoEncoding;
extern std::string const kCharsetASCII;
extern std::string const kCharsetUTF8;
extern std::string const kCharsetUTF16BE;
extern std::string const kCharsetUTF16LE;
extern std::string const kCharsetUTF32BE;
extern std::string const kCharsetUTF32LE;
extern std::string const kCharsetUnknown;

namespace encoding
{
	template <typename _InputIter>
	std::string charset_from_bom (_InputIter const& first, _InputIter const& last)
	{
		static struct UTFBOMTests { std::string bom; std::string encoding; } const BOMTests[] =
		{
			{ std::string("\x00\x00\xFE\xFF", 4), kCharsetUTF32BE },
			{ std::string("\xFE\xFF",         2), kCharsetUTF16BE },
			{ std::string("\xFF\xFE\x00\x00", 4), kCharsetUTF32LE },
			{ std::string("\xFF\xFE",         2), kCharsetUTF16LE },
			{ std::string("\uFEFF",           3), kCharsetUTF8    }
		};

		for(auto const& test : BOMTests)
		{
			if(oak::has_prefix(first, last, test.bom.begin(), test.bom.end()))
				return test.encoding + "//BOM";
		}
		return kCharsetNoEncoding;
	}

	io::bytes_ptr convert (io::bytes_ptr content, std::string const& from, std::string const& to);

	struct type
	{
		type () { }
		type (std::string const& newlines, std::string const& charset) : _newlines(newlines), _charset(charset) { }

		std::string const& newlines () const { return _newlines; }
		std::string const& charset () const  { return _charset; }

		void set_newlines (std::string const& newlines) { _newlines = newlines; }
		void set_charset (std::string const& charset)   { _charset = text::uppercase(charset); }

	private:
		std::string _newlines = NULL_STR;
		std::string _charset  = kCharsetNoEncoding;
	};

} /* encoding */

#endif /* end of include guard: FILE_ENCODING_H_CM7CPD1M */
