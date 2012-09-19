#ifndef FILE_ENCODING_H_CM7CPD1M
#define FILE_ENCODING_H_CM7CPD1M

#include "bytes.h"
#include <oak/oak.h>

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

	struct type
	{
		type () { }
		type (std::string const& newlines, std::string const& charset, bool byte_order_mark = false) : _newlines(newlines), _charset(charset), _byte_order_mark(byte_order_mark) { }

		std::string const& newlines () const { return _newlines; }
		std::string const& charset () const  { return _charset; }
		bool byte_order_mark () const        { return _byte_order_mark && supports_byte_order_mark(_charset); }

		void set_newlines (std::string const& newlines) { _newlines = newlines; }
		void set_charset (std::string const& charset)   { _charset = charset; _byte_order_mark = charset != kCharsetUTF8 && supports_byte_order_mark(charset); }
		void set_byte_order_mark (bool flag)            { _byte_order_mark = flag; }

		static bool supports_byte_order_mark (std::string const& charset)
		{
			static std::set<std::string> const Encodings = { kCharsetUTF8, kCharsetUTF16BE, kCharsetUTF16LE, kCharsetUTF32BE, kCharsetUTF32LE };
			return Encodings.find(charset) != Encodings.end();
		}

	private:
		std::string _newlines = NULL_STR;
		std::string _charset  = kCharsetNoEncoding;
		bool _byte_order_mark = false;
	};

} /* encoding */

#endif /* end of include guard: FILE_ENCODING_H_CM7CPD1M */
