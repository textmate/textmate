#include "reader.h"
#include "path_info.h"
#include <encoding/encoding.h>
#include <text/utf8.h>
#include <io/path.h>
#include <settings/settings.h>

static void grow (char*& outBuf, size_t& outBufSize, std::string& dst, size_t copied)
{
	dst.resize(dst.size() * 3 / 2);
	outBuf     = &dst.front() + copied;
	outBufSize = dst.size() - copied;
}

static size_t convert (iconv_t cd, char const* src, size_t len, std::string& dst)
{
	char* outBuf      = &dst.front();
	size_t outBufSize = dst.size();

	char* inBuf       = (char*)src;
	size_t inBufSize  = len;

	while(inBufSize)
	{
		size_t rc = iconv(cd, &inBuf, &inBufSize, &outBuf, &outBufSize);
		if(rc == (size_t)-1)
		{
			if(errno == EILSEQ)
			{
				++inBuf;
				--inBufSize;

				while(iconv(cd, nullptr, nullptr, &outBuf, &outBufSize) == (size_t)-1 && errno == E2BIG)
					grow(outBuf, outBufSize, dst, outBuf - &dst.front());

				static char const kReplacementChar[]     = "\uFFFD";
				static char const kReplacementCharLength = strlen(kReplacementChar);
				while(outBufSize < kReplacementCharLength)
					grow(outBuf, outBufSize, dst, outBuf - &dst.front());

				memcpy(outBuf, kReplacementChar, kReplacementCharLength);
				outBuf += kReplacementCharLength;
				outBufSize -= kReplacementCharLength;
			}
			else if(errno == E2BIG)
			{
				grow(outBuf, outBufSize, dst, outBuf - &dst.front());
			}
			else if(errno == EINVAL)
			{
				// Incomplete multibyte sequence
				break;
			}
			else
			{
				fprintf(stderr, "iconv: %s after %zu bytes, %zu bytes left (unexpected)\n", strerror(errno), inBuf - src, inBufSize);
				break;
			}
		}
	}
	dst.resize(outBuf - &dst.front());
	return inBuf - src;
}

namespace file
{
	reader_t::reader_t (std::string const& path) : _path(path)
	{
		_fd = open(_path.c_str(), O_RDONLY|O_CLOEXEC);
		if(_fd == -1)
			return io_error("open");

		char buf[4];
		ssize_t len = read(_fd, buf, sizeof(buf));
		if(len == -1)
			return io_error("read");

		size_t bomSize = 0;
		std::string charset = encoding::charset_from_bom(std::begin(buf), std::begin(buf) + len, &bomSize);
		if(charset != kCharsetNoEncoding && charset != kCharsetUTF8)
		{
			set_charset(charset);
			if(_cd == (iconv_t)-1)
				return io_error("iconv_open");
		}

		lseek(_fd, bomSize, SEEK_SET);
		fcntl(_fd, F_NOCACHE, 1);
	}

	reader_t::~reader_t ()
	{
		if(_cd != (iconv_t)(-1))
			iconv_close(_cd);
		if(_fd != -1)
			close(_fd);
	}

	io::bytes_ptr reader_t::next ()
	{
		if(_fd == -1)
			return io::bytes_ptr();

		std::string buf(8192, ' ');

		ssize_t len = read(_fd, &buf.front(), buf.size());
		buf.resize(len < 0 ? 0 : len);

		buf.insert(buf.begin(), _spillover.begin(), _spillover.end());
		_spillover.clear();

		if(buf.empty() || len == -1)
		{
			if(len == -1)
				io_error("read");
			return io::bytes_ptr();
		}

		if(_cd == (iconv_t)(-1))
		{
			auto first = buf.begin(), last = utf8::find_safe_end(buf.begin(), buf.end());
			if(utf8::is_valid(first, last))
			{
				_spillover = std::string(last, buf.end());
				buf.resize(last - first);
				return std::make_shared<io::bytes_t>(buf);
			}

			std::string charset = path::get_attr(_path, "com.apple.TextEncoding");
			if(charset != NULL_STR)
				set_charset(charset.substr(0, charset.find(';')));

			if(_cd == (iconv_t)-1)
			{
				charset = settings_for_path(_path, "attr.file.unknown-encoding " + file::path_attributes(_path)).get(kSettingsEncodingKey, kCharsetUnknown);
				if(charset != kCharsetUnknown && charset != kCharsetUTF8)
					set_charset(charset);
			}

			if(_cd == (iconv_t)-1)
			{
				charset = "ISO-8859-1";

				encoding::classifier_t db;
				static std::string const kEncodingFrequenciesPath = path::join(path::home(), "Library/Caches/com.macromates.TextMate/EncodingFrequencies.binary");
				db.load(kEncodingFrequenciesPath);

				std::multimap<double, std::string> probabilities;
				for(auto const& charset : db.charsets())
					probabilities.emplace(1 - db.probability(buf.begin(), buf.end(), charset), charset);

				if(!probabilities.empty() && probabilities.begin()->first < 1)
					charset = probabilities.begin()->second;

				set_charset(charset);
			}

			if(_cd == (iconv_t)-1)
			{
				io_error("iconv_open");
				return io::bytes_ptr();
			}
		}

		std::string dst(buf.size(), ' ');
		size_t consumed = convert(_cd, buf.data(), buf.size(), dst);
		_spillover = std::string(buf.begin() + consumed, buf.end());
		return std::make_shared<io::bytes_t>(dst);
	}

	void reader_t::io_error (char const* msg)
	{
		perror(msg);
		if(_fd != -1)
		{
			close(_fd);
			_fd = -1;
		}
	}

	void reader_t::set_charset (std::string const& charset)
	{
		_cd = iconv_open(kCharsetUTF8.c_str(), charset.c_str());
		if(_cd != (iconv_t)-1)
			_encoding.set_charset(charset);
	}

	encoding::type reader_t::encoding () const
	{
		return _encoding;
	}

	std::string read_utf8 (std::string const& path, std::string* charset)
	{
		reader_t reader(path);

		std::string res;
		while(auto bytes = reader.next())
			res.insert(res.end(), bytes->begin(), bytes->end());

		if(charset)
			*charset = reader.encoding().charset();

		return res;
	}

} /* file */
