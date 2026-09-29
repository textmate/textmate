#include "reader.h"
#include "path_info.h"
#include <encoding/src/encoding.h>
#include <text/src/utf8.h>
#include <io/src/path.h>
#include <settings/src/settings.h>

namespace file
{
	reader_t::reader_t (std::string const& path) : _path(path)
	{
		_fd = open(_path.c_str(), O_RDONLY|O_CLOEXEC);
		if(_fd == -1)
		{
			io_error("open");
			return;
		}

		char buf[4];
		ssize_t len = read(_fd, buf, sizeof(buf));
		if(len == -1)
		{
			io_error("read");
			return;
		}

		std::string charset = encoding::charset_from_bom(std::begin(buf), std::begin(buf) + len);
		if(charset != kCharsetNoEncoding)
		{
			set_charset(charset);
			if(!_transcode)
				return;
		}

		lseek(_fd, 0, SEEK_SET);
		fcntl(_fd, F_NOCACHE, 1);
	}

	reader_t::~reader_t ()
	{
		if(_fd != -1)
			close(_fd);
	}

	io::bytes_ptr reader_t::next ()
	{
		if(_fd == -1)
			return io::bytes_ptr();

		std::string buf(8192, '\0');

		ssize_t len = read(_fd, &buf.front(), buf.size());
		buf.resize(len < 0 ? 0 : len);

		if(len == 0 && !utf8::is_valid(_spillover.begin(), _spillover.end()))
			_spillover = "\uFFFD";

		buf.insert(buf.begin(), _spillover.begin(), _spillover.end());
		_spillover.clear();

		if(buf.empty() || len == -1)
		{
			if(len == -1)
				io_error("read");
			return io::bytes_ptr();
		}

		if(!_transcode)
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

			if(!_transcode)
			{
				charset = settings_for_path(_path, "attr.file.unknown-encoding " + file::path_attributes(_path)).get(kSettingsEncodingKey, kCharsetUnknown);
				if(charset != kCharsetUnknown && charset != kCharsetUTF8)
					set_charset(charset);
			}

			if(!_transcode)
			{
				charset = "ISO-8859-1";

				std::multimap<double, std::string> probabilities;
				for(auto const& charset : encoding::charsets())
					probabilities.emplace(1 - encoding::probability(buf.data(), buf.data() + buf.size(), charset), charset);

				if(!probabilities.empty() && probabilities.begin()->first < 1)
					charset = probabilities.begin()->second;

				set_charset(charset);
			}

			if(!_transcode)
			{
				os_log_error(OS_LOG_DEFAULT, "Unable to create text::transcode_t");
				return io::bytes_ptr();
			}
		}

		std::string dst;
		if(buf.empty())
				(*_transcode)(back_inserter(dst));
		else	(*_transcode)(buf.data(), buf.data() + buf.size(), back_inserter(dst));
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
		_transcode = std::make_unique<text::transcode_t>(charset, kCharsetUTF8);
		if(*_transcode)
				_encoding.set_charset(charset);
		else	_transcode.reset();
	}

	encoding::type reader_t::encoding () const
	{
		return _encoding;
	}

	std::string read_utf8 (std::string const& path, std::string* charset, size_t limit)
	{
		reader_t reader(path);

		std::string res;
		while(auto bytes = reader.next())
		{
			res.insert(res.end(), bytes->begin(), bytes->end());
			if(res.size() > limit)
			{
				res.resize(limit);
				break;
			}
		}

		if(charset)
			*charset = reader.encoding().charset();

		return res;
	}

} /* file */
