#include <oak/debug.h>
#include <oak/misc.h>

namespace text
{
	struct transcode_t
	{
		transcode_t (std::string fromCharset, std::string toCharset)
		{
			std::string const kBOMCharacter = "\uFEFF";

			_buffer = new char[_buffer_size];
			reset_buffer();

			bool readBOM = false;
			fromCharset = parse_charset(fromCharset, &readBOM);
			if(readBOM)
			{
				if(auto bomWriter = transcode_t("UTF-8", fromCharset))
					bomWriter(kBOMCharacter.data(), kBOMCharacter.data() + kBOMCharacter.size(), back_inserter(_skip_prefix));
			}

			bool writeBOM = false;
			toCharset = parse_charset(toCharset, &writeBOM);
			if(writeBOM)
			{
				if(auto bomWriter = transcode_t("UTF-8", toCharset))
				{
					_dest = bomWriter(kBOMCharacter.data(), kBOMCharacter.data() + kBOMCharacter.size(), _dest);
					_left = _buffer_size - (_dest - _buffer);
				}
			}

			_handle = iconv_open(toCharset.c_str(), fromCharset.c_str());
			if(_handle == (iconv_t)-1)
				perrorf("transcode_t: iconv_open(\"%s\", \"%s\")", toCharset.c_str(), fromCharset.c_str());
		}

		~transcode_t ()
		{
			ASSERT_EQ(_left, _buffer_size);
			if(_handle != (iconv_t)-1)
				iconv_close(_handle);
			delete[] _buffer;
		}

		operator bool () const
		{
			return _handle != (iconv_t)-1;
		}

		template <typename _OutputIter>
		_OutputIter operator() (_OutputIter out)
		{
			for(auto ch : _partial_sequence)
			{
				// This asserts that the output encoding is an ASCII superset
				int bytesWritten = std::min(snprintf(_dest, _left, "\\x%02X", ch), (int)_left);
				if(bytesWritten > 0)
				{
					_dest += bytesWritten;
					_left -= bytesWritten;
				}
			}
			_partial_sequence.clear();

			out = std::copy(_buffer, _dest, out);
			reset_buffer();
			return out;
		}

		template <typename _OutputIter>
		_OutputIter operator() (char const* first, char const* last, _OutputIter out)
		{
			ASSERT_NE(_handle, (iconv_t)-1);

			char* inBuf      = (char*)first;
			size_t inBufLeft = last - first;

			if(_skip_offset < _skip_prefix.size())
			{
				size_t len = std::min(_skip_prefix.size() - _skip_offset, inBufLeft);
				if(std::equal(inBuf, inBuf + len, _skip_prefix.data() + _skip_offset))
				{
					_skip_offset += len;
				}
				else
				{
					size_t oldOffset = _skip_offset;
					_skip_offset = _skip_prefix.size();
					out = (*this)(_skip_prefix.data(), _skip_prefix.data() + oldOffset + len, out);
				}

				inBuf        += len;
				inBufLeft    -= len;
			}

			while(inBufLeft)
			{
				size_t rc;
				if(_partial_sequence.empty())
				{
					rc = iconv(_handle, &inBuf, &inBufLeft, &_dest, &_left);
				}
				else
				{
					do {

						_partial_sequence.append(1, *inBuf++);
						inBufLeft -= 1;

						char* partialBuf      = (char*)_partial_sequence.data();
						size_t partialBufLeft = _partial_sequence.size();

						rc = iconv(_handle, &partialBuf, &partialBufLeft, &_dest, &_left);
						_partial_sequence.erase(0, _partial_sequence.size() - partialBufLeft);

						if(rc == (size_t)-1 && errno != EINVAL)
							break;

					} while(!_partial_sequence.empty() && inBufLeft);
				}

				out = std::copy(_buffer, _dest, out);
				reset_buffer();

				if(rc == (size_t)-1)
				{
					if(errno == EINVAL)
					{
						_partial_sequence.append(inBuf, inBufLeft);
						break;
					}
					else if(errno == EILSEQ)
					{
						++_invalid_count;

						if(_partial_sequence.empty())
						{
							_partial_sequence.append(1, *inBuf++);
							--inBufLeft;
						}

						out = (*this)(out); // Force writing current partial sequence to ‘out’
					}
					else if(errno == E2BIG)
					{
						ASSERT_NE(_left, 0);
					}
					else
					{
						perror("transcode_t: iconv");
						break;
					}
				}
			}

			out = std::copy(_buffer, _dest, out);
			reset_buffer();

			return out;
		}

		size_t invalid_count () const { return _invalid_count; }

	private:
		std::string parse_charset (std::string charset, bool* bom)
		{
			std::string const kBOMOption = "//BOM";
			auto i = charset.find(kBOMOption);
			if(i != std::string::npos)
			{
				charset.erase(i, kBOMOption.size());
				if(bom)
					*bom = true;
			}
			return charset;
		}

		void reset_buffer ()
		{
			_dest = _buffer;
			_left = _buffer_size;
		}

		iconv_t _handle = (iconv_t)-1;
		size_t _invalid_count = 0;

		std::string _skip_prefix;
		size_t _skip_offset = 0;

		std::string _partial_sequence;

		char* _buffer;
		size_t _buffer_size = 4096;

		char* _dest;
		size_t _left;
	};

} /* text */
