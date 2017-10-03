#include "encoding.h"
#include "frequencies.capnp.h"
#include <capnp/message.h>
#include <capnp/serialize-packed.h>

static uint32_t const kCapnpClassifierFormatVersion = 1;

namespace encoding
{
	std::vector<std::string> classifier_t::charsets () const
	{
		std::vector<std::string> res;
		for(auto const& pair : _charsets)
			res.emplace_back(pair.first);
		return res;
	}

	void classifier_t::load (std::string const& path)
	{
		try {
			real_load(path);
		}
		catch(std::exception const& e) {
			fprintf(stderr, "exception thrown while loading ‘%s’: %s\n", path.c_str(), e.what());
		}
	}

	void classifier_t::real_load (std::string const& path)
	{
		int fd = open(path.c_str(), O_RDONLY|O_CLOEXEC);
		if(fd != -1)
		{
			capnp::PackedFdMessageReader message(kj::AutoCloseFd{fd});
			auto freq = message.getRoot<Frequencies>();
			if(freq.getVersion() != kCapnpClassifierFormatVersion)
			{
				fprintf(stderr, "skip ‘%s’ version %u (expected %u)\n", path.c_str(), freq.getVersion(), kCapnpClassifierFormatVersion);
				return;
			}

			for(auto const& src : freq.getCharsets())
			{
				record_t r;
				for(auto const& word : src.getWords())
					r.words.emplace(word.getType().getWord(), word.getCount());
				for(auto const& byte : src.getBytes())
					r.bytes.emplace(byte.getType().getByte(), byte.getCount());
				_charsets.emplace(src.getCharset(), r);
			}

			for(auto& pair : _charsets)
			{
				for(auto const& word : pair.second.words)
				{
					_combined.words[word.first] += word.second;
					_combined.total_words += word.second;
					pair.second.total_words += word.second;
				}

				for(auto const& byte : pair.second.bytes)
				{
					_combined.bytes[byte.first] += byte.second;
					_combined.total_bytes += byte.second;
					pair.second.total_bytes += byte.second;
				}
			}
		}
	}

	void classifier_t::save (std::string const& path) const
	{
		capnp::MallocMessageBuilder message;
		auto freq = message.initRoot<Frequencies>();
		freq.setVersion(kCapnpClassifierFormatVersion);
		auto charsets = freq.initCharsets(_charsets.size());
		size_t i = 0;

		for(auto const& pair : _charsets)
		{
			auto entry = charsets[i++];
			entry.setCharset(pair.first);

			auto words = entry.initWords(pair.second.words.size());
			size_t j = 0;
			for(auto const& word : pair.second.words)
			{
				auto tmp = words[j++];
				tmp.getType().setWord(word.first);
				tmp.setCount(word.second);
			}

			auto bytes = entry.initBytes(pair.second.bytes.size());
			j = 0;
			for(auto const& byte : pair.second.bytes)
			{
				auto tmp = bytes[j++];
				tmp.getType().setByte(byte.first);
				tmp.setCount(byte.second);
			}
		}

		int fd = open(path.c_str(), O_CREAT|O_TRUNC|O_WRONLY|O_CLOEXEC, S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH);
		if(fd != -1)
		{
			writePackedMessageToFd(fd, message);
			close(fd);
		}
	}

} /* encoding */
