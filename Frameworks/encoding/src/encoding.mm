#include "encoding.h"
#include "frequencies.capnp.h"
#include <capnp/message.h>
#include <capnp/serialize-packed.h>

static uint32_t const kCapnpClassifierFormatVersion = 1;

namespace encoding
{
	struct classifier_t
	{
		void load (std::string const& path);
		void save (std::string const& path) const;

		void learn (char const* first, char const* last, std::string const& charset)
		{
			auto& r = _charsets[charset];
			each_word(first, last, [&](std::string const& word){
				r.words[word] += 1;
				r.total_words += 1;
				_combined.words[word] += 1;
				_combined.total_words += 1;

				for(char ch : word)
				{
					if(ch > 0x7F)
					{
						r.bytes[ch] += 1;
						r.total_bytes += 1;
						_combined.bytes[ch] += 1;
						_combined.total_bytes += 1;
					}
				}
			});
		}

		double probability (char const* first, char const* last, std::string const& charset) const
		{
			auto record = _charsets.find(charset);
			if(record == _charsets.end())
				return 0;

			std::set<std::string> seen;
			double a = 1, b = 1;

			each_word(first, last, [&](std::string const& word){
				auto global = _combined.words.find(word);
				if(global != _combined.words.end() && seen.insert(word).second)
				{
					auto local = record->second.words.find(word);
					if(local != record->second.words.end())
					{
						double pWT = local->second / (double)record->second.total_words;
						double pWF = (global->second - local->second) / (double)_combined.total_words;
						double p = pWT / (pWT + pWF);

						a *= p;
						b *= 1-p;
					}
					else
					{
						a = 0;
					}
				}
				else
				{
					for(char ch : word)
					{
						if(ch > 0x7F)
						{
							auto global = _combined.bytes.find(ch);
							if(global != _combined.bytes.end())
							{
								auto local = record->second.bytes.find(ch);
								if(local != record->second.bytes.end())
								{
									double pWT = local->second / (double)record->second.total_bytes;
									double pWF = (global->second - local->second) / (double)_combined.total_bytes;
									double p = pWT / (pWT + pWF);

									a *= p;
									b *= 1-p;
								}
								else
								{
									a = 0;
								}
							}
						}
					}
				}
			});

			return (a + b) == 0 ? 0 : a / (a + b);
		}

		std::vector<std::string> charsets () const;

		bool operator== (classifier_t const& rhs) const
		{
			return _charsets == rhs._charsets && _combined == rhs._combined;
		}

		bool operator!= (classifier_t const& rhs) const
		{
			return !(*this == rhs);
		}

	private:
		void real_load (std::string const& path);

		template <typename _F>
		static void each_word (char const* first, char const* last, _F op)
		{
			for(auto eow = first; eow != last; )
			{
				auto bow = std::find_if(eow, last, [](char ch){ return isalpha(ch) || ch > 0x7F; });
				eow = std::find_if(bow, last, [](char ch){ return !isalnum(ch) && ch < 0x80; });
				if(std::find_if(bow, eow, [](char ch){ return ch > 0x7F; }) != eow)
					op(std::string(bow, eow));
			}
		}

		struct record_t
		{
			bool operator== (record_t const& rhs) const
			{
				return words == rhs.words && bytes == rhs.bytes && total_words == rhs.total_words && total_bytes == rhs.total_bytes;
			}

			bool operator!= (record_t const& rhs) const
			{
				return !(*this == rhs);
			}

			std::map<std::string, size_t> words;
			std::map<char, size_t> bytes;
			size_t total_words = 0;
			size_t total_bytes = 0;
		};

		std::map<std::string, record_t> _charsets;
		record_t _combined;
	};

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

@interface EncodingClassifier : NSObject
{
	NSString* _path;
	encoding::classifier_t _database;
	std::mutex _databaseMutex;

	BOOL _needsSaveDatabase;
	NSTimer* _saveDatabaseTimer;
}
@end

@implementation EncodingClassifier
+ (instancetype)sharedInstance
{
	static EncodingClassifier* sharedInstance = [self new];
	return sharedInstance;
}

- (instancetype)init
{
	if(self = [super init])
	{
		_path = [NSSearchPathForDirectoriesInDomains(NSCachesDirectory, NSUserDomainMask, YES).firstObject stringByAppendingPathComponent:@"com.macromates.TextMate/EncodingFrequencies.binary"];
		_database.load(_path.fileSystemRepresentation);

		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(applicationWillTerminate:) name:NSApplicationWillTerminateNotification object:NSApp];
	}
	return self;
}

- (void)applicationWillTerminate:(NSNotification*)aNotification
{
	[self synchronize];
}

- (std::vector<std::string>)charsets
{
	std::lock_guard<std::mutex> lock(_databaseMutex);
	return _database.charsets();
}

- (double)probabilityForData:(NSData*)data asCharset:(std::string const&)charset
{
	std::lock_guard<std::mutex> lock(_databaseMutex);
	return _database.probability((char const*)data.bytes, (char const*)data.bytes + data.length, charset);
}

- (void)learnData:(NSData*)data asCharset:(std::string const&)charset
{
	std::lock_guard<std::mutex> lock(_databaseMutex);
	_database.learn((char const*)data.bytes, (char const*)data.bytes + data.length, charset);
	self.needsSaveDatabase = YES;
}

- (void)synchronize
{
	std::lock_guard<std::mutex> lock(_databaseMutex);
	if(_needsSaveDatabase)
		_database.save(_path.fileSystemRepresentation);
	self.needsSaveDatabase = NO;
}

- (void)setNeedsSaveDatabase:(BOOL)flag
{
	if(_saveDatabaseTimer)
	{
		[_saveDatabaseTimer invalidate];
		_saveDatabaseTimer = nil;
	}

	if(_needsSaveDatabase = flag)
		_saveDatabaseTimer = [NSTimer scheduledTimerWithTimeInterval:5 target:self selector:@selector(saveDatabaseTimerDidFire:) userInfo:nil repeats:NO];
}

- (void)saveDatabaseTimerDidFire:(NSTimer*)aTimer
{
	[self synchronize];
}
@end

namespace encoding
{
	// ==============
	// = Public API =
	// ==============

	std::vector<std::string> charsets ()
	{
		return EncodingClassifier.sharedInstance.charsets;
	}

	double probability (char const* first, char const* last, std::string const& charset)
	{
		NSData* data = [NSData dataWithBytesNoCopy:(void*)first length:last - first freeWhenDone:NO];
		return [EncodingClassifier.sharedInstance probabilityForData:data asCharset:charset];
	}

	void learn (char const* first, char const* last, std::string const& charset)
	{
		NSData* data = [NSData dataWithBytesNoCopy:(void*)first length:last - first freeWhenDone:NO];
		return [EncodingClassifier.sharedInstance learnData:data asCharset:charset];
	}

} /* encoding */
