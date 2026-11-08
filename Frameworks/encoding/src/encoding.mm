#include "encoding.h"

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
			os_log_error(OS_LOG_DEFAULT, "Exception thrown while loading ‘%{public}s’: %{public}s", path.c_str(), e.what());
		}
	}

	void classifier_t::real_load (std::string const& path)
	{
		NSData* data = [NSData dataWithContentsOfFile:@(path.c_str())];
		if(!data)
			return;

		NSError* error = nil;
		NSSet* classes = [NSSet setWithObjects:NSDictionary.class, NSString.class, NSNumber.class, nil];
		NSDictionary* root = [NSKeyedUnarchiver unarchivedObjectOfClasses:classes fromData:data error:&error];
		if(!root)
		{
			os_log_error(OS_LOG_DEFAULT, "Failed to load ‘%{public}s’: %{public}@", path.c_str(), error);
			return;
		}
		if([root[@"version"] unsignedIntegerValue] != 1)
			return;

		NSDictionary* charsets = root[@"charsets"];
		for(NSString* charset in charsets)
		{
			NSDictionary* rec = charsets[charset];
			record_t r;
			NSDictionary* words = rec[@"words"];
			for(NSString* word in words)
				r.words.emplace(word.UTF8String, [words[word] unsignedLongLongValue]);
			NSDictionary* bytes = rec[@"bytes"];
			for(NSNumber* byte in bytes)
				r.bytes.emplace(byte.unsignedCharValue, [bytes[byte] unsignedLongLongValue]);
			_charsets.emplace(charset.UTF8String, r);
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

	void classifier_t::save (std::string const& path) const
	{
		NSMutableDictionary* charsets = [NSMutableDictionary dictionary];
		for(auto const& pair : _charsets)
		{
			NSString* charsetKey = @(pair.first.c_str());
			if(!charsetKey)
				continue;

			NSMutableDictionary* words = [NSMutableDictionary dictionary];
			for(auto const& word : pair.second.words)
			{
				if(NSString* key = @(word.first.c_str()))
					words[key] = @(word.second);
			}

			NSMutableDictionary* bytes = [NSMutableDictionary dictionary];
			for(auto const& byte : pair.second.bytes)
				bytes[@(byte.first)] = @(byte.second);

			charsets[charsetKey] = @{ @"words": words, @"bytes": bytes };
		}

		NSDictionary* root = @{ @"version": @1, @"charsets": charsets };
		NSError* error = nil;
		NSData* data = [NSKeyedArchiver archivedDataWithRootObject:root requiringSecureCoding:YES error:&error];
		if(!data)
		{
			os_log_error(OS_LOG_DEFAULT, "Failed to save '%{public}s': %{public}@", path.c_str(), error);
			return;
		}
		[data writeToFile:@(path.c_str()) atomically:YES];
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
		_path = [NSSearchPathForDirectoriesInDomains(NSCachesDirectory, NSUserDomainMask, YES).firstObject stringByAppendingPathComponent:@"com.macromates.TextMate/EncodingFrequencies.plist"];
		_database.load(_path.fileSystemRepresentation);

		[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(applicationWillTerminate:) name:NSApplicationWillTerminateNotification object:NSApp];
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
