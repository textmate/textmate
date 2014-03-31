#ifndef OAK_CFXX_H_PA9RZ6MB
#define OAK_CFXX_H_PA9RZ6MB

namespace cf
{
	template <typename T = CFTypeRef>
	struct value
	{
		value (T value = NULL) : cf_value(value)  { retain(); }
		value (value const& rhs) : cf_value(rhs.cf_value) { retain(); }
		~value ()                                         { release(); }
		value& operator= (value const& rhs)               { reset(rhs.cf_value); return *this; }

		void reset (T value = NULL)
		{
			if(value != cf_value)
			{
				release();
				cf_value = value;
				retain();
			}
		}

		explicit operator bool () const { return cf_value != NULL; }
		operator T () const { return cf_value; }
		operator T () { return cf_value; }

	protected:
		T cf_value;

	private:
		void retain ()  { if(cf_value) CFRetain(cf_value); }
		void release () { if(cf_value) CFRelease(cf_value); }
	};

	struct data : value<CFDataRef>
	{
		data (void const* first, void const* last) { reset(CFDataCreate(kCFAllocatorDefault, (UInt8*)first, (UInt8*)last - (UInt8*)first)); }
	};

	struct string : value<CFStringRef>
	{
		string (char const* str) { reset(CFStringCreateWithBytes(kCFAllocatorDefault, (UInt8*)str, strlen(str), kCFStringEncodingUTF8, false)); }
		string (std::string const& str) { reset(CFStringCreateWithBytes(kCFAllocatorDefault, (UInt8*)str.data(), str.size(), kCFStringEncodingUTF8, false)); }
	};

	struct dict_helper;

	struct dictionary : value<CFMutableDictionaryRef>
	{
		dictionary () { reset(CFDictionaryCreateMutable(kCFAllocatorDefault, 0, &kCFTypeDictionaryKeyCallBacks, &kCFTypeDictionaryValueCallBacks)); }

		dict_helper operator[] (string const& key);
		dict_helper operator[] (char const* key);
	};

	struct dict_helper
	{
		cf::value<> operator= (CFTypeRef const& value)
		{
			CFDictionarySetValue(dict, key, value);
			return value;
		}

	private:
		friend struct dictionary;
		dict_helper (dictionary const& dict, string const& key) : dict(dict), key(key) { }
		dictionary dict;
		string key;
	};

	inline dict_helper dictionary::operator[] (string const& key) { return dict_helper(*this, key); }
	inline dict_helper dictionary::operator[] (char const* key)   { return dict_helper(*this, cf::string(key)); }

	struct array : value<CFMutableArrayRef>
	{
		array ()             { reset(CFArrayCreateMutable(kCFAllocatorDefault, 0, &kCFTypeArrayCallBacks)); }
		size_t size () const { return CFArrayGetCount(cf_value); }
		bool empty () const  { return size() == 0; }

		array& operator<< (CFTypeRef const& value)
		{
			CFArrayAppendValue(cf_value, value);
			return *this;
		}
	};

	struct number : value<CFNumberRef>
	{
		number (int flag)
		{
			reset(CFNumberCreate(kCFAllocatorDefault, kCFNumberIntType, &flag));
		}
	};

} /* cf */

#endif /* end of include guard: OAK_CFXX_H_PA9RZ6MB */
