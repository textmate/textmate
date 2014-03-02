#ifndef CF_H_HHFWBJSW
#define CF_H_HHFWBJSW

#include "image.h"
#include <oak/oak.h>

namespace cf
{
	PUBLIC std::string to_s (CFStringRef aString);
	PUBLIC std::string to_s (CFErrorRef error);

	struct PUBLIC string_t
	{
		string_t (std::string const& str);
		operator CFStringRef () const { return string.get(); }
		CFStringRef get () const { return string.get(); }
	private:
		std::shared_ptr<__CFString const> string;
	};

	inline cf::string_t wrap (std::string const& str) { return cf::string_t(str); }

	struct PUBLIC number_t
	{
		number_t (int32_t i)
		{
			number.reset(CFNumberCreate(kCFAllocatorDefault, kCFNumberSInt32Type, &i), CFRelease);
		}
		operator CFNumberRef () const { return number.get(); }
		CFNumberRef get () const { return number.get(); }
	private:
		std::shared_ptr<__CFNumber const> number;
	};

	inline cf::number_t wrap (int32_t number) { return cf::number_t(number); }

	struct PUBLIC array_t
	{
		template <typename T>
		array_t (std::vector<T> const& v)
		{
			CFMutableArrayRef value = CFArrayCreateMutable(kCFAllocatorDefault, 0, &kCFTypeArrayCallBacks);
			for(auto const& it : v)
				CFArrayAppendValue(value, cf::wrap(it));
			array.reset(value, CFRelease);
		}
		operator CFArrayRef () const { return array.get(); }
		CFArrayRef get () const { return array.get(); }
	private:
		std::shared_ptr<__CFArray const> array;
	};

	template <typename T> cf::array_t wrap (std::vector<T> const& v) { return cf::array_t(v); }

	struct PUBLIC dictionary_t
	{
		template <typename K, typename V>
		dictionary_t (std::map<K, V> const& map)
		{
			CFMutableDictionaryRef value = CFDictionaryCreateMutable(kCFAllocatorDefault, 0, &kCFTypeDictionaryKeyCallBacks, &kCFTypeDictionaryValueCallBacks);
			for(auto const& it : map)
				CFDictionaryAddValue(value, cf::wrap(it.first).get(), cf::wrap(it.second).get());
			dictionary.reset(value, CFRelease);
		}
		operator CFDictionaryRef () const { return dictionary.get(); }
		CFDictionaryRef get () const { return dictionary.get(); }
	private:
		std::shared_ptr<__CFDictionary const> dictionary;
	};

	template <typename K, typename V> cf::dictionary_t wrap (std::map<K, V> const& map) { return cf::dictionary_t(map); }

	struct PUBLIC set_t
	{
		template <typename T>
		set_t (std::set<T> const& v)
		{
			CFMutableSetRef value = CFSetCreateMutable(kCFAllocatorDefault, 0, &kCFTypeSetCallBacks);
			for(auto const& it : v)
				CFSetAddValue(value, cf::wrap(it));
			set.reset(value, CFRelease);
		}
		operator CFSetRef () const { return set.get(); }
		CFSetRef get () const { return set.get(); }
	private:
		std::shared_ptr<__CFSet const> set;
	};

	template <typename T> cf::set_t wrap (std::set<T> const& v) { return cf::set_t(v); }

} /* cf */

#endif /* end of include guard: CF_H_HHFWBJSW */
