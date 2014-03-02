#import "to_dictionary.h"

namespace
{
	struct create_cf_property_list : boost::static_visitor<CFPropertyListRef>
	{
		CFPropertyListRef operator() (bool flag) const                     { return CFRetain(flag ? kCFBooleanTrue : kCFBooleanFalse); }
		CFPropertyListRef operator() (int32_t i) const                     { return CFNumberCreate(kCFAllocatorDefault, kCFNumberSInt32Type, &i); }
		CFPropertyListRef operator() (uint64_t i) const                    { return CFNumberCreate(kCFAllocatorDefault, kCFNumberSInt64Type, &i); }
		CFPropertyListRef operator() (std::string const& str) const        { return CFStringCreateWithBytes(kCFAllocatorDefault, (UInt8*)str.data(), str.size(), kCFStringEncodingUTF8, false); }
		CFPropertyListRef operator() (std::vector<char> const& data) const { return CFDataCreate(kCFAllocatorDefault, data.empty() ? NULL : (UInt8*)&data[0], data.size()); }
		CFPropertyListRef operator() (oak::date_t const& date) const       { return CFDateCreate(kCFAllocatorDefault, date.value()); }

		CFPropertyListRef operator() (plist::array_t const& array) const
		{
			CFMutableArrayRef res = CFArrayCreateMutable(kCFAllocatorDefault, 0, &kCFTypeArrayCallBacks);
			for(auto const& it : array)
			{
				CFPropertyListRef value = boost::apply_visitor(*this, it);
				CFArrayAppendValue(res, value);
				CFRelease(value);
			}
			return res;
		}

		CFPropertyListRef operator() (plist::dictionary_t const& dict) const
		{
			CFMutableDictionaryRef res = CFDictionaryCreateMutable(kCFAllocatorDefault, 0, &kCFTypeDictionaryKeyCallBacks, &kCFTypeDictionaryValueCallBacks);
			for(auto const& it : dict)
			{
				CFPropertyListRef key = (*this)(it.first);
				CFPropertyListRef value = boost::apply_visitor(*this, it.second);
				CFDictionarySetValue(res, key, value);
				CFRelease(key);
				CFRelease(value);
			}
			return res;
		}
	};
}

namespace ns
{
	NSDictionary* to_dictionary (plist::any_t const& plist)
	{
		CFPropertyListRef res = plist::create_cf_property_list(plist);
		NSDictionary* dict = (NSDictionary*)CFBridgingRelease(res);
		return dict;
	}

	NSMutableDictionary* to_mutable_dictionary (plist::any_t const& plist)
	{
		return [NSMutableDictionary dictionaryWithDictionary:to_dictionary(plist)];
	}

} /* ns */