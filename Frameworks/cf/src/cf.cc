#include "cf.h"

namespace cf
{
	std::string to_s (CFStringRef aString)
	{
		if(!aString)
			return NULL_STR;

		CFIndex byteCount;
		CFStringGetBytes(aString, CFRangeMake(0, CFStringGetLength(aString)), kCFStringEncodingUTF8, 0, false, nullptr, 0, &byteCount);
		std::string res(byteCount, '\0');
		CFStringGetBytes(aString, CFRangeMake(0, CFStringGetLength(aString)), kCFStringEncodingUTF8, 0, false, (UInt8*)&res[0], byteCount, nullptr);
		return res;
	}

	std::string to_s (CFErrorRef error)
	{
		CFStringRef error_str = CFErrorCopyDescription(error);
		std::string res(to_s(error_str));
		CFRelease(error_str);
		return res;
	}

	string_t::string_t (std::string const& str)
	{
		string.reset(CFStringCreateWithBytes(kCFAllocatorDefault, (UInt8*)str.data(), str.size(), kCFStringEncodingUTF8, false), CFRelease);
	}

} /* cf */
