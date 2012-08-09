#include "case.h"
#include "utf8.h"
#include <oak/iterator_macros.h>

namespace text
{
	static std::string convert_helper (std::string bytes, void(*operation)(CFMutableStringRef, CFLocaleRef))
	{
		CFStringRef tmp = CFStringCreateWithBytes(kCFAllocatorDefault, (UInt8*)bytes.data(), bytes.size(), kCFStringEncodingUTF8, FALSE);
		CFMutableStringRef str = CFStringCreateMutableCopy(kCFAllocatorDefault, 0, tmp);

		operation(str, NULL);

		CFIndex len = 0;
		if(CFStringGetBytes(str, CFRangeMake(0, CFStringGetLength(str)), kCFStringEncodingUTF8, '?', false, NULL, 0, &len) > 0)
		{
			char buf[len];
			if(CFStringGetBytes(str, CFRangeMake(0, CFStringGetLength(str)), kCFStringEncodingUTF8, '?', false, (UInt8*)buf, len, NULL) > 0)
				bytes = std::string(buf, buf + len);
		}

		CFRelease(str);
		CFRelease(tmp);

		return bytes;
	}

	std::string uppercase (std::string const& str)
	{
		return convert_helper(str, &CFStringUppercase);
	}

	std::string lowercase (std::string const& str)
	{
		return convert_helper(str, &CFStringLowercase);
	}

	static bool is_upper (uint32_t ch) { return ch < 0x80 ? isupper(ch) : CFCharacterSetIsLongCharacterMember(CFCharacterSetGetPredefined(kCFCharacterSetUppercaseLetter), ch); }
	static bool is_lower (uint32_t ch) { return ch < 0x80 ? islower(ch) : CFCharacterSetIsLongCharacterMember(CFCharacterSetGetPredefined(kCFCharacterSetLowercaseLetter), ch); }

	std::string opposite_case (std::string const& str)
	{
		static struct { bool(*tester)(uint32_t); void(*flipper)(CFMutableStringRef, CFLocaleRef); } const tests[2] = { { &is_upper, &CFStringLowercase }, { &is_lower, &CFStringUppercase } };

		char const* first = str.data();
		char const* last  = first + str.size();

		std::string res = "";
		size_t from = 0, i = 0;
		foreach(ch, utf8::make(first), utf8::make(last))
		{
			if(tests[i].tester(*ch))
				continue;

			size_t to = &ch - first;
			res += convert_helper(str.substr(from, to - from), tests[i].flipper);
			i = 1 - i;
			from = to;
		}
		res += convert_helper(str.substr(from), tests[i].flipper);
		return res;
	}

} /* text */
