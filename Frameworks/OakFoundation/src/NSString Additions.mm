#import "NSString Additions.h"
#import <text/utf8.h>
#import <oak/oak.h>

@implementation NSString (Additions)
+ (NSString*)stringWithUTF8String:(char const*)aString length:(unsigned)aLength
{
	ASSERT(utf8::is_valid(aString, aString + aLength));

	NSString* res = [[NSString alloc] initWithBytes:aString length:aLength encoding:NSUTF8StringEncoding];
	if(!res && aLength && utf8::is_valid(aString, aString + aLength))
	{
		// Re-encode every code point to remove redundant encodings, which NSString does not like.
		std::string tmp;
		foreach(ch, utf8::make(aString), utf8::make(aString + aLength))
			tmp += utf8::to_s(*ch);
		res = [[NSString alloc] initWithBytes:tmp.data() length:tmp.size() encoding:NSUTF8StringEncoding];
	}
	return res;
}

+ (NSString*)stringWithCxxString:(std::string const&)aString
{
	return aString == NULL_STR ? nil : [self stringWithUTF8String:aString.data() length:aString.size()];
}
@end
