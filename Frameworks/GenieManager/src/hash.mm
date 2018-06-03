#import "hash.h"

NSString* hash (NSString* key)
{
	uint8_t digest[CC_SHA1_DIGEST_LENGTH];

	NSData* data = [key dataUsingEncoding:NSUTF8StringEncoding];
	CC_SHA1(data.bytes, data.length, digest);

	NSMutableString* output = [NSMutableString stringWithCapacity:CC_SHA1_DIGEST_LENGTH * 2];
	for(int i = 0; i < CC_SHA1_DIGEST_LENGTH; i++)
		[output appendFormat:@"%02x", digest[i]];

	return output;
}
