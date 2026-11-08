#import "OakHTMLOutputRequestMetadata.h"

static NSMutableDictionary<NSString*, OakHTMLOutputRequestMetadata*>* sMetadataByURL;
static dispatch_queue_t sMetadataQueue;

@implementation OakHTMLOutputRequestMetadata
+ (void)initialize
{
	sMetadataByURL = [NSMutableDictionary new];
	sMetadataQueue = dispatch_queue_create("com.macromates.metadata", DISPATCH_QUEUE_SERIAL);
}

+ (void)setMetadata:(OakHTMLOutputRequestMetadata*)metadata forURLString:(NSString*)urlString
{
	dispatch_sync(sMetadataQueue, ^{
		sMetadataByURL[urlString] = metadata;
	});
}

+ (OakHTMLOutputRequestMetadata*)metadataForURLString:(NSString*)urlString
{
	__block OakHTMLOutputRequestMetadata* result;
	dispatch_sync(sMetadataQueue, ^{
		result = sMetadataByURL[urlString];
	});
	return result;
}

+ (void)removeMetadataForURLString:(NSString*)urlString
{
	dispatch_sync(sMetadataQueue, ^{
		[sMetadataByURL removeObjectForKey:urlString];
	});
}

@end
