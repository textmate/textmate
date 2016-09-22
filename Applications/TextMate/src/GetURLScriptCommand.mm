@interface GetURLScriptCommand : NSScriptCommand
@end

@implementation GetURLScriptCommand
- (id)performDefaultImplementation
{
	NSString* urlString = [self directParameter];
	if([urlString hasPrefix:@"txmt:"] && ![urlString hasPrefix:@"txmt://"])
		urlString = [@"txmt://" stringByAppendingString:[urlString substringFromIndex:5]];
	[NSApp sendAction:@selector(handleTxMtURL:) to:nil from:[NSURL URLWithString:urlString]];
	return nil;
}
@end
