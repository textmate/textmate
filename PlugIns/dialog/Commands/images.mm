#import "../TMDCommand.h"
#import "../Dialog2.h"

@interface TMDImages : TMDCommand
@end

@implementation TMDImages
+ (void)load
{
	[TMDImages registerObject:[self new] forCommand:@"images"];
}

- (void)handleCommand:(CLIProxy*)proxy
{
	NSDictionary* args = [proxy parameters];

	// Convert image paths to NSImages
	NSDictionary* imagePaths = [args objectForKey:@"register"];
	if([imagePaths isKindOfClass:[NSString class]])
		imagePaths = [NSPropertyListSerialization propertyListWithData:[(NSString*)imagePaths dataUsingEncoding:NSUTF8StringEncoding] options:NSPropertyListImmutable format:NULL error:NULL];

	for(NSString* imageName in [imagePaths allKeys])
	{
		if([NSImage imageNamed:imageName]) // presumably this is not the first time the menu is invoked with this image, so skip loading it to avoid potential leaks
			continue;

		NSImage* image = [[NSImage alloc] initByReferencingFile:[imagePaths objectForKey:imageName]];
		if(image && [image isValid])
			[image setName:imageName];
	}
}

- (NSString*)commandDescription
{
	return @"Add image files as named images for use by other commands/nibs.";
}

- (NSString*)usageForInvocation:(NSString*)invocation;
{
	return [NSString stringWithFormat:@"\t%1$@ --register  \"{ macro = '$(find_app com.macromates.textmate)/Contents/Resources/Bundle Item Icons/Macros.png'; }\"\n", invocation];
}
@end
