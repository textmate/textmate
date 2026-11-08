#import "TMDCommand.h"

static NSMutableDictionary* Commands = nil;

@implementation TMDCommand
+ (void)registerObject:(id)anObject forCommand:(NSString*)aCommand
{
	if(!Commands)
		Commands = [NSMutableDictionary new];
	[Commands setObject:anObject forKey:aCommand];
}

+ (NSDictionary*)registeredCommands
{
	return [Commands copy];
}

+ (id)objectForCommand:(NSString*)aCommand
{
	return [Commands objectForKey:aCommand];
}

+ (id)readPropertyList:(NSFileHandle*)aFileHandle error:(NSError**)error;
{
	NSData* data = [aFileHandle readDataToEndOfFile];
	if([data length] == 0)
		return nil;

	id plist = [NSPropertyListSerialization propertyListWithData:data options:NSPropertyListMutableContainersAndLeaves format:nil error:error];

	return plist;
}

+ (void)writePropertyList:(id)aPlist toFileHandle:(NSFileHandle*)aFileHandle
{
	NSError* error = nil;
	if(NSData* data = [NSPropertyListSerialization  dataWithPropertyList:aPlist format:NSPropertyListXMLFormat_v1_0 options:0 error:&error])
	{
		[aFileHandle writeData:data];
	}
	else
	{
		fprintf(stderr, "%s\n", [[error localizedDescription] UTF8String] ?: "unknown error serializing returned property list");
		fprintf(stderr, "%s\n", [[aPlist description] UTF8String]);
	}
}

- (NSString*)commandDescription
{
	return @"No information available for this command";
}

- (NSString*)usageForInvocation:(NSString*)invocation;
{
	return @"No usage information available for this command";
}
@end
