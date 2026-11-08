#import "TMDChameleon.h"
#import "../../Dialog2.h"
#import "../../TMDCommand.h"

@interface TMDPrototype : TMDCommand
@end

@implementation TMDPrototype
+ (void)load
{
	[TMDPrototype registerObject:[self new] forCommand:@"prototype"];
}

- (void)handleCommand:(CLIProxy*)proxy
{
	NSDictionary* args = [proxy parameters];

	if(NSDictionary* values = [args objectForKey:@"register"])
	{
		// FIXME this is needed only because we presently can’t express argument constraints (CLIProxy would otherwise correctly validate/convert CLI arguments)
		if([values isKindOfClass:[NSString class]])
			values = [NSPropertyListSerialization propertyListWithData:[(NSString*)values dataUsingEncoding:NSUTF8StringEncoding] options:NSPropertyListImmutable format:NULL error:NULL];

		for(id key in [values allKeys])
			[TMD2Chameleon createSubclassNamed:key withValues:[values objectForKey:key]];
	}

	if(NSString* show = [args objectForKey:@"show"])
	{
		id obj = [NSClassFromString(show) new];
		[proxy writeStringToOutput:[obj description] ?: [NSString stringWithFormat:@"error: no class named ‘%@’", show]];
		[proxy writeStringToOutput:@"\n"];
	}
}

- (NSString*)commandDescription
{
	return @"Register classes for use with NSArrayController.";
}

- (NSString*)usageForInvocation:(NSString*)invocation;
{
	 return [NSString stringWithFormat:@"\t%1$@ --register \"{ SQL_New_Connection = { title = untitled; serverType = MySQL; hostName = localhost; userName = '$LOGNAME'; }; }\"\n\t%1$@ --show SQL_New_Connection\n", invocation];
}
@end
