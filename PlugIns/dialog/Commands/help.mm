#import "../TMDCommand.h"

// ========
// = Help =
// ========

@interface TMDHelpCommand : TMDCommand
@end

@implementation TMDHelpCommand
+ (void)load
{
	[TMDCommand registerObject:[self new] forCommand:@"help"];
}

- (NSString*)commandDescription
{
	return @"Gives a brief list of available commands, or usage details for a specific command.";
}

- (NSString*)usageForInvocation:(NSString*)invocation;
{
	return [NSString stringWithFormat:@"%@ <command>", invocation];
}

- (NSString*)commandSummaryText
{
	NSDictionary* commands = [TMDCommand registeredCommands];

	NSMutableArray* help = [NSMutableArray arrayWithCapacity:100];

	NSMutableArray* registeredCommands = [NSMutableArray arrayWithCapacity:100];
	for(NSString* commandName in commands)
	{
		if(![commandName hasPrefix:@"x-"])
		{
			TMDCommand* command = [commands objectForKey:commandName];
			NSString* description = [command commandDescription];
			[registeredCommands addObject:[NSString stringWithFormat:@"\t%@: %@", commandName, description]];
		}
	}

	[help addObject:@"usage: \"$DIALOG\" [--version] <command> [<args>]"];
	[help addObject:[NSString stringWithFormat:@"%ld commands registered:", [registeredCommands count]]];
	[help addObjectsFromArray:[registeredCommands sortedArrayUsingSelector:@selector(caseInsensitiveCompare:)]];
	[help addObject:@"Use `\"$DIALOG\" help <command>` for detailed help.\n"];

	return [help componentsJoinedByString:@"\n"];
}

- (NSString*)helpForCommand:(NSString*)commandName
{
	NSMutableString* help = [NSMutableString stringWithCapacity:100];

	TMDCommand* command = nil;
	if(![commandName hasPrefix:@"x-"] && (command = [TMDCommand objectForCommand:commandName]))
	{
		[help appendFormat:@"%@\n\n",[command commandDescription]];
		[help appendFormat:@"%@ usage:\n",commandName];
		[help appendFormat:@"%@\n",[command usageForInvocation:[NSString stringWithFormat:@"\"$DIALOG\" %@", commandName]]];
	}
	else
	{
		[help appendFormat:@"Unknown command '%@'\n", commandName];
	}

	return help;
}

- (void)handleCommand:(CLIProxy*)proxy
{
	if([proxy numberOfArguments] < 3)
			[proxy writeStringToError:[self commandSummaryText]];
	else	[proxy writeStringToOutput:[self helpForCommand:[proxy argumentAtIndex:2]]];
}
@end
/*
"$DIALOG" help
"$DIALOG" help help
*/
