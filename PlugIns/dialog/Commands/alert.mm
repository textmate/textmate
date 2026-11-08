#import "../Dialog2.h"
#import "../TMDCommand.h"

/*
echo '{alertStyle = warning; button1 = 'OK'; title = 'test'; body = 'Testing';}' | "$DIALOG" alert

"$DIALOG" help alert
"$DIALOG" alert --alertStyle critical --title "FOOL!" --body "test" --button1 foo --button2 bar --button3 baz
*/

// =========
// = Alert =
// =========

@interface TMDAlertCommand : TMDCommand
@end

NSAlertStyle alert_style_from_string (NSString* str)
{
	if([str isEqualToString:@"warning"])
		return NSAlertStyleWarning;
	else if([str isEqualToString:@"critical"])
		return NSAlertStyleCritical;
	else
		return NSAlertStyleInformational;
}

@implementation TMDAlertCommand
+ (void)load
{
	[super registerObject:[self new] forCommand:@"alert"];
}

- (void)handleCommand:(CLIProxy*)proxy
{
	NSDictionary* args = [proxy parameters];

	NSAlert* alert = [NSAlert new];
	[alert setAlertStyle:alert_style_from_string([args objectForKey:@"alertStyle"])];
	if(NSString* msg = [args objectForKey:@"title"])
		[alert setMessageText:msg];
	if(NSString* txt = [args objectForKey:@"body"])
		[alert setInformativeText:txt];

	NSInteger i = 0;
	while(NSString* button = [args objectForKey:[NSString stringWithFormat:@"button%ld", ++i]])
		[alert addButtonWithTitle:button];

	NSInteger alertResult = ([alert runModal] - NSAlertFirstButtonReturn);
	NSDictionary* resultDict = @{ @"buttonClicked": @(alertResult) };

	[TMDCommand writePropertyList:resultDict toFileHandle:[proxy outputHandle]];
}

- (NSString*)commandDescription
{
	return @"Show an alert box.";
}

- (NSString*)usageForInvocation:(NSString*)invocation;
{
	return [NSString stringWithFormat:@"\t%1$@ --alertStyle warning --title 'Delete File?' --body 'You cannot undo this action.' --button1 Delete --button2 Cancel\n", invocation];
}
@end
