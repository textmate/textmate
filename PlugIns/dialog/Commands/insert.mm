#import "../Dialog2.h"
#import "../TMDCommand.h"
#import "Utilities/TextMate.h"

@interface TMDInsertCommands : TMDCommand
@end

@implementation TMDInsertCommands
+ (void)load
{
	[TMDCommand registerObject:[self new] forCommand:@"x-insert"];
}

- (void)handleCommand:(CLIProxy*)proxy;
{
	NSDictionary* args = [proxy parameters];

	if(NSString* text = [args objectForKey:@"text"])
		insert_text(text);
	else if(NSString* snippet = [args objectForKey:@"snippet"])
		insert_snippet(snippet);
}
@end
