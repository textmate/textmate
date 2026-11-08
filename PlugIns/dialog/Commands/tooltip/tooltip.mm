#import "../../TMDCommand.h"
#import "../../Dialog2.h"
#import "TMDHTMLTips.h"
#import "../Utilities/TextMate.h" // -positionForWindowUnderCaret

@interface TMDHTMLTipsCommand : TMDCommand
@end

@implementation TMDHTMLTipsCommand
+ (void)load
{
	[TMDCommand registerObject:[self new] forCommand:@"tooltip"];
}

- (NSString*)commandDescription
{
	return @"Shows a tooltip at the caret with the provided content, optionally rendered as HTML.";
}

- (NSString*)usageForInvocation:(NSString*)invocation;
{
	return [NSString stringWithFormat:@"\t%1$@ --text 'regular text'\n\t%1$@ --html '<some>html</some>'\nUse --transparent to give the tooltip window a transparent background", invocation];
}

- (void)handleCommand:(CLIProxy*)proxy
{
	NSDictionary* args = [proxy parameters];

	NSString* html = nil;
	if(NSMutableString* text = [[args objectForKey:@"text"] mutableCopy])
	{
		[text replaceOccurrencesOfString:@"&" withString:@"&amp;" options:0 range:NSMakeRange(0, [text length])];
		[text replaceOccurrencesOfString:@"<" withString:@"&lt;" options:0 range:NSMakeRange(0, [text length])];
		[text insertString:@"<pre>" atIndex:0];
		[text appendString:@"</pre>"];
		html = text;
	}
	else
	{
		html = [args objectForKey:@"html"];
	}

	NSPoint pos = [NSEvent mouseLocation];
	if(id textView = [NSApp targetForAction:@selector(positionForWindowUnderCaret)])
		pos = [textView positionForWindowUnderCaret];

	[TMDHTMLTip showWithContent:html atLocation:pos transparent:([args objectForKey:@"transparent"] ? YES : NO)];
}
@end
