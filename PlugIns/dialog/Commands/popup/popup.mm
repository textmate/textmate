#import "../../TMDCommand.h"
#import "../../Dialog2.h"
#import "TMDIncrementalPopUpMenu.h"
#import "../Utilities/TextMate.h" // -positionForWindowUnderCaret

/*
"$DIALOG" popup --suggestions '( { display = "**law**"; image = NSRefreshTemplate; match = "law"; }, { display = "**laws**"; match = "laws"; insert = "(${1:hello}, ${2:again})"; children=({display="lawsless";}); } )'
*/

// ==================
// = Extended Popup =
// ==================
@interface TMDXPopUp : TMDCommand
@end

@implementation TMDXPopUp
+ (void)load
{
	[TMDCommand registerObject:[self new] forCommand:@"popup"];
}

- (void)handleCommand:(CLIProxy*)proxy
{
	NSDictionary* args = [proxy parameters];

	NSString* filter     = [args objectForKey:@"alreadyTyped"];
	NSString* prefix     = [args objectForKey:@"staticPrefix"];
	NSString* allow      = [args objectForKey:@"additionalWordCharacters"];
	BOOL wait            = [args objectForKey:@"returnChoice"] ? YES : NO;
	BOOL caseInsensitive = [args objectForKey:@"caseInsensitive"] ? YES : NO;

	NSArray* suggestions = [args objectForKey:@"suggestions"];
	if([suggestions isKindOfClass:[NSString class]])
		suggestions = [NSPropertyListSerialization propertyListWithData:[(NSString*)suggestions dataUsingEncoding:NSUTF8StringEncoding] options:NSPropertyListImmutable format:NULL error:NULL];

	TMDIncrementalPopUpMenu* xPopUp = [[TMDIncrementalPopUpMenu alloc] initWithItems:suggestions alreadyTyped:filter staticPrefix:prefix additionalWordCharacters:allow caseSensitive:!caseInsensitive writeChoiceToFileDescriptor:(wait ? [proxy outputHandle] : nil)];

	NSPoint pos = [NSEvent mouseLocation];
	if(id textView = [NSApp targetForAction:@selector(positionForWindowUnderCaret)])
		pos = [textView positionForWindowUnderCaret];

	if(filter)
	{
		NSFont* font = [NSFont fontWithName:[NSUserDefaults.standardUserDefaults stringForKey:@"OakTextViewNormalFontName"] ?: [[NSFont userFixedPitchFontOfSize:12.0] fontName] size:[NSUserDefaults.standardUserDefaults integerForKey:@"OakTextViewNormalFontSize"] ?: 12];
		pos.x -= [filter sizeWithAttributes:[NSDictionary dictionaryWithObject:font forKey:NSFontAttributeName]].width;
	}

	[xPopUp setCaretPos:pos];
	[xPopUp orderFront:self];
}

- (NSString*)commandDescription
{
	return @"Presents the user with a list of items which can be filtered down by typing to select the item they want.";
}

- (NSString*)usageForInvocation:(NSString*)invocation;
{
	return [NSString stringWithFormat:@"\t%1$@ --suggestions '( { display = law; }, { display = laws; insert = \"(${1:hello}, ${2:again})\"; } )'\n", invocation];
}

@end
