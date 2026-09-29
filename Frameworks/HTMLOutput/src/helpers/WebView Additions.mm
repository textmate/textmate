#import <OakAppKit/src/OakPasteboard.h>
#import <OakFoundation/src/OakFoundation.h>
#import <OakFoundation/src/OakFindProtocol.h>
#import <OakFoundation/src/NSString Additions.h>
#import <document/src/OakDocument.h>
#import <document/src/OakDocumentController.h>
#import <ns/src/ns.h>

@interface WebView (OakFindNextPrevious)
- (void)performFindOperation:(id <OakFindServerProtocol>)aFindServer;

- (IBAction)findNext:(id)sender;
- (IBAction)findPrevious:(id)sender;

- (IBAction)copySelectionToFindPboard:(id)sender;
- (IBAction)copySelectionToReplacePboard:(id)sender;
@end

@implementation WebView (OakFindNextPrevious)
- (NSString*)selection
{
	DOMDocumentFragment* selection = [[self selectedDOMRange] cloneContents];
	DOMNodeIterator* iter = selection ? [[[self selectedFrame] DOMDocument] createNodeIterator:selection whatToShow:DOM_SHOW_TEXT filter:nil expandEntityReferences:YES] : nil;

	NSMutableString* str = [NSMutableString string];
	while(DOMNode* node = [iter nextNode])
		[str appendString:[node nodeValue]];

	return OakIsEmptyString(str) ? nil : str;
}

- (IBAction)copySelectionToFindPboard:(id)sender
{
	if(NSString* str = [self selection])
			[OakPasteboard.findPasteboard addEntryWithString:str];
	else	NSBeep();
}

- (IBAction)copySelectionToReplacePboard:(id)sender
{
	if(NSString* str = [self selection])
			[OakPasteboard.replacePasteboard addEntryWithString:str];
	else	NSBeep();
}

- (void)performFindOperation:(id <OakFindServerProtocol>)aFindServer
{
	switch(aFindServer.findOperation)
	{
		case kFindOperationFind:
		case kFindOperationFindInSelection:
		{
			BOOL backwards  = aFindServer.findOptions & find::backwards;
			BOOL ignoreCase = aFindServer.findOptions & find::ignore_case;
			BOOL wrapAround = aFindServer.findOptions & find::wrap_around;

			if([self searchFor:aFindServer.findString direction:!backwards caseSensitive:!ignoreCase wrap:wrapAround])
					[aFindServer didFind:1 occurrencesOf:[self selection] atPosition:text::pos_t::undefined wrapped:NO];
			else	[aFindServer didFind:0 occurrencesOf:aFindServer.findString atPosition:text::pos_t::undefined wrapped:NO];
		}
		break;
	}
}

- (IBAction)findNext:(id)sender
{
	OakPasteboardEntry* entry = [OakPasteboard.findPasteboard current];
	if(OakNotEmptyString(entry.string))
		[self searchFor:entry.string direction:YES caseSensitive:![NSUserDefaults.standardUserDefaults boolForKey:kUserDefaultsFindIgnoreCase] wrap:[NSUserDefaults.standardUserDefaults boolForKey:kUserDefaultsFindWrapAround]];
}

- (IBAction)findPrevious:(id)sender
{
	OakPasteboardEntry* entry = [OakPasteboard.findPasteboard current];
	if(OakNotEmptyString(entry.string))
		[self searchFor:entry.string direction:NO caseSensitive:![NSUserDefaults.standardUserDefaults boolForKey:kUserDefaultsFindIgnoreCase] wrap:[NSUserDefaults.standardUserDefaults boolForKey:kUserDefaultsFindWrapAround]];
}

- (void)viewSource:(id)sender
{
	WebDataSource* dataSource = [[self mainFrame] dataSource];

	NSString* encoding = [[dataSource textEncodingName] lowercaseString];
	if(OakIsEmptyString(encoding))
		encoding = @"utf-8";

	NSString* str;
	if([encoding isEqualToString:@"utf-8"])
	{
		str = [[NSString alloc] initWithData:[dataSource data] encoding:NSUTF8StringEncoding];
	}
	else if([encoding isEqualToString:@"utf-16"] || [encoding isEqualToString:@"utf16"])
	{
		str = [[NSString alloc] initWithData:[dataSource data] encoding:NSUnicodeStringEncoding];
	}
	else if([encoding isEqualToString:@"macintosh"])
	{
		str = [[NSString alloc] initWithData:[dataSource data] encoding:NSMacOSRomanStringEncoding];
	}
	else
	{
		NSAlert* alert        = [[NSAlert alloc] init];
		alert.messageText     = @"Unknown Encoding";
		alert.informativeText = [NSString stringWithFormat:@"The encoding used for this HTML buffer (“%@”) is unsupported.\nPlease file a bug report stating the encoding name and how you got to it.", [dataSource textEncodingName]];
		[alert addButtonWithTitle:@"Continue"];
		[alert runModal];
		return;
	}

	NSString* name = OakNotEmptyString(self.mainFrameTitle) ? self.mainFrameTitle : nil;
	OakDocument* doc = [OakDocument documentWithString:str fileType:@"text.html.basic" customName:name];
	[OakDocumentController.sharedInstance showDocument:doc inProject:nil bringToFront:YES];
}
@end
