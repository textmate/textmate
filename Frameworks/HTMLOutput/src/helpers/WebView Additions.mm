#import <OakAppKit/OakPasteboard.h>
#import <OakFoundation/OakFoundation.h>
#import <OakFoundation/OakFindProtocol.h>
#import <OakFoundation/NSString Additions.h>
#import <document/collection.h>
#import <ns/ns.h>

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

	return NSIsEmptyString(str) ? nil : str;
}

- (IBAction)copySelectionToFindPboard:(id)sender
{
	if(NSString* str = [self selection])
			[[OakPasteboard pasteboardWithName:NSFindPboard] addEntry:[OakPasteboardEntry pasteboardEntryWithString:str andOptions:nil]];
	else	NSBeep();
}

- (IBAction)copySelectionToReplacePboard:(id)sender
{
	if(NSString* str = [self selection])
			[[OakPasteboard pasteboardWithName:NSReplacePboard] addEntry:[OakPasteboardEntry pasteboardEntryWithString:str]];
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
	OakPasteboardEntry* entry = [[OakPasteboard pasteboardWithName:NSFindPboard] current];
	if(NSNotEmptyString(entry.string))
		[self searchFor:entry.string direction:YES caseSensitive:![[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsFindIgnoreCase] wrap:[[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsFindWrapAround]];
}

- (IBAction)findPrevious:(id)sender
{
	OakPasteboardEntry* entry = [[OakPasteboard pasteboardWithName:NSFindPboard] current];
	if(NSNotEmptyString(entry.string))
		[self searchFor:entry.string direction:NO caseSensitive:![[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsFindIgnoreCase] wrap:[[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsFindWrapAround]];
}

- (void)viewSource:(id)sender
{
	WebDataSource* dataSource = [[self mainFrame] dataSource];

	NSString* encoding = [[dataSource textEncodingName] lowercaseString];
	if(NSIsEmptyString(encoding))
		encoding = @"utf-8";

	std::string str;
	if([encoding isEqualToString:@"utf-8"])
		str = to_s((NSString*)[[NSString alloc] initWithData:[dataSource data] encoding:NSUTF8StringEncoding]);
	else if([encoding isEqualToString:@"utf-16"] || [encoding isEqualToString:@"utf16"])
		str = to_s((NSString*)[[NSString alloc] initWithData:[dataSource data] encoding:NSUnicodeStringEncoding]);
	else if([encoding isEqualToString:@"macintosh"])
		str = to_s((NSString*)[[NSString alloc] initWithData:[dataSource data] encoding:NSMacOSRomanStringEncoding]);
	else
		return (void)NSRunAlertPanel(@"Unknown Encoding", @"The encoding used for this HTML buffer (“%@”) is unsupported.\nPlease file a bug report stating the encoding name and how you got to it.", @"Continue", nil, nil, [dataSource textEncodingName]);

	document::show(document::from_content(str.find("<!--" + std::string(1017, ' ') + "-->") == 0 ? str.substr(1024) : str, "text.html.basic"));
}
@end
