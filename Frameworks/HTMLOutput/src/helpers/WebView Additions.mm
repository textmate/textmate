#import <OakAppKit/OakPasteboard.h>
#import <OakFoundation/OakFoundation.h>
#import <OakFoundation/OakFindProtocol.h>
#import <OakFoundation/NSString Additions.h>
#import <document/OakDocument.h>
#import <document/OakDocumentController.h>
#import <ns/ns.h>

@interface WKWebView (OakFindNextPrevious)
- (void)performFindOperation:(id <OakFindServerProtocol>)aFindServer;

- (IBAction)findNext:(id)sender;
- (IBAction)findPrevious:(id)sender;

- (IBAction)copySelectionToFindPboard:(id)sender;
- (IBAction)copySelectionToReplacePboard:(id)sender;
@end

@implementation WKWebView (OakFindNextPrevious)

- (void)selection:(void(^)(NSString*))callback
{
	[self evaluateJavaScript:@"window.getSelection().toString()" completionHandler:^(id result, NSError* error){
		callback([result isKindOfClass:[NSString class]] && [(NSString*)result length] > 0 ? result : nil);
	}];
}

- (IBAction)copySelectionToFindPboard:(id)sender
{
	[self selection:^(NSString* str) {
		if(str)
			[OakPasteboard.findPasteboard addEntryWithString:str];
		else
			NSBeep();
	}];
}

- (IBAction)copySelectionToReplacePboard:(id)sender
{
	[self selection:^(NSString* str) {
		if(str)
			[OakPasteboard.replacePasteboard addEntryWithString:str];
		else
			NSBeep();
	}];
}

- (void)performFindOperation:(id <OakFindServerProtocol>)aFindServer
{
	switch(aFindServer.findOperation)
	{
		case kFindOperationFind:
		case kFindOperationFindInSelection:
		{
			WKFindConfiguration* config = [WKFindConfiguration new];
			config.backwards = aFindServer.findOptions & find::backwards;
			config.caseSensitive = !(aFindServer.findOptions & find::ignore_case);
			config.wraps = aFindServer.findOptions & find::wrap_around;

			[self findString:aFindServer.findString withConfiguration:config completionHandler:^(WKFindResult* result){
				if(result.matchFound)
					[aFindServer didFind:1 occurrencesOf:aFindServer.findString atPosition:text::pos_t::undefined wrapped:NO];
				else
					[aFindServer didFind:0 occurrencesOf:aFindServer.findString atPosition:text::pos_t::undefined wrapped:NO];
			}];
		}
		break;
	}
}

- (IBAction)findNext:(id)sender
{
	OakPasteboardEntry* entry = [OakPasteboard.findPasteboard current];
	if(OakNotEmptyString(entry.string))
	{
		WKFindConfiguration* config = [WKFindConfiguration new];
		config.backwards = NO;
		config.caseSensitive = ![NSUserDefaults.standardUserDefaults boolForKey:kUserDefaultsFindIgnoreCase];
		config.wraps = [NSUserDefaults.standardUserDefaults boolForKey:kUserDefaultsFindWrapAround];
		[self findString:entry.string withConfiguration:config completionHandler:^(WKFindResult* result){ }];
	}
}

- (IBAction)findPrevious:(id)sender
{
	OakPasteboardEntry* entry = [OakPasteboard.findPasteboard current];
	if(OakNotEmptyString(entry.string))
	{
		WKFindConfiguration* config = [WKFindConfiguration new];
		config.backwards = YES;
		config.caseSensitive = ![NSUserDefaults.standardUserDefaults boolForKey:kUserDefaultsFindIgnoreCase];
		config.wraps = [NSUserDefaults.standardUserDefaults boolForKey:kUserDefaultsFindWrapAround];
		[self findString:entry.string withConfiguration:config completionHandler:^(WKFindResult* result){ }];
	}
}

- (void)viewSource:(id)sender
{
	[self evaluateJavaScript:@"document.documentElement.outerHTML" completionHandler:^(id result, NSError* error){
		if([result isKindOfClass:[NSString class]])
		{
			NSString* name = self.title.length > 0 ? self.title : nil;
			OakDocument* doc = [OakDocument documentWithString:result fileType:@"text.html.basic" customName:name];
			[OakDocumentController.sharedInstance showDocument:doc inProject:nil bringToFront:YES];
		}
	}];
}
@end
