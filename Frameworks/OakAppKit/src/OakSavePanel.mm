#import "NSSavePanel Additions.h"
#import "OakSavePanel.h"

@implementation OakSavePanel
- (id)initWithPath:(NSString*)aPathSuggestion directory:(NSString*)aDirectorySuggestion fowWindow:(NSWindow*)aWindow delegate:(id)aDelegate contextInfo:(void*)info
{
	if((self = [super init]))
	{
		delegate    = aDelegate;
		contextInfo = info;

		[[aWindow attachedSheet] orderOut:self]; // incase there already is a sheet showing (like “Do you want to save?”)

		NSSavePanel* savePanel = [NSSavePanel savePanel];
		[savePanel setTreatsFilePackagesAsDirectories:YES];
		[savePanel setDirectoryURL:[NSURL fileURLWithPath:aDirectorySuggestion]];
		[savePanel setNameFieldStringValue:[aPathSuggestion lastPathComponent]];
		[savePanel beginSheetModalForWindow:aWindow completionHandler:^(NSInteger result) {
			NSString* path = result == NSOKButton ? [[savePanel.URL filePathURL] path] : nil;
			[delegate savePanelDidEnd:self path:path contextInfo:contextInfo];
			[self release];
		}];
		[savePanel deselectExtension];
	}
	return self;
}

+ (void)showWithPath:(NSString*)aPathSuggestion directory:(NSString*)aDirectorySuggestion fowWindow:(NSWindow*)aWindow delegate:(id)aDelegate contextInfo:(void*)info
{
	[[OakSavePanel alloc] initWithPath:aPathSuggestion directory:aDirectorySuggestion fowWindow:aWindow delegate:aDelegate contextInfo:info];
}

@end
