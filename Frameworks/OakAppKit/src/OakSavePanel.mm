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
		[savePanel beginSheetForDirectory:aDirectorySuggestion file:aPathSuggestion modalForWindow:aWindow modalDelegate:self didEndSelector:@selector(savePanelDidEnd:returnCode:contextInfo:) contextInfo:NULL];
		[savePanel deselectExtension];
	}
	return self;
}

+ (void)showWithPath:(NSString*)aPathSuggestion directory:(NSString*)aDirectorySuggestion fowWindow:(NSWindow*)aWindow delegate:(id)aDelegate contextInfo:(void*)info
{
	[[OakSavePanel alloc] initWithPath:aPathSuggestion directory:aDirectorySuggestion fowWindow:aWindow delegate:aDelegate contextInfo:info];
}

- (void)savePanelDidEnd:(NSSavePanel*)sheet returnCode:(NSInteger)returnCode contextInfo:(void*)info
{
	NSString* path = returnCode == NSOKButton ? sheet.filename : nil;
	[delegate savePanelDidEnd:self path:path contextInfo:contextInfo];
	[self release];
}
@end
