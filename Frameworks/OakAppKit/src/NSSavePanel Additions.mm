#import "NSSavePanel Additions.h"
#import <OakFoundation/NSString Additions.h>
#import <OakFoundation/OakFoundation.h>

@implementation NSSavePanel (DeselectExtension)
- (void)deselectExtension
{
	NSTextView* tw = (NSTextView*)self.firstResponder;
	if([tw isKindOfClass:[NSTextView class]])
	{
		NSString* str = [tw.textStorage.string stringByDeletingPathExtensions];
		[tw setSelectedRange:NSMakeRange(0, str.length)];
	}
}
@end

@implementation NSSavePanel (HiddenFiles)
+ (void)initialize
{
	[[NSUserDefaults standardUserDefaults] registerDefaults:@{ @"NSOpenPanelShowHiddenFiles" : NO_obj }];
}

- (void)setShowsHiddenFilesCheckBox:(BOOL)flag
{
	if(flag)
	{
		if([self respondsToSelector:@selector(showsHiddenFiles)] && [self respondsToSelector:@selector(setShowsHiddenFiles:)])
		{
			NSViewController* viewController = [[[NSViewController alloc] initWithNibName:@"HiddenFilesAccessoryView" bundle:[NSBundle bundleWithIdentifier:@"com.macromates.TextMate.OakAppKit"]] autorelease];
			[self setAccessoryView:viewController.view];
			[self bind:@"showsHiddenFiles" toObject:[NSUserDefaultsController sharedUserDefaultsController] withKeyPath:@"values.NSOpenPanelShowHiddenFiles" options:nil];
		}
	}
	else
	{
		[self unbind:@"showsHiddenFiles"];
		[self setAccessoryView:nil];
	}
}
@end
