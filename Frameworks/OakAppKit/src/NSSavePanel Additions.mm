#import "NSSavePanel Additions.h"
#import <OakFoundation/NSString Additions.h>

@implementation NSSavePanel (HiddenFiles)
+ (void)initialize
{
	[[NSUserDefaults standardUserDefaults] registerDefaults:@{ @"NSOpenPanelShowHiddenFiles" : @NO }];
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
