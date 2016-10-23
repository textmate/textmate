#import "NSSavePanel Additions.h"
#import <OakAppKit/OakUIConstructionFunctions.h>

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
			NSButton* checkbox = OakCreateCheckBox(@"Show Hidden Files");
			[checkbox sizeToFit];
			[checkbox bind:NSValueBinding toObject:[NSUserDefaultsController sharedUserDefaultsController] withKeyPath:@"values.NSOpenPanelShowHiddenFiles" options:nil];

			[self setAccessoryView:checkbox];
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
