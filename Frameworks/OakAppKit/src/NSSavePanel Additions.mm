#import "NSSavePanel Additions.h"
#import <OakAppKit/OakUIConstructionFunctions.h>

@implementation NSSavePanel (HiddenFiles)
+ (void)initialize
{
	[[NSUserDefaults standardUserDefaults] registerDefaults:@{ @"NSOpenPanelShowHiddenFiles": @NO }];
}

- (void)setShowsHiddenFilesCheckBox:(BOOL)flag
{
	if(flag)
	{
		if([self respondsToSelector:@selector(showsHiddenFiles)] && [self respondsToSelector:@selector(setShowsHiddenFiles:)])
		{
			NSButton* checkbox = OakCreateCheckBox(@"Show Hidden Files");
			[checkbox bind:NSValueBinding toObject:[NSUserDefaultsController sharedUserDefaultsController] withKeyPath:@"values.NSOpenPanelShowHiddenFiles" options:nil];

			NSDictionary* views = @{
				@"checkbox": checkbox,
			};

			NSView* contentView = [[NSView alloc] initWithFrame:NSZeroRect];
			OakAddAutoLayoutViewsToSuperview(views.allValues, contentView);

			[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[checkbox]-|" options:0 metrics:nil views:views]];
			[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-[checkbox]-|" options:0 metrics:nil views:views]];

			contentView.autoresizingMask = NSViewWidthSizable|NSViewHeightSizable;
			[self setAccessoryView:contentView];

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
