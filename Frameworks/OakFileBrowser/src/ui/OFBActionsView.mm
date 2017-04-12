#import "OFBActionsView.h"
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/OakUIConstructionFunctions.h>
#import <OakAppKit/NSImage Additions.h>

static NSButton* OakCreateImageButton (NSImage* image)
{
	NSButton* res = [NSButton new];
	[[res cell] setBackgroundStyle:NSBackgroundStyleRaised];
	[res setButtonType:NSMomentaryChangeButton];
	[res setBordered:NO];
	[res setImage:image];
	[res setImagePosition:NSImageOnly];
	[res setContentHuggingPriority:NSLayoutPriorityDefaultLow forOrientation:NSLayoutConstraintOrientationHorizontal];
	[res setContentHuggingPriority:NSLayoutPriorityDefaultLow forOrientation:NSLayoutConstraintOrientationVertical];
	return res;
}

@implementation OFBActionsView
- (id)initWithFrame:(NSRect)aRect
{
	if(self = [super initWithFrame:aRect])
	{
		[self setupStatusBarBackground];

		self.createButton       = OakCreateImageButton([NSImage imageNamed:NSImageNameAddTemplate]);
		self.actionsPopUpButton = OakCreateActionPopUpButton();
		self.reloadButton       = OakCreateImageButton([NSImage imageNamed:NSImageNameRefreshTemplate]);
		self.searchButton       = OakCreateImageButton([NSImage imageNamed:@"SearchTemplate" inSameBundleAsClass:[self class]]);
		self.favoritesButton    = OakCreateImageButton([NSImage imageNamed:@"FavoritesTemplate" inSameBundleAsClass:[self class]]);
		self.scmButton          = OakCreateImageButton([NSImage imageNamed:@"SCMTemplate" inSameBundleAsClass:[self class]]);

		self.createButton.toolTip       = @"Create new file";
		self.reloadButton.toolTip       = @"Reload file browser";
		self.searchButton.toolTip       = @"Search current folder";
		self.favoritesButton.toolTip    = @"Show favorites";
		self.scmButton.toolTip          = @"Show source control management status";

		[self.createButton.cell    accessibilitySetOverrideValue:self.createButton.toolTip    forAttribute:NSAccessibilityDescriptionAttribute];
		[self.reloadButton.cell    accessibilitySetOverrideValue:self.reloadButton.toolTip    forAttribute:NSAccessibilityDescriptionAttribute];
		[self.searchButton.cell    accessibilitySetOverrideValue:self.searchButton.toolTip    forAttribute:NSAccessibilityDescriptionAttribute];
		[self.favoritesButton.cell accessibilitySetOverrideValue:self.favoritesButton.toolTip forAttribute:NSAccessibilityDescriptionAttribute];
		[self.scmButton.cell       accessibilitySetOverrideValue:self.scmButton.toolTip       forAttribute:NSAccessibilityDescriptionAttribute];

		NSView* wrappedActionsPopUpButton = [NSView new];
		OakAddAutoLayoutViewsToSuperview(@[ self.actionsPopUpButton ], wrappedActionsPopUpButton);
		[wrappedActionsPopUpButton addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:[popup]|" options:0 metrics:nil views:@{ @"popup" : self.actionsPopUpButton }]];
		[wrappedActionsPopUpButton addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[popup]|" options:0 metrics:nil views:@{ @"popup" : self.actionsPopUpButton }]];

		NSView* divider = OakCreateDividerImageView();

		NSDictionary* views = @{
			@"create"    : self.createButton,
			@"divider"   : divider,
			@"actions"   : wrappedActionsPopUpButton,
			@"reload"    : self.reloadButton,
			@"search"    : self.searchButton,
			@"favorites" : self.favoritesButton,
			@"scm"       : self.scmButton,
		};

		OakAddAutoLayoutViewsToSuperview([views allValues], self);
		OakSetupKeyViewLoop(@[ self, _createButton, _actionsPopUpButton, _reloadButton, _searchButton, _favoritesButton, _scmButton ], NO);

		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-3-[create(==24)]-1-[divider]-5-[actions(==30)]-(>=8)-[reload(==22,==search,==favorites,==scm)][search]-1-[favorites]-2-[scm]-(12)-|" options:NSLayoutFormatAlignAllCenterY metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[create(==divider,==actions,==reload,==search,==favorites,==scm)]|"                                                                   options:0 metrics:nil views:views]];
	}
	return self;
}

- (NSSize)intrinsicContentSize
{
	return NSMakeSize(NSViewNoInstrinsicMetric, 24);
}

- (void)drawRect:(NSRect)aRect
{
	if([self.window contentBorderThicknessForEdge:NSMinYEdge] < NSMaxY(self.frame))
	{
		[[NSColor windowBackgroundColor] set];
		NSRectFill(aRect);
		[super drawRect:aRect];
	}
}
@end
