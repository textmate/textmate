#import "OFBActionsView.h"
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/OakUIConstructionFunctions.h>
#import <OakAppKit/NSImage Additions.h>

static NSButton* OakCreateImageButton (NSImage* image)
{
	NSButton* res = [NSButton new];

	[[res cell] setBackgroundStyle:NSBackgroundStyleRaised];
	[res setButtonType:NSMomentaryChangeButton];
	[res setBezelStyle:NSRecessedBezelStyle];
	[res setBordered:NO];

	image = [image copy];
	[image setTemplate:YES];
	[res setImage:image];
	[res setImagePosition:NSImageOnly];

	[res setContentHuggingPriority:NSLayoutPriorityDefaultLow forOrientation:NSLayoutConstraintOrientationHorizontal];
	[res setContentHuggingPriority:NSLayoutPriorityDefaultLow forOrientation:NSLayoutConstraintOrientationVertical];

	return res;
}

static NSPopUpButton* OakCreateActionPopUpButton ()
{
	NSPopUpButton* res = [NSPopUpButton new];
	res.bordered  = NO;
	res.pullsDown = YES;
	[[res cell] setBackgroundStyle:NSBackgroundStyleRaised];

	NSMenuItem* item = [NSMenuItem new];
	item.title = @"";
	item.image = [NSImage imageNamed:NSImageNameActionTemplate];
	[item.image setSize:NSMakeSize(14, 14)];

	[[res cell] setUsesItemFromMenu:NO];
	[[res cell] setMenuItem:item];

	return res;
}

@implementation OFBActionsView
- (id)initWithFrame:(NSRect)aRect
{
	if(self = [super initWithGradient:[[NSGradient alloc] initWithColorsAndLocations: [NSColor colorWithCalibratedWhite:1.000 alpha:0.68], 0.0, [NSColor colorWithCalibratedWhite:1.000 alpha:0.5], 0.0416, [NSColor colorWithCalibratedWhite:1.000 alpha:0.0], 1.0, nil] inactiveGradient:[[NSGradient alloc] initWithColorsAndLocations: [NSColor colorWithCalibratedWhite:1.000 alpha:0.68], 0.0, [NSColor colorWithCalibratedWhite:1.000 alpha:0.5], 0.0416, [NSColor colorWithCalibratedWhite:1.000 alpha:0.0], 1.0, nil]])
	{
		self.createButton       = OakCreateImageButton([NSImage imageNamed:NSImageNameAddTemplate]);
		self.actionsPopUpButton = OakCreateActionPopUpButton();
		self.reloadButton       = OakCreateImageButton([NSImage imageNamed:NSImageNameRefreshTemplate]);
		self.searchButton       = OakCreateImageButton([NSImage imageNamed:@"Search" inSameBundleAsClass:[self class]]);
		self.favoritesButton    = OakCreateImageButton([NSImage imageNamed:@"Favorites" inSameBundleAsClass:[self class]]);
		self.scmButton          = OakCreateImageButton([NSImage imageNamed:@"SCM" inSameBundleAsClass:[self class]]);
		
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

		[self.actionsPopUpButton.cell accessibilitySetOverrideValue:@"Actions" forAttribute:NSAccessibilityDescriptionAttribute];

		NSMenu* menu = [NSMenu new];
		[menu addItemWithTitle:@"Unused" action:@selector(nop:) keyEquivalent:@""];
		[menu addItemWithTitle:@"Create Folder" action:@selector(nop:) keyEquivalent:@""];
		self.actionsPopUpButton.menu = menu;

		NSView* wrappedActionsPopUpButton = [NSView new];
		[wrappedActionsPopUpButton addSubview:self.actionsPopUpButton];
		[self.actionsPopUpButton setTranslatesAutoresizingMaskIntoConstraints:NO];
		[wrappedActionsPopUpButton addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:[popup]|" options:0 metrics:nil views:@{ @"popup" : self.actionsPopUpButton }]];
		[wrappedActionsPopUpButton addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[popup]|" options:0 metrics:nil views:@{ @"popup" : self.actionsPopUpButton }]];

		NSDictionary* views = @{
			@"create"    : self.createButton,
			@"divider"   : OakCreateDividerImageView(),
			@"actions"   : wrappedActionsPopUpButton,
			@"reload"    : self.reloadButton,
			@"search"    : self.searchButton,
			@"favorites" : self.favoritesButton,
			@"scm"       : self.scmButton,
		};

		for(NSView* view in [views allValues])
		{
			[view setTranslatesAutoresizingMaskIntoConstraints:NO];
			[self addSubview:view];
		}

		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-3-[create(==24)]-1-[divider]-5-[actions(==30)]-(>=8)-[reload(==22,==search,==favorites,==scm)][search]-1-[favorites]-2-[scm]-(12)-|" options:0 metrics:nil views:views]];
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
