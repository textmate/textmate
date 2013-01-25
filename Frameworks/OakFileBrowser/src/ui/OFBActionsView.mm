#import "OFBActionsView.h"
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/NSImage Additions.h>

static NSButton* OakCreateImageButton (NSImage* image, NSSize imageSize = NSMakeSize(16, 16))
{
	NSButton* res = [NSButton new];

	// [[res cell] setBackgroundStyle:NSBackgroundStyleRaised];
	[res setButtonType:NSMomentaryChangeButton];
	[res setBezelStyle:NSSmallSquareBezelStyle];
	[res setBordered:NO];

	// image = [image copy];
	// [image setSize:imageSize];
	[res setImage:image];
	[res setImagePosition:NSImageOnly];

	[res setContentHuggingPriority:NSLayoutPriorityDefaultLow forOrientation:NSLayoutConstraintOrientationHorizontal];
	[res setContentHuggingPriority:NSLayoutPriorityDefaultLow forOrientation:NSLayoutConstraintOrientationVertical];

	return res;
}

static NSPopUpButton* OakCreatePopUpButton ()
{
	NSPopUpButton* res = [NSPopUpButton new];
	res.bordered  = NO;
	res.pullsDown = YES;

	NSMenuItem* item = [NSMenuItem new];
	item.title = @"";
	item.image = [NSImage imageNamed:NSImageNameActionTemplate];
	[item.image setSize:NSMakeSize(12, 12)];

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
		self.actionsPopUpButton = OakCreatePopUpButton();
		self.reloadButton       = OakCreateImageButton([NSImage imageNamed:NSImageNameRefreshTemplate]);
		self.searchButton       = OakCreateImageButton([NSImage imageNamed:NSImageNameRevealFreestandingTemplate]);
		self.favoritesButton    = OakCreateImageButton([NSImage imageNamed:@"Favorites" inSameBundleAsClass:[self class]]);
		self.scmButton          = OakCreateImageButton([NSImage imageNamed:@"SmartFolder" inSameBundleAsClass:[self class]]);

		self.createButton.toolTip    = @"Create new file";
		self.reloadButton.toolTip    = @"Reload ";
		self.searchButton.toolTip    = @"";
		self.favoritesButton.toolTip = @"Show favorites";
		self.scmButton.toolTip       = @"Show source control management status";

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
			@"leftDivider" : OakCreateVerticalLine([NSColor colorWithCalibratedWhite:0.551 alpha:1], [NSColor colorWithCalibratedWhite:0.801 alpha:1]),
			@"leftShading" : OakCreateVerticalLine([NSColor colorWithCalibratedWhite:0.869 alpha:1], [NSColor colorWithCalibratedWhite:0.869 alpha:0]),
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

		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(24)-[create(==22)][leftDivider][leftShading][actions(==30)]-(>=8)-[reload(==22,==search,==favorites,==scm)][search][favorites][scm]-(24)-|" options:0 metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[create(==leftDivider,==leftShading,==actions,==reload,==search,==favorites,==scm)]|"                                                         options:0 metrics:nil views:views]];
	}
	return self;
}

- (NSSize)intrinsicContentSize
{
	return NSMakeSize(NSViewNoInstrinsicMetric, 24);
}

- (void)drawRect:(NSRect)aRect
{
	[[NSColor windowBackgroundColor] set]; NSRectFill(aRect);
	
	[super drawRect:aRect];
}
@end
