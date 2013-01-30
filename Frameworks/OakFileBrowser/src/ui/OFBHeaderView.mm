#import "OFBHeaderView.h"
#import <OakAppKit/OakAppKit.h>

static NSButton* OakCreateImageButton (NSString* imageName)
{
	NSButton* res = [NSButton new];

	[[res cell] setBackgroundStyle:NSBackgroundStyleRaised];
	[res setButtonType:NSMomentaryChangeButton];
	[res setBezelStyle:NSRecessedBezelStyle];
	[res setBordered:NO];

	NSImage* image = [[NSImage imageNamed:imageName] copy];
	[res setImage:image];
	[res setImagePosition:NSImageOnly];

	return res;
}

static NSPopUpButton* OakCreatePopUpButton ()
{
	NSPopUpButton* res = [NSPopUpButton new];
	[[res cell] setBackgroundStyle:NSBackgroundStyleLight];
	[res setContentCompressionResistancePriority:NSLayoutPriorityDefaultLow forOrientation:NSLayoutConstraintOrientationHorizontal];
	[res setBordered:NO];
	return res;
}

@implementation OFBHeaderView
- (id)initWithFrame:(NSRect)aRect
{
	if(self = [super initWithGradient:[[NSGradient alloc] initWithStartingColor:[NSColor colorWithCalibratedWhite:0.915 alpha:1] endingColor:[NSColor colorWithCalibratedWhite:0.760 alpha:1]] inactiveGradient:[[NSGradient alloc] initWithStartingColor:[NSColor colorWithCalibratedWhite:0.915 alpha:1] endingColor:[NSColor colorWithCalibratedWhite:0.915 alpha:1]]])
	{
		self.folderPopUpButton       = OakCreatePopUpButton();
		self.goBackButton            = OakCreateImageButton(NSImageNameGoLeftTemplate);
		self.goBackButton.toolTip    = @"Go Back";
		self.goForwardButton         = OakCreateImageButton(NSImageNameGoRightTemplate);
		self.goForwardButton.toolTip = @"Go Forward";

		NSDictionary* views = @{
			@"folder"   : self.folderPopUpButton,
			@"divider"  : OakCreateVerticalLine([NSColor colorWithCalibratedWhite:0.551 alpha:1], [NSColor colorWithCalibratedWhite:0.801 alpha:1]),
			@"shading"  : OakCreateVerticalLine([NSColor colorWithCalibratedWhite:0.869 alpha:1], [NSColor colorWithCalibratedWhite:0.869 alpha:0]),
			@"back"     : self.goBackButton,
			@"forward"  : self.goForwardButton,
		};

		for(NSView* view in [views allValues])
		{
			[view setTranslatesAutoresizingMaskIntoConstraints:NO];
			[self addSubview:view];
		}

		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(-3)-[folder(>=75)]-(3)-[divider][shading]-(2)-[back(==22)]-(2)-[forward(==back)]-(3)-|" options:0 metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[folder(==divider,==shading,==back,==forward)]|"                                          options:0 metrics:nil views:views]];
	}
	return self;
}

- (NSSize)intrinsicContentSize
{
	return NSMakeSize(NSViewNoInstrinsicMetric, 24);
}
@end
