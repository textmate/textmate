#import "LiveSearchView.h"
#import <OakAppKit/OakUIConstructionFunctions.h>

@interface LiveSearchView ()
@property (nonatomic) NSView* divider;
@end

@implementation LiveSearchView
+ (void)initialize
{
	[[NSUserDefaults standardUserDefaults] registerDefaults:@{
		@"incrementalSearchIgnoreCase" : @YES,
		@"incrementalSearchWrapAround" : @NO,
	}];
}

- (id)initWithFrame:(NSRect)aRect
{
	if(self = [super initWithFrame:aRect])
	{
		self.divider = OakCreateHorizontalLine([NSColor colorWithCalibratedWhite:0.500 alpha:1], [NSColor colorWithCalibratedWhite:0.750 alpha:1]);

		self.textField = [[NSTextField alloc] initWithFrame:NSZeroRect];
		self.textField.focusRingType = NSFocusRingTypeNone;

		self.ignoreCaseCheckBox = OakCreateCheckBox(@"Ignore Case");
		self.wrapAroundCheckBox = OakCreateCheckBox(@"Wrap Around");

		NSDictionary* views = @{
			@"divider"    : self.divider,
			@"textField"  : self.textField,
			@"ignoreCase" : self.ignoreCaseCheckBox,
			@"wrapAround" : self.wrapAroundCheckBox,
		};

		OakAddAutoLayoutViewsToSuperview([views allValues], self);

		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[divider]|" options:0 metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(8)-[textField]-[ignoreCase]-[wrapAround]-(8)-|" options:NSLayoutFormatAlignAllBaseline metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[divider]-(8)-[textField]-(8)-|" options:0 metrics:nil views:views]];

		[self.ignoreCaseCheckBox bind:NSValueBinding toObject:[NSUserDefaultsController sharedUserDefaultsController] withKeyPath:@"values.incrementalSearchIgnoreCase" options:nil];
		[self.wrapAroundCheckBox bind:NSValueBinding toObject:[NSUserDefaultsController sharedUserDefaultsController] withKeyPath:@"values.incrementalSearchWrapAround" options:nil];
	}
	return self;
}

- (BOOL)isFlipped
{
	return YES;
}

- (BOOL)isOpaque
{
	return YES;
}

- (void)drawRect:(NSRect)dirtyRect
{
	NSColor* cornerColor = [NSColor colorWithDeviceWhite:239.0/255.0 alpha:1];
	NSColor* middleColor = [NSColor colorWithDeviceWhite:223/255.0 alpha:1];
	int angle = 270;

	NSGradient* aGradient = [[NSGradient alloc] initWithColorsAndLocations:
		cornerColor, 0.0,
		middleColor, 0.5,
		cornerColor, 1.0, nil];

	NSRect bounds = NSMakeRect(self.bounds.origin.x, self.bounds.origin.y+1, self.bounds.size.width, self.bounds.size.height-1);
	[aGradient drawInRect:bounds angle:angle];
}
@end
