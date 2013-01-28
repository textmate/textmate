#import "LiveSearchView.h"

@implementation LiveSearchView
- (id)initWithFrame:(NSRect)aRect
{
	if(self = [super initWithFrame:aRect])
	{
		self.textField = [[[NSTextField alloc] initWithFrame:NSZeroRect] autorelease];
		self.textField.focusRingType = NSFocusRingTypeNone;

		NSDictionary* views = @{
			@"textField" : self.textField,
		};

		for(NSView* view in [views allValues])
		{
			[view setTranslatesAutoresizingMaskIntoConstraints:NO];
			[self addSubview:view];
		}

		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(8)-[textField]-(8)-|" options:0 metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-(9)-[textField]-(8)-|" options:0 metrics:nil views:views]];
	}
	return self;
}

- (void)dealloc
{
	self.textField = nil;
	[super dealloc];
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

	[[NSColor colorWithDeviceWhite:169.0/255.0 alpha:1] setFill];
	NSRectFill(NSMakeRect(0, 0, self.bounds. size.width, 1));
}
@end
