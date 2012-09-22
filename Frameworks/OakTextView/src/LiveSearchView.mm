#import "LiveSearchView.h"

@implementation LiveSearchView

- (void)drawRect:(NSRect)dirtyRect {
		
	NSColor *startingColor = [NSColor colorWithDeviceWhite:239.0/255.0 alpha:1];
	NSColor *middleColor = [NSColor colorWithDeviceWhite:223/255.0 alpha:1];
	NSColor *endingColor = [NSColor colorWithDeviceWhite:209.0/255.0 alpha:1];
	int angle = 270;

	NSGradient* aGradient = [[NSGradient alloc] initWithColorsAndLocations:
		startingColor,0.0f,
		middleColor,0.1f,
	   middleColor,0.9f,
	   endingColor,1.0f, nil];
	[aGradient drawInRect:[self bounds] angle:angle];
	
	[[NSColor colorWithDeviceWhite:169.0/255.0 alpha:1] setFill];
	NSRectFill(NSMakeRect(0,0,self.bounds.size.width,1));
	
}

- (BOOL)isFlipped{
	return YES;
}

@end