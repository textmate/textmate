#import "LiveSearchView.h"

@implementation LiveSearchView

- (void)drawRect:(NSRect)dirtyRect {
		
	NSColor *cornerColor = [NSColor colorWithDeviceWhite:239.0/255.0 alpha:1];
	NSColor *middleColor = [NSColor colorWithDeviceWhite:223/255.0 alpha:1];
	int angle = 270;

	NSGradient* aGradient = [[NSGradient alloc] initWithColorsAndLocations:
		cornerColor,0.0f,
	   middleColor,0.5f,
	   cornerColor,1.0f, nil];
	
	NSRect bounds = NSMakeRect(self.bounds.origin.x, self.bounds.origin.y+1, self.bounds.size.width, self.bounds.size.height-1);
	[aGradient drawInRect:bounds angle:angle];
	
	[[NSColor colorWithDeviceWhite:169.0/255.0 alpha:1] setFill];
	NSRectFill(NSMakeRect(0,0,self.bounds.size.width,1));
	
}

- (BOOL)isFlipped{
	return YES;
}

@end