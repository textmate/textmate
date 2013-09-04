#import "OakFinderLabelChooser.h"

static const CGFloat SwatchDiameter  = 9;
static const CGFloat SwatchMargin    = 4;
static const CGFloat LabelNameHeight = 15;

@interface OakFinderLabelChooser ()
{
	OBJC_WATCH_LEAKS(OakFinderLabelChooser);
}
@property (nonatomic) NSMutableArray* labelNames;
@property (nonatomic) NSInteger highlightedIndex;
@end

@implementation OakFinderLabelChooser
// ==================
// = Setup/Teardown =
// ==================

- (id)initWithFrame:(NSRect)rect
{
	if(self = [super initWithFrame:rect])
	{
		self.font             = [NSFont menuFontOfSize:0];
		self.enabled          = YES;
		self.highlightedIndex = -1;
	}
	return self;
}

// =============
// = Accessors =
// =============

- (void)setSelectedIndex:(NSInteger)index
{
	_selectedIndex = index;
	[self setNeedsDisplay:YES];
}

- (void)setHighlightedIndex:(NSInteger)index
{
	_highlightedIndex = index;
	[self setNeedsDisplay:YES];
}

- (NSArray*)labelNames
{
	// GetLabel is deprecated and fails for non-MacRoman, but there is no replacement: <rdar://4772578>
	if(!_labelNames)
	{
		NSMutableDictionary* customLabels = [NSMutableDictionary dictionary];
		for(NSString* path in [NSSearchPathForDirectoriesInDomains(NSLibraryDirectory, NSAllDomainsMask, YES) reverseObjectEnumerator])
		{
			if(NSDictionary* dict = [NSDictionary dictionaryWithContentsOfFile:[path stringByAppendingPathComponent:@"Preferences/com.apple.Labels.plist"]])
				[customLabels addEntriesFromDictionary:dict];
		}

		_labelNames = [[NSMutableArray alloc] initWithObjects:@"None", @"Gray", @"Green", @"Purple", @"Blue", @"Yellow", @"Red", @"Orange", nil];
		for(NSUInteger i = 1; i <= self.labelNames.count; ++i)
		{
			if(NSString* label = [customLabels objectForKey:[NSString stringWithFormat:@"Label_Name_%lu", i]])
				[_labelNames replaceObjectAtIndex:i withObject:label];
		}
	}
	return _labelNames;
}

// ===========
// = Drawing =
// ===========

- (NSRect)rectForSwatchAtIndex:(NSInteger)index
{
	static int const LabelIndexMap[] = { 0, 6, 7, 5, 2, 4, 3, 1 };
	index = std::distance(std::begin(LabelIndexMap), std::find(std::begin(LabelIndexMap), std::end(LabelIndexMap), index));
	return (index < 8) ? NSMakeRect(22 + index*(SwatchDiameter + SwatchMargin*2), LabelNameHeight + 5, SwatchDiameter, SwatchDiameter) : NSZeroRect;
}

- (void)drawRect:(NSRect)rect
{
	static struct swatch_t { struct { CGFloat red, green, blue; } from, to; } const swatches[] =
	{
		{ {   0,   0,   0}, {   0,   0,   0} },
		{ { 205, 205, 206}, { 169, 169, 169} }, // Gray
		{ { 212, 233, 151}, { 180, 214,  71} }, // Green
		{ { 224, 190, 234}, { 192, 142, 217} }, // Purple
		{ { 167, 208, 255}, {  90, 162, 255} }, // Blue
		{ { 249, 242, 151}, { 239, 219,  71} }, // Yellow
		{ { 252, 162, 154}, { 251, 100,  91} }, // Red
		{ { 249, 206, 143}, { 246, 170,  68} }, // Orange
	};

	[@"Label:" drawInRect:NSInsetRect(rect, 22, 0) withAttributes:@{ NSFontAttributeName : self.font, NSForegroundColorAttributeName : self.enabled ? [NSColor blackColor] : [NSColor grayColor] }];

	for(NSInteger i = 0; i < 8; ++i)
	{
		NSRect swatchRect = [self rectForSwatchAtIndex:i];

		if((i == self.highlightedIndex || (i == self.selectedIndex && self.selectedIndex != 0)) && self.enabled)
		{
			NSRect outerRect = NSInsetRect(swatchRect, -SwatchMargin, -SwatchMargin);
			NSBezierPath* path = [NSBezierPath bezierPathWithRoundedRect:outerRect xRadius:2 yRadius:2];
			if(i == self.highlightedIndex)
			{
				[[NSColor colorWithCalibratedWhite:0.8 alpha:1] set];
				[path fill];
			}
			[[NSColor colorWithCalibratedWhite:0.6 alpha:1] set];
			[path stroke];
		}

		if(i > 0)
		{
			[NSGraphicsContext saveGraphicsState];

			if(self.enabled)
			{
				NSShadow* shadow = [NSShadow new];
				[shadow setShadowColor:[NSColor colorWithCalibratedWhite:0 alpha:0.75]];
				[shadow setShadowOffset:NSMakeSize(0, -1)];
				[shadow setShadowBlurRadius:2];
				[shadow set];
			}

			[[NSColor whiteColor] set];
			[[NSBezierPath bezierPathWithRect:swatchRect] fill];

			swatch_t const& swatch = swatches[i];
			NSColor* startColor = [NSColor colorWithCalibratedRed:swatch.from.red/255.0 green:swatch.from.green/255.0 blue:swatch.from.blue/255.0 alpha:1];
			NSColor* endColor   = [NSColor colorWithCalibratedRed:swatch.to.red/255.0 green:swatch.to.green/255.0 blue:swatch.to.blue/255.0 alpha:1];
			if(!self.enabled)
			{
				startColor = [startColor blendedColorWithFraction:0.3 ofColor:[NSColor grayColor]];
				endColor   = [startColor blendedColorWithFraction:0.5 ofColor:[NSColor grayColor]];
			}
			NSGradient* gradient = [[NSGradient alloc] initWithStartingColor:startColor endingColor:endColor];
			[gradient drawInRect:swatchRect angle:-90];

			[NSGraphicsContext restoreGraphicsState];
		}
		else
		{
			if(self.enabled)
					[[NSColor colorWithCalibratedWhite:0.4 alpha:1] set];
			else	[[NSColor colorWithCalibratedWhite:0.7 alpha:1] set];

			swatchRect = NSInsetRect(swatchRect, 1.5, 1.5);

			NSBezierPath* line = [NSBezierPath bezierPath];
			[line moveToPoint:swatchRect.origin];
			[line lineToPoint:NSMakePoint(swatchRect.origin.x+swatchRect.size.width, swatchRect.origin.y+swatchRect.size.height)];
			[line setLineWidth:2];
			[line stroke];

			line = [NSBezierPath bezierPath];
			[line moveToPoint:NSMakePoint(swatchRect.origin.x+swatchRect.size.width, swatchRect.origin.y)];
			[line lineToPoint:NSMakePoint(swatchRect.origin.x, swatchRect.origin.y+swatchRect.size.height)];
			[line setLineWidth:2];
			[line stroke];
		}
	}

	if(self.highlightedIndex != -1)
	{
		NSMutableParagraphStyle* style = [[NSParagraphStyle defaultParagraphStyle] mutableCopy];
		[style setAlignment:NSCenterTextAlignment];
		NSDictionary* attributes = @{
			NSFontAttributeName            : [[NSFontManager sharedFontManager] convertFont:[NSFont menuFontOfSize:12] toHaveTrait:NSBoldFontMask],
			NSForegroundColorAttributeName : [NSColor grayColor],
			NSParagraphStyleAttributeName  : style,
		};
		NSString* name = [[self labelNames] objectAtIndex:self.highlightedIndex];
		[[NSString stringWithFormat:@"“%@”", name] drawInRect:NSMakeRect(0, 0, [self bounds].size.width, LabelNameHeight) withAttributes:attributes];
	}
}

// ==========
// = Events =
// ==========

- (void)mouseDown:(NSEvent*)theEvent
{
	if(!self.enabled)
		return;

	NSPoint localPoint = [self convertPoint:[theEvent locationInWindow] fromView:nil];
	for(NSInteger i = 0; i < 8; ++i)
	{
		NSRect hitRect = NSInsetRect([self rectForSwatchAtIndex:i], -(SwatchMargin+1), -(SwatchMargin+1));
		if(NSPointInRect(localPoint, hitRect))
		{
			self.selectedIndex = i;

			if([self target] && [[self target] respondsToSelector:[self action]])
				[NSApp sendAction:[self action] to:[self target] from:self];

			if([self enclosingMenuItem])
				[[[self enclosingMenuItem] menu] cancelTracking];
		}
	}
}

- (void)updateTrackingAreas
{
	[super updateTrackingAreas];

	for(NSInteger i = self.trackingAreas.count - 1; i >= 0; --i)
		[self removeTrackingArea:[self.trackingAreas objectAtIndex:i]];

	for(NSInteger i = 0; i < 8; i++)
	{
		NSTrackingArea* trackingArea = [[NSTrackingArea alloc] initWithRect:NSInsetRect([self rectForSwatchAtIndex:i], -SwatchMargin, -SwatchMargin)
                                                                  options:NSTrackingMouseEnteredAndExited|NSTrackingActiveInKeyWindow
                                                                    owner:self
                                                                 userInfo:@{ @"index" : @(i) }];
		[self addTrackingArea:trackingArea];
	}
}

- (void)mouseEntered:(NSEvent*)event
{
	self.highlightedIndex = [[[[event trackingArea] userInfo] objectForKey:@"index"] intValue];
}

- (void)mouseExited:(NSEvent*)event
{
	self.highlightedIndex = -1;
}
@end
