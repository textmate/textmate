#import "OFBFinderTagsChooser.h"
#import <OakAppKit/OakFinderTag.h>

static constexpr CGFloat SwatchDiameter  = 20;
static constexpr CGFloat SwatchMargin    = 2;
static constexpr CGFloat LabelNameHeight = 15;

@interface OFBFinderTagsChooser ()
{
	OakFinderTag* _hoverTag;
}
@property (nonatomic) NSArray<OakFinderTag*>* favoriteFinderTags;
@end

@implementation OFBFinderTagsChooser
+ (OFBFinderTagsChooser*)finderTagsChooserForMenu:(NSMenu*)aMenu
{
	OFBFinderTagsChooser* chooser = [[OFBFinderTagsChooser alloc] initWithFrame:NSMakeRect(0, 0, 200, 45)];
	chooser.font = [aMenu font];
	chooser.favoriteFinderTags = [OakFinderTagManager favoriteFinderTags];
	[chooser setNeedsDisplay:YES];
	return chooser;
}

- (NSSize)intrinsicContentSize
{
	CGFloat const numberOfFavoriteTags = _favoriteFinderTags.count;
	CGFloat const width = SwatchDiameter * numberOfFavoriteTags + SwatchMargin * (numberOfFavoriteTags + 1);
	CGFloat const height = SwatchDiameter + LabelNameHeight;
	return NSMakeSize(width, height);
}

- (void)setEnabled:(BOOL)flag
{
	_enabled = flag;
	[self updateTrackingAreas];
}

- (NSDictionary*)labelAttributes
{
	NSMutableParagraphStyle* style = [[NSParagraphStyle defaultParagraphStyle] mutableCopy];
	[style setAlignment:NSCenterTextAlignment];

	NSColor* labelColor;
	if([NSColor respondsToSelector:@selector(secondaryLabelColor)]) // MAC_OS_X_VERSION_10_10
			labelColor = [NSColor secondaryLabelColor];
	else	labelColor = [NSColor colorWithCalibratedWhite:0 alpha:0.5];

	NSDictionary* labelAttributes = @{
		NSFontAttributeName : [NSFont boldSystemFontOfSize:[NSFont systemFontSizeForControlSize:NSSmallControlSize]],
		NSForegroundColorAttributeName : labelColor,
		NSParagraphStyleAttributeName  : style,
	};
	return labelAttributes;
}

- (NSRect)rectForFavoriteTag:(OakFinderTag*)aTag
{
	NSUInteger index = [_favoriteFinderTags indexOfObject:aTag];
	return (index < _favoriteFinderTags.count) ? NSMakeRect(22 + index*(SwatchDiameter + SwatchMargin * 2), LabelNameHeight + 5, SwatchDiameter, SwatchDiameter) : NSZeroRect;
}

- (OakFinderTag*)tagAtPoint:(NSPoint)aPoint
{
	for(OakFinderTag* tag in _favoriteFinderTags)
	{
		NSRect r = [self rectForFavoriteTag:tag];
		if(NSMouseInRect(aPoint, r, [self isFlipped]))
			return tag;
	}
	return nil;
}

- (void)drawXInRect:(NSRect)aRect
{
	NSRect r = NSInsetRect(aRect, 3, 3);
	CGFloat const inscribedRectLength = r.size.width;
	NSBezierPath* line = [NSBezierPath bezierPath];
	[line moveToPoint:r.origin];
	[line lineToPoint:NSMakePoint(r.origin.x + inscribedRectLength, r.origin.y + inscribedRectLength)];
	[line moveToPoint:NSMakePoint(r.origin.x + inscribedRectLength, r.origin.y)];
	[line lineToPoint:NSMakePoint(r.origin.x, r.origin.y + inscribedRectLength)];
	[line setLineWidth:1.5];
	[[NSColor labelColor] set];
	[line stroke];
}

- (void)drawRect:(NSRect)aRect
{
	if(!self.isEnabled)
		CGContextSetAlpha((CGContextRef)[[NSGraphicsContext currentContext] graphicsPort], 0.5);

	for(OakFinderTag* tag in _favoriteFinderTags)
	{
		NSRect tagRect = [self rectForFavoriteTag:tag];
		BOOL tagSelected = [_selectedTags containsObject:tag];
		BOOL tagHovered = [_hoverTag isEqual:tag];
		BOOL shouldRemoveSelectedTag = _selectedTagsToRemove && [_selectedTagsToRemove containsObject:tag];

		if(tagSelected)
		{
			NSBezierPath* path = [NSBezierPath bezierPathWithOvalInRect:tagRect];
			[[NSColor colorWithCalibratedWhite:0.5 alpha:1.0] set];
			[path stroke];
			[[NSColor whiteColor] set];
			[path fill];
		}

		if(tagHovered)
		{
			[[NSColor colorWithCalibratedWhite:0.5 alpha:1.0] set];
			NSBezierPath* path = [NSBezierPath bezierPathWithOvalInRect:tagRect];
			[path stroke];
			[[NSColor whiteColor] set];
			[path fill];
		}

		NSRect innerSwatchRect = NSInsetRect(tagRect, 3.5, 3.5);
		NSBezierPath* path = [NSBezierPath bezierPathWithOvalInRect:innerSwatchRect];

		[tag.backgroundColor set];
		[path fill];
		[tag.foregroundColor set];
		[path stroke];

		if(tagHovered)
		{
			[[NSColor colorWithCalibratedWhite:0.45 alpha:0.2] set];
			NSBezierPath* path = [NSBezierPath bezierPathWithOvalInRect:NSInsetRect(innerSwatchRect, -4, -4)];
			[path fill];

			NSRect labelRect = NSMakeRect(0, 0, self.bounds.size.width, LabelNameHeight);
			if(shouldRemoveSelectedTag)
			{
				[self drawXInRect:innerSwatchRect];
				[[NSString stringWithFormat:@"Remove Tag “%@”", _hoverTag.displayName] drawInRect:labelRect withAttributes:[self labelAttributes]];
			}
			else
			{
				[[NSString stringWithFormat:@"Add Tag “%@”", _hoverTag.displayName] drawInRect:labelRect withAttributes:[self labelAttributes]];
			}
		}
	}
}

- (void)updateTrackingAreas
{
	[super updateTrackingAreas];

	for(NSTrackingArea* trackingArea in self.trackingAreas)
		[self removeTrackingArea:trackingArea];

	if(self.isEnabled)
	{
		for(OakFinderTag* tag in _favoriteFinderTags)
		{
			NSTrackingArea* trackingArea = [[NSTrackingArea alloc] initWithRect:[self rectForFavoriteTag:tag] options:NSTrackingMouseEnteredAndExited|NSTrackingActiveInKeyWindow owner:self userInfo:nil];
			[self addTrackingArea:trackingArea];
		}
	}
}

// ================
// = Mouse Events =
// ================

- (BOOL)acceptsFirstMouse:(NSEvent*)anEvent
{
	return YES;
}

- (void)_handleMouseMoved:(NSEvent*)anEvent
{
	if(!self.isEnabled)
		return;

	NSPoint pos = [self convertPoint:[anEvent locationInWindow] fromView:nil];
	_hoverTag = [self tagAtPoint:pos];
	[self setNeedsDisplay:YES];
}

- (void)mouseDown:(NSEvent*)anEvent
{
	if(!self.isEnabled)
		return;

	NSEventMask eventMask = NSLeftMouseUpMask|NSRightMouseUpMask|NSLeftMouseDraggedMask|NSRightMouseDraggedMask;
	while(!([anEvent type] == NSLeftMouseUp || [anEvent type] == NSRightMouseUp))
	{
		[self _handleMouseMoved:anEvent];
		anEvent = [[self window] nextEventMatchingMask:eventMask];
	}
	[self mouseUp:anEvent];
}

- (void)rightMouseDown:(NSEvent*)anEvent
{
	[self mouseDown:anEvent];
}

- (void)mouseUp:(NSEvent*)anEvent
{
	if(!self.isEnabled)
		return;

	NSPoint localPoint = [self convertPoint:[anEvent locationInWindow] fromView:nil];
	for(OakFinderTag* tag in _favoriteFinderTags)
	{
		if(NSPointInRect(localPoint, [self rectForFavoriteTag:tag]))
		{
			_removeChosenTag = _selectedTagsToRemove && [_selectedTagsToRemove containsObject:tag];
			_chosenTag       = tag;

			if([self target] && [[self target] respondsToSelector:[self action]])
				[NSApp sendAction:[self action] to:[self target] from:self];

			if([self enclosingMenuItem])
				[[[self enclosingMenuItem] menu] cancelTracking];
		}
	}

	if([self enclosingMenuItem])
		[[[self enclosingMenuItem] menu] cancelTracking];
}

- (void)rightMouseUp:(NSEvent*)anEvent
{
	[self mouseUp:anEvent];
}

- (void)mouseDragged:(NSEvent*)anEvent
{
	[self _handleMouseMoved:anEvent];
}

- (void)rightMouseDragged:(NSEvent*)anEvent
{
	[self _handleMouseMoved:anEvent];
}

- (void)mouseEntered:(NSEvent*)anEvent
{
	[self _handleMouseMoved:anEvent];
}

- (void)mouseExited:(NSEvent*)anEvent
{
	[self _handleMouseMoved:anEvent];
}
@end
