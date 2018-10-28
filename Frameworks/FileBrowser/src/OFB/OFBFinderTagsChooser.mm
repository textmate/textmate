#import "OFBFinderTagsChooser.h"
#import <OakAppKit/OakFinderTag.h>
#import <OakAppKit/OakRolloverButton.h>
#import <OakAppKit/OakUIConstructionFunctions.h>

static constexpr CGFloat SwatchDiameter  = 20;
static constexpr CGFloat SwatchMargin    = 2;
static constexpr CGFloat LabelNameHeight = 15;

@interface OFBFinderTagImage : NSImage
+ (NSImage*)imageWithSize:(NSSize)aSize backgroundColor:(NSColor*)bgColor foregroundColor:(NSColor*)fgColor selected:(BOOL)selectedFlag removable:(BOOL)removableFlag mouseOver:(BOOL)mouseOverFlag;
@end

@implementation OFBFinderTagImage
+ (NSImage*)imageWithSize:(NSSize)aSize backgroundColor:(NSColor*)bgColor foregroundColor:(NSColor*)fgColor selected:(BOOL)selectedFlag removable:(BOOL)removableFlag mouseOver:(BOOL)mouseOverFlag
{
	auto drawXInRect = ^(NSRect aRect){
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
	};

	return [NSImage imageWithSize:aSize flipped:NO drawingHandler:^BOOL(NSRect dstRect){
		// Allow 1 pixel for anti-aliasing the drawn circle
		dstRect = NSInsetRect(dstRect, 1, 1);

		if(selectedFlag)
		{
			NSBezierPath* path = [NSBezierPath bezierPathWithOvalInRect:dstRect];
			[[NSColor colorWithCalibratedWhite:0.5 alpha:1.0] set];
			[path stroke];
			[[NSColor whiteColor] set];
			[path fill];
		}

		if(mouseOverFlag)
		{
			[[NSColor colorWithCalibratedWhite:0.5 alpha:1.0] set];
			NSBezierPath* path = [NSBezierPath bezierPathWithOvalInRect:dstRect];
			[path stroke];
			[[NSColor whiteColor] set];
			[path fill];
		}

		NSRect innerSwatchRect = NSInsetRect(dstRect, 3.5, 3.5);
		NSBezierPath* path = [NSBezierPath bezierPathWithOvalInRect:innerSwatchRect];

		[bgColor set];
		[path fill];
		[fgColor set];
		[path stroke];

		if(mouseOverFlag)
		{
			[[NSColor colorWithCalibratedWhite:0.45 alpha:0.2] set];
			NSBezierPath* path = [NSBezierPath bezierPathWithOvalInRect:NSInsetRect(innerSwatchRect, -4, -4)];
			[path fill];

			if(mouseOverFlag && removableFlag)
				drawXInRect(innerSwatchRect);
		}

		return YES;
	}];
}
@end

@interface OFBFinderTagsChooser ()
{
	BOOL _didCreateSubviews;
}
@property (nonatomic) NSArray<OakFinderTag*>* favoriteFinderTags;
@property (nonatomic) OakFinderTag* hoverTag;
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

- (void)dealloc
{
	[NSNotificationCenter.defaultCenter removeObserver:self];
}

- (NSSize)intrinsicContentSize
{
	CGFloat const numberOfFavoriteTags = _favoriteFinderTags.count;
	CGFloat const width = SwatchDiameter * numberOfFavoriteTags + SwatchMargin * (numberOfFavoriteTags + 1);
	CGFloat const height = SwatchDiameter + LabelNameHeight;
	return NSMakeSize(width, height);
}

- (void)viewDidMoveToSuperview
{
	if(_didCreateSubviews || !self.superview)
		return;
	_didCreateSubviews = YES;

	[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(mouseDidEnterFinderTagButton:) name:OakRolloverButtonMouseDidEnterNotification object:nil];
	[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(mouseDidLeaveFinderTagButton:) name:OakRolloverButtonMouseDidLeaveNotification object:nil];

	for(NSUInteger i = 0; i < _favoriteFinderTags.count; ++i)
	{
		OakFinderTag* tag = _favoriteFinderTags[i];
		BOOL isSelected   = [_selectedTags containsObject:tag];
		BOOL isRemovable  = [_selectedTagsToRemove containsObject:tag];

		OakRolloverButton* button = [[OakRolloverButton alloc] initWithFrame:[self rectForFavoriteTag:tag]];
		OakSetAccessibilityLabel(button, isRemovable ? [NSString stringWithFormat:@"Remove tag %@", tag.displayName] : [NSString stringWithFormat:@"Add tag %@", tag.displayName]);

		button.regularImage  = [OFBFinderTagImage imageWithSize:NSMakeSize(SwatchDiameter, SwatchDiameter) backgroundColor:tag.backgroundColor foregroundColor:tag.foregroundColor selected:isSelected removable:isRemovable mouseOver:NO];
		button.pressedImage  = [OFBFinderTagImage imageWithSize:NSMakeSize(SwatchDiameter, SwatchDiameter) backgroundColor:tag.backgroundColor foregroundColor:tag.foregroundColor selected:isSelected removable:isRemovable mouseOver:YES];
		button.rolloverImage = [OFBFinderTagImage imageWithSize:NSMakeSize(SwatchDiameter, SwatchDiameter) backgroundColor:tag.backgroundColor foregroundColor:tag.foregroundColor selected:isSelected removable:isRemovable mouseOver:YES];
		button.target = self;
		button.action = @selector(didClickFinderTag:);
		button.tag = i;

		[self addSubview:button];
	}
}

- (void)mouseDidEnterFinderTagButton:(NSNotification*)aNotificaiton
{
	OakRolloverButton* button = aNotificaiton.object;
	if([self.subviews containsObject:button])
	{
		self.hoverTag = _favoriteFinderTags[button.tag];
		[self setNeedsDisplay:YES];
	}
}

- (void)mouseDidLeaveFinderTagButton:(NSNotification*)aNotificaiton
{
	OakRolloverButton* button = aNotificaiton.object;
	if([self.subviews containsObject:button])
	{
		self.hoverTag = nil;
		[self setNeedsDisplay:YES];
	}
}

- (void)didClickFinderTag:(id)sender
{
	NSUInteger tagIndex = [sender tag];
	OakFinderTag* tag = _favoriteFinderTags[tagIndex];

	if(self.action && (!self.target || [self.target respondsToSelector:self.action]))
	{
		_chosenTag       = tag;
		_removeChosenTag = [_selectedTagsToRemove containsObject:tag];
		[NSApp sendAction:self.action to:self.target from:self];
		[self.enclosingMenuItem.menu cancelTracking];
	}
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
		NSFontAttributeName:            [NSFont boldSystemFontOfSize:[NSFont systemFontSizeForControlSize:NSSmallControlSize]],
		NSForegroundColorAttributeName: labelColor,
		NSParagraphStyleAttributeName:  style,
	};
	return labelAttributes;
}

- (NSRect)rectForFavoriteTag:(OakFinderTag*)aTag
{
	NSUInteger index = [_favoriteFinderTags indexOfObject:aTag];
	return (index < _favoriteFinderTags.count) ? NSMakeRect(22 + index*(SwatchDiameter + SwatchMargin * 2), LabelNameHeight + 5, SwatchDiameter, SwatchDiameter) : NSZeroRect;
}

- (void)drawRect:(NSRect)aRect
{
	if(_hoverTag)
	{
		NSRect labelRect = NSMakeRect(0, 0, self.bounds.size.width, LabelNameHeight);
		if([_selectedTagsToRemove containsObject:_hoverTag])
				[[NSString stringWithFormat:@"Remove Tag “%@”", _hoverTag.displayName] drawInRect:labelRect withAttributes:[self labelAttributes]];
		else	[[NSString stringWithFormat:@"Add Tag “%@”", _hoverTag.displayName] drawInRect:labelRect withAttributes:[self labelAttributes]];
	}
}
@end
