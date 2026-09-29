#import "OFBFinderTagsChooser.h"
#import <OakAppKit/src/OakFinderTag.h>
#import <OakAppKit/src/OakRolloverButton.h>
#import <OakAppKit/src/OakUIConstructionFunctions.h>

static constexpr CGFloat SwatchButtonWidth  = 24;

@interface OFBFinderTagImage : NSImage
+ (NSImage*)imageWithSize:(NSSize)aSize forLabelColor:(NSColor*)aLabelColor selected:(BOOL)selectedFlag removable:(BOOL)removableFlag mouseOver:(BOOL)mouseOverFlag;
@end

@implementation OFBFinderTagImage
+ (NSImage*)imageWithSize:(NSSize)aSize forLabelColor:(NSColor*)aLabelColor selected:(BOOL)selectedFlag removable:(BOOL)removableFlag mouseOver:(BOOL)mouseOverFlag
{
	return [NSImage imageWithSize:aSize flipped:NO drawingHandler:^BOOL(NSRect dstRect){
		NSRect outerSwatchRect = NSInsetRect(dstRect, 2.5, 2.5);
		NSRect innerSwatchRect = NSInsetRect(dstRect, 5.5, 5.5);

		NSColor* borderColor = aLabelColor;
		NSColor* fillColor = nil;
		NSColor* markColor = nil;

		if(borderColor)
		{
			NSColor* rgbColor = [borderColor colorUsingColorSpace:NSColorSpace.sRGBColorSpace];

			CGFloat factor = 0.8;
			CGFloat r = 1 - factor*(1 - rgbColor.redComponent);
			CGFloat g = 1 - factor*(1 - rgbColor.greenComponent);
			CGFloat b = 1 - factor*(1 - rgbColor.blueComponent);

			fillColor = [NSColor colorWithSRGBRed:r green:g blue:b alpha:1.0];

			markColor = [NSColor whiteColor];
		}
		else
		{
			borderColor = [NSColor secondaryLabelColor];
			fillColor = [NSColor clearColor];
			markColor = borderColor;
		}

		if(mouseOverFlag)
		{
			NSBezierPath* path = [NSBezierPath bezierPathWithOvalInRect:outerSwatchRect];
			[fillColor set];
			[path fill];
			[borderColor set];
			[path stroke];

			if(removableFlag)
			{
				NSRect r = NSInsetRect(innerSwatchRect, 3, 3);
				CGFloat const inscribedRectLength = r.size.width;
				NSBezierPath* line = [NSBezierPath bezierPath];
				[line moveToPoint:r.origin];
				[line lineToPoint:NSMakePoint(r.origin.x + inscribedRectLength, r.origin.y + inscribedRectLength)];
				[line moveToPoint:NSMakePoint(r.origin.x + inscribedRectLength, r.origin.y)];
				[line lineToPoint:NSMakePoint(r.origin.x, r.origin.y + inscribedRectLength)];
				[line setLineWidth:1.5];
				[markColor set];
				[line stroke];
			}
			else
			{
				NSRect r = NSInsetRect(innerSwatchRect, 3, 3);
				NSBezierPath* line = [NSBezierPath bezierPath];
				[line moveToPoint:NSMakePoint(r.origin.x + r.size.width/2, r.origin.y)];
				[line lineToPoint:NSMakePoint(r.origin.x + r.size.width/2, r.origin.y + r.size.height)];
				[line moveToPoint:NSMakePoint(r.origin.x, r.origin.y + r.size.height/2)];
				[line lineToPoint:NSMakePoint(r.origin.x+r.size.width, r.origin.y + r.size.height/2)];
				[line setLineWidth:1.5];
				[markColor set];
				[line stroke];
			}
		}
		else
		{
			NSBezierPath* path = [NSBezierPath bezierPathWithOvalInRect:innerSwatchRect];

			[fillColor set];
			[path fill];
			[borderColor set];
			[path stroke];

			if(selectedFlag)
			{
				NSRect r = NSInsetRect(innerSwatchRect, 3, 3);
				NSBezierPath* line = [NSBezierPath bezierPath];
				[line moveToPoint:NSMakePoint(r.origin.x, r.origin.y + r.size.width * 0.5)];
				[line lineToPoint:NSMakePoint(r.origin.x + r.size.width/4, r.origin.y)];
				[line lineToPoint:NSMakePoint(r.origin.x + r.size.width, r.origin.y + r.size.height)];
				[line setLineWidth:1.5];
				[markColor set];
				[line stroke];
			}

		}

		return YES;
	}];
}
@end

@interface OFBFinderTagsChooser ()
{
	NSTextField* _tagTextField;
}
@property (nonatomic) NSArray<OakFinderTag*>* favoriteFinderTags;
@property (nonatomic) NSArray<OakFinderTag*>* selectedTags;
@property (nonatomic) NSArray<OakFinderTag*>* selectedTagsToRemove;
@property (nonatomic) OakFinderTag* hoverTag;
@end

@implementation OFBFinderTagsChooser
+ (OFBFinderTagsChooser*)finderTagsChooserWithSelectedTags:(NSArray<OakFinderTag*>*)selectedTags andSelectedTagsToRemove:(NSArray<OakFinderTag*>*)selectedTagsToRemove forMenu:(NSMenu*)aMenu
{
	OFBFinderTagsChooser* chooser = [[OFBFinderTagsChooser alloc] initWithSelectedTags:selectedTags andSelectedTagsToRemove:selectedTagsToRemove forMenu:aMenu];
	return chooser;
}

- (void)dealloc
{
	[NSNotificationCenter.defaultCenter removeObserver:self];
}

- (NSSize)intrinsicContentSize
{
	return NSMakeSize(SwatchButtonWidth * (_favoriteFinderTags.count + 1), SwatchButtonWidth + [_tagTextField intrinsicContentSize].height);
}

- (instancetype)initWithSelectedTags:(NSArray<OakFinderTag*>*)selectedTags andSelectedTagsToRemove:(NSArray<OakFinderTag*>*)selectedTagsToRemove forMenu:(NSMenu*)aMenu
{
	if(self = [super initWithFrame:NSZeroRect])
	{
		[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(mouseDidEnterFinderTagButton:) name:OakRolloverButtonMouseDidEnterNotification object:nil];
		[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(mouseDidLeaveFinderTagButton:) name:OakRolloverButtonMouseDidLeaveNotification object:nil];

		_favoriteFinderTags = [OakFinderTagManager favoriteFinderTags];
		_selectedTags = selectedTags;
		_selectedTagsToRemove = selectedTagsToRemove;

		self.autoresizesSubviews = YES;
		self.autoresizingMask = NSViewWidthSizable|NSViewHeightSizable;

		_tagTextField = [[NSTextField alloc] initWithFrame:NSZeroRect];
		_tagTextField.cell.accessibilityElement = NO;
		_tagTextField.font            = [aMenu font];
		_tagTextField.textColor       = [NSColor disabledControlTextColor];
		_tagTextField.bezeled         = NO;
		_tagTextField.bordered        = NO;
		_tagTextField.drawsBackground = NO;
		_tagTextField.editable        = NO;
		_tagTextField.selectable      = NO;
		_tagTextField.stringValue     = @"Tags…";

		_tagTextField.translatesAutoresizingMaskIntoConstraints = NO;
		[self addSubview:_tagTextField];

		NSMutableArray<OakRolloverButton*>* buttons = [NSMutableArray arrayWithCapacity:_favoriteFinderTags.count];
		for(NSUInteger i = 0; i < _favoriteFinderTags.count; ++i)
		{
			OakFinderTag* tag = _favoriteFinderTags[i];
			BOOL isSelected   = [_selectedTags containsObject:tag];
			BOOL isRemovable  = [_selectedTagsToRemove containsObject:tag];

			OakRolloverButton* button = [[OakRolloverButton alloc] initWithFrame:NSZeroRect];
			button.accessibilityLabel = [NSString stringWithFormat:@"%@ tag %@", (isRemovable ? @"Remove" : @"Add"), tag.displayName];

			button.regularImage  = [OFBFinderTagImage imageWithSize:NSMakeSize(SwatchButtonWidth, SwatchButtonWidth) forLabelColor:tag.labelColor selected:isSelected removable:isRemovable mouseOver:NO];
			button.pressedImage  = [OFBFinderTagImage imageWithSize:NSMakeSize(SwatchButtonWidth, SwatchButtonWidth) forLabelColor:tag.labelColor selected:isSelected removable:isRemovable mouseOver:YES];
			button.rolloverImage = [OFBFinderTagImage imageWithSize:NSMakeSize(SwatchButtonWidth, SwatchButtonWidth) forLabelColor:tag.labelColor selected:isSelected removable:isRemovable mouseOver:YES];
			button.target = self;
			button.action = @selector(didClickFinderTag:);
			button.tag = i;

			[buttons addObject:button];
		}

		if(buttons.count)
		{
			NSStackView* stackView = [NSStackView stackViewWithViews:buttons];
			stackView.spacing = 0;

			OakAddAutoLayoutViewsToSuperview(@[ stackView ], self);

			NSDictionary* views = @{ @"tagButtons": stackView, @"tagTextField": _tagTextField };
			[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[tagButtons]-(5)-[tagTextField]|" options:0 metrics:nil views:views]];
			[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[tagTextField]|"                 options:0 metrics:nil views:views]];
			[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[tagButtons]-(>=20)-|"           options:0 metrics:nil views:views]];
		}
		else
		{
			[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[tagTextField]|" options:0 metrics:nil views:@{ @"tagTextField" : _tagTextField }]];
			[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[tagTextField]|" options:0 metrics:nil views:@{ @"tagTextField" : _tagTextField }]];
		}

		[self setFrameSize:[self fittingSize]];
	}

	return self;
}

- (void)mouseDidEnterFinderTagButton:(NSNotification*)aNotificaiton
{
	OakRolloverButton* button = aNotificaiton.object;
	if(button.target == self)
	{
		self.hoverTag = _favoriteFinderTags[button.tag];
		if([_selectedTagsToRemove containsObject:_hoverTag])
				_tagTextField.stringValue = [NSString stringWithFormat:@"Remove “%@”", _hoverTag.displayName];
		else	_tagTextField.stringValue = [NSString stringWithFormat:@"Add “%@”", _hoverTag.displayName];
	}
}

- (void)mouseDidLeaveFinderTagButton:(NSNotification*)aNotificaiton
{
	OakRolloverButton* button = aNotificaiton.object;
	if(button.target == self)
	{
		self.hoverTag = nil;
		_tagTextField.stringValue = @"Tags…";
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

@end
