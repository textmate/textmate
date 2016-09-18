#import "OakUIConstructionFunctions.h"
#import "NSImage Additions.h"

NSFont* OakStatusBarFont ()
{
	return [NSFont messageFontOfSize:11];
}

NSFont* OakControlFont ()
{
	return [NSFont messageFontOfSize:0];
}

NSTextField* OakCreateLabel (NSString* label, NSFont* font, NSTextAlignment alignment, NSLineBreakMode lineBreakMode)
{
	NSTextField* res = [[NSTextField alloc] initWithFrame:NSZeroRect];
	[[res cell] setWraps:NO];
	[[res cell] setLineBreakMode:lineBreakMode];
	res.alignment       = alignment;
	res.bezeled         = NO;
	res.bordered        = NO;
	res.drawsBackground = NO;
	res.editable        = NO;
	res.font            = font ?: OakControlFont();
	res.selectable      = NO;
	res.stringValue     = label;
	return res;
}

NSButton* OakCreateCheckBox (NSString* label)
{
	NSButton* res = [[NSButton alloc] initWithFrame:NSZeroRect];
	[res setContentHuggingPriority:NSLayoutPriorityDefaultHigh forOrientation:NSLayoutConstraintOrientationVertical];
	res.buttonType = NSSwitchButton;
	res.font       = OakControlFont();
	res.title      = label;
	return res;
}

NSButton* OakCreateButton (NSString* label, NSBezelStyle bezel)
{
	NSButton* res = [[NSButton alloc] initWithFrame:NSZeroRect];
	[res setContentHuggingPriority:NSLayoutPriorityDefaultHigh forOrientation:NSLayoutConstraintOrientationHorizontal];
	[res setContentHuggingPriority:NSLayoutPriorityDefaultHigh forOrientation:NSLayoutConstraintOrientationVertical];
	res.bezelStyle = bezel;
	res.buttonType = NSMomentaryPushInButton;
	res.font       = OakControlFont();
	res.title      = label;
	return res;
}

NSPopUpButton* OakCreatePopUpButton (BOOL pullsDown, NSString* initialItemTitle, NSObject* accessibilityLabel)
{
	NSPopUpButton* res = [[NSPopUpButton alloc] initWithFrame:NSZeroRect pullsDown:pullsDown];
	if(initialItemTitle)
		[[res cell] setMenuItem:[[NSMenuItem alloc] initWithTitle:initialItemTitle action:NULL keyEquivalent:@""]];
	OakSetAccessibilityLabel(res, accessibilityLabel);
	return res;
}

NSPopUpButton* OakCreateActionPopUpButton (BOOL bordered)
{
	NSPopUpButton* res = [NSPopUpButton new];
	res.pullsDown = YES;
	if(!(res.bordered = bordered))
		[[res cell] setBackgroundStyle:NSBackgroundStyleRaised];

	NSMenuItem* item = [NSMenuItem new];
	item.title = @"";
	item.image = [NSImage imageNamed:NSImageNameActionTemplate];
	[item.image setSize:NSMakeSize(14, 14)];

	[[res cell] setUsesItemFromMenu:NO];
	[[res cell] setMenuItem:item];
	OakSetAccessibilityLabel(res, @"Actions");

	return res;
}

NSPopUpButton* OakCreateStatusBarPopUpButton (NSString* initialItemTitle, NSObject* accessibilityLabel)
{
	NSPopUpButton* res = OakCreatePopUpButton(NO, initialItemTitle);
	[[res cell] setBackgroundStyle:NSBackgroundStyleRaised];
	res.font     = OakStatusBarFont();
	res.bordered = NO;
	OakSetAccessibilityLabel(res, accessibilityLabel);
	return res;
}

NSComboBox* OakCreateComboBox (NSObject* accessibilityLabel)
{
	NSComboBox* res = [[NSComboBox alloc] initWithFrame:NSZeroRect];
	res.font = OakControlFont();
	OakSetAccessibilityLabel(res, accessibilityLabel);
	return res;
}

OakRolloverButton* OakCreateCloseButton (NSString* accessibilityLabel)
{
	OakRolloverButton* closeButton = [[OakRolloverButton alloc] initWithFrame:NSZeroRect];
	closeButton.regularImage  = [NSImage imageNamed:@"CloseTemplate"         inSameBundleAsClass:[OakRolloverButton class]];
	closeButton.pressedImage  = [NSImage imageNamed:@"ClosePressedTemplate"  inSameBundleAsClass:[OakRolloverButton class]];
	closeButton.rolloverImage = [NSImage imageNamed:@"CloseRolloverTemplate" inSameBundleAsClass:[OakRolloverButton class]];

	OakSetAccessibilityLabel(closeButton, accessibilityLabel);
	return closeButton;
}

// =========================
// = OakBackgroundFillView =
// =========================

@implementation OakBackgroundFillView
{
	id _activeBackgroundValue;
	id _inactiveBackgroundValue;
}

- (void)setupHeaderBackground
{
	self.activeBackgroundGradient   = [[NSGradient alloc] initWithStartingColor:[NSColor colorWithCalibratedWhite:0.915 alpha:1] endingColor:[NSColor colorWithCalibratedWhite:0.760 alpha:1]];
	self.inactiveBackgroundGradient = [[NSGradient alloc] initWithStartingColor:[NSColor colorWithCalibratedWhite:0.915 alpha:1] endingColor:[NSColor colorWithCalibratedWhite:0.915 alpha:1]];
}

- (void)setupStatusBarBackground
{
	self.activeBackgroundGradient   = [[NSGradient alloc] initWithColorsAndLocations:[NSColor colorWithCalibratedWhite:1 alpha:0.68], 0.0, [NSColor colorWithCalibratedWhite:1 alpha:0.5], 0.0416, [NSColor colorWithCalibratedWhite:1 alpha:0], 1.0, nil];
	self.inactiveBackgroundGradient = [[NSGradient alloc] initWithColorsAndLocations:[NSColor colorWithCalibratedWhite:1 alpha:0.68], 0.0, [NSColor colorWithCalibratedWhite:1 alpha:0.5], 0.0416, [NSColor colorWithCalibratedWhite:1 alpha:0], 1.0, nil];
}

- (void)viewWillMoveToWindow:(NSWindow*)newWindow
{
	if(self.window)
	{
		[[NSNotificationCenter defaultCenter] removeObserver:self name:NSWindowDidBecomeMainNotification object:self.window];
		[[NSNotificationCenter defaultCenter] removeObserver:self name:NSWindowDidResignMainNotification object:self.window];
		[[NSNotificationCenter defaultCenter] removeObserver:self name:NSWindowDidBecomeKeyNotification object:self.window];
		[[NSNotificationCenter defaultCenter] removeObserver:self name:NSWindowDidResignKeyNotification object:self.window];
	}

	if(newWindow)
	{
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(windowDidChangeMainOrKey:) name:NSWindowDidBecomeMainNotification object:newWindow];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(windowDidChangeMainOrKey:) name:NSWindowDidResignMainNotification object:newWindow];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(windowDidChangeMainOrKey:) name:NSWindowDidBecomeKeyNotification object:newWindow];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(windowDidChangeMainOrKey:) name:NSWindowDidResignKeyNotification object:newWindow];
	}

	self.active = ([newWindow styleMask] & NSFullScreenWindowMask) || [newWindow isMainWindow] || [newWindow isKeyWindow];
}

- (void)windowDidChangeMainOrKey:(NSNotification*)aNotification
{
	self.active = ([self.window styleMask] & NSFullScreenWindowMask) || [self.window isMainWindow] || [self.window isKeyWindow];
}

- (void)setActive:(BOOL)flag
{
	if(_active == flag)
		return;
	_active = flag;
	self.needsDisplay = YES;
}

- (void)setActiveBackgroundValue:(id)value
{
	if(value == _activeBackgroundValue || [value isEqual:_activeBackgroundValue])
		return;
	_activeBackgroundValue = value;
	if(_active)
		self.needsDisplay = YES;
}

- (void)setInactiveBackgroundValue:(id)value
{
	if(value == _inactiveBackgroundValue || [value isEqual:_inactiveBackgroundValue])
		return;
	_inactiveBackgroundValue = value;
	if(!_active)
		self.needsDisplay = YES;
}

- (void)setActiveBackgroundColor:(NSColor*)aColor             { self.activeBackgroundValue = aColor;    }
- (void)setActiveBackgroundImage:(NSImage*)anImage            { self.activeBackgroundValue = anImage;   }
- (void)setActiveBackgroundGradient:(NSGradient*)aGradient    { self.activeBackgroundValue = aGradient; }
- (void)setInactiveBackgroundColor:(NSColor*)aColor           { self.inactiveBackgroundValue = aColor;    }
- (void)setInactiveBackgroundImage:(NSImage*)anImage          { self.inactiveBackgroundValue = anImage;   }
- (void)setInactiveBackgroundGradient:(NSGradient*)aGradient  { self.inactiveBackgroundValue = aGradient; }

- (NSColor*)activeBackgroundColor          { return [_activeBackgroundValue isKindOfClass:[NSColor class]]      ? _activeBackgroundValue   : nil; }
- (NSImage*)activeBackgroundImage          { return [_activeBackgroundValue isKindOfClass:[NSImage class]]      ? _activeBackgroundValue   : nil; }
- (NSGradient*)activeBackgroundGradient    { return [_activeBackgroundValue isKindOfClass:[NSGradient class]]   ? _activeBackgroundValue   : nil; }
- (NSColor*)inactiveBackgroundColor        { return [_inactiveBackgroundValue isKindOfClass:[NSColor class]]    ? _inactiveBackgroundValue : nil; }
- (NSImage*)inactiveBackgroundImage        { return [_inactiveBackgroundValue isKindOfClass:[NSImage class]]    ? _inactiveBackgroundValue : nil; }
- (NSGradient*)inactiveBackgroundGradient  { return [_inactiveBackgroundValue isKindOfClass:[NSGradient class]] ? _inactiveBackgroundValue : nil; }

- (NSSize)intrinsicContentSize
{
	if(NSImage* image = self.activeBackgroundImage ?: self.inactiveBackgroundImage)
			return image.size;
	else	return NSMakeSize(NSViewNoInstrinsicMetric, NSViewNoInstrinsicMetric);
}

- (void)drawRect:(NSRect)aRect
{
	id value = _active || !_inactiveBackgroundValue ? _activeBackgroundValue : _inactiveBackgroundValue;
	if([value isKindOfClass:[NSGradient class]])
	{
		NSGradient* gradient = value;
		[gradient drawInRect:self.bounds angle:270];
	}
	else if([value isKindOfClass:[NSImage class]])
	{
		NSImage* image = value;
		[[NSColor colorWithPatternImage:image] set];
		CGContextRef context = (CGContextRef)[[NSGraphicsContext currentContext] graphicsPort];
		CGAffineTransform affineTransform = CGContextGetCTM(context);
		CGContextSetPatternPhase(context, CGSizeMake(affineTransform.tx, affineTransform.ty));
		NSRectFillUsingOperation(aRect, NSCompositeSourceOver);
	}
	else if([value isKindOfClass:[NSColor class]])
	{
		NSColor* color = value;
		[color set];
		NSRectFill(aRect);
	}
}
@end

OakBackgroundFillView* OakCreateVerticalLine (NSColor* primaryColor, NSColor* secondaryColor)
{
	OakBackgroundFillView* view = [[OakBackgroundFillView alloc] initWithFrame:NSZeroRect];
	view.activeBackgroundColor   = primaryColor;
	view.inactiveBackgroundColor = secondaryColor;
	[view addConstraint:[NSLayoutConstraint constraintWithItem:view attribute:NSLayoutAttributeWidth relatedBy:NSLayoutRelationEqual toItem:nil attribute:NSLayoutAttributeNotAnAttribute multiplier:1 constant:1]];
	view.translatesAutoresizingMaskIntoConstraints = NO;
	return view;
}

OakBackgroundFillView* OakCreateHorizontalLine (NSColor* primaryColor, NSColor* secondaryColor)
{
	OakBackgroundFillView* view = [[OakBackgroundFillView alloc] initWithFrame:NSZeroRect];
	view.activeBackgroundColor   = primaryColor;
	view.inactiveBackgroundColor = secondaryColor;
	[view addConstraint:[NSLayoutConstraint constraintWithItem:view attribute:NSLayoutAttributeHeight relatedBy:NSLayoutRelationEqual toItem:nil attribute:NSLayoutAttributeNotAnAttribute multiplier:1 constant:1]];
	view.translatesAutoresizingMaskIntoConstraints = NO;
	return view;
}

NSView* OakCreateDividerImageView ()
{
	OakBackgroundFillView* divider = [[OakBackgroundFillView alloc] initWithFrame:NSZeroRect];
	divider.activeBackgroundImage = [NSImage imageNamed:@"Divider" inSameBundleAsClass:[OakBackgroundFillView class]];
	divider.translatesAutoresizingMaskIntoConstraints = NO;
	[divider setContentHuggingPriority:NSLayoutPriorityRequired forOrientation:NSLayoutConstraintOrientationHorizontal];

	NSView* res = [[NSView alloc] initWithFrame:NSZeroRect];
	[res addSubview:divider];

	[res addConstraint:[NSLayoutConstraint constraintWithItem:divider attribute:NSLayoutAttributeCenterX relatedBy:NSLayoutRelationEqual toItem:res attribute:NSLayoutAttributeCenterX multiplier:1 constant:0]];
	[res addConstraint:[NSLayoutConstraint constraintWithItem:divider attribute:NSLayoutAttributeCenterY relatedBy:NSLayoutRelationEqual toItem:res attribute:NSLayoutAttributeCenterY multiplier:1 constant:0]];
	[res addConstraint:[NSLayoutConstraint constraintWithItem:res attribute:NSLayoutAttributeWidth relatedBy:NSLayoutRelationEqual toItem:divider attribute:NSLayoutAttributeWidth multiplier:1 constant:0]];

	return res;
}

void OakSetupKeyViewLoop (NSArray* superviews, BOOL setFirstResponder)
{
	std::set<id> seen;
	for(NSView* candidate in superviews)
		seen.insert(candidate);

	NSMutableArray* views = [NSMutableArray new];
	for(NSView* view in superviews)
	{
		if([view isEqual:[NSNull null]])
			continue;

		[views addObject:view];
		NSView* subview = view;
		while((subview = subview.nextKeyView) && [subview isDescendantOf:view] && seen.insert(subview).second)
			[views addObject:subview];
	}

	if(views.count == 1)
	{
		[views.firstObject setNextKeyView:nil];
	}
	else
	{
		for(size_t i = 0; i < views.count; ++i)
			[views[i] setNextKeyView:views[(i + 1) % views.count]];
	}

	if(setFirstResponder)
	{
		if(NSView* view = views.firstObject)
			view.window.initialFirstResponder = view;
	}
}

void OakAddAutoLayoutViewsToSuperview (NSArray* views, NSView* superview)
{
	for(NSView* view in views)
	{
		if([view isEqual:[NSNull null]])
			continue;
		[view setTranslatesAutoresizingMaskIntoConstraints:NO];
		[superview addSubview:view];
	}
}

BOOL OakSetAccessibilityLabel (NSObject* element, NSObject* label)
{
	if(!(element = NSAccessibilityUnignoredDescendant(element)))
		return NO;

	NSString* attribute = NSAccessibilityDescriptionAttribute;
	if(![label isKindOfClass:NSString.class])
	{
		attribute = NSAccessibilityTitleUIElementAttribute;
		if(!(label = NSAccessibilityUnignoredDescendant(label)))
			return NO;
	}

	return [element accessibilitySetOverrideValue:label forAttribute:attribute];
}
