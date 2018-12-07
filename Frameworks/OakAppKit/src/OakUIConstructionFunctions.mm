#import "OakUIConstructionFunctions.h"
#import "NSColor Additions.h"
#import "NSImage Additions.h"

NSFont* OakStatusBarFont ()
{
	return [NSFont messageFontOfSize:[NSUserDefaults.standardUserDefaults integerForKey:@"statusBarFontSize"] ?: 12];
}

NSFont* OakControlFont ()
{
	return [NSFont messageFontOfSize:0];
}

NSTextField* OakCreateLabel (NSString* label, NSFont* font, NSTextAlignment alignment, NSLineBreakMode lineBreakMode)
{
	// This was introduced in 10.12 but does not appear to use controlTextColor until 10.14, which is required for proper highlight when used in a table view
	if(@available(macos 10.14, *))
	{
		NSTextField* res = [NSTextField labelWithString:label];
		[[res cell] setLineBreakMode:lineBreakMode];
		res.alignment = alignment;
		if(font)
			res.font = font;
		return res;
	}

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
	if(@available(macos 10.12, *))
	{
		NSButton* res = [NSButton checkboxWithTitle:(label ?: @"") target:nil action:nil];
		// When we have a row that only contains checkboxes (e.g. Find options), nothing restrains the height of that row
		[res setContentHuggingPriority:NSLayoutPriorityDefaultHigh forOrientation:NSLayoutConstraintOrientationVertical];
		return res;
	}

	NSButton* res = [[NSButton alloc] initWithFrame:NSZeroRect];
	[res setContentHuggingPriority:NSLayoutPriorityDefaultHigh forOrientation:NSLayoutConstraintOrientationVertical];
	res.buttonType = NSSwitchButton;
	res.font       = OakControlFont();
	res.title      = label;
	return res;
}

NSButton* OakCreateButton (NSString* label, NSBezelStyle bezel)
{
	if(@available(macos 10.12, *))
	{
		NSButton* res = [NSButton buttonWithTitle:label target:nil action:nil];
		if(bezel != NSRoundedBezelStyle)
			res.bezelStyle = bezel;
		return res;
	}

	NSButton* res = [[NSButton alloc] initWithFrame:NSZeroRect];
	res.bezelStyle = bezel;
	res.buttonType = NSMomentaryPushInButton;
	res.font       = OakControlFont();
	res.title      = label;
	return res;
}

NSPopUpButton* OakCreatePopUpButton (BOOL pullsDown, NSString* initialItemTitle, NSView* labelView)
{
	NSPopUpButton* res = [[NSPopUpButton alloc] initWithFrame:NSZeroRect pullsDown:pullsDown];
	if(initialItemTitle)
		[[res cell] setMenuItem:[[NSMenuItem alloc] initWithTitle:initialItemTitle action:NULL keyEquivalent:@""]];
	if(labelView)
		res.accessibilityTitleUIElement = labelView;
	return res;
}

NSPopUpButton* OakCreateActionPopUpButton (BOOL bordered)
{
	NSPopUpButton* res = [NSPopUpButton new];
	res.pullsDown = YES;
	res.bordered = bordered;

	NSMenuItem* item = [NSMenuItem new];
	item.title = @"";
	item.image = [NSImage imageNamed:NSImageNameActionTemplate];
	[item.image setSize:NSMakeSize(14, 14)];

	[[res cell] setUsesItemFromMenu:NO];
	[[res cell] setMenuItem:item];
	res.accessibilityLabel = @"Actions";

	return res;
}

NSComboBox* OakCreateComboBox (NSView* labelView)
{
	NSComboBox* res = [[NSComboBox alloc] initWithFrame:NSZeroRect];
	res.font = OakControlFont();
	res.accessibilityTitleUIElement = labelView;
	return res;
}

OakRolloverButton* OakCreateCloseButton (NSString* accessibilityLabel)
{
	OakRolloverButton* closeButton = [[OakRolloverButton alloc] initWithFrame:NSZeroRect];
	closeButton.regularImage  = [NSImage imageNamed:@"CloseTemplate"         inSameBundleAsClass:[OakRolloverButton class]];
	closeButton.pressedImage  = [NSImage imageNamed:@"ClosePressedTemplate"  inSameBundleAsClass:[OakRolloverButton class]];
	closeButton.rolloverImage = [NSImage imageNamed:@"CloseRolloverTemplate" inSameBundleAsClass:[OakRolloverButton class]];

	closeButton.accessibilityLabel = accessibilityLabel;
	return closeButton;
}

// =========================
// = OakBackgroundFillView =
// =========================

@implementation OakBackgroundFillView
{
	NSView* _visualEffectBackgroundView;
	id _activeBackgroundValue;
	id _inactiveBackgroundValue;
}

- (instancetype)initWithFrame:(NSRect)aRect
{
	if(self = [super initWithFrame:aRect])
	{
		_style = OakBackgroundFillViewStyleNone;
		[self setWantsLayer:YES]; // required by NSVisualEffectBlendingModeWithinWindow
	}
	return self;
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

	self.active = ([newWindow styleMask] & NSWindowStyleMaskFullScreen) || [newWindow isMainWindow] || [newWindow isKeyWindow];
}

- (void)windowDidChangeMainOrKey:(NSNotification*)aNotification
{
	self.active = ([self.window styleMask] & NSWindowStyleMaskFullScreen) || [self.window isMainWindow] || [self.window isKeyWindow];
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

- (void)setStyle:(OakBackgroundFillViewStyle)aStyle
{
	if(_style == aStyle)
		return;

	_style = aStyle;
	[self updateBackgroundStyle];
	self.needsDisplay = YES;
}

- (void)updateBackgroundStyle
{
	if(_visualEffectBackgroundView)
	{
		[_visualEffectBackgroundView removeFromSuperview];
		_visualEffectBackgroundView = nil;
	}

	if(self.style == OakBackgroundFillViewStyleHeader)
	{
		if(@available(macos 10.14, *))
		{
			NSVisualEffectView* effectView = [[NSVisualEffectView alloc] initWithFrame:[self bounds]];
			effectView.material     = NSVisualEffectMaterialHeaderView; // MAC_OS_X_VERSION_10_14
			effectView.blendingMode = NSVisualEffectBlendingModeBehindWindow;
			_visualEffectBackgroundView = effectView;
			[_visualEffectBackgroundView setAutoresizingMask:NSViewWidthSizable|NSViewHeightSizable];
			[self addSubview:_visualEffectBackgroundView positioned:NSWindowBelow relativeTo:nil];
		}
		else
		{
			self.activeBackgroundGradient   = [[NSGradient alloc] initWithStartingColor:[NSColor colorWithCalibratedWhite:0.915 alpha:1] endingColor:[NSColor colorWithCalibratedWhite:0.760 alpha:1]];
			self.inactiveBackgroundGradient = [[NSGradient alloc] initWithStartingColor:[NSColor colorWithCalibratedWhite:0.915 alpha:1] endingColor:[NSColor colorWithCalibratedWhite:0.915 alpha:1]];
		}
	}

	if(self.style == OakBackgroundFillViewStyleDivider)
	{
		if(@available(macos 10.14, *))
		{
			self.activeBackgroundColor   = [NSColor separatorColor];
			self.inactiveBackgroundColor = nil;
		}
		else
		{
			self.activeBackgroundColor   = [NSColor colorWithCalibratedWhite:0.500 alpha:1];
			self.inactiveBackgroundColor = [NSColor colorWithCalibratedWhite:0.750 alpha:1];
		}
	}

	if(self.style == OakBackgroundFillViewStyleDarkDivider)
	{
		self.activeBackgroundColor = [NSColor tmDarkDividerColor];
	}
}

- (void)drawRect:(NSRect)aRect
{
	if(_visualEffectBackgroundView != nil)
	{
		[super drawRect:aRect];
		return;
	}

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
		NSRectFillUsingOperation(aRect, NSCompositingOperationSourceOver);
	}
	else if([value isKindOfClass:[NSColor class]])
	{
		NSColor* color = value;
		[color set];
		NSRectFill(aRect);
	}
}
@end

OakBackgroundFillView* OakCreateVerticalLine (OakBackgroundFillViewStyle style)
{
	OakBackgroundFillView* view = [[OakBackgroundFillView alloc] initWithFrame:NSZeroRect];
	view.style = style;
	[view addConstraint:[NSLayoutConstraint constraintWithItem:view attribute:NSLayoutAttributeWidth relatedBy:NSLayoutRelationEqual toItem:nil attribute:NSLayoutAttributeNotAnAttribute multiplier:1 constant:1]];
	view.translatesAutoresizingMaskIntoConstraints = NO;
	return view;
}

OakBackgroundFillView* OakCreateHorizontalLine (OakBackgroundFillViewStyle style)
{
	OakBackgroundFillView* view = [[OakBackgroundFillView alloc] initWithFrame:NSZeroRect];
	view.style = style;
	[view addConstraint:[NSLayoutConstraint constraintWithItem:view attribute:NSLayoutAttributeHeight relatedBy:NSLayoutRelationEqual toItem:nil attribute:NSLayoutAttributeNotAnAttribute multiplier:1 constant:1]];
	view.translatesAutoresizingMaskIntoConstraints = NO;
	return view;
}

NSView* OakCreateDividerImageView ()
{
	NSBox* box = [[NSBox alloc] initWithFrame:NSZeroRect];
	box.boxType = NSBoxSeparator;

	NSDictionary* views = @{
		@"box": box,
	};

	NSView* contentView = [[NSView alloc] initWithFrame:NSZeroRect];
	OakAddAutoLayoutViewsToSuperview(views.allValues, contentView);

	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[box(==1)]|" options:0 metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:[box(==16)]" options:0 metrics:nil views:views]];
	[contentView addConstraint:[NSLayoutConstraint constraintWithItem:box attribute:NSLayoutAttributeCenterY relatedBy:NSLayoutRelationEqual toItem:contentView attribute:NSLayoutAttributeCenterY multiplier:1 constant:0]];

	return contentView;
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
