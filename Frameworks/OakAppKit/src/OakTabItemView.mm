#import "OakTabItemView.h"
#import "OakRolloverButton.h"
#import "NSImage Additions.h"

static NSString* kUserDefaultsTabItemMinWidthKey       = @"tabItemMinWidth";
static NSString* kUserDefaultsTabItemMaxWidthKey       = @"tabItemMaxWidth";
static NSString* kUserDefaultsTabItemLineBreakStyleKey = @"tabItemLineBreakStyle";

@interface OakTabBarStyle ()
{
	NSDictionary* _images;
	NSMutableDictionary* _activeTabTextStyles;
	NSMutableDictionary* _selectedTabTextStyles;
	NSMutableDictionary* _inactiveTabTextStyles;
}
@end

@implementation OakTabBarStyle
+ (instancetype)sharedInstance
{
	static OakTabBarStyle* sharedInstance = [self new];
	return sharedInstance;
}

+ (void)initialize
{
	[[NSUserDefaults standardUserDefaults] registerDefaults:@{
		kUserDefaultsTabItemMinWidthKey       : @(120),
		kUserDefaultsTabItemMaxWidthKey       : @(250),
		kUserDefaultsTabItemLineBreakStyleKey : @(NSLineBreakByTruncatingMiddle),
	}];
}

- (NSDictionary*)imagesForNames:(NSDictionary*)imageNames
{
	NSMutableDictionary* res = [NSMutableDictionary dictionary];
	for(NSString* state in imageNames)
	{
		NSMutableDictionary* dict = [NSMutableDictionary dictionary];
		for(NSString* key in imageNames[state])
			dict[key] = [NSImage imageNamed:imageNames[state][key] inSameBundleAsClass:[self class]];
		res[state] = dict;
	}
	return res;
}

- (NSDictionary*)mavericksImages
{
	NSDictionary* imageNames = @{
		@"tabBar" : @{
			@"AW_normal"   : @"AW InactiveTabBG",
			@"IW_normal"   : @"IW InactiveTabBG",
		},
		@"tabItemSelected" : @{
			@"AW_normal"   : @"AW ActiveTabFill",
			@"IW_normal"   : @"IW ActiveTabFill",
		},
		@"leftTabCap" : @{
			@"AW_normal"   : @"AW InactiveTabLeftCap",
			@"IW_normal"   : @"IW InactiveTabLeftCap",
		},
		@"leftTabCapSelected" : @{
			@"AW_normal"   : @"AW ActiveTabLeftCap",
			@"IW_normal"   : @"IW ActiveTabLeftCap",
		},
		@"rightTabCap" : @{
			@"AW_normal"   : @"AW InactiveTabRightCap",
			@"IW_normal"   : @"IW InactiveTabRightCap",
		},
		@"rightTabCapSelected" : @{
			@"AW_normal"   : @"AW ActiveTabRightCap",
			@"IW_normal"   : @"IW ActiveTabRightCap",
		},
		@"closeButton" : @{
			@"AW_normal"   : @"AW ActiveTabClose",
			@"AW_pressed"  : @"AW ActiveTabClosePressed",
			@"AW_rollover" : @"AW ActiveTabCloseRollover",
			@"IW_normal"   : @"IW ActiveTabClose",
			@"IW_pressed"  : @"IW ActiveTabClosePressed",
			@"IW_rollover" : @"IW ActiveTabCloseRollover",
		},
		@"closeButtonModified" : @{
			@"AW_normal"   : @"TabClose_Modified",
			@"AW_pressed"  : @"TabClose_ModifiedPressed",
			@"AW_rollover" : @"TabClose_ModifiedRollover",
		},
		@"overflowButton" : @{
			@"AW_normal"   : @"TabOverflowTemplate",
		},
	};
	return [self imagesForNames:imageNames];
}

- (NSDictionary*)yosemiteImages
{
	NSDictionary* imageNames = @{
		@"tabBar" : @{
			@"AW_normal"   : @"TabAWBackground",
			@"IW_normal"   : @"TabIWBackground",
		},
		@"tabItemSelected" : @{
			@"AW_normal"   : @"TabAWBackgroundSelected",
			@"IW_normal"   : @"TabIWBackgroundSelected",
		},
		@"leftTabCap" : @{
			@"AW_normal"   : @"TabAWDivider",
			@"IW_normal"   : @"TabIWDivider",
		},
		@"leftTabCapSelected" : @{
			@"AW_normal"   : @"TabAWDivider",
			@"IW_normal"   : @"TabIWDivider",
		},
		@"rightTabCap" : @{
			@"AW_normal"   : @"TabAWDivider",
			@"IW_normal"   : @"TabIWDivider",
		},
		@"rightTabCapSelected" : @{
			@"AW_normal"   : @"TabAWDivider",
			@"IW_normal"   : @"TabIWDivider",
		},
		@"closeButton" : @{
			@"AW_normal"   : @"TabCloseThin",
			@"AW_pressed"  : @"TabCloseThin_Pressed",
			@"AW_rollover" : @"TabCloseThin_Rollover",
		},
		@"closeButtonModified" : @{
			@"AW_normal"   : @"TabCloseThin_Modified",
			@"AW_pressed"  : @"TabCloseThin_ModifiedPressed",
			@"AW_rollover" : @"TabCloseThin_ModifiedRollover",
		},
		@"overflowButton" : @{
			@"AW_normal"   : @"TabOverflowThinTemplate",
		},
	};
	return [self imagesForNames:imageNames];
}

- (id)init
{
	if(self = [super init])
	{
		NSMutableParagraphStyle* parStyle = [NSMutableParagraphStyle new];
		[parStyle setLineBreakMode:(NSLineBreakMode)[[NSUserDefaults standardUserDefaults] integerForKey:kUserDefaultsTabItemLineBreakStyleKey]];

		_activeTabTextStyles = @{
			NSParagraphStyleAttributeName  : parStyle,
			NSFontAttributeName            : [NSFont systemFontOfSize:11],
			NSForegroundColorAttributeName : [NSColor colorWithCalibratedWhite:0.2 alpha:1],
		}.mutableCopy;

		_inactiveTabTextStyles = _activeTabTextStyles.mutableCopy;
		_inactiveTabTextStyles[NSForegroundColorAttributeName] = [NSColor colorWithCalibratedWhite:0.5 alpha:1];

		// MAC_OS_X_VERSION_10_10
		if([NSProcessInfo instancesRespondToSelector:@selector(isOperatingSystemAtLeastVersion:)] && [[NSProcessInfo processInfo] isOperatingSystemAtLeastVersion:{ 10, 10, 0 }])
		{
			_selectedTabTextStyles = _activeTabTextStyles.mutableCopy;
			_selectedTabTextStyles[NSForegroundColorAttributeName] = [NSColor blackColor];

			_images = [self yosemiteImages];
			_leftPadding  = -1;
			_rightPadding = 0;
		}
		else
		{
			_selectedTabTextStyles = _activeTabTextStyles.copy;

			_images = [self mavericksImages];
			_leftPadding  = 0;
			_rightPadding = -5;

			NSShadow* shadow = [NSShadow new];
			[shadow setShadowColor:[NSColor colorWithCalibratedWhite:1 alpha:0.5]];
			[shadow setShadowOffset:NSMakeSize(0, -1)];
			[shadow setShadowBlurRadius:1];

			NSDictionary* fontStyles = @{ NSShadowAttributeName : shadow, NSFontAttributeName : [NSFont boldSystemFontOfSize:11] };
			[_activeTabTextStyles addEntriesFromDictionary:fontStyles];
			[_inactiveTabTextStyles addEntriesFromDictionary:fontStyles];
		}

		NSImage* rightCapImage = _images[@"rightTabCap"][@"AW_normal"];
		_tabViewSpacing = rightCapImage ? -rightCapImage.size.width : 0;
		_minimumTabSize = [[NSUserDefaults standardUserDefaults] integerForKey:kUserDefaultsTabItemMinWidthKey];
		_maximumTabSize = [[NSUserDefaults standardUserDefaults] integerForKey:kUserDefaultsTabItemMaxWidthKey];
	}
	return self;
}

- (void)updateButton:(OakRolloverButton*)aButton forState:(NSString*)aState
{
	NSDictionary* images = _images[aState];
	if([images[@"AW_normal"] isTemplate])
		[[aButton cell] setBackgroundStyle:NSBackgroundStyleRaised];
	aButton.regularImage  = images[@"AW_normal"];
	aButton.pressedImage  = images[@"AW_pressed"];
	aButton.rolloverImage = images[@"AW_rollover"];
	aButton.inactiveRegularImage  = images[@"IW_normal"];
	aButton.inactivePressedImage  = images[@"IW_pressed"];
	aButton.inactiveRolloverImage = images[@"IW_rollover"];
}

- (void)updateView:(OakBackgroundFillView*)aView forState:(NSString*)aState
{
	aView.activeBackgroundImage   = _images[aState][@"AW_normal"];
	aView.inactiveBackgroundImage = _images[aState][@"IW_normal"];
}

// ================
// = Tab Bar View =
// ================

- (void)setupTabBarView:(OakBackgroundFillView*)aView
{
	[self updateView:aView forState:@"tabBar"];
	[aView setContentHuggingPriority:NSLayoutPriorityRequired forOrientation:NSLayoutConstraintOrientationVertical];
}

// =================
// = Tab Item View =
// =================

- (void)updateLeftCapView:(OakBackgroundFillView*)aView inSelectedTab:(BOOL)selected
{
	[self updateView:aView forState:(selected ? @"leftTabCapSelected" : @"leftTabCap")];
}

- (void)updateRightCapView:(OakBackgroundFillView*)aView inSelectedTab:(BOOL)selected
{
	[self updateView:aView forState:(selected ? @"rightTabCapSelected" : @"rightTabCap")];
}

- (void)updateTabItemView:(OakBackgroundFillView*)aView inSelectedTab:(BOOL)selected
{
	[self updateView:aView forState:(selected ? @"tabItemSelected" : nil)];
}

- (void)updateCloseButton:(OakRolloverButton*)aButton inSelectedTab:(BOOL)selected modified:(BOOL)modified
{
	[self updateButton:aButton forState:(modified ? @"closeButtonModified" : @"closeButton")];
}

- (void)updateOverflowButton:(OakRolloverButton*)aButton inSelectedTab:(BOOL)selected
{
	[self updateButton:aButton forState:@"overflowButton"];
}
@end

// ==================
// = OakTabItemView =
// ==================

@interface OakTabItemView ()
@property (nonatomic) OakBackgroundFillView* leftCapView;
@property (nonatomic) OakBackgroundFillView* rightCapView;
@property (nonatomic) NSTextField* textField;
@property (nonatomic) NSMutableArray* myConstraints;
@property (nonatomic) NSTrackingArea* trackingArea;
@property (nonatomic, getter = isMouseInside) BOOL mouseInside;
@end

@implementation OakTabItemView
- (id)initWithFrame:(NSRect)aRect title:(NSString*)aTitle modified:(BOOL)modified
{
	if(self = [super initWithFrame:aRect])
	{
		_title    = aTitle;
		_modified = modified;

		_leftCapView  = [[OakBackgroundFillView alloc] initWithFrame:NSZeroRect];
		_rightCapView = [[OakBackgroundFillView alloc] initWithFrame:NSZeroRect];

		_closeButton = [[OakRolloverButton alloc] initWithFrame:NSZeroRect];
		OakSetAccessibilityLabel(_closeButton, @"Close tab");
		_closeButton.disableWindowOrderingForFirstMouse = YES;

		_textField = OakCreateLabel(aTitle);
		[_textField setContentHuggingPriority:NSLayoutPriorityRequired forOrientation:NSLayoutConstraintOrientationHorizontal];
		[_textField setContentCompressionResistancePriority:NSLayoutPriorityDefaultLow+1 forOrientation:NSLayoutConstraintOrientationHorizontal];
		[self updateTextFieldTitle];

		[self updateStyle];
		OakAddAutoLayoutViewsToSuperview(@[ _leftCapView, _rightCapView, _textField, _closeButton ], self);
	}
	return self;
}

- (NSString*)toolTip                     { return _textField.toolTip; }
- (void)setToolTip:(NSString*)newToolTip { _textField.toolTip = newToolTip; }

- (void)updateStyle
{
	OakTabBarStyle* style = [OakTabBarStyle sharedInstance];
	[style updateLeftCapView:_leftCapView inSelectedTab:_selected];
	[style updateRightCapView:_rightCapView inSelectedTab:_selected];
	[style updateTabItemView:self inSelectedTab:_selected];
	[style updateCloseButton:_closeButton inSelectedTab:_selected modified:_modified];
	[style updateOverflowButton:_overflowButton inSelectedTab:_selected];
	_closeButton.hidden = !_mouseInside && !_modified;
}

- (NSRect)contentFrame
{
	NSRect bounds = NSInsetRect(self.bounds, 0, 2);
	bounds.origin.x   += _leftCapView.activeBackgroundImage.size.width;
	bounds.size.width -= _leftCapView.activeBackgroundImage.size.width + _rightCapView.activeBackgroundImage.size.width;
	return NSIntegralRectWithOptions(bounds, NSAlignAllEdgesInward);
}

// =========================================
// = Support dragging from inactive window =
// =========================================

- (NSView*)hitTest:(NSPoint)aPoint
{
	NSView* res = [super hitTest:aPoint];
	return res == _textField ? self : res;
}

- (BOOL)acceptsFirstMouse:(NSEvent*)anEvent
{
	return YES;
}

- (BOOL)shouldDelayWindowOrderingForEvent:(NSEvent*)anEvent
{
	return YES;
}

- (NSMenu*)menuForEvent:(NSEvent*)anEvent
{
	// Control-clicks are not sent to superview <rdar://20200363>
	return [[self superview] menuForEvent:anEvent];
}

// =========================================

- (void)updateConstraints
{
	[super updateConstraints];

	if(_myConstraints)
		[self removeConstraints:_myConstraints];
	_myConstraints = [NSMutableArray new];

	NSDictionary* views = @{ @"left" : _leftCapView, @"right" : _rightCapView, @"title" : _textField, @"close" : _closeButton, @"overflow" : (_overflowButton ?: [NSNull null]) };
	[_myConstraints addObjectsFromArray:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[left]-(3)-[close]-(>=3)-[title]-(>=3)-[right]-(0@450)-|" options:0 metrics:nil views:views]];
	[_myConstraints addObjectsFromArray:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[left(==right)]|" options:0 metrics:nil views:views]];
	[_myConstraints addObjectsFromArray:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[right]|" options:0 metrics:nil views:views]];
	[_myConstraints addObjectsFromArray:[NSLayoutConstraint constraintsWithVisualFormat:@"V:[close]-(5)-|" options:0 metrics:nil views:views]];
	[_myConstraints addObjectsFromArray:[NSLayoutConstraint constraintsWithVisualFormat:@"V:[title]-(5)-|" options:0 metrics:nil views:views]];

	NSLayoutConstraint* centerTitleConstraint = [NSLayoutConstraint constraintWithItem:_textField attribute:NSLayoutAttributeCenterX relatedBy:NSLayoutRelationEqual toItem:self attribute:NSLayoutAttributeCenterX multiplier:1 constant:0];
	centerTitleConstraint.priority = NSLayoutPriorityFittingSizeCompression-1;
	[_myConstraints addObject:centerTitleConstraint];

	if(_overflowButton)
	{
		[_myConstraints addObjectsFromArray:[NSLayoutConstraint constraintsWithVisualFormat:@"H:[title]-(>=3)-[overflow][right]" options:0 metrics:nil views:views]];
		[_myConstraints addObjectsFromArray:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[overflow]|" options:0 metrics:nil views:views]];
	}

	[self addConstraints:_myConstraints];
}

- (void)setVisibleCaps:(OakTabItemViewVisibleCaps)visibleCaps
{
	_visibleCaps = visibleCaps;
	_leftCapView.hidden  = visibleCaps == OakTabItemViewVisibleCapsNone || visibleCaps == OakTabItemViewVisibleCapsRight;
	_rightCapView.hidden = visibleCaps == OakTabItemViewVisibleCapsNone || visibleCaps == OakTabItemViewVisibleCapsLeft;
}

- (void)updateTrackingAreas
{
	[super updateTrackingAreas];
	if(_trackingArea)
		[self removeTrackingArea:_trackingArea];
	NSTrackingAreaOptions options = NSTrackingMouseEnteredAndExited|NSTrackingActiveAlways;
	if(self.mouseInside = NSMouseInRect([self convertPoint:[self.window mouseLocationOutsideOfEventStream] fromView:nil], [self visibleRect], [self isFlipped]))
		options |= NSTrackingAssumeInside;

	CGFloat x1 = 0.5 * _leftCapView.activeBackgroundImage.size.width;
	CGFloat x2 = NSWidth(self.bounds) - 0.5 * _rightCapView.activeBackgroundImage.size.width;
	_trackingArea = [[NSTrackingArea alloc] initWithRect:NSIntegralRectWithOptions(NSMakeRect(x1, 0, x2-x1, NSHeight(self.bounds)), NSAlignAllEdgesInward) options:options owner:self userInfo:nil];
	[self addTrackingArea:_trackingArea];
}

- (void)mouseEntered:(NSEvent*)anEvent
{
	self.mouseInside = YES;
}

- (void)mouseExited:(NSEvent*)anEvent
{
	self.mouseInside = NO;
}

- (void)setMouseInside:(BOOL)flag
{
	if(_mouseInside == flag)
		return;
	_mouseInside = flag;
	_closeButton.hidden = !_mouseInside && !_modified;
}

- (void)updateTextFieldTitle
{
	_textField.attributedStringValue = [[NSAttributedString alloc] initWithString:_title attributes:self.active ? (self.selected ? OakTabBarStyle.sharedInstance.selectedTabTextStyles : OakTabBarStyle.sharedInstance.activeTabTextStyles) : OakTabBarStyle.sharedInstance.inactiveTabTextStyles];
}

- (void)setActive:(BOOL)flag
{
	[super setActive:flag];
	[self updateTextFieldTitle];
}

- (void)setTitle:(NSString*)aTitle
{
	if(_title == aTitle || [_title isEqualToString:aTitle])
		return;
	_title = aTitle;
	[self updateTextFieldTitle];
}

- (void)setModified:(BOOL)flag
{
	if(_modified == flag)
		return;
	_modified = flag;
	[self updateStyle];
}

- (void)setSelected:(BOOL)flag
{
	if(_selected == flag)
		return;
	_selected = flag;
	[self updateStyle];
	[self updateTextFieldTitle];
}

- (void)setShowOverflowButton:(BOOL)flag
{
	if(_showOverflowButton == flag)
		return;
	_showOverflowButton = flag;

	if(_showOverflowButton)
	{
		_overflowButton = [[OakRolloverButton alloc] initWithFrame:NSZeroRect];
		OakSetAccessibilityLabel(_overflowButton, @"Show tab overflow menu");

		[_overflowButton sendActionOn:NSLeftMouseDownMask];
		[self updateStyle];

		OakAddAutoLayoutViewsToSuperview(@[ _overflowButton ], self);
	}
	else
	{
		[_overflowButton removeFromSuperview];
		_overflowButton = nil;
	}
	self.needsUpdateConstraints = YES;
}

- (void)drawRect:(NSRect)aRect
{
	// Ideally we would use NSMaxX(_leftCapView.frame)
	// and NSMinX(_rightCapView.frame) but presumably
	// because we are layer backed, we might be asked to
	// draw before these subviews have been positioned.

	NSRect bounds = self.bounds;
	bounds.origin.x   += _leftCapView.activeBackgroundImage.size.width;
	bounds.size.width -= _leftCapView.activeBackgroundImage.size.width + _rightCapView.activeBackgroundImage.size.width;
	[super drawRect:NSIntersectionRect(aRect, bounds)];
}

// =================
// = Accessibility =
// =================

- (BOOL)accessibilityIsIgnored
{
	return NO;
}

- (NSSet*)myAccessibilityAttributeNames
{
	static NSSet* set = [NSSet setWithArray:@[
		NSAccessibilityRoleAttribute,
		NSAccessibilityRoleDescriptionAttribute,
		// radio button
		NSAccessibilityFocusedAttribute,
		NSAccessibilityTitleAttribute,
		NSAccessibilityValueAttribute,
		NSAccessibilityHelpAttribute,
	]];
	return set;
}

- (NSArray*)accessibilityAttributeNames
{
	static NSArray* attributes = [[[self myAccessibilityAttributeNames] setByAddingObjectsFromArray:[super accessibilityAttributeNames]] allObjects];
	return attributes;
}

- (id)accessibilityAttributeValue:(NSString*)attribute
{
	if([attribute isEqualToString:NSAccessibilityRoleAttribute])
		return NSAccessibilityRadioButtonRole;
	else if([attribute isEqualToString:NSAccessibilityRoleDescriptionAttribute])
		return @"tab";
	else if([attribute isEqualToString:NSAccessibilityTitleAttribute])
		return _modified ? [_title stringByAppendingString:@" (modified)"] : _title;
	else if([attribute isEqualToString:NSAccessibilityValueAttribute])
		return @(_selected);
	else if([attribute isEqualToString:NSAccessibilityHelpAttribute])
		return self.toolTip;
	else
		return [super accessibilityAttributeValue:attribute];
}

- (BOOL)accessibilityIsAttributeSettable:(NSString*)attribute
{
	if([[self myAccessibilityAttributeNames] containsObject:attribute])
		return NO;
	return [super accessibilityIsAttributeSettable:attribute];
}

- (NSSet*)myAccessibilityActionNames
{
	static NSSet* set = [NSSet setWithArray:@[
		NSAccessibilityPressAction,
		NSAccessibilityShowMenuAction,
	]];
	return set;
}

- (NSArray*)accessibilityActionNames
{
	static NSArray* actions = [[[self myAccessibilityActionNames] setByAddingObjectsFromArray:[super accessibilityAttributeNames]] allObjects];
	return actions;
}

- (NSString*)accessibilityActionDescription:(NSString*)action
{
	if([[self myAccessibilityActionNames] containsObject:action])
		return NSAccessibilityActionDescription(action);
	return [super accessibilityActionDescription:action];
}

- (void)accessibilityPerformAction:(NSString*)action
{
	if([action isEqualToString:NSAccessibilityPressAction])
		[[self superview] performSelector:@selector(trySelectTabForView:) withObject:self];
	else if([action isEqualToString:NSAccessibilityShowMenuAction])
	{
		NSMenu* menu = [[self superview] performSelector:@selector(menuForView:) withObject:self];
		[menu popUpMenuPositioningItem:nil atLocation:NSZeroPoint inView:self];
	}
	else
	{
		[super accessibilityPerformAction:action];
	}
}
@end
