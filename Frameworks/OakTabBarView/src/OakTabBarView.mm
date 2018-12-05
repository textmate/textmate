#import "OakTabBarView.h"
#import <OakAppKit/NSImage Additions.h>
#import <OakAppKit/OakUIConstructionFunctions.h>
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/NSMenuItem Additions.h>
#import <OakAppKit/OakFileIconImage.h>
#import <OakFoundation/OakFoundation.h>
#import <oak/misc.h>

static NSString* kUserDefaultsTabItemMinWidthKey = @"tabItemMinWidth";
static NSString* kUserDefaultsTabItemMaxWidthKey = @"tabItemMaxWidth";

static void DisableImplicitAnimationForBlock (void(^handler)())
{
	[NSAnimationContext runAnimationGroup:^(NSAnimationContext* context){
		context.allowsImplicitAnimation = NO;
		handler();
	} completionHandler:^{
	}];
}

// ==============
// = OakTabItem =
// ==============

static NSString* const OakTabItemPasteboardType = @"com.macromates.TextMate.tabItem";

@class OakTabView;

@interface OakTabItem () <NSPasteboardWriting>
@property (nonatomic, getter = isSelected) BOOL selected;
@property (nonatomic) CGFloat fittingWidth;
@property (nonatomic) BOOL needsLayout;
@property (nonatomic, weak) OakTabView* tabView;
@end

@implementation OakTabItem
+ (instancetype)tabItemWithTitle:(NSString*)aTitle path:(NSString*)aPath identifier:(NSString*)anIdentifier modified:(BOOL)flag
{
	return [[OakTabItem alloc] initWithTitle:aTitle path:aPath identifier:anIdentifier modified:flag];
}

+ (instancetype)tabItemFromPasteboardItem:(NSPasteboardItem*)pasteboardItem
{
	NSDictionary* plist = [pasteboardItem propertyListForType:OakTabItemPasteboardType];
	return plist ? [[OakTabItem alloc] initWithTitle:plist[@"title"] path:plist[@"path"] identifier:plist[@"identifier"] modified:[plist[@"modified"] boolValue]] : nil;
}

- (instancetype)initWithTitle:(NSString*)title path:(NSString*)path identifier:(NSString*)identifier modified:(BOOL)modified
{
	if(self = [super init])
	{
		_title       = title;
		_path        = path;
		_identifier  = identifier;
		_modified    = modified;
		_needsLayout = YES;
	}
	return self;
}

- (NSString*)description
{
	return [NSString stringWithFormat:@"<%@: %@>", [self class], _title];
}

- (void)setTitle:(NSString*)newTitle
{
	if(_title == newTitle || [_title isEqualToString:newTitle])
		return;
	_title = newTitle;
	self.needsLayout = YES;
}

- (NSArray*)writableTypesForPasteboard:(NSPasteboard*)aPasteboard
{
	return OakIsEmptyString(_path) ? @[ OakTabItemPasteboardType ] : @[ OakTabItemPasteboardType, (NSString*)kUTTypeFileURL ];
}

- (NSPasteboardWritingOptions)writingOptionsForType:(NSString*)aType pasteboard:(NSPasteboard*)aPasteboard
{
	return 0;
}

- (id)pasteboardPropertyListForType:(NSString*)aType
{
	if([aType isEqualToString:(NSString*)kUTTypeFileURL])
		return [NSURL fileURLWithPath:_path].absoluteString;

	NSMutableDictionary* dict = [NSMutableDictionary dictionary];
	dict[@"identifier"] = _identifier;
	dict[@"title"]      = _title;
	dict[@"path"]       = OakNotEmptyString(_path) ? _path : nil;
	dict[@"modified"]   = _modified ? @YES : nil;
	return dict;
}
@end

// ====================
// = OakAnimatorProxy =
// ====================

@interface OakAnimatorProxy : NSProxy
@property (nonatomic) id realObject;
@end

@implementation OakAnimatorProxy
- (instancetype)initWithRealObject:(id)realObject
{
	_realObject = realObject;
	return self;
}

- (void)forwardInvocation:(NSInvocation*)anInvocation
{
	[NSAnimationContext runAnimationGroup:^(NSAnimationContext* context){
		context.allowsImplicitAnimation = YES;
		[anInvocation setTarget:_realObject];
		[anInvocation invoke];
	} completionHandler:^{
	}];
}

- (NSMethodSignature*)methodSignatureForSelector:(SEL)aSelector
{
	return [_realObject methodSignatureForSelector:aSelector];
}
@end

// ==========
// = OakBox =
// ==========

@interface OakBox : NSView
@property (nonatomic) NSColor* fillColor;
@end

@implementation OakBox
- (BOOL)wantsUpdateLayer
{
	return YES;
}

- (void)updateLayer
{
	self.layer.backgroundColor = _fillColor.CGColor;
}

- (void)drawRect:(NSRect)aRect
{
	[_fillColor set];
	[NSBezierPath fillRect:aRect];
}

- (void)setFillColor:(NSColor*)color
{
	_fillColor = color;
	self.needsDisplay = YES;
}
@end

// ==============
// = OakTabView =
// ==============

@class OakTabBarView;
@class OakTabFrame;

@interface OakTabView : NSView <NSAccessibilityRadioButton>
@property (nonatomic, weak) OakTabBarView* tabBarView;
@property (nonatomic) OakTabItem* tabItem;

@property (nonatomic, weak) id target;
@property (nonatomic) SEL action;
@property (nonatomic) SEL doubleAction;
@property (nonatomic) SEL dragAction;

@property (nonatomic, getter = isSelected) BOOL selected;
@property (nonatomic, getter = isModified) BOOL modified;
@property (nonatomic, getter = isOverflowButtonVisible) BOOL overflowButtonVisible;

@property (nonatomic, getter = isMouseInside) BOOL mouseInside;
@property (nonatomic, getter = isVoiceOverEnabled) BOOL voiceOverEnabled;

@property (nonatomic, readonly) BOOL shouldShowCloseButton;
@property (nonatomic) NSArray<NSLayoutConstraint*>* overflowButtonConstraints;

@property (nonatomic) OakBox* backgroundView;
@property (nonatomic) OakBox* topBorderView;
@property (nonatomic) OakBox* leftBorderView;
@property (nonatomic) OakRolloverButton* closeButton;
@property (nonatomic) OakRolloverButton* overflowButton;
@property (nonatomic) NSTextField* textField;

@property (nonatomic) NSTrackingArea* trackingArea;
@property (nonatomic) NSPoint mouseDownLocation;
@end

@interface OakTabBarView () <NSAccessibilityGroup, NSDraggingSource>
{
	NSInteger _minimumTabSize;
	NSInteger _maximumTabSize;

	NSInteger _tag;
}
@property (nonatomic, readwrite) NSMutableArray<OakTabItem*>* tabItems;
@property (nonatomic) NSInteger draggedTabIndex;
@property (nonatomic) NSInteger dropTabAtIndex;
@property (nonatomic) NSInteger freezeTabFramesLeftOfIndex;
@property (nonatomic, readonly) OakTabView* backgroundView;
@property (nonatomic) NSButton* createNewTabButton;
@property (nonatomic) NSTrackingArea* trackingArea;
@property (nonatomic, getter = isDragging) BOOL dragging;
- (void)didClickCloseButtonForTabView:(OakTabView*)tabView;
- (void)didClickOverflorButtonForTabView:(OakTabView*)tabView;
- (NSMenu*)menuForTabView:(OakTabView*)tabView withEvent:(NSEvent*)anEvent;

@property (nonatomic) NSArray<OakTabFrame*>* fromLayout;
@property (nonatomic) NSArray<OakTabFrame*>* toLayout;
@property (nonatomic) NSArray<OakTabFrame*>* currentLayout;
@property (nonatomic) CGFloat tabLayoutAnimationProgressOffset;
@property (nonatomic) CGFloat tabLayoutAnimationProgress;
@end

static void* kOakTabViewVoiceOverContext = &kOakTabViewVoiceOverContext;
static void* kOakTabViewTitleContext     = &kOakTabViewTitleContext;
static void* kOakTabViewPathContext      = &kOakTabViewPathContext;
static void* kOakTabViewModifiedContext  = &kOakTabViewModifiedContext;
static void* kOakTabViewSelectedContext  = &kOakTabViewSelectedContext;

@implementation OakTabView
+ (NSSet*)keyPathsForValuesAffectingCloseButtonAlphaValue
{
	return [NSSet setWithObjects:@"tabItem", @"mouseInside", @"modified", @"voiceOverEnabled", nil];
}

- (instancetype)animator
{
	return (OakTabView*)[[OakAnimatorProxy alloc] initWithRealObject:super.animator];
}

- (instancetype)initWithFrame:(NSRect)frameRect tabItem:(OakTabItem*)tabItem parent:(OakTabBarView*)tabBarView
{
	if(self = [super initWithFrame:frameRect])
	{
		self.accessibilityRole            = NSAccessibilityRadioButtonRole;
		self.accessibilityRoleDescription = @"Tab";

		_tabBarView = tabBarView;

		_backgroundView = [[OakBox alloc] initWithFrame:NSZeroRect];
		_topBorderView  = [[OakBox alloc] initWithFrame:NSZeroRect];
		_leftBorderView = [[OakBox alloc] initWithFrame:NSZeroRect];

		_textField = OakCreateLabel();

		DisableImplicitAnimationForBlock(^{
			_backgroundView.fillColor  = NSColor.textColor;
			_backgroundView.alphaValue = _selected ? 0 : (_mouseInside ? 0.2 : 0.1);
			_topBorderView.fillColor   = [NSColor colorWithCalibratedWhite:NSBlack alpha:0.25];
			_leftBorderView.fillColor  = [NSColor colorWithCalibratedWhite:NSBlack alpha:0.25];

			_textField.textColor  = NSColor.secondaryLabelColor;
			_textField.alphaValue = _selected ? 1 : 0.5;
		});

		NSArray<NSView*>* subviews = @[ _backgroundView, _topBorderView, _leftBorderView, _textField, self.closeButton, self.overflowButton ];
		for(NSView* view in subviews)
		{
			view.translatesAutoresizingMaskIntoConstraints = NO;
			[self addSubview:view positioned:NSWindowAbove relativeTo:nil];
		}

		self.overflowButton.hidden = YES;

		if(@available(macos 10.13, *))
			[NSWorkspace.sharedWorkspace addObserver:self forKeyPath:@"voiceOverEnabled" options:NSKeyValueObservingOptionInitial context:kOakTabViewVoiceOverContext];

		NSDictionary* views = @{
			@"background": _backgroundView,
			@"topBorder":  _topBorderView,
			@"leftBorder": _leftBorderView,

			@"close":      self.closeButton,
			@"title":      self.textField,
			@"overflow":   self.overflowButton,
		};

		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[leftBorder(==1@75)][topBorder]|" options:NSLayoutFormatAlignAllTop metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[topBorder(==1@75)][background]|" options:NSLayoutFormatAlignAllLeft|NSLayoutFormatAlignAllRight metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[leftBorder]|" options:0 metrics:nil views:views]];

		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(3@53)-[close]-(>=3@53)-[title]-(>=6@53)-|" options:0 metrics:nil views:views]];

		_overflowButtonConstraints = [NSLayoutConstraint constraintsWithVisualFormat:@"H:[title]-(>=3@53)-[overflow]" options:0 metrics:nil views:views];
		[NSLayoutConstraint deactivateConstraints:_overflowButtonConstraints];
		[self addConstraints:_overflowButtonConstraints];

		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:[overflow]|" options:0 metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:[close]-(4)-|" options:0 metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:[title]-(3)-|" options:0 metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[overflow]|" options:0 metrics:nil views:views]];

		[_textField setContentHuggingPriority:NSLayoutPriorityRequired forOrientation:NSLayoutConstraintOrientationHorizontal];
		[_textField setContentCompressionResistancePriority:NSLayoutPriorityFittingSizeCompression+2 forOrientation:NSLayoutConstraintOrientationHorizontal];

		NSLayoutConstraint* centerTitleConstraint = [NSLayoutConstraint constraintWithItem:_textField attribute:NSLayoutAttributeCenterX relatedBy:NSLayoutRelationEqual toItem:self attribute:NSLayoutAttributeCenterX multiplier:1 constant:0];
		centerTitleConstraint.priority = NSLayoutPriorityFittingSizeCompression+1;
		[self addConstraint:centerTitleConstraint];

		self.tabItem = tabItem;
	}
	return self;
}

- (void)dealloc
{
	self.tabItem = nil;
	if(@available(macos 10.13, *))
		[NSWorkspace.sharedWorkspace removeObserver:self forKeyPath:@"voiceOverEnabled" context:kOakTabViewVoiceOverContext];
}

- (void)setHidden:(BOOL)flag
{
	if(self.isHidden == flag)
		return;
	[super setHidden:flag];

	if(!flag)
	{
		DisableImplicitAnimationForBlock(^{
			self.mouseInside = NSMouseInRect([self convertPoint:self.window.mouseLocationOutsideOfEventStream fromView:nil], self.visibleRect, self.isFlipped);
		});
	}
}

- (void)setMouseInside:(BOOL)flag
{
	if(_mouseInside == flag)
		return;

	_mouseInside = flag;
	if(!self.isSelected && _tabItem)
		self.backgroundView.alphaValue = _mouseInside ? 0.2 : 0.1;
}

- (void)setModified:(BOOL)flag
{
	_modified = flag;
	[self updateCloseButtonImage];
}

- (void)setSelected:(BOOL)flag
{
	if(_selected == flag)
		return;

	_selected = flag;

	self.textField.alphaValue      = _selected ? 1 : 0.5;
	self.backgroundView.alphaValue = _selected ? 0 : (_mouseInside ? 0.2 : 0.1);
	self.topBorderView.alphaValue  = _selected ? 0 : 1;
}

- (void)setTabItem:(OakTabItem*)tabItem
{
	[_tabItem removeObserver:self forKeyPath:@"title"    context:kOakTabViewTitleContext];
	[_tabItem removeObserver:self forKeyPath:@"path"     context:kOakTabViewPathContext];
	[_tabItem removeObserver:self forKeyPath:@"modified" context:kOakTabViewModifiedContext];
	[_tabItem removeObserver:self forKeyPath:@"selected" context:kOakTabViewSelectedContext];

	_tabItem = tabItem;

	[_tabItem addObserver:self forKeyPath:@"title"    options:NSKeyValueObservingOptionInitial context:kOakTabViewTitleContext];
	[_tabItem addObserver:self forKeyPath:@"path"     options:NSKeyValueObservingOptionInitial context:kOakTabViewPathContext];
	[_tabItem addObserver:self forKeyPath:@"modified" options:NSKeyValueObservingOptionInitial context:kOakTabViewModifiedContext];
	[_tabItem addObserver:self forKeyPath:@"selected" options:NSKeyValueObservingOptionInitial context:kOakTabViewSelectedContext];

	if(_tabItem)
	{
		self.accessibilityElement                     = YES;
		self.closeButton.cell.accessibilityElement    = YES;
		self.overflowButton.cell.accessibilityElement = YES;
	}
	else
	{
		self.accessibilityElement                     = NO;
		self.closeButton.cell.accessibilityElement    = NO;
		self.overflowButton.cell.accessibilityElement = NO;

		self.textField.alphaValue      = 0.0;
		self.backgroundView.alphaValue = 0.1;
		self.topBorderView.alphaValue  = 1;
		self.overflowButtonVisible     = NO;
	}
}

- (void)updateCloseButtonImage
{
	if(_modified)
	{
		_closeButton.regularImage  = [NSImage imageNamed:@"TabCloseThin_Modified_Template"         inSameBundleAsClass:self];
		_closeButton.pressedImage  = [NSImage imageNamed:@"TabCloseThin_ModifiedPressed_Template"  inSameBundleAsClass:self];
		_closeButton.rolloverImage = [NSImage imageNamed:@"TabCloseThin_ModifiedRollover_Template" inSameBundleAsClass:self];
	}
	else
	{
		_closeButton.regularImage  = [NSImage imageNamed:@"TabCloseThinTemplate"           inSameBundleAsClass:self];
		_closeButton.pressedImage  = [NSImage imageNamed:@"TabCloseThin_Pressed_Template"  inSameBundleAsClass:self];
		_closeButton.rolloverImage = [NSImage imageNamed:@"TabCloseThin_Rollover_Template" inSameBundleAsClass:self];
	}
}

- (OakRolloverButton*)closeButton
{
	if(!_closeButton)
	{
		_closeButton = [[OakRolloverButton alloc] initWithFrame:NSZeroRect];

		_closeButton.accessibilityLabel                 = @"Close tab";
		_closeButton.action                             = @selector(didClickCloseButton:);
		_closeButton.target                             = self;
		_closeButton.disableWindowOrderingForFirstMouse = YES;

		[self updateCloseButtonImage];
	}
	return _closeButton;
}

- (OakRolloverButton*)overflowButton
{
	if(!_overflowButton)
	{
		_overflowButton = [[OakRolloverButton alloc] initWithFrame:NSZeroRect];
		[_overflowButton sendActionOn:NSEventMaskLeftMouseDown];

		_overflowButton.accessibilityLabel = @"Show tab overflow menu";
		_overflowButton.action             = @selector(didClickOverflorButton:);
		_overflowButton.target             = self;
		_overflowButton.regularImage       = [NSImage imageNamed:@"TabOverflowThinTemplate" inSameBundleAsClass:self];
	}
	return _overflowButton;
}

- (CGFloat)closeButtonAlphaValue
{
	return _tabItem && (_mouseInside || _modified || _voiceOverEnabled) ? 1 : 0;
}

- (void)setOverflowButtonVisible:(BOOL)flag
{
	_overflowButtonVisible = flag;
	self.overflowButton.hidden = !flag;
	if(flag)
			[NSLayoutConstraint activateConstraints:_overflowButtonConstraints];
	else	[NSLayoutConstraint deactivateConstraints:_overflowButtonConstraints];
}

- (NSString*)accessibilityLabel { return self.tabItem.isModified ? [self.tabItem.title stringByAppendingString:@" (modified)"] : self.tabItem.title; }
- (NSNumber*)accessibilityValue { return @(self.isSelected); }

- (BOOL)accessibilityPerformPress
{
	return [NSApp sendAction:self.action to:self.target from:self];
}

- (BOOL)accessibilityPerformShowMenu
{
	[[self menuForEvent:NSApp.currentEvent] popUpMenuPositioningItem:nil atLocation:NSZeroPoint inView:self];
	return YES;
}

- (void)observeValueForKeyPath:(NSString*)keyPath ofObject:(id)someObject change:(NSDictionary*)changes context:(void*)context
{
	if(context == kOakTabViewVoiceOverContext)
	{
		if(@available(macos 10.13, *))
			self.voiceOverEnabled = NSWorkspace.sharedWorkspace.isVoiceOverEnabled;
	}
	else if(context == kOakTabViewTitleContext)
		self.textField.stringValue = self.tabItem.title;
	else if(context == kOakTabViewPathContext)
		self.toolTip = self.tabItem.path.stringByAbbreviatingWithTildeInPath;
	else if(context == kOakTabViewModifiedContext)
		self.modified = self.tabItem.isModified;
	else if(context == kOakTabViewSelectedContext)
		self.selected = self.tabItem.isSelected;
}

- (void)viewWillMoveToWindow:(NSWindow*)newWindow
{
	if(self.window)
	{
		// Break retain-cycle when view is removed from window
		[self.closeButton unbind:@"alphaValue"];

		[[NSNotificationCenter defaultCenter] removeObserver:self name:NSWindowDidBecomeMainNotification object:self.window];
		[[NSNotificationCenter defaultCenter] removeObserver:self name:NSWindowDidResignMainNotification object:self.window];
		[[NSNotificationCenter defaultCenter] removeObserver:self name:NSWindowDidBecomeKeyNotification object:self.window];
		[[NSNotificationCenter defaultCenter] removeObserver:self name:NSWindowDidResignKeyNotification object:self.window];
	}

	if(newWindow)
	{
		[self.closeButton bind:@"alphaValue" toObject:self withKeyPath:@"closeButtonAlphaValue" options:nil];

		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(windowDidChangeMainOrKey:) name:NSWindowDidBecomeMainNotification object:newWindow];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(windowDidChangeMainOrKey:) name:NSWindowDidResignMainNotification object:newWindow];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(windowDidChangeMainOrKey:) name:NSWindowDidBecomeKeyNotification object:newWindow];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(windowDidChangeMainOrKey:) name:NSWindowDidResignKeyNotification object:newWindow];
	}
}

- (void)windowDidChangeMainOrKey:(NSNotification*)aNotification
{
	BOOL isActive = self.window.isKeyWindow || self.window.isMainWindow || self.isInFullScreenMode;
	_textField.textColor = isActive ? NSColor.secondaryLabelColor : NSColor.tertiaryLabelColor;
}

- (NSView*)hitTest:(NSPoint)aPoint
{
	// This is required to receive menuForEvent: when control-clicking the text field (right click works)
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

- (void)mouseEntered:(NSEvent*)anEvent
{
	self.animator.mouseInside = YES;
}

- (void)mouseExited:(NSEvent*)anEvent
{
	self.animator.mouseInside = NO;
}

- (void)mouseDown:(NSEvent*)anEvent
{
	_mouseDownLocation = anEvent.locationInWindow;
}

- (void)otherMouseDown:(NSEvent*)anEvent
{
	[self.tabBarView didClickCloseButtonForTabView:self];
}

- (void)mouseDragged:(NSEvent*)anEvent
{
	if(self.dragAction && !NSEqualPoints(_mouseDownLocation, NSZeroPoint) && hypot(_mouseDownLocation.x - anEvent.locationInWindow.x, _mouseDownLocation.y - anEvent.locationInWindow.y) >= 2.5)
	{
		[NSApp sendAction:self.dragAction to:self.target from:self];
		_mouseDownLocation = NSZeroPoint;
	}
}

- (void)mouseUp:(NSEvent*)anEvent
{
	if(NSEqualPoints(_mouseDownLocation, NSZeroPoint)) // Ignore mouse up after a dragging session
		return;
	else if(anEvent.clickCount == 1 && !self.isSelected && hypot(_mouseDownLocation.x - anEvent.locationInWindow.x, _mouseDownLocation.y - anEvent.locationInWindow.y) < 2.5)
		[NSApp sendAction:self.action to:self.target from:self];
	else if(anEvent.clickCount == 2)
		[NSApp sendAction:self.doubleAction to:self.target from:self];
}

- (void)didClickCloseButton:(id)sender
{
	[self.tabBarView didClickCloseButtonForTabView:self];
}

- (void)didClickOverflorButton:(id)sender
{
	[self.tabBarView didClickOverflorButtonForTabView:self];
}

- (NSMenu*)menuForEvent:(NSEvent*)anEvent
{
	return [self.tabBarView menuForTabView:self withEvent:anEvent];
}

- (void)updateTrackingAreas
{
	[super updateTrackingAreas];
	if(_trackingArea)
		[self removeTrackingArea:_trackingArea];

	NSTrackingAreaOptions options = NSTrackingMouseEnteredAndExited|NSTrackingActiveAlways;
	if(!self.tabBarView.isDragging)
	{
		BOOL isInside = NSMouseInRect([self convertPoint:self.window.mouseLocationOutsideOfEventStream fromView:nil], self.visibleRect, self.isFlipped);
		if(isInside)
			options |= NSTrackingAssumeInside;
		if(self.mouseInside != isInside)
			self.animator.mouseInside = isInside;
	}

	_trackingArea = [[NSTrackingArea alloc] initWithRect:self.bounds options:options owner:self userInfo:nil];
	[self addTrackingArea:_trackingArea];
}

- (NSImage*)dragImage
{
	NSRect bounds = self.backgroundView.frame;
	if(NSIsEmptyRect(bounds))
		return nil;

	NSImage* image = [[NSImage alloc] initWithSize:bounds.size];
	[image lockFocusFlipped:self.isFlipped];

	_overflowButton.hidden     = YES;
	_textField.alphaValue      = 1.0;
	_backgroundView.alphaValue = 0.1;
	_topBorderView.alphaValue  = 1;

	[self displayRectIgnoringOpacity:bounds inContext:NSGraphicsContext.currentContext];

	_overflowButton.hidden     = !_overflowButtonVisible;
	_textField.alphaValue      = _selected ? 1 : 0.5;
	_backgroundView.alphaValue = _selected ? 0 : (_mouseInside ? 0.2 : 0.1);
	_topBorderView.alphaValue  = _selected ? 0 : 1;

	[image unlockFocus];
	return image;
}
@end

// ===============
// = OakTabFrame =
// ===============

@interface OakTabFrame : NSObject
@property (nonatomic) OakTabItem* tabItem;
@property (nonatomic) CGFloat width;
@end

@implementation OakTabFrame
- (instancetype)initWithTabItem:(OakTabItem*)tabItem width:(CGFloat)width
{
	if(self = [super init])
	{
		_tabItem = tabItem;
		_width   = width;
	}
	return self;
}

- (BOOL)isEqual:(id)lhs
{
	OakTabFrame* otherFrame = [lhs isKindOfClass:[self class]] ? lhs : nil;
	return otherFrame && _width == otherFrame.width && [_tabItem.identifier isEqual:otherFrame.tabItem.identifier];
}
@end

// =================
// = OakTabBarView =
// =================

@implementation OakTabBarView
+ (void)initialize
{
	[[NSUserDefaults standardUserDefaults] registerDefaults:@{
		kUserDefaultsTabItemMinWidthKey: @(120),
		kUserDefaultsTabItemMaxWidthKey: @(250),
	}];
}

+ (id)defaultAnimationForKey:(NSString*)key
{
	if([key isEqualToString:@"tabLayoutAnimationProgress"])
		return [CABasicAnimation animation];
	return [super defaultAnimationForKey:key];
}

- (instancetype)animator
{
	return (OakTabBarView*)[[OakAnimatorProxy alloc] initWithRealObject:super.animator];
}

- (instancetype)initWithFrame:(NSRect)aRect
{
	if(self = [super initWithFrame:aRect])
	{
		self.accessibilityRole  = NSAccessibilityTabGroupRole;
		self.accessibilityLabel = @"Open files";

		_minimumTabSize = [NSUserDefaults.standardUserDefaults integerForKey:kUserDefaultsTabItemMinWidthKey];
		_maximumTabSize = [NSUserDefaults.standardUserDefaults integerForKey:kUserDefaultsTabItemMaxWidthKey];

		_draggedTabIndex = -1;
		_dropTabAtIndex  = -1;

		_backgroundView = [[OakTabView alloc] initWithFrame:NSZeroRect tabItem:nil parent:nil];
		_backgroundView.target       = self;
		_backgroundView.doubleAction = @selector(newTab:);
		[self addSubview:_backgroundView positioned:NSWindowBelow relativeTo:nil];

		[self addSubview:self.createNewTabButton positioned:NSWindowAbove relativeTo:nil];

		[self registerForDraggedTypes:@[ OakTabItemPasteboardType ]];
	}
	return self;
}

- (NSSize)intrinsicContentSize
{
	return NSMakeSize(NSViewNoInstrinsicMetric, 23);
}

- (BOOL)mouseDownCanMoveWindow
{
	return NO;
}

- (NSButton*)createNewTabButton
{
	if(!_createNewTabButton)
	{
		_createNewTabButton = [[NSButton alloc] initWithFrame:NSMakeRect(0, 2, 26, 20)];
		_createNewTabButton.accessibilityLabel = @"Create new tab";
		_createNewTabButton.image      = [NSImage imageNamed:NSImageNameAddTemplate];
		_createNewTabButton.bordered   = NO;
		_createNewTabButton.buttonType = NSMomentaryChangeButton;
		_createNewTabButton.toolTip    = @"Create new tab";
		_createNewTabButton.action     = @selector(newTab:);
		_createNewTabButton.target     = self;
	}
	return _createNewTabButton;
}

- (NSInteger)selectedTabIndex
{
	NSUInteger res = [_tabItems indexOfObjectPassingTest:^BOOL(OakTabItem* tabItem, NSUInteger index, BOOL* stop){ return tabItem.isSelected; }];
	return res != NSNotFound ? res : -1;
}

- (void)setDraggedTabIndex:(NSInteger)newDraggedTabIndex
{
	if(_draggedTabIndex == newDraggedTabIndex)
		return;

	if(_draggedTabIndex != -1)
		_tabItems[_draggedTabIndex].tabView.hidden = NO;

	_draggedTabIndex = newDraggedTabIndex;

	if(_draggedTabIndex != -1)
		_tabItems[_draggedTabIndex].tabView.hidden = YES;

	[self updateToLayout:[self makeLayout]];
}

- (void)setDropTabAtIndex:(NSInteger)newDropTabAtIndex
{
	if(_dropTabAtIndex == newDropTabAtIndex)
		return;

	_dropTabAtIndex = newDropTabAtIndex;
	[self updateToLayout:[self makeLayout]];
}

- (void)reloadData
{
	if(!_tabItems)
		_tabItems = [NSMutableArray array];

	NSUInteger newCount = [_dataSource numberOfRowsInTabBarView:self];
	if(newCount > _tabItems.count)
		_freezeTabFramesLeftOfIndex = 0;

	OakTabItem* draggedTabItem = _draggedTabIndex != -1 ? _tabItems[_draggedTabIndex] : nil;
	OakTabItem* droppedTabItem = _dropTabAtIndex != -1 && _dropTabAtIndex < _tabItems.count ? _tabItems[_dropTabAtIndex] : nil;

	NSMutableDictionary<NSString*, OakTabItem*>* oldTabItems = [NSMutableDictionary dictionary];
	for(OakTabItem* tabItem in _tabItems)
		oldTabItems[tabItem.identifier] = tabItem;

	NSMutableArray<OakTabItem*>* newTabItems = [NSMutableArray array];
	for(NSUInteger i = 0; i < newCount; ++i)
	{
		NSString* identifier = [_dataSource tabBarView:self identifierForIndex:i];
		OakTabItem* tabItem = oldTabItems[identifier];
		if(tabItem)
		{
			tabItem.title    = [_dataSource tabBarView:self titleForIndex:i];
			tabItem.path     = [_dataSource tabBarView:self pathForIndex:i];
			tabItem.modified = [_dataSource tabBarView:self isEditedAtIndex:i];

			oldTabItems[identifier] = nil;
		}
		else
		{
			tabItem = [[OakTabItem alloc] initWithTitle:[_dataSource tabBarView:self titleForIndex:i] path:[_dataSource tabBarView:self pathForIndex:i] identifier:identifier modified:[_dataSource tabBarView:self isEditedAtIndex:i]];
		}
		[newTabItems addObject:tabItem];
	}

	_tabItems = newTabItems;

	_draggedTabIndex = draggedTabItem ? ([_tabItems indexOfObject:draggedTabItem] == NSNotFound ? -1 : [_tabItems indexOfObject:draggedTabItem]) : -1;
	_dropTabAtIndex  = droppedTabItem ? ([_tabItems indexOfObject:droppedTabItem] == NSNotFound ? -1 : [_tabItems indexOfObject:droppedTabItem]) : (_dropTabAtIndex == -1 ? -1 : _tabItems.count);

	NSArray<OakTabFrame*>* newLayout = [self makeLayout];
	if(![_toLayout isEqual:newLayout])
		[self updateToLayout:newLayout];
}

- (void)updateTrackingAreas
{
	[super updateTrackingAreas];
	if(_trackingArea)
		[self removeTrackingArea:_trackingArea];
	NSTrackingAreaOptions options = NSTrackingMouseEnteredAndExited|NSTrackingActiveAlways;
	if(NSMouseInRect([self convertPoint:self.window.mouseLocationOutsideOfEventStream fromView:nil], self.visibleRect, self.isFlipped))
		options |= NSTrackingAssumeInside;

	_trackingArea = [[NSTrackingArea alloc] initWithRect:self.bounds options:options owner:self userInfo:nil];
	[self addTrackingArea:_trackingArea];
}

- (void)mouseEntered:(NSEvent*)anEvent
{
}

- (void)mouseExited:(NSEvent*)anEvent
{
	if(_freezeTabFramesLeftOfIndex > 0)
	{
		_freezeTabFramesLeftOfIndex = 0;
		[self.animator updateToLayout:[self makeLayout]];
	}
}

- (void)didClickOverflorButtonForTabView:(OakTabView*)clickedTabView
{
	NSMenu* menu = [NSMenu new];
	for(NSUInteger i = 0; i < _tabItems.count; ++i)
	{
		OakTabItem* tabItem = _tabItems[i];
		if(tabItem.tabView && !tabItem.tabView.overflowButtonVisible && NSWidth(tabItem.tabView.frame) != 0)
			continue;

		NSMenuItem* item = [menu addItemWithTitle:tabItem.title action:@selector(takeSelectedTabIndexFrom:) keyEquivalent:@""];
		item.representedObject = tabItem;
		item.tag = i;

		if(NSString* path = tabItem.path)
		{
			item.image   = [OakFileIconImage fileIconImageWithPath:(OakIsEmptyString(path) ? nil : path) isModified:tabItem.isModified];
			item.toolTip = [path stringByAbbreviatingWithTildeInPath];
		}

		if(tabItem.isSelected)
			item.state = NSOnState;
		else if(tabItem.isModified)
			item.modifiedState = YES;
	}
	[menu popUpMenuPositioningItem:nil atLocation:NSMakePoint(NSWidth(clickedTabView.overflowButton.frame), 0) inView:clickedTabView.overflowButton];
}

// ===========
// = Actions =
// ===========

- (OakTabItem*)selectedTabItem
{
	return [_tabItems filteredArrayUsingPredicate:[NSPredicate predicateWithFormat:@"selected == YES"]].firstObject;
}

- (void)setSelectedTabIndex:(NSInteger)index
{
	for(NSUInteger i = 0; i < _tabItems.count; ++i)
		_tabItems[i].selected = i == index;

	if(0 <= index && index < _tabItems.count)
	{
		if(!_tabItems[index].tabView || NSWidth(_tabItems[index].tabView.frame) == 0)
			[self updateToLayout:[self makeLayout]];
	}
}

- (void)didClickCloseButtonForTabView:(OakTabView*)tabView
{
	_tag = [_tabItems indexOfObject:tabView.tabItem]; // performCloseTab: asks for [sender tag]
	if(_tag == NSNotFound)
		return;

	BOOL closeOther = OakIsAlternateKeyOrMouseEvent();
	if(closeOther && [_delegate respondsToSelector:@selector(performCloseOtherTabsXYZ:)])
	{
		[_delegate performCloseOtherTabsXYZ:self];
	}
	else if([_delegate respondsToSelector:@selector(performCloseTab:)])
	{
		_freezeTabFramesLeftOfIndex = _tag;
		[_delegate performCloseTab:self];
	}
}

- (void)didSingleClickTabView:(OakTabView*)tabView
{
	NSInteger index = [_tabItems indexOfObject:tabView.tabItem];
	if(index == NSNotFound)
		return;

	self.selectedTabIndex = index;
	if([_delegate respondsToSelector:@selector(tabBarView:shouldSelectIndex:)])
		[_delegate tabBarView:self shouldSelectIndex:index];
}

- (void)didDoubleClickTabView:(OakTabView*)tabView
{
	NSInteger index = [_tabItems indexOfObject:tabView.tabItem];
	if(index == NSNotFound)
		return;

	if([_delegate respondsToSelector:@selector(tabBarView:didDoubleClickIndex:)])
		[_delegate tabBarView:self didDoubleClickIndex:index];
}

- (NSInteger)tag
{
	return _tag;
}

- (NSMenu*)menuForTabView:(OakTabView*)tabView withEvent:(NSEvent*)anEvent
{
	_tag = [_tabItems indexOfObject:tabView.tabItem];
	if(_tag != NSNotFound && [_delegate respondsToSelector:@selector(menuForTabBarView:)])
		return [_delegate menuForTabBarView:self];
	return nil;
}

- (void)newTab:(id)sender
{
	if([_delegate respondsToSelector:@selector(tabBarViewDidDoubleClick:)])
		[_delegate tabBarViewDidDoubleClick:self];
}

- (void)performClose:(id)sender
{
	for(OakTabItem* tabItem in [_tabItems filteredArrayUsingPredicate:[NSPredicate predicateWithFormat:@"selected == YES"]])
	{
		_tag = [_tabItems indexOfObject:tabItem]; // performCloseTab: asks for [sender tag]
		if([_delegate respondsToSelector:@selector(performCloseTab:)])
			[_delegate performCloseTab:self];
	}
}

// ===============
// = Drag’n’drop =
// ===============

- (void)didDragTabView:(OakTabView*)tabView
{
	NSDraggingItem* dragItem = [[NSDraggingItem alloc] initWithPasteboardWriter:tabView.tabItem];
	if(NSImage* dragImage = tabView.dragImage)
		[dragItem setDraggingFrame:[self convertRect:tabView.backgroundView.frame fromView:tabView] contents:dragImage];

	self.draggedTabIndex = [_tabItems indexOfObject:tabView.tabItem] == NSNotFound ? -1 : [_tabItems indexOfObject:tabView.tabItem];
	self.dropTabAtIndex  = _draggedTabIndex+1;

	[self beginDraggingSessionWithItems:@[ dragItem ] event:self.window.currentEvent source:self];
}

- (NSDragOperation)draggingSession:(NSDraggingSession*)session sourceOperationMaskForDraggingContext:(NSDraggingContext)context
{
	return context == NSDraggingContextOutsideApplication ? (NSDragOperationCopy|NSDragOperationGeneric) : (NSDragOperationCopy|NSDragOperationMove|NSDragOperationLink);
}

- (void)draggingSession:(NSDraggingSession*)session endedAtPoint:(NSPoint)screenPoint operation:(NSDragOperation)operation
{
	self.draggedTabIndex = -1;
}

// ========================
// = Dragging Destination =
// ========================

- (NSDragOperation)draggingEntered:(id <NSDraggingInfo>)sender
{
	self.dragging = YES;
	return [self draggingUpdated:sender];
}

- (NSDragOperation)draggingUpdated:(id <NSDraggingInfo>)sender
{
	NSDragOperation operation = [sender draggingSourceOperationMask];
	operation = (operation & NSDragOperationMove) ?: (operation & NSDragOperationCopy);
	if(operation == NSDragOperationNone)
		return operation;

	NSPoint pos = [self convertPoint:sender.draggingLocation fromView:nil];

	NSInteger i = 0;
	CGFloat x = 0;
	for(OakTabItem* tabItem in _tabItems)
	{
		if(tabItem.tabView && !tabItem.tabView.hidden)
		{
			CGFloat width = NSWidth(tabItem.tabView.frame);

			if(_dropTabAtIndex == -1 && pos.x < x + width-20)
				break;
			else if(_dropTabAtIndex != -1 && pos.x < x + width/2)
				break;

			x += width;
		}
		++i;
	}

	self.animator.dropTabAtIndex = i;

	return operation;
}

- (void)draggingExited:(id <NSDraggingInfo>)sender
{
	self.dragging = NO;
	self.animator.dropTabAtIndex = -1;
}

- (BOOL)prepareForDragOperation:(id <NSDraggingInfo>)sender
{
	sender.animatesToDestination = YES;
	return YES;
}

- (BOOL)performDragOperation:(id <NSDraggingInfo>)sender
{
	OakTabBarView* sourceTabBar = sender.draggingSource;
	NSDragOperation mask = sender.draggingSourceOperationMask;
	NSInteger fromIndex = sourceTabBar.draggedTabIndex;
	NSInteger toIndex   = _dropTabAtIndex;

	__block OakTabItem* tabItem = nil;
	[sender enumerateDraggingItemsWithOptions:0 forView:self classes:@[ [NSPasteboardItem class] ] searchOptions:@{ } usingBlock:^(NSDraggingItem* draggingItem, NSInteger idx, BOOL* stop){
		CGFloat x0 = _dropTabAtIndex > 0 ? NSMaxX(_tabItems[_dropTabAtIndex-1].tabView.frame) : NSMinX(self.bounds);
		CGFloat x1 = NSMinX((_dropTabAtIndex < _tabItems.count ? _tabItems[_dropTabAtIndex].tabView : self.backgroundView).frame);
		draggingItem.draggingFrame = NSMakeRect(x0, NSMinY(self.bounds), x1 - x0, NSHeight(self.bounds));
		tabItem = [OakTabItem tabItemFromPasteboardItem:draggingItem.item];
	}];

	self.draggedTabIndex = -1;
	self.dropTabAtIndex  = -1;

	if(sourceTabBar == self && fromIndex == toIndex)
		return NO;

	BOOL shouldDelegate = [_delegate respondsToSelector:@selector(performDropOfTabItem:fromTabBar:index:toTabBar:index:operation:)];
	return shouldDelegate && [_delegate performDropOfTabItem:tabItem fromTabBar:sourceTabBar index:fromIndex toTabBar:self index:toIndex operation:(mask & NSDragOperationMove) ?: (mask & NSDragOperationCopy)];
}

- (void)concludeDragOperation:(id <NSDraggingInfo>)sender
{
	self.dragging = NO;
}

// ==========
// = Layout =
// ==========

- (NSArray<OakTabFrame*>*)interpolatedLayout:(NSArray<OakTabFrame*>*)oldLayout withFraction:(CGFloat)fraction ofLayout:(NSArray<OakTabFrame*>*)newLayout
{
	if(fraction == 0)
		return oldLayout;
	else if(fraction == 1)
		return newLayout;

	struct tab_t
	{
		tab_t (OakTabItem* tabItem, CGFloat oldWidth, CGFloat newWidth) : tabItem(tabItem), oldWidth(oldWidth), newWidth(newWidth) { }
		OakTabItem* tabItem;
		CGFloat oldWidth;
		CGFloat newWidth;
		CGFloat currentWidth = 0;
	};

	NSSet<NSString*>* oldTabIdentifiers = [NSSet setWithArray:[oldLayout valueForKeyPath:@"tabItem.identifier"]];
	NSSet<NSString*>* newTabIdentifiers = [NSSet setWithArray:[newLayout valueForKeyPath:@"tabItem.identifier"]];

	std::vector<tab_t> tabs;
	for(NSUInteger i = 0, j = 0; i < oldLayout.count || j < newLayout.count; )
	{
		if(j == newLayout.count || i < oldLayout.count && ![newTabIdentifiers containsObject:oldLayout[i].tabItem.identifier])
		{
			if(oldLayout[i].width > 0)
				tabs.emplace_back(oldLayout[i].tabItem, oldLayout[i].width, 0);
			++i;
		}
		else if(i == oldLayout.count || j < newLayout.count && ![oldTabIdentifiers containsObject:newLayout[j].tabItem.identifier])
		{
			if(newLayout[j].width > 0)
				tabs.emplace_back(newLayout[j].tabItem, 0, newLayout[j].width);
			++j;
		}
		else if([oldLayout[i].tabItem.identifier isEqualToString:newLayout[j].tabItem.identifier])
		{
			if(oldLayout[i].width > 0 || newLayout[j].width > 0)
				tabs.emplace_back(oldLayout[i].tabItem, oldLayout[i].width, newLayout[j].width);
			++i;
			++j;
		}
		else
		{
			NSLog(@"%s *** assertion failure for %@ != %@", sel_getName(_cmd), oldLayout[i].tabItem, newLayout[j].tabItem);
			break;
		}
	}

	CGFloat oldX0 = 0, newX0 = 0, x0 = 0;
	for(auto& tab : tabs)
	{
		CGFloat const oldX1 = oldX0 + tab.oldWidth;
		CGFloat const newX1 = newX0 + tab.newWidth;

		CGFloat x1 = oldX1 + round(fraction * (newX1 - oldX1));
		tab.currentWidth = x1 - x0;

		x0    = x1;
		oldX0 = oldX1;
		newX0 = newX1;
	}

	NSMutableArray<OakTabFrame*>* result = [NSMutableArray array];
	for(auto const& tab : tabs)
		[result addObject:[[OakTabFrame alloc] initWithTabItem:tab.tabItem width:tab.currentWidth]];
	return result;
}

- (NSArray<OakTabFrame*>*)makeLayoutForTabItems:(NSArray<OakTabItem*>*)tabItems inRectOfWidth:(CGFloat)totalWidth
{
	totalWidth += 1; // We place leftmost tab at position -1

	NSMutableArray<OakTabFrame*>* array = [NSMutableArray array];
	if(_maximumTabSize * tabItems.count <= totalWidth)
	{
		for(NSUInteger i = 0; i < tabItems.count; ++i)
			[array addObject:[[OakTabFrame alloc] initWithTabItem:tabItems[i] width:_maximumTabSize]];
	}
	else
	{
		CGFloat supply = 0, demand = 0;

		for(NSUInteger i = 0; i < tabItems.count; ++i)
		{
			CGFloat x0 = round(totalWidth * (i+0) / tabItems.count);
			CGFloat x1 = round(totalWidth * (i+1) / tabItems.count);
			CGFloat width = x1 - x0;

			if(width < tabItems[i].fittingWidth)
					demand += tabItems[i].fittingWidth - width;
			else	supply += width - tabItems[i].fittingWidth;
		}

		CGFloat counter = 0;
		for(NSUInteger i = 0; i < tabItems.count; ++i)
		{
			CGFloat x0 = round(totalWidth * (i+0) / tabItems.count);
			CGFloat x1 = round(totalWidth * (i+1) / tabItems.count);
			CGFloat width = x1 - x0;

			if(supply != 0 && demand != 0)
			{
				if(supply <= demand)
				{
					if(tabItems[i].fittingWidth < width)
					{
						width = tabItems[i].fittingWidth;
					}
					else
					{
						CGFloat x0 = round(supply * counter / demand);
						counter += tabItems[i].fittingWidth - width;
						CGFloat x1 = round(supply * counter / demand);
						width += x1 - x0;
					}
				}
				else if(width < tabItems[i].fittingWidth)
				{
					width = tabItems[i].fittingWidth;
				}
				else
				{
					CGFloat x0 = round(demand * counter / supply);
					counter += width - tabItems[i].fittingWidth;
					CGFloat x1 = round(demand * counter / supply);
					width -= x1 - x0;
				}
			}

			[array addObject:[[OakTabFrame alloc] initWithTabItem:tabItems[i] width:width]];
		}
	}
	return array;
}

- (NSArray<OakTabFrame*>*)makeLayout
{
	static NSString* const firstTabIdentifier = [NSUUID UUID].UUIDString;
	CGFloat const visibleWidth = NSWidth(self.bounds) - NSWidth(self.createNewTabButton.frame);
	NSUInteger const countOfVisibleTabs = MIN(MAX(0, floor(visibleWidth / _minimumTabSize)), _tabItems.count);

	NSMutableArray<OakTabItem*>* tabItems = [NSMutableArray array];
	BOOL didIncludeSelected = [_tabItems filteredArrayUsingPredicate:[NSPredicate predicateWithFormat:@"selected == YES"]].count == 0;
	for(NSUInteger visibleCount = 0, i = 0; i <= _tabItems.count; ++i)
	{
		if(i == _dropTabAtIndex)
		{
			NSString* identifier = _dropTabAtIndex == 0 ? firstTabIdentifier : [_tabItems[_dropTabAtIndex-1].identifier stringByAppendingString:@".margin"];
			OakTabItem* dummyTabItem = [[OakTabItem alloc] initWithTitle:@"" path:nil identifier:identifier modified:NO];
			[tabItems addObject:dummyTabItem];
		}

		if(i == _tabItems.count)
			break;

		OakTabItem* tabItem = _tabItems[i];
		tabItem.tabView.overflowButtonVisible = NO;

		if(visibleCount >= countOfVisibleTabs || (_tabItems.count > countOfVisibleTabs && visibleCount+1 == countOfVisibleTabs && !tabItem.isSelected && !didIncludeSelected))
			continue;

		++visibleCount;
		didIncludeSelected = didIncludeSelected || tabItem.isSelected;

		if(i == _draggedTabIndex)
			continue;

		[tabItems addObject:tabItem];
	}

	NSMutableDictionary<NSString*, OakTabView*>* existingTabViews = [NSMutableDictionary dictionary];
	for(OakTabFrame* tabFrame in _currentLayout)
		existingTabViews[tabFrame.tabItem.identifier] = tabFrame.tabItem.tabView;

	for(OakTabItem* tabItem in tabItems)
	{
		if(!tabItem.tabView)
		{
			if(existingTabViews[tabItem.identifier])
			{
				tabItem.tabView = existingTabViews[tabItem.identifier];
				existingTabViews[tabItem.identifier] = nil;
			}
			else
			{
				OakTabView* tabView = [[OakTabView alloc] initWithFrame:NSZeroRect tabItem:tabItem parent:self];
				tabView.target       = self;
				tabView.action       = @selector(didSingleClickTabView:);
				tabView.doubleAction = @selector(didDoubleClickTabView:);
				tabView.dragAction   = @selector(didDragTabView:);

				tabItem.tabView = tabView;
				[self addSubview:tabView];
			}
		}

		if(tabItem.needsLayout)
		{
			tabItem.fittingWidth = tabItem.tabView.fittingSize.width;
			tabItem.needsLayout = NO;
		}
	}

	if(_tabItems.count > countOfVisibleTabs)
		tabItems.lastObject.tabView.overflowButtonVisible = YES;

	NSArray<OakTabFrame*>* res = [self makeLayoutForTabItems:tabItems inRectOfWidth:visibleWidth];

	if(_freezeTabFramesLeftOfIndex > 0)
	{
		NSSet<NSString*>* frozenTabIdentifiers = [NSSet setWithArray:[[_tabItems subarrayWithRange:NSMakeRange(0, MIN(_freezeTabFramesLeftOfIndex, _tabItems.count))] valueForKeyPath:@"identifier"]];
		for(OakTabFrame* tabFrame in res)
		{
			if([frozenTabIdentifiers containsObject:tabFrame.tabItem.identifier])
			{
				if(OakTabView* tabView = tabFrame.tabItem.tabView)
				{
					if(NSWidth(tabView.frame) > 0)
						tabFrame.width = NSWidth(tabView.frame);
				}
			}
		}
	}

	return res;
}

- (void)updateToLayout:(NSArray<OakTabFrame*>*)newLayout
{
	if(NSAnimationContext.currentContext.allowsImplicitAnimation)
	{
		_fromLayout = _currentLayout;
		_toLayout   = newLayout;

		_tabLayoutAnimationProgressOffset = _tabLayoutAnimationProgress;
		self.animator.tabLayoutAnimationProgress = _tabLayoutAnimationProgress + 1;
	}
	else
	{
		_fromLayout = _toLayout = newLayout;
		self.currentLayout = [self interpolatedLayout:_fromLayout withFraction:1 ofLayout:_toLayout];
	}
}

- (void)setTabLayoutAnimationProgress:(CGFloat)newTabAnimationProgress
{
	if(_tabLayoutAnimationProgress == newTabAnimationProgress)
		return;
	_tabLayoutAnimationProgress = newTabAnimationProgress;
	self.currentLayout = [self interpolatedLayout:_fromLayout withFraction:(_tabLayoutAnimationProgress - _tabLayoutAnimationProgressOffset) ofLayout:_toLayout];
}

- (void)setCurrentLayout:(NSArray<OakTabFrame*>*)newLayout
{
	NSMutableDictionary<NSString*, OakTabView*>* existingTabViews = [NSMutableDictionary dictionary];
	for(OakTabFrame* tabFrame in _currentLayout)
		existingTabViews[tabFrame.tabItem.identifier] = tabFrame.tabItem.tabView;

	for(OakTabFrame* tabFrame in newLayout)
	{
		if(tabFrame.tabItem.tabView == existingTabViews[tabFrame.tabItem.identifier])
			existingTabViews[tabFrame.tabItem.identifier] = nil;
	}

	_currentLayout = newLayout;

	for(OakTabView* tabView in existingTabViews.allValues)
		[tabView removeFromSuperview];

	NSRect createNewTabButtonFrame = _createNewTabButton.frame;
	CGFloat x = -1, y = NSMinY(self.bounds)+1, height = NSHeight(self.bounds)-1;

	for(OakTabFrame* tabFrame in _currentLayout)
	{
		tabFrame.tabItem.tabView.tabItem = tabFrame.tabItem;
		tabFrame.tabItem.tabView.frame = NSMakeRect(x, y, tabFrame.width, height);
		x += tabFrame.width;
	}

	_backgroundView.frame = NSMakeRect(x, y, NSWidth(self.bounds)+2 - x, height);
	_createNewTabButton.frame  = { { x, round((height - NSHeight(createNewTabButtonFrame)) / 2) }, createNewTabButtonFrame.size };
}

- (void)resizeSubviewsWithOldSize:(NSSize)aSize
{
	[super resizeSubviewsWithOldSize:aSize];
	if(!NSEqualSizes(self.bounds.size, aSize))
		[self updateToLayout:[self makeLayout]];
}
@end
