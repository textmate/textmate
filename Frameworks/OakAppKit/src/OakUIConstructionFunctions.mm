#import "OakUIConstructionFunctions.h"
#import "NSImage Additions.h"

NSFont* OakStatusBarFont ()
{
	return [NSFont controlContentFontOfSize:[NSFont smallSystemFontSize]];
}

NSFont* OakControlFont ()
{
	return [NSFont controlContentFontOfSize:[NSFont systemFontSizeForControlSize:NSRegularControlSize]];
}

NSTextField* OakCreateLabel (NSString* label, Class cl)
{
	NSTextField* res = [[cl alloc] initWithFrame:NSZeroRect];
	[[res cell] setWraps:NO];
	res.alignment       = NSRightTextAlignment;
	res.bezeled         = NO;
	res.bordered        = NO;
	res.drawsBackground = NO;
	res.editable        = NO;
	res.font            = [NSFont controlContentFontOfSize:[NSFont systemFontSize]];
	res.selectable      = NO;
	res.stringValue     = label;
	return res;
}

NSTextField* OakCreateSmallLabel (NSString* label, Class cl)
{
	NSTextField* res = OakCreateLabel(label, cl);
	res.alignment = NSLeftTextAlignment;
	res.font      = [NSFont controlContentFontOfSize:[NSFont smallSystemFontSize]];
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
	res.font = OakControlFont();
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

// =========================
// = OakBackgroundFillView =
// =========================

@implementation OakBackgroundFillView
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

- (void)setActiveBackgroundImage:(NSImage*)anImage
{
	if(_activeBackgroundImage == anImage)
		return;
	_activeBackgroundImage = anImage;
	if(_active)
		self.needsDisplay = YES;
}

- (void)setInactiveBackgroundImage:(NSImage*)anImage
{
	if(_inactiveBackgroundImage == anImage)
		return;
	_inactiveBackgroundImage = anImage;
	if(!_active)
		self.needsDisplay = YES;
}

- (void)setActiveBackgroundColor:(NSColor*)anColor
{
	if(_activeBackgroundColor == anColor)
		return;
	_activeBackgroundColor = anColor;
	if(_active)
		self.needsDisplay = YES;
}

- (void)setInactiveBackgroundColor:(NSColor*)anColor
{
	if(_inactiveBackgroundColor == anColor)
		return;
	_inactiveBackgroundColor = anColor;
	if(!_active)
		self.needsDisplay = YES;
}

- (BOOL)isOpaque
{
	return _activeBackgroundColor != nil;
}

- (NSSize)intrinsicContentSize
{
	if(NSImage* image = _activeBackgroundImage ?: _inactiveBackgroundImage)
			return image.size;
	else	return NSMakeSize(NSViewNoInstrinsicMetric, NSViewNoInstrinsicMetric);
}

- (NSColor*)currentColor
{
	if(NSImage* image = _active ? _activeBackgroundImage : _inactiveBackgroundImage)
		return [NSColor colorWithPatternImage:image];
	return _active ? _activeBackgroundColor : (_inactiveBackgroundColor ?: _activeBackgroundColor);
}

- (void)drawRect:(NSRect)aRect
{
	if(NSColor* color = [self currentColor])
	{
		[color set];
		CGContextRef context = (CGContextRef)[[NSGraphicsContext currentContext] graphicsPort];
		CGAffineTransform affineTransform = CGContextGetCTM(context);
		CGContextSetPatternPhase(context, CGSizeMake(affineTransform.tx, affineTransform.ty));
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

// =============================

@interface OakDisableAccessibilityImageCell : NSImageCell
@end

@implementation OakDisableAccessibilityImageCell
- (BOOL)accessibilityIsIgnored
{
	return YES;
}
@end

@interface OakDisableAccessibilityImageView : NSImageView
@end

@implementation OakDisableAccessibilityImageView
+ (void)initialize
{
	if(self == OakDisableAccessibilityImageView.class)
	{
		[OakDisableAccessibilityImageView setCellClass:[OakDisableAccessibilityImageCell class]];
	}
}
@end

NSImageView* OakCreateDividerImageView ()
{
	NSImageView* res = [[OakDisableAccessibilityImageView alloc] initWithFrame:NSZeroRect];
	[res setImage:[NSImage imageNamed:@"Divider" inSameBundleAsClass:[OakBackgroundFillView class]]];
	[res setContentHuggingPriority:NSLayoutPriorityRequired forOrientation:NSLayoutConstraintOrientationHorizontal];
	[res setContentCompressionResistancePriority:NSLayoutPriorityDefaultLow forOrientation:NSLayoutConstraintOrientationVertical];
	return res;
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
