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

NSTextField* OakCreateLabel (NSString* label)
{
	NSTextField* res = [[NSTextField alloc] initWithFrame:NSZeroRect];
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

NSTextField* OakCreateSmallLabel (NSString* label)
{
	NSTextField* res = OakCreateLabel(label);
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
		[[res cell] setMenuItem:[[NSMenuItem alloc] initWithTitle:initialItemTitle action:@selector(nop:) keyEquivalent:@""]];
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

// =============================
// = NSBox-based Divider Lines =
// =============================

@interface OakDividerLineView : NSBox
@property (nonatomic) NSColor* primaryColor;
@property (nonatomic) NSColor* secondaryColor;
@property (nonatomic) BOOL     usePrimaryColor;
@property (nonatomic) NSSize   intrinsicContentSize;
@end

@implementation OakDividerLineView
- (void)viewWillMoveToWindow:(NSWindow*)newWindow
{
	if(!self.secondaryColor)
		return;

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

	self.usePrimaryColor = ([newWindow styleMask] & NSFullScreenWindowMask) || [newWindow isMainWindow] || [newWindow isKeyWindow];
}

- (void)windowDidChangeMainOrKey:(NSNotification*)aNotification
{
	self.usePrimaryColor = ([self.window styleMask] & NSFullScreenWindowMask) || [self.window isMainWindow] || [self.window isKeyWindow];
}

- (void)setUsePrimaryColor:(BOOL)flag
{
	if(_usePrimaryColor != flag)
	{
		_usePrimaryColor = flag;
		self.borderColor = flag ? self.primaryColor : self.secondaryColor;
	}
}

- (BOOL)isOpaque
{
	return YES;
}
@end

static OakDividerLineView* OakCreateDividerLineWithColor (NSColor* color, NSColor* secondaryColor)
{
	OakDividerLineView* box = [[OakDividerLineView alloc] initWithFrame:NSZeroRect];
	box.translatesAutoresizingMaskIntoConstraints = NO;
	box.boxType         = NSBoxCustom;
	box.borderType      = NSLineBorder;
	box.borderColor     = color;
	box.primaryColor    = color;
	box.secondaryColor  = secondaryColor;
	box.usePrimaryColor = YES;
	return box;
}

NSBox* OakCreateVerticalLine (NSColor* primaryColor, NSColor* secondaryColor)
{
	OakDividerLineView* res = OakCreateDividerLineWithColor(primaryColor, secondaryColor);
	res.intrinsicContentSize = NSMakeSize(1, NSViewNoInstrinsicMetric);
	[res setContentHuggingPriority:NSLayoutPriorityRequired forOrientation:NSLayoutConstraintOrientationHorizontal];
	return res;
}

NSBox* OakCreateHorizontalLine (NSColor* primaryColor, NSColor* secondaryColor)
{
	OakDividerLineView* res = OakCreateDividerLineWithColor(primaryColor, secondaryColor);
	res.intrinsicContentSize = NSMakeSize(NSViewNoInstrinsicMetric, 1);
	[res setContentHuggingPriority:NSLayoutPriorityRequired forOrientation:NSLayoutConstraintOrientationVertical];
	return res;
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
	[res setImage:[NSImage imageNamed:@"Divider" inSameBundleAsClass:[OakDividerLineView class]]];
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
