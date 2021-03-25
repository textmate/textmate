#import "OakRolloverButton.h"
typedef NS_ENUM(NSUInteger, OakBackgroundFillViewStyle) {
	OakBackgroundFillViewStyleNone = 0,
	OakBackgroundFillViewStyleHeader,
};

@interface OakBackgroundFillView : NSView
@property (nonatomic) OakBackgroundFillViewStyle style;
@property (nonatomic) NSColor* activeBackgroundColor;
@property (nonatomic) NSColor* inactiveBackgroundColor;
@property (nonatomic) NSGradient* activeBackgroundGradient;
@property (nonatomic) NSGradient* inactiveBackgroundGradient;
@property (nonatomic) BOOL active;
@end

NSFont* OakStatusBarFont ();
NSFont* OakControlFont ();

NSTextField* OakCreateLabel (NSString* label = @"", NSFont* font = nil, NSTextAlignment alignment = NSTextAlignmentLeft, NSLineBreakMode lineBreakMode = NSLineBreakByTruncatingMiddle);
NSButton* OakCreateCheckBox (NSString* label);
NSButton* OakCreateButton (NSString* label, NSBezelStyle bezel = NSBezelStyleRounded);
NSPopUpButton* OakCreatePopUpButton (BOOL pullsDown = NO, NSString* initialItemTitle = nil, NSView* labelView = nil);
NSPopUpButton* OakCreateActionPopUpButton (BOOL bordered = NO);
NSComboBox* OakCreateComboBox (NSView* labelView = nil);
OakRolloverButton* OakCreateCloseButton (NSString* accessibilityLabel = @"Close document");
NSView* OakCreateNSBoxSeparator ();

OakBackgroundFillView* OakCreateVerticalLine (OakBackgroundFillViewStyle style);
void OakSetupKeyViewLoop (NSArray<NSView*>* views);
void OakAddAutoLayoutViewsToSuperview (NSArray<NSView*>* views, NSView* superview);
