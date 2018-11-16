#import "OakRolloverButton.h"
#import <oak/misc.h>

typedef NS_ENUM(NSUInteger, OakBackgroundFillViewStyle) {
	OakBackgroundFillViewStyleNone = 0,
	OakBackgroundFillViewStyleHeader,
	OakBackgroundFillViewStyleDivider,
	OakBackgroundFillViewStyleDarkDivider,
};

PUBLIC @interface OakBackgroundFillView : NSView
@property (nonatomic) OakBackgroundFillViewStyle style;
@property (nonatomic) NSImage* activeBackgroundImage;
@property (nonatomic) NSImage* inactiveBackgroundImage;
@property (nonatomic) NSColor* activeBackgroundColor;
@property (nonatomic) NSColor* inactiveBackgroundColor;
@property (nonatomic) NSGradient* activeBackgroundGradient;
@property (nonatomic) NSGradient* inactiveBackgroundGradient;
@property (nonatomic) BOOL active;
@end

PUBLIC NSFont* OakStatusBarFont ();
PUBLIC NSFont* OakControlFont ();

PUBLIC NSTextField* OakCreateLabel (NSString* label = @"", NSFont* font = nil, NSTextAlignment alignment = NSTextAlignmentLeft, NSLineBreakMode lineBreakMode = NSLineBreakByTruncatingMiddle);
PUBLIC NSButton* OakCreateCheckBox (NSString* label);
PUBLIC NSButton* OakCreateButton (NSString* label, NSBezelStyle bezel = NSRoundedBezelStyle);
PUBLIC NSPopUpButton* OakCreatePopUpButton (BOOL pullsDown = NO, NSString* initialItemTitle = nil, NSView* labelView = nil);
PUBLIC NSPopUpButton* OakCreateActionPopUpButton (BOOL bordered = NO);
PUBLIC NSComboBox* OakCreateComboBox (NSView* labelView = nil);
PUBLIC OakRolloverButton* OakCreateCloseButton (NSString* accessibilityLabel = @"Close document");
PUBLIC NSView* OakCreateDividerImageView ();

PUBLIC OakBackgroundFillView* OakCreateVerticalLine (OakBackgroundFillViewStyle style);
PUBLIC OakBackgroundFillView* OakCreateHorizontalLine (OakBackgroundFillViewStyle style);
PUBLIC void OakSetupKeyViewLoop (NSArray* views, BOOL setFirstResponder = YES);
PUBLIC void OakAddAutoLayoutViewsToSuperview (NSArray* views, NSView* superview);
