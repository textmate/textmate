#import <oak/misc.h>

PUBLIC @interface OakBackgroundFillView : NSView
- (instancetype)initWithBackground:(id)activeBackground inactiveBackground:(id)inactiveBackground;
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

PUBLIC NSTextField* OakCreateLabel (NSString* label = @"", Class cl = [NSTextField class]);
PUBLIC NSTextField* OakCreateSmallLabel (NSString* label = @"", Class cl = [NSTextField class]);
PUBLIC NSButton* OakCreateCheckBox (NSString* label);
PUBLIC NSButton* OakCreateButton (NSString* label, NSBezelStyle bezel = NSRoundedBezelStyle);
PUBLIC NSPopUpButton* OakCreatePopUpButton (BOOL pullsDown = NO, NSString* initialItemTitle = nil, NSObject* accessibilityLabel = nil);
PUBLIC NSPopUpButton* OakCreateActionPopUpButton (BOOL bordered = NO);
PUBLIC NSPopUpButton* OakCreateStatusBarPopUpButton (NSString* initialItemTitle = nil, NSObject* accessibilityLabel = nil);
PUBLIC NSComboBox* OakCreateComboBox (NSObject* accessibilityLabel = nil);
PUBLIC NSImageView* OakCreateDividerImageView ();

PUBLIC OakBackgroundFillView* OakCreateVerticalLine (NSColor* primaryColor, NSColor* secondaryColor = nil);
PUBLIC OakBackgroundFillView* OakCreateHorizontalLine (NSColor* primaryColor, NSColor* secondaryColor = nil);
PUBLIC BOOL OakSetAccessibilityLabel (NSObject* element, NSObject* label);
