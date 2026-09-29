#include <bundles/src/bundles.h>
#include <scope/src/scope.h>

@class OakThemeStyles;

@interface OakTheme : NSObject
@property (nonatomic, readonly) NSUUID* identifier;
@property (nonatomic, readonly) NSColor* foregroundColor;
@property (nonatomic, readonly) NSColor* backgroundColor;
@property (nonatomic, readonly) NSFont* font;
+ (instancetype)theme;
- (instancetype)initWithBundleItem:(bundles::item_ptr const&)themeItem;
- (OakThemeStyles*)stylesForScope:(scope::scope_t const&)scope;
@end

@interface OakThemeStyles : NSObject
@property (nonatomic, readonly) NSColor* foregroundColor;
@property (nonatomic, readonly) NSColor* backgroundColor;
@property (nonatomic, readonly) NSColor* caretColor;
@property (nonatomic, readonly) NSColor* selectionColor;
@property (nonatomic, readonly) NSColor* invisiblesColor;
@property (nonatomic, readonly) NSFont* font;
@property (nonatomic, readonly) NSFontTraitMask fontTraits;
@property (nonatomic, readonly) BOOL underlined;
@property (nonatomic, readonly) BOOL strikethrough;
@property (nonatomic, readonly) BOOL misspelled;
@end
