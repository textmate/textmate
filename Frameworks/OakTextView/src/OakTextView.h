#import "GutterView.h"
#import <OakAppKit/OakView.h>
#import <editor/editor.h>
#import <buffer/buffer.h>
#import <theme/theme.h>
#import <document/document.h>
#import <oak/debug.h>

extern int32_t const NSWrapColumnWindowWidth;
extern int32_t const NSWrapColumnAskUser;
extern NSString* const kUserDefaultsDisableAntiAliasKey;

namespace bundles { struct item_t; typedef std::shared_ptr<item_t> item_ptr; }

enum folding_state_t { kFoldingNone, kFoldingTop, kFoldingCollapsed, kFoldingBottom };

enum OTVFontSmoothing : NSUInteger
{
	OTVFontSmoothingDisabled             = 0,
	OTVFontSmoothingEnabled              = 1,
	OTVFontSmoothingDisabledForDark      = 2,
	OTVFontSmoothingDisabledForDarkHiDPI = 3,
};

@protocol OakTextViewDelegate <NSObject>
@optional
- (void)bundleItemPreExec:(pre_exec::type)preExec completionHandler:(void(^)(BOOL success))callback;
- (NSString*)scopeAttributes;
- (std::map<std::string, std::string>)variables;
@end

PUBLIC @interface OakTextView : OakView <NSTextInput, NSTextFieldDelegate>
- (void)setDocument:(document::document_ptr const&)aDocument;

@property (nonatomic, weak) id <OakTextViewDelegate>        delegate;
@property (nonatomic) theme_ptr const&                      theme;
@property (nonatomic) NSCursor*                             ibeamCursor;
@property (nonatomic) NSFont*                               font;
@property (nonatomic) BOOL                                  antiAlias;
@property (nonatomic) OTVFontSmoothing                      fontSmoothing;
@property (nonatomic) size_t                                tabSize;
@property (nonatomic) BOOL                                  showInvisibles;
@property (nonatomic) BOOL                                  softWrap;
@property (nonatomic) BOOL                                  scrollPastEnd;
@property (nonatomic) BOOL                                  softTabs;
@property (nonatomic, readonly) ng::indent_correction_t     indentCorrections;

@property (nonatomic, readonly) BOOL                        hasMultiLineSelection;
@property (nonatomic, readonly) BOOL                        hasSelection;
@property (nonatomic) NSString*                             selectionString;

@property (nonatomic) BOOL                                  isMacroRecording;

- (GVLineRecord)lineRecordForPosition:(CGFloat)yPos;
- (GVLineRecord)lineFragmentForLine:(NSUInteger)aLine column:(NSUInteger)aColumn;

- (BOOL)filterDocumentThroughCommand:(NSString*)commandString input:(input::type)inputUnit output:(output::type)outputUnit;

- (NSPoint)positionForWindowUnderCaret;
- (scope::context_t)scopeContext;
- (folding_state_t)foldingStateForLine:(NSUInteger)lineNumber;

- (IBAction)toggleColumnSelection:(id)sender;
- (IBAction)toggleMacroRecording:(id)sender;
- (IBAction)toggleFoldingAtLine:(NSUInteger)lineNumber recursive:(BOOL)flag;
- (IBAction)toggleShowInvisibles:(id)sender;
- (IBAction)toggleScrollPastEnd:(id)sender;

- (IBAction)saveScratchMacro:(id)sender;

- (void)performBundleItem:(bundles::item_ptr)anItem;
@end
