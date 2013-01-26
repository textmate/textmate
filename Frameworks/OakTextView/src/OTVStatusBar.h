#import <OakAppKit/OakGradientView.h>

@protocol OTVStatusBarDelegate <NSObject>
- (void)showBundleItemSelector:(NSPopUpButton*)popUpButton;
- (void)showSymbolSelector:(NSPopUpButton*)popUpButton;
@end

@interface OTVStatusBar : OakGradientView
- (void)showBundlesMenu:(id)sender;
- (void)setCaretPosition:(std::string const&)range;
@property (nonatomic, copy)   NSString* grammarName;
@property (nonatomic, copy)   NSString* symbolName;
@property (nonatomic, assign) BOOL isMacroRecording;
@property (nonatomic, assign) BOOL softTabs;
@property (nonatomic, assign) int32_t tabSize;

@property (nonatomic, assign) id <OTVStatusBarDelegate> delegate;
@end
