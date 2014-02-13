#import <OakAppKit/OakGradientView.h>

@protocol OTVStatusBarDelegate <NSObject>
- (void)showBundleItemSelector:(NSPopUpButton*)popUpButton;
- (void)showSymbolSelector:(NSPopUpButton*)popUpButton;
@end

@interface OTVStatusBar : OakGradientView
- (void)showBundlesMenu:(id)sender;
@property (nonatomic) NSString* selectionString;
@property (nonatomic) NSString* grammarName;
@property (nonatomic) NSString* symbolName;
@property (nonatomic) BOOL isMacroRecording;
@property (nonatomic) BOOL softTabs;
@property (nonatomic) int32_t tabSize;

@property (nonatomic, weak) id <OTVStatusBarDelegate> delegate;
@property (nonatomic, weak) id target;
@end
