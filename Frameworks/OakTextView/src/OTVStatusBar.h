@protocol OTVStatusBarDelegate <NSObject>
- (void)showBundleItemSelector:(NSPopUpButton*)popUpButton;
- (void)showSymbolSelector:(NSPopUpButton*)popUpButton;
@optional
- (void)showLSPStatusMenu:(NSPopUpButton*)popUpButton;
- (void)showCopilotStatusMenu:(NSPopUpButton*)popUpButton;
@end

@interface OTVStatusBar : NSVisualEffectView
- (void)showBundlesMenu:(id)sender;
- (void)setLspStatus:(NSString*)status errors:(NSUInteger)errors warnings:(NSUInteger)warnings info:(NSUInteger)info;
- (void)flashLspError;
- (void)setCopilotStatus:(NSInteger)status;
@property (nonatomic) NSString* selectionString;
@property (nonatomic) NSString* grammarName;
@property (nonatomic) NSString* symbolName;
@property (nonatomic) NSString* fileType; // This will update grammarName
@property (nonatomic, getter = isRecordingMacro) BOOL recordingMacro;
@property (nonatomic) BOOL softTabs;
@property (nonatomic) NSUInteger tabSize;

@property (nonatomic, weak) id <OTVStatusBarDelegate> delegate;
@property (nonatomic, weak) id target;
@end
