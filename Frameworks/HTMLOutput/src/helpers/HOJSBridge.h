@protocol HOJSBridgeDelegate
@property (nonatomic) BOOL isBusy;
@property (nonatomic) double progress;
@end

@interface HOJSBridge : NSObject
@property (nonatomic, weak) id /*<HOJSBridgeDelegate>*/ delegate;

- (void)setEnvironment:(const std::map<std::string, std::string>&)variables;
- (std::map<std::string, std::string> const&)environment;

- (id)system:(NSString*)aCommand handler:(id)aHandler;
- (void)log:(NSString*)aMessage;
@end
