@protocol HOJSBridgeDelegate
@property (nonatomic, assign) BOOL isBusy;
@property (nonatomic, assign) double progress;
@end

@interface HOJSBridge : NSObject
{
	id <HOJSBridgeDelegate> delegate;
	std::map<std::string, std::string> environment;
	BOOL isBusy; // dummy key
	float progress; // dummy key
}
- (id)system:(NSString*)aCommand handler:(id)aHandler;
- (void)log:(NSString*)aMessage;
- (std::map<std::string, std::string> const&)environment;
- (void)setDelegate:(id)aDelegate;
- (void)setEnvironment:(const std::map<std::string, std::string>&)variables;
@end
