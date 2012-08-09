@protocol HOWebViewDelegateHelperProtocol
- (NSString*)statusText;
- (void)setStatusText:(NSString*)text;
@end

@interface HOWebViewDelegateHelper : NSObject
{
	id delegate;
	NSString* projectUUID;
}
@property (nonatomic, assign) id delegate;
@property (nonatomic, retain) NSString* projectUUID;
@end
