@interface FFStatusBarViewController : NSViewController
@property (nonatomic) NSString* statusText;
@property (nonatomic) NSString* alternateStatusText;
@property (nonatomic) BOOL      progressIndicatorVisible;
@property (nonatomic) SEL       stopAction;
@property (nonatomic) id        stopTarget;
@end
