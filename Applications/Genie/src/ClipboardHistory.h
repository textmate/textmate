@interface ClipboardHistory : NSObject
+ (instancetype)sharedInstance;
- (BOOL)trySetEnabled:(BOOL)flag;
@property (nonatomic) BOOL enabled;
@end
