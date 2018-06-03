@interface GlobalHotkey : NSObject
+ (instancetype)globalHotkeyForEventString:(NSString*)anEventString handler:(OSStatus(^)())callback;
- (instancetype)initWithEventString:(NSString*)anEventString handler:(OSStatus(^)())callback;
@property (nonatomic, readonly) NSString* eventString;
@end
