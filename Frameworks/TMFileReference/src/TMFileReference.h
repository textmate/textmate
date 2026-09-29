#import <scm/src/status.h>

NS_ASSUME_NONNULL_BEGIN

extern NSNotificationName const TMURLWillCloseNotification;

@interface TMFileReference : NSObject
+ (instancetype)fileReferenceWithURL:(nullable NSURL*)url;
+ (instancetype)fileReferenceWithImage:(NSImage*)image;
+ (NSImage*)imageForURL:(NSURL*)url size:(NSSize)size;

@property (nonatomic, readonly)                      NSImage* image;
@property (nonatomic, readonly)                      NSImage* icon;     // image with alpha = 0.4 when isModified == YES
@property (nonatomic, readonly, getter = isClosable) BOOL     closable;
@property (nonatomic, readonly, getter = isModified) BOOL     modified;

- (void)performClose:(id)sender;

// ==================================================================
// = These methods are for “owners”, e.g. TMDocument and SCMManager =
// ==================================================================

@property (nonatomic) scm::status::type SCMStatus;

- (void)increaseOpenCount;
- (void)decreaseOpenCount;

- (void)increaseModifiedCount;
- (void)decreaseModifiedCount;
@end

NS_ASSUME_NONNULL_END
