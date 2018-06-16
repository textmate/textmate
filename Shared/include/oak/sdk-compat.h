#if !defined(MAC_OS_X_VERSION_10_12) || (MAC_OS_X_VERSION_MAX_ALLOWED < MAC_OS_X_VERSION_10_12)
@interface NSWindow (Sierra)
+ (void)setAllowsAutomaticWindowTabbing:(BOOL)flag;
@end

@protocol CAAnimationDelegate <NSObject>
@end

#define NSAlertStyleWarning       NSAlertStyleWarning
#define NSAlertStyleInformational NSAlertStyleInformational
#define NSAlertStyleCritical      NSAlertStyleCritical
#endif
