#import <oak/misc.h>

PUBLIC @interface Preferences : NSWindowController
@property (class, readonly) Preferences* sharedInstance;
@end
