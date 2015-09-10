#import <oak/debug.h>

PUBLIC @interface OTVHUD : NSWindowController
+ (OTVHUD*)showHudForView:(NSView*)aView withText:(NSString*)someText;
@end
