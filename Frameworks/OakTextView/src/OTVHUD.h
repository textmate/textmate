#import <oak/debug.h>

@interface OTVHUD : NSWindowController
+ (OTVHUD*)showHudForView:(NSView*)aView withText:(NSString*)someText;
@end
