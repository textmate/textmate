#import <Cocoa/Cocoa.h>
#import "ProjectChanges.h"

@interface ProjectChangesWindowController : NSWindowController
@property (copy) NSString* directory; // follows the window's project; applied by the next refresh
- (instancetype)initWithDirectory:(NSString*)directory buffers:(NSDictionary<NSString*, ProjectChangesBuffer*>* (^)(void))buffers openFile:(void (^)(NSString*, NSUInteger))openFile;
- (IBAction)refresh:(id)sender;
@end
