#import "OakView.h"
#import <oak/misc.h>

PUBLIC @interface OakKeyEquivalentView : OakView
@property (nonatomic) NSString* eventString;
@property (nonatomic) BOOL disableGlobalHotkeys;
@property (nonatomic) BOOL recording;
@end
