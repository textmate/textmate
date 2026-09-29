#import "OakChooser.h"
#import <scope/src/scope.h>

@interface BundleItemChooser : OakChooser
@property (class, readonly) BundleItemChooser* sharedInstance;
@property (nonatomic) NSString* path;
@property (nonatomic) NSString* directory;
@property (nonatomic) scope::context_t scope;
@property (nonatomic) BOOL hasSelection;
@property (nonatomic) SEL editAction;
@end
