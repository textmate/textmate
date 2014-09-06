#import "OakChooser.h"
#import <scope/scope.h>

PUBLIC @interface BundleItemChooser : OakChooser
+ (instancetype)sharedInstance;
@property (nonatomic) NSString* path;
@property (nonatomic) NSString* directory;
@property (nonatomic) scope::context_t scope;
@property (nonatomic) BOOL hasSelection;
@property (nonatomic) SEL editAction;
@end
