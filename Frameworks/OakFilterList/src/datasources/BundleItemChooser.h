#ifdef OakFilterList_EXPORTS
#import "../OakChooser.h"
#else
#import "OakChooser.h"
#endif
#import <scope/scope.h>

PUBLIC @interface BundleItemChooser : OakChooser
+ (instancetype)sharedInstance;
@property (nonatomic) scope::context_t scope;
@property (nonatomic) BOOL hasSelection;
@property (nonatomic) SEL editAction;
@end
