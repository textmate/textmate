#include "status.h"
#include <oak/misc.h>

PUBLIC @interface OakSourceControlManager : NSObject
+ (instancetype)sharedInstance;

- (NSString*)rootForPath:(NSString*)aPath;
- (NSString*)scmNameForPath:(NSString*)aPath;
- (NSString*)branchNameForPath:(NSString*)aPath;
- (scm::status::type)statusForFile:(NSString*)aPath;

- (id)addObserverForPath:(NSString*)aPath handler:(void(^)(NSString* path, scm::status::type status))handler;
- (void)removeObserver:(id)anObserver;
@end
