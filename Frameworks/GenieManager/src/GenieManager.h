#include <oak/misc.h>

@class GenieItem;

@interface GenieManager : NSObject
@property (nonatomic, readonly) NSMutableArray<GenieItem*>* items;
@property (nonatomic, readonly) NSDictionary* environment;
@property (nonatomic) NSArray* variables; // Only accessed by Genie Prefs
+ (instancetype)sharedInstance;
+ (NSUserDefaults*)userDefaults;
- (NSString*)cacheFolderByAppendingPathComponent:(NSString*)aPath;
- (BOOL)synchronize;

- (void)runAsActive:(void(^)())callback;
- (void)runAsInactive:(void(^)())callback;
@end
