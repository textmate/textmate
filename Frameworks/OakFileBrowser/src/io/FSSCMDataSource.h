#import "FSDataSource.h"

@interface FSSCMDataSource : FSDataSource
+ (NSURL*)scmURLWithPath:(NSString*)aPath;
- (id)initWithURL:(NSURL*)anURL options:(NSUInteger)someOptions;
@end
