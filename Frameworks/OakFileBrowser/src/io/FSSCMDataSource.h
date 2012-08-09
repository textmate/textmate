#import "FSDataSource.h"
#import <scm/scm.h>

@interface FSSCMDataSource : FSDataSource
{
	NSUInteger options;

	scm::info_ptr scmInfo;
	scm::callback_t* scmCallback;
}
+ (NSURL*)scmURLWithPath:(NSString*)aPath;
- (id)initWithURL:(NSURL*)anURL options:(NSUInteger)someOptions;
@end
