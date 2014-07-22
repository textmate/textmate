#import "FSDataSource.h"

@interface FSXcodeProjectDataSource : FSDataSource
{
	NSString* _developerDirectoryPath;
}
- (id)initWithURL:(NSURL*)anURL options:(NSUInteger)someOptions;
@end
