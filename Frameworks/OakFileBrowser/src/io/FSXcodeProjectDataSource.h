#import "FSDataSource.h"

@interface FSXcodeProjectDataSource : FSDataSource
{
	NSMutableDictionary* _projects;
	NSString* _developerDirectoryPath;
}
- (id)initWithURL:(NSURL*)anURL options:(NSUInteger)someOptions;
@end
