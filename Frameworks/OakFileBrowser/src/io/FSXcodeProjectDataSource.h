#import "FSDataSource.h"
#import <io/events.h>
#import <scm/scm.h>
#import <oak/CocoaSTL.h>

@class XCProject;

@interface FSXcodeProjectDataSource : FSDataSource
{
	NSMutableDictionary* _projects;
	NSString* _developerDirectoryPath;
}
- (id)initWithURL:(NSURL*)anURL options:(NSUInteger)someOptions;
@end
