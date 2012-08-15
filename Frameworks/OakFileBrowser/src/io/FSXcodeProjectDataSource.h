#import "FSDataSource.h"
#import <io/events.h>
#import <scm/scm.h>
#import <oak/CocoaSTL.h>

@class XCProject;

@interface FSXcodeProjectDataSource : FSDataSource
{
	XCProject *_project;
}
- (id)initWithURL:(NSURL*)anURL options:(NSUInteger)someOptions;
@end
