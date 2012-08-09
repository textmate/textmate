#import "FSDataSource.h"
#import <io/events.h>
#import <scm/scm.h>
#import <oak/CocoaSTL.h>

@interface FSDirectoryDataSource : FSDataSource
{
	NSUInteger dataSourceOptions;

	fs::event_callback_t* callback;
	std::map< std::string, objc_ptr<FSItem*> > visible;

	scm::callback_t* scmCallback;
	std::map<std::string, scm::info_ptr> scmDrivers;
	std::map<std::string, size_t> scmReferenceCounts;
}
- (id)initWithURL:(NSURL*)anURL options:(NSUInteger)someOptions;
@end
