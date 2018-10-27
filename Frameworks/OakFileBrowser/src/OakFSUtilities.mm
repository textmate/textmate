#import "OakFSUtilities.h"
#import <io/io.h>
#import <OakFoundation/NSString Additions.h>

NSURL* kURLLocationComputer;
NSURL* kURLLocationHome;
NSURL* kURLLocationDesktop;
NSURL* kURLLocationFavorites;
NSURL* kURLLocationBundles;

__attribute__((constructor)) // executed after +loads and initializers in linked frameworks
static void initializeConstants ()
{
	@autoreleasepool {
		kURLLocationComputer  = [[NSURL alloc] initWithString:@"computer:///"];
		kURLLocationFavorites = [[NSURL alloc] initFileURLWithPath:[NSString stringWithCxxString:path::join(path::home(), "Library/Application Support/TextMate/Favorites")] isDirectory:YES];
	}
}
