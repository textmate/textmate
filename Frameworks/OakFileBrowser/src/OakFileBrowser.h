#import <oak/misc.h>

extern PUBLIC NSString* OakFileBrowserDidDuplicateURLs;
extern PUBLIC NSString* OakFileBrowserURLMapKey;

@class OakFileBrowser;

@protocol OakFileBrowserDelegate
- (void)fileBrowser:(OakFileBrowser*)aFileBrowser openURLs:(NSArray*)someURLs;
- (void)fileBrowser:(OakFileBrowser*)aFileBrowser closeURL:(NSURL*)anURL;
@end
