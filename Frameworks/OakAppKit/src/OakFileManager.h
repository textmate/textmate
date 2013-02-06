#import <oak/misc.h>

PUBLIC extern NSString* const OakFileManagerDidChangeContentsOfDirectory;
PUBLIC extern NSString* const OakFileManagerDirectoryKey;

PUBLIC @interface OakFileManager : NSObject
+ (OakFileManager*)sharedInstance;
- (NSURL*)createUntitledDirectoryAtURL:(NSURL*)anURL window:(NSWindow*)window;
- (NSURL*)createDuplicateOfURL:(NSURL*)srcURL window:(NSWindow*)window;
- (void)createSymbolicLinkAtURL:(NSURL*)anURL withDestinationURL:(NSURL*)dstURL window:(NSWindow*)window;
- (BOOL)renameItemAtURL:(NSURL*)srcURL toURL:(NSURL*)dstURL window:(NSWindow*)window;
- (void)copyItemAtURL:(NSURL*)srcURL toURL:(NSURL*)dstURL window:(NSWindow*)window;
- (void)moveItemAtURL:(NSURL*)srcURL toURL:(NSURL*)dstURL window:(NSWindow*)window;
- (void)trashItemAtURL:(NSURL*)anURL window:(NSWindow*)window;
@end
