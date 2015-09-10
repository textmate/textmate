#import <oak/misc.h>

PUBLIC extern NSString* const OakFileManagerWillDeleteItemAtPath;
PUBLIC extern NSString* const OakFileManagerDidChangeContentsOfDirectory;
PUBLIC extern NSString* const OakFileManagerPathKey;

PUBLIC @interface OakFileManager : NSObject
+ (OakFileManager*)sharedInstance;
- (NSURL*)createUntitledDirectoryAtURL:(NSURL*)anURL view:(NSView*)view;
- (BOOL)createFileAtURL:(NSURL*)anURL view:(NSView*)view;
- (NSURL*)createDuplicateOfURL:(NSURL*)srcURL view:(NSView*)view;
- (void)createSymbolicLinkAtURL:(NSURL*)anURL withDestinationURL:(NSURL*)dstURL view:(NSView*)view;
- (BOOL)renameItemAtURL:(NSURL*)srcURL toURL:(NSURL*)dstURL view:(NSView*)view;
- (void)copyItemAtURL:(NSURL*)srcURL toURL:(NSURL*)dstURL view:(NSView*)view;
- (void)moveItemAtURL:(NSURL*)srcURL toURL:(NSURL*)dstURL view:(NSView*)view;
- (void)trashItemAtURL:(NSURL*)anURL view:(NSView*)view;
@end

// Only used in tests so not made public (exported)
NSString* OakReplaceDateInString (NSString* srcPath, NSDate* newDate = [NSDate date]);
