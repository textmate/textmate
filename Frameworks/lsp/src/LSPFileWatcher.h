#ifndef LSP_FILE_WATCHER_H_C7D4E8A2
#define LSP_FILE_WATCHER_H_C7D4E8A2

#import <Foundation/Foundation.h>

@interface LSPFileWatcher : NSObject
- (instancetype)initWithRootDirectory:(NSString*)root excludes:(NSArray<NSString*>*)excludes;
- (void)addExtensions:(NSSet<NSString*>*)exts;
- (void)addExactNames:(NSSet<NSString*>*)names;
@property (nonatomic) BOOL watchAll;
- (void)performInitialScanOnQueue:(dispatch_queue_t)queue completion:(void(^)(void))completion;
- (void)asyncDiffForChangedDirectory:(NSString*)dirPath onQueue:(dispatch_queue_t)queue completion:(void(^)(NSArray<NSDictionary*>*))completion;
- (void)clearSnapshot;
@end

#endif /* LSP_FILE_WATCHER_H_C7D4E8A2 */
