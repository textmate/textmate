@interface NSFileManager (TMFileManagerAdditions)
- (BOOL)tmMoveItemAtURL:(NSURL*)srcURL toURL:(NSURL*)dstURL error:(NSError **)error;
- (BOOL)tmTrashItemAtURL:(NSURL*)trashURL resultingItemURL:(NSURL**)resultingURL error:(NSError**)error;
@end
