@interface NSFileManager (TMFileManagerAdditions)
- (BOOL)tmTrashItemAtURL:(NSURL*)trashURL resultingItemURL:(NSURL**)resultingURL error:(NSError**)error;
@end
