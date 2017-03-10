@interface NSFileManager (TMFileManagerAdditions)
- (BOOL)tmMoveItemAtURL:(NSURL*)srcURL toURL:(NSURL*)dstURL error:(NSError **)error;
@end
