#import "OakDocument.h"
#import <undo/undo.h> // ng::buffer_t and ng::undo_manager_t types

@interface OakDocument (Private)
@property (nonatomic) NSInteger   revision;
@property (nonatomic) NSInteger   savedRevision;
@property (nonatomic) NSString*   backupPath;
@property (nonatomic) NSString*   folded;

- (ng::buffer_t&)buffer;
- (ng::undo_manager_t&)undoManager;

- (BOOL)saveBackup:(id)sender;
- (BOOL)performReplacements:(std::multimap<std::pair<size_t, size_t>, std::string> const&)someReplacements checksum:(uint32_t)crc32;
@end
