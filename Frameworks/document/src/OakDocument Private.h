#import "OakDocument.h"
#import <undo/src/undo.h> // ng::buffer_t and ng::undo_manager_t types

@interface OakDocument (Private)
- (instancetype)initWithPath:(NSString*)aPath;

@property (nonatomic) NSUInteger  untitledCount;
@property (nonatomic) NSString*   folded;

- (ng::buffer_t&)buffer;
- (ng::undo_manager_t&)undoManager;

- (BOOL)performReplacements:(std::multimap<std::pair<size_t, size_t>, std::string> const&)someReplacements checksum:(uint32_t)crc32;
@end
