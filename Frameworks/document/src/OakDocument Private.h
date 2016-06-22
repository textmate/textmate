#import "OakDocument.h"

namespace document
{
	struct open_callback_t;
	struct save_callback_t;
	struct document_t;

	typedef std::shared_ptr<open_callback_t> open_callback_ptr;
	typedef std::shared_ptr<save_callback_t> save_callback_ptr;
	typedef std::shared_ptr<document_t> document_ptr;

} /* document */

@interface OakDocument (Private)
@property (nonatomic) NSInteger   revision;
@property (nonatomic) NSInteger   savedRevision;
@property (nonatomic) NSString*   backupPath;
@property (nonatomic) NSString*   folded;
@property (nonatomic) NSString*   selection;
@property (nonatomic) ng::index_t visibleIndex;

- (ng::buffer_t&)buffer;
- (ng::undo_manager_t&)undoManager;

- (BOOL)saveBackup:(id)sender;
- (BOOL)performReplacements:(std::multimap<std::pair<size_t, size_t>, std::string> const&)someReplacements checksum:(uint32_t)crc32;

- (BOOL)tryOpenUsingCallback:(document::open_callback_ptr)callback forDocument:(document::document_ptr)document;
- (void)trySaveUsingCallback:(document::save_callback_ptr)callback forDocument:(document::document_ptr)document;
@end
