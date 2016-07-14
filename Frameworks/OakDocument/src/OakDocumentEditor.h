#import <document/OakDocument.h>

namespace ng
{
	struct editor_t;
	struct layout_t;

} /* ng */

PUBLIC @interface OakDocumentEditor : NSObject <OakDocumentEditorProtocol>
+ (instancetype)documentEditorWithDocument:(OakDocument*)aDocument font:(NSFont*)font;
- (instancetype)initWithDocument:(OakDocument*)aDocument font:(NSFont*)font;
@property (nonatomic, readonly) OakDocument* document;
- (ng::editor_t&)editor;
- (ng::layout_t&)layout;

- (void)performReplacements:(std::multimap<std::pair<size_t, size_t>, std::string> const&)someReplacements;
- (BOOL)handleOutput:(std::string const&)string placement:(output::type)place format:(output_format::type)format caret:(output_caret::type)caret inputRanges:(ng::ranges_t const&)ranges environment:(std::map<std::string, std::string> const&)environment;
@end
