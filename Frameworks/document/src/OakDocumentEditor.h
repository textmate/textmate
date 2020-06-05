#import "OakDocument.h"

namespace ng
{
	struct buffer_t;
	struct editor_t;
	struct layout_t;

} /* ng */

@interface OakDocumentEditor : NSObject
+ (instancetype)documentEditorWithDocument:(OakDocument*)aDocument fontScaleFactor:(CGFloat)scale themeUUID:(NSString*)themeUUID;
- (instancetype)initWithDocument:(OakDocument*)aDocument fontScaleFactor:(CGFloat)scale themeUUID:(NSString*)themeUUID;
@property (nonatomic, readonly) OakDocument* document;
@property (nonatomic) ng::ranges_t selection;
- (ng::buffer_t&)buffer;
- (ng::editor_t&)editor;
- (ng::layout_t&)layout;

@property (nonatomic) NSFont* font;
@property (nonatomic) CGFloat fontScaleFactor;

- (BOOL)beginChangeGrouping;
- (BOOL)endChangeGrouping;

- (void)documentWillSave:(OakDocument*)aDocument;
- (void)performReplacements:(std::multimap<std::pair<size_t, size_t>, std::string> const&)someReplacements;
- (BOOL)handleOutput:(std::string const&)string placement:(output::type)place format:(output_format::type)format caret:(output_caret::type)caret inputRanges:(ng::ranges_t const&)ranges environment:(std::map<std::string, std::string> const&)environment;
@end
