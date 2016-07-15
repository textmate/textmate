#import "OakDocumentEditor.h"
#import "OakDocument Private.h"
#import "clipboard.h"
#import <OakAppKit/OakPasteboard.h>
#import <settings/settings.h>
#import <ns/ns.h>
#import <editor/editor.h>
#import <layout/layout.h>

// Defined in OakTextView.h
static int32_t const NSWrapColumnWindowWidth = 0;

@interface OakDocumentEditor ()
{
	std::unique_ptr<ng::editor_t> _editor;
	std::unique_ptr<ng::layout_t> _layout;
}
@property (nonatomic, readwrite) OakDocument* document;
@end

// ====================================
// = OakDocumentEditor Implementation =
// ====================================

@implementation OakDocumentEditor
+ (instancetype)documentEditorWithDocument:(OakDocument*)aDocument font:(NSFont*)font
{
	return [[OakDocumentEditor alloc] initWithDocument:aDocument font:font];
}

- (instancetype)initWithDocument:(OakDocument*)aDocument font:(NSFont*)font
{
	if(self = [self init])
	{
		ASSERT(aDocument.isOpen);

		// TODO Get from somewhere else (settings?)
		bool scrollPastEnd = false;

		_document = aDocument;
		[_document loadModalForWindow:nil completionHandler:nil];

		_editor = std::make_unique<ng::editor_t>([_document buffer]);
		_editor->set_clipboard(get_clipboard(NSGeneralPboard));
		_editor->set_find_clipboard(get_clipboard(NSFindPboard));
		_editor->set_replace_clipboard(get_clipboard(OakReplacePboard));

		settings_t const settings = settings_for_path(to_s(_document.virtualPath ?: _document.path), to_s(_document.fileType), to_s(_document.directory ?: [_document.path stringByDeletingLastPathComponent]));
		std::string const invisibles_map = settings.get(kSettingsInvisiblesMapKey, "");

		bool softWrap     = settings.get(kSettingsSoftWrapKey, false);
		size_t wrapColumn = settings.get(kSettingsWrapColumnKey, NSWrapColumnWindowWidth);

		theme_ptr theme = parse_theme(bundles::lookup(settings.get(kSettingsThemeKey, NULL_STR)));
		_layout = std::make_unique<ng::layout_t>([_document buffer], theme, to_s(font.fontName), font.pointSize, softWrap, scrollPastEnd, wrapColumn, to_s(_document.folded));

		if(settings.get(kSettingsShowWrapColumnKey, false))
			_layout->set_draw_wrap_column(true);

		if(settings.get(kSettingsShowIndentGuidesKey, false))
			_layout->set_draw_indent_guides(true);

		[_document registerDocumentEditor:self];
	}
	return self;
}

- (void)dealloc
{
	_layout.reset();
	_editor.reset();
	[_document close];
	[_document unregisterDocumentEditor:self];
}

- (ng::editor_t&)editor { return *_editor; }
- (ng::layout_t&)layout { return *_layout; }

- (ng::ranges_t)selection
{
	return _editor->ranges();
}

- (void)setSelection:(ng::ranges_t)newSelection
{
	_editor->clear_snippets();
	_editor->set_selections(newSelection);
}

- (void)performReplacements:(std::multimap<std::pair<size_t, size_t>, std::string> const&)someReplacements
{
	[_document undoManager].begin_undo_group(_editor->ranges());
	_editor->perform_replacements(someReplacements);
	[_document undoManager].end_undo_group(_editor->ranges());
	_document.revision = [_document buffer].revision();
}

- (BOOL)handleOutput:(std::string const&)string placement:(output::type)place format:(output_format::type)format caret:(output_caret::type)caret inputRanges:(ng::ranges_t const&)ranges environment:(std::map<std::string, std::string> const&)environment
{
	return _editor->handle_result(string, place, format, caret, ranges, environment);
}
@end
