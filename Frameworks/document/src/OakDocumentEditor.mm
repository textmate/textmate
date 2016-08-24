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
+ (instancetype)documentEditorWithDocument:(OakDocument*)aDocument fontScaleFactor:(NSInteger)scale
{
	return [[OakDocumentEditor alloc] initWithDocument:aDocument fontScaleFactor:scale];
}

- (instancetype)initWithDocument:(OakDocument*)aDocument fontScaleFactor:(NSInteger)scale
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

		NSString* fontName = to_ns(settings.get(kSettingsFontNameKey, NULL_STR));
		CGFloat fontSize = settings.get(kSettingsFontSizeKey, 11.0);
		_font = fontName ? [NSFont fontWithName:fontName size:fontSize] : [NSFont userFixedPitchFontOfSize:fontSize];
		_fontScaleFactor = scale;

		bool softWrap     = settings.get(kSettingsSoftWrapKey, false);
		size_t wrapColumn = settings.get(kSettingsWrapColumnKey, NSWrapColumnWindowWidth);

		theme_ptr theme = parse_theme(bundles::lookup(settings.get(kSettingsThemeKey, NULL_STR)));
		_layout = std::make_unique<ng::layout_t>([_document buffer], theme, to_s(_font.fontName), _font.pointSize * _fontScaleFactor / 100, softWrap, scrollPastEnd, wrapColumn, to_s(_document.folded));

		if(settings.get(kSettingsShowWrapColumnKey, false))
			_layout->set_draw_wrap_column(true);

		if(settings.get(kSettingsShowIndentGuidesKey, false))
			_layout->set_draw_indent_guides(true);

		[_document registerDocumentEditor:self];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(documentContentDidChange:) name:OakDocumentContentDidChangeNotification object:_document];
	}
	return self;
}

- (void)dealloc
{
	[[NSNotificationCenter defaultCenter] removeObserver:self name:OakDocumentContentDidChangeNotification object:_document];
	[self documentWillSave:_document];
	_layout.reset();
	_editor.reset();
	[_document close];
	[_document unregisterDocumentEditor:self];
}

- (ng::buffer_t&)buffer { return [_document buffer]; }
- (ng::editor_t&)editor { return *_editor; }
- (ng::layout_t&)layout { return *_layout; }

- (void)setFont:(NSFont*)newFont
{
	_font = newFont;
	_layout->set_font(to_s(_font.fontName), _font.pointSize * _fontScaleFactor / 100);
}

- (void)setFontScaleFactor:(NSInteger)scale
{
	_fontScaleFactor = scale;
	_layout->set_font(to_s(_font.fontName), _font.pointSize * _fontScaleFactor / 100);
}

- (ng::ranges_t)selection
{
	return _editor->ranges();
}

- (void)setSelection:(ng::ranges_t)newSelection
{
	_editor->clear_snippets();
	_editor->set_selections(newSelection);
}

- (void)documentWillSave:(OakDocument*)aDocument
{
	if(!_layout || !_editor)
		return;

	text::selection_t ranges;
	for(auto const& range : _editor->ranges())
	{
		text::pos_t from = [self buffer].convert(range.first.index);
		text::pos_t to   = [self buffer].convert(range.last.index);
		from.offset = range.first.carry;
		to.offset   = range.last.carry;
		ranges.push_back(text::range_t(from, to, range.columnar));
	}
	aDocument.selection = to_ns(ranges);
	aDocument.folded = to_ns(_layout->folded_as_string());
}

- (void)documentContentDidChange:(NSNotification*)aNotification
{
	_editor->sanitize_selection();
}

- (void)performReplacements:(std::multimap<std::pair<size_t, size_t>, std::string> const&)someReplacements
{
	[_document beginUndoGrouping];
	_editor->perform_replacements(someReplacements);
	[_document endUndoGrouping];
}

- (BOOL)handleOutput:(std::string const&)string placement:(output::type)place format:(output_format::type)format caret:(output_caret::type)caret inputRanges:(ng::ranges_t const&)ranges environment:(std::map<std::string, std::string> const&)environment
{
	return _editor->handle_result(string, place, format, caret, ranges, environment);
}
@end
