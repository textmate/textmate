#ifndef OAKTEXTVIEW_PRIVATE_H_EKFN2MX9
#define OAKTEXTVIEW_PRIVATE_H_EKFN2MX9

#import "OakTextView.h"
#import <document/OakDocumentEditor.h>
#import <document/OakDocumentController.h>
#import <layout/layout.h>
#import <editor/editor.h>
#import <buffer/indexed_map.h>
#import <settings/settings.h>
#import <text/utf8.h>
#import <ns/ns.h>
#import <OakFoundation/OakFoundation.h>
#import <OakFoundation/NSString Additions.h>

#import "OakSwiftUI-Swift.h"

@class OakAccessibleLink;
@class OakChoiceMenu;
@class LiveSearchView;

typedef indexed_map_t<OakAccessibleLink*> links_t;
typedef std::shared_ptr<links_t> links_ptr;

typedef NS_ENUM(NSUInteger, OakFlagsState) {
	OakFlagsStateClear = 0,
	OakFlagsStateOptionDown,
	OakFlagsStateShiftDown,
	OakFlagsStateShiftTapped,
	OakFlagsStateSecondShiftDown,
};

struct document_view_t : ng::buffer_api_t
{
	document_view_t (OakDocument* document, NSString* themeUUID, std::string const& scopeAttributes, bool scrollPastEnd, CGFloat fontScaleFactor = 1) : _document(document)
	{
		_document_editor = [OakDocumentEditor documentEditorWithDocument:document fontScaleFactor:fontScaleFactor themeUUID:themeUUID];

		_editor = &[_document_editor editor];
		_layout = &[_document_editor layout];

		set_scroll_past_end(scrollPastEnd);

		settings_t const settings = settings_for_path(logical_path(), file_type() + " " + scopeAttributes, path::parent(path()));
		invisibles_map = settings.get(kSettingsInvisiblesMapKey, "");
	}

	bool begin_change_grouping ()                 { return [_document_editor beginChangeGrouping]; }
	bool end_change_grouping ()                   { return [_document_editor endChangeGrouping]; }

	NSFont* font () const                         { return _document_editor.font; }
	void set_font (NSFont* newFont)               { _document_editor.font = newFont; }

	CGFloat font_scale_factor () const            { return _document_editor.fontScaleFactor; }
	void set_font_scale_factor (CGFloat scale)    { _document_editor.fontScaleFactor = scale; }

	void set_command_runner (std::function<void(bundle_command_t const&, ng::buffer_api_t const&, ng::ranges_t const&, std::map<std::string, std::string> const&)> const& runner)
	{
		_command_runner = runner;
	}

	std::map<std::string, std::string> variables (std::string const& scopeAttributes) const
	{
		std::map<std::string, std::string> res = _document.variables;
		res << _editor->editor_variables(scopeAttributes);
		return res;
	}

	std::string symbol () const
	{
		ng::buffer_t const& buf = [_document_editor buffer];
		return buf.symbol_at(ranges().last().first.index);
	}

	std::map<size_t, std::string> symbols () const
	{
		ng::buffer_t const& buf = [_document_editor buffer];
		return buf.symbols();
	}

	bool has_marks (std::string const& type = NULL_STR) const
	{
		return [_document_editor buffer].prev_mark(SIZE_T_MAX, type).second != NULL_STR;
	}

	bool current_line_has_marks (std::string const& type) const
	{
		ng::buffer_t const& buf = [_document_editor buffer];
		size_t n = buf.convert(ranges().last().max().index).line;
		return !buf.get_marks(buf.begin(n), buf.eol(n), type).empty();
	}

	void jump_to_next_bookmark (std::string const& type = NULL_STR)
	{
		std::pair<size_t, std::string> const& pair = [_document_editor buffer].next_mark(ranges().last().max().index, type);
		if(pair.second != NULL_STR)
			set_ranges(ng::index_t(pair.first));
	}

	void jump_to_previous_bookmark (std::string const& type = NULL_STR)
	{
		std::pair<size_t, std::string> const& pair = [_document_editor buffer].prev_mark(ranges().last().max().index, type);
		if(pair.second != NULL_STR)
			set_ranges(ng::index_t(pair.first));
	}

	void toggle_current_bookmark ()
	{
		ng::buffer_t& buf = [_document_editor buffer];
		size_t n = buf.convert(ranges().last().max().index).line;

		std::vector<size_t> toRemove;
		for(auto const& pair : buf.get_marks(buf.begin(n), buf.eol(n), to_s(OakDocumentBookmarkIdentifier)))
			toRemove.push_back(pair.first);

		if(toRemove.empty())
		{
			buf.set_mark(ranges().last().max().index, to_s(OakDocumentBookmarkIdentifier));
		}
		else
		{
			for(auto const& index : toRemove)
				buf.remove_mark(index, to_s(OakDocumentBookmarkIdentifier));
		}
		[NSNotificationCenter.defaultCenter postNotificationName:OakDocumentMarksDidChangeNotification object:_document];
	}

	std::string invisibles_map;

	// ============
	// = Document =
	// ============

	oak::uuid_t identifier () const                 { return to_s(_document.identifier); }
	std::string path () const                       { return to_s(_document.path); }
	std::string directory () const                  { return to_s(_document.directory); }
	std::string virtual_path () const               { return to_s(_document.virtualPath); }
	std::string logical_path () const               { return to_s(_document.virtualPath ?: _document.path); }
	std::string file_type () const                  { return to_s(_document.fileType); }
	void set_file_type (std::string const& newType) { _document.fileType = to_ns(newType); }

	// ==========
	// = Buffer =
	// ==========

	size_t size () const { return [_document_editor buffer].size(); }
	size_t revision () const { return [_document_editor buffer].revision(); }
	std::string operator[] (size_t i) const { return [_document_editor buffer][i]; }
	std::string substr (size_t from = 0, size_t to = SIZE_T_MAX) const { return [_document_editor buffer].substr(from, to != SIZE_T_MAX ? to : size()); }
	std::string xml_substr (size_t from = 0, size_t to = SIZE_T_MAX) const { return [_document_editor buffer].xml_substr(from, to); }
	bool visit_data (std::function<void(char const*, size_t, size_t, bool*)> const& f) const { return [_document_editor buffer].visit_data(f); }
	size_t begin (size_t n) const { return [_document_editor buffer].begin(n); }
	size_t eol (size_t n) const { return [_document_editor buffer].eol(n); }
	size_t end (size_t n) const { return [_document_editor buffer].end(n); }
	size_t lines () const { return [_document_editor buffer].lines(); }
	size_t sanitize_index (size_t i) const { return [_document_editor buffer].sanitize_index(i); }
	size_t convert (text::pos_t const& p) const { return [_document_editor buffer].convert(p); }
	text::pos_t convert (size_t i) const { return [_document_editor buffer].convert(i); }
	void set_tab_size (size_t i) { _document.tabSize = i; }
	size_t tab_size () const { return _document.tabSize; }
	void set_soft_tabs (bool flag) { _document.softTabs = flag; }
	bool soft_tabs () const { return _document.softTabs; }
	text::indent_t indent () const { return text::indent_t(tab_size(), SIZE_T_MAX, soft_tabs()); }
	scope::context_t scope (size_t i, bool includeDynamic = true) const { return [_document_editor buffer].scope(i, includeDynamic); }
	std::map<size_t, scope::scope_t> scopes (size_t from, size_t to) const { return [_document_editor buffer].scopes(from, to); }
	void set_live_spelling (bool flag) { [_document_editor buffer].set_live_spelling(flag); }
	bool live_spelling () const { return [_document_editor buffer].live_spelling(); }
	void set_spelling_language (std::string const& lang) { [_document_editor buffer].set_spelling_language(lang); }
	std::string const& spelling_language () const { return [_document_editor buffer].spelling_language(); }
	std::map<size_t, bool> misspellings (size_t from, size_t to) const { return [_document_editor buffer].misspellings(from, to); }
	std::pair<size_t, size_t> next_misspelling (size_t from) const { return [_document_editor buffer].next_misspelling(from); }
	ns::spelling_tag_t spelling_tag () const { return [_document_editor buffer].spelling_tag(); }
	void recheck_spelling (size_t from, size_t to) { [_document_editor buffer].recheck_spelling(from, to); }
	void add_callback (ng::callback_t* callback) { [_document_editor buffer].add_callback(callback); }
	void remove_callback (ng::callback_t* callback) { [_document_editor buffer].remove_callback(callback); }

	// ================
	// = Undo Manager =
	// ================

	bool can_undo () const { return _document.canUndo; }
	bool can_redo () const { return _document.canRedo; }
	void undo () { [_document undo]; }
	void redo () { [_document redo]; }

	// ==========
	// = Editor =
	// ==========

	ng::editor_delegate_t* delegate () const { return _editor->delegate(); }
	void set_delegate (ng::editor_delegate_t* delegate) { _editor->set_delegate(delegate); }
	void perform (ng::action_t action, ng::indent_correction_t indentCorrections = ng::kIndentCorrectAlways, std::string const& scopeAttributes = NULL_STR) { _editor->perform(action, _layout, indentCorrections, scopeAttributes); }
	bool disallow_tab_expansion () const { return _editor->disallow_tab_expansion(); }
	void insert (std::string const& str, bool selectInsertion = false) { _editor->insert(str, selectInsertion); }
	void insert_with_pairing (std::string const& str, ng::indent_correction_t indentCorrections, bool autoPairing, std::string const& scopeAttributes = NULL_STR) { _editor->insert_with_pairing(str, indentCorrections, autoPairing, scopeAttributes); }
	void move_selection_to (ng::index_t const& index, bool selectInsertion = true) { _editor->move_selection_to(index, selectInsertion); }
	ng::ranges_t replace_all (std::string const& searchFor, std::string const& replaceWith, find::options_t options = find::none, bool searchOnlySelection = false) { return _editor->replace_all(searchFor, replaceWith, options, searchOnlySelection); }
	void perform_replacements (std::multimap<std::pair<size_t, size_t>, std::string> const& replacements) { _editor->perform_replacements(replacements); }
	void delete_tab_trigger (std::string const& str) { _editor->delete_tab_trigger(str); }
	void macro_dispatch (plist::dictionary_t const& args, std::map<std::string, std::string> const& variables) { _editor->macro_dispatch(args, variables, _command_runner); }
	void snippet_dispatch (plist::dictionary_t const& args, std::map<std::string, std::string> const& variables) { _editor->snippet_dispatch(args, variables); }
	std::vector<std::string> const& choices () const { return _editor->choices(); }
	std::string placeholder_content (ng::range_t* placeholderSelection = NULL) const { return _editor->placeholder_content(placeholderSelection); }
	void set_placeholder_content (std::string const& str, size_t selectFrom) { _editor->set_placeholder_content(str, selectFrom); }
	ng::ranges_t ranges () const { return _editor->ranges(); }
	void set_ranges (ng::ranges_t const& r) { _editor->set_selections(r); }
	bool has_selection () const { return _editor->has_selection(); }
	bool handle_result (std::string const& out, output::type placement, output_format::type format, output_caret::type outputCaret, ng::ranges_t const& inputRanges, std::map<std::string, std::string> environment) { return _editor->handle_result(out, placement, format, outputCaret, inputRanges, environment); }
	// ==========
	// = Layout =
	// ==========

	theme_ptr theme () const { return _layout->theme(); }
	void set_theme (theme_ptr const& theme) { _layout->set_theme(theme); }
	void set_wrapping (bool softWrap, size_t wrapColumn) { _layout->set_wrapping(softWrap, wrapColumn); }
	void set_scroll_past_end (bool scrollPastEnd) { _layout->set_scroll_past_end(scrollPastEnd); }
	ng::layout_t::margin_t const& margin () const { return _layout->margin(); }
	bool soft_wrap () const { return _layout->soft_wrap(); }
	size_t wrap_column () const { return _layout->wrap_column(); }
	void set_draw_as_key (bool isKey) { _layout->set_is_key(isKey); }
	void set_draw_caret (bool drawCaret) { _layout->set_draw_caret(drawCaret); }
	void set_draw_wrap_column (bool drawWrapColumn) { _layout->set_draw_wrap_column(drawWrapColumn); }
	void set_draw_indent_guides (bool drawIndentGuides) { _layout->set_draw_indent_guides(drawIndentGuides); }
	void set_drop_marker (ng::index_t dropMarkerIndex) { _layout->set_drop_marker(dropMarkerIndex); }
	void set_viewport (CGRect rect) { _layout->set_viewport_size(rect.size); }
	bool draw_wrap_column () const { return _layout->draw_wrap_column(); }
	bool draw_indent_guides () const { return _layout->draw_indent_guides(); }
	void update_metrics (CGRect visibleRect) { _layout->update_metrics(visibleRect); }
	void draw (ng::context_t const& context, CGRect rectangle, bool isFlipped, ng::ranges_t const& selection, ng::ranges_t const& highlightRanges = ng::ranges_t(), bool drawBackground = true) { _layout->draw(context, rectangle, isFlipped, selection, highlightRanges, drawBackground); }
	ng::index_t index_at_point (CGPoint point) const { return _layout->index_at_point(point); }
	CGRect rect_at_index (ng::index_t const& index, bool bol_as_eol = false, bool wantsBaseline = false) const { return _layout->rect_at_index(index, bol_as_eol, wantsBaseline); }
	CGRect rect_for_range (size_t first, size_t last, bool bol_as_eol = false) const { return _layout->rect_for_range(first, last, bol_as_eol); }
	std::vector<CGRect> rects_for_ranges (ng::ranges_t const& ranges, kRectsIncludeMode mode = kRectsIncludeAll) const { return _layout->rects_for_ranges(ranges, mode); }
	CGFloat width () const { return _layout->width(); }
	CGFloat height () const { return _layout->height(); }
	void begin_refresh_cycle (ng::ranges_t const& selection, ng::ranges_t const& highlightRanges = ng::ranges_t()) { _layout->begin_refresh_cycle(selection, highlightRanges); }
	std::vector<CGRect> end_refresh_cycle (ng::ranges_t const& selection, CGRect visibleRect, ng::ranges_t const& highlightRanges = ng::ranges_t()) { return _layout->end_refresh_cycle(selection, visibleRect, highlightRanges); }
	void did_update_scopes (size_t from, size_t to) { _layout->did_update_scopes(from, to); }
	size_t softline_for_index (ng::index_t const& index) const { return _layout->softline_for_index(index); }
	ng::range_t range_for_softline (size_t softline) const { return _layout->range_for_softline(softline); }
	bool is_line_folded (size_t n) const { return _layout->is_line_folded(n); }
	bool is_line_fold_start_marker (size_t n) const { return _layout->is_line_fold_start_marker(n); }
	bool is_line_fold_stop_marker (size_t n) const { return _layout->is_line_fold_stop_marker(n); }
	void fold (size_t from, size_t to) { _layout->fold(from, to); }
	void unfold (size_t from, size_t to) { _layout->unfold(from, to); }
	void remove_enclosing_folds (size_t from, size_t to) { _layout->remove_enclosing_folds(from, to); }
	void toggle_fold_at_line (size_t n, bool recursive) { _layout->toggle_fold_at_line(n, recursive); }
	void toggle_all_folds_at_level (size_t level) { _layout->toggle_all_folds_at_level(level); }
	std::string folded_as_string () const { return _layout->folded_as_string(); }
	ng::range_t folded_range_at_point (CGPoint point) const { return _layout->folded_range_at_point(point); }
	ng::line_record_t line_record_for (CGFloat y) const { return _layout->line_record_for(y); }
	ng::line_record_t line_record_for (text::pos_t const& pos) const { return _layout->line_record_for(pos); }

private:
	OakDocument* _document;
	OakDocumentEditor* _document_editor;
	std::function<void(bundle_command_t const&, ng::buffer_api_t const&, ng::ranges_t const&, std::map<std::string, std::string> const&)> _command_runner;
	ng::editor_t* _editor;
	ng::layout_t* _layout;
};

static size_t const kNoExpandedDiagnostic = SIZE_T_MAX;

struct diagnostic_banner_t
{
	NSRect rect;
	size_t line;
	NSDictionary* diagnostic;
};

@interface OakTextView () <NSTextInputClient, NSDraggingSource, NSIgnoreMisspelledWords, NSChangeSpelling, NSTextFieldDelegate, NSTouchBarDelegate, NSAccessibilityCustomRotorItemSearchDelegate, OakUserDefaultsObserver>
{
@public
	std::shared_ptr<document_view_t> documentView;
	ng::callback_t* callback;

	BOOL hideCaret;
	NSTimer* blinkCaretTimer;

	NSImage* spellingDotImage;
	NSImage* foldingDotsImage;

	// =================
	// = Mouse Support =
	// =================

	NSPoint mouseDownPos;
	ng::index_t mouseDownIndex;
	NSInteger mouseDownModifierFlags;
	NSInteger mouseDownClickCount;

	BOOL ignoreMouseDown;
	BOOL delayMouseDown;
	BOOL scopeInspectionMouseDown;

	// ===============
	// = Drag'n'drop =
	// ===============

	ng::index_t dropPosition;
	ng::ranges_t pendingMarkedRanges;

	NSString* selectionString;
	BOOL isUpdatingSelection;

	NSMutableArray* macroRecordingArray;

	// ======================
	// = Incremental Search =
	// ======================

	ng::ranges_t liveSearchAnchor;

	// ===================
	// = Snippet Choices =
	// ===================

	std::vector<std::string> choiceVector;

	// ==================
	// = LSP Completion =
	// ==================

	OakCompletionPopup* _lspCompletionPopup;
	OakThemeEnvironment* _lspTheme;
	NSUInteger _lspInitialPrefixLength;
	NSString* _lspFilterPrefix;

	// = Custom Formatter =

	NSString* _lastFormatterError;

	// = LSP Hover =

	OakInfoTooltip* _lspHoverTooltip;
	int _lspHoverRequestId;
	NSMutableDictionary* _lspHoverCache;
	ng::range_t _lspHoverHighlightRange;

	// = LSP References =

	OakReferencesPanel* _lspReferencesPanel;

	// = LSP Rename =

	OakRenameField* _lspRenameField;
	OakRenamePreviewPanel* _lspRenamePreviewPanel;
	NSDictionary* _pendingRenameEdits;
	NSString* _pendingRenameOldName;
	NSString* _pendingRenameNewName;
	size_t _renameRevision;
	size_t _renameCaret;
	text::pos_t _renamePos;

	// Nilable one-shot flag: set before applyWorkspaceEdit so handleApplyEditRequest
	// can auto-ACK the server's mirrored workspace/applyEdit request. Block (not BOOL)
	// to allow future post-edit logic (e.g. scroll to change, toast).
	void (^_codeActionEditCompletion)(void);

	// = LSP Workspace Edit (S3: instance-scoped, replaces static local) =
	id _lastHandledWorkspaceEditRequestId;

	// = Copilot =
	BOOL _copilotCompletionActive;

	// = Copilot Ghost Text =
	NSString* _ghostText;
	NSDictionary* _ghostTextItem;
	size_t _ghostTextCaret;
	int _ghostTextRequestId;
	NSTimer* _ghostTextTimer;
	CGFloat _ghostTextExtraHeight;

	// = LSP Diagnostics =

	std::vector<diagnostic_banner_t> _diagnosticBannerRects;
	BOOL _diagnosticBannersVisible;
	CGFloat _diagnosticLastScrollX;
	__weak NSClipView* _diagnosticObservedClipView;
	size_t _expandedDiagnosticLine;   // kNoExpandedDiagnostic when collapsed
	NSTextView* _expandedDiagnosticBox;
	BOOL _showDiagnosticBannerCursor;

	// =================
	// = Accessibility =
	// =================

	links_ptr _links;

	NSTimer* _scmDiffGutterTimer;
	uint64_t _scmDiffGutterGeneration;
}

// Core methods used by categories
- (void)ensureSelectionIsInVisibleArea:(id)sender;
- (void)updateChoiceMenu:(id)sender;
- (void)resetBlinkCaretTimer;
- (void)updateSelection;
- (void)updateSymbol;
- (void)updateMarkedRanges;
- (void)redisplayFrom:(size_t)from to:(size_t)to;
- (NSImage*)imageForRanges:(ng::ranges_t const&)ranges imageRect:(NSRect*)outRect;
- (void)showToolTip:(NSString*)text;
- (std::map<std::string, std::string>)variables;
- (void)insertSnippetWithOptions:(NSDictionary*)someOptions;

@property (nonatomic, readonly) ng::ranges_t markedRanges;
@property (nonatomic) NSDate* lastFlagsChangeDate;
@property (nonatomic) NSUInteger lastFlags;
@property (nonatomic) OakFlagsState flagsState;
@property (nonatomic) NSTimer* initiateDragTimer;
@property (nonatomic) NSTimer* dragScrollTimer;
@property (nonatomic) BOOL showDragCursor;
@property (nonatomic) BOOL showColumnSelectionCursor;
@property (nonatomic) BOOL showDefinitionCursor;
@property (nonatomic) ng::range_t definitionHighlightRange;
@property (nonatomic) NSTrackingArea* definitionTrackingArea;
@property (nonatomic) OakChoiceMenu* choiceMenu;
@property (nonatomic) LiveSearchView* liveSearchView;
@property (nonatomic, copy) NSString* liveSearchString;
@property (nonatomic) ng::ranges_t liveSearchRanges;
@property (nonatomic, readonly) links_ptr links;
@property (nonatomic) BOOL needsEnsureSelectionIsInVisibleArea;
@property (nonatomic, readwrite) NSString* symbol;
@property (nonatomic) scm::status::type scmStatus;
@end

// Category method declarations for cross-category calls
@interface OakTextView (Copilot)
- (void)scheduleCopilotGhostText;
- (void)clearGhostText;
- (BOOL)hasGhostText;
- (CGFloat)ghostTextExtraHeight;
- (void)acceptGhostText;
- (void)drawGhostText:(CGContextRef)ctx inRect:(NSRect)aRect;
- (void)insertCopilotCompletion:(NSDictionary*)item;
@end

@interface OakTextView (Completion)
- (void)lspComplete:(id)sender;
- (void)ensureCompletionPopup;
- (NSPoint)caretPointForCompletionPopup;
@end

@interface OakTextView (Hover)
- (void)lspRequestHoverAtIndex:(ng::index_t)index;
- (void)cancelLSPHoverRequest;
- (void)dismissLSPHoverPanel;
- (NSMutableAttributedString*)syntaxHighlight:(NSString*)code withGrammar:(NSString*)grammarScope;
- (void)showLSPHoverTooltip:(OakTooltipContent*)content atRect:(NSRect)rect;
- (OakTooltipContent*)createTooltipContentFromHover:(NSDictionary*)hover grammarScope:(NSString*)grammarScope;
- (NSAttributedString*)parseMarkdownToAttributedString:(NSString*)markdown;
- (NSAttributedString*)parseMarkdownDocumentation:(NSString*)text;
@end

@interface OakTextView (LSP)
- (IBAction)lspCodeActions:(id)sender;
- (void)showCodeActionsMenu:(NSArray<NSDictionary*>*)actions;
- (void)showCodeActionsMenu:(NSArray<NSDictionary*>*)actions atPoint:(NSPoint)point;
- (BOOL)canRequestCodeActions;
- (OakThemeEnvironment*)lspTheme;
- (NSDictionary*)bestDefinitionLocation:(NSArray<NSDictionary*>*)locations currentURI:(NSString*)currentUri;
@end

@interface OakTextView (Formatting)
- (void)performFormatOnSave;
@end

@interface OakTextView (Diagnostics)
- (void)drawDiagnosticsInRect:(NSRect)aRect;
- (BOOL)handleDiagnosticBannerClickAtPoint:(NSPoint)point;
- (BOOL)isPointInExpandableDiagnosticBanner:(NSPoint)point;
- (void)collapseExpandedDiagnostic;
- (void)updateDiagnosticBannerCursor;
- (void)lspDiagnosticsDidChange:(NSNotification*)notification;
- (void)diagnosticScrollBoundsDidChange:(NSNotification*)notification;
- (void)diagnosticAccessibilityOptionsDidChange:(NSNotification*)notification;
@end

#endif /* end of include guard: OAKTEXTVIEW_PRIVATE_H_EKFN2MX9 */
