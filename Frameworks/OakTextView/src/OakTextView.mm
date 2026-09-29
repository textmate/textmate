#import "OakTextView.h"
#import <oak/TextMateResponderActions.h>
#import "OakChoiceMenu.h"
#import "OakDocumentView.h" // addAuxiliaryView:atEdge: signature
#import "OakCommandRefresh.h"
#import "LiveSearchView.h"
#import "OTVHUD.h"
#import <OakCommand/src/OakCommand.h>
#import <OakAppKit/src/OakAppKit.h>
#import <OakAppKit/src/NSAlert Additions.h>
#import <OakAppKit/src/NSEvent Additions.h>
#import <OakAppKit/src/NSImage Additions.h>
#import <OakAppKit/src/NSMenuItem Additions.h>
#import <OakAppKit/src/OakPasteboard.h>
#import <OakAppKit/src/OakPopOutAnimation.h>
#import <OakAppKit/src/OakToolTip.h>
#import <OakAppKit/src/OakSound.h>
#import <OakFoundation/src/NSString Additions.h>
#import <OakFoundation/src/OakFoundation.h>
#import <OakFoundation/src/OakFindProtocol.h>
#import <OakSystem/src/application.h>
#import <crash/src/info.h>
#import <buffer/src/indexed_map.h>
#import <BundleMenu/src/BundleMenu.h>
#import <BundlesManager/src/BundlesManager.h>
#import <Preferences/src/Keys.h>
#import <bundles/src/bundles.h>
#import <cf/src/cf.h>
#import <command/src/runner.h>
#import <document/src/OakDocumentEditor.h>
#import <document/src/OakDocumentController.h>
#import <file/src/type.h>
#import <layout/src/layout.h>
#import <ns/src/ns.h>
#import <ns/src/spellcheck.h>
#import <text/src/case.h>
#import <text/src/classification.h>
#import <text/src/format.h>
#import <text/src/newlines.h>
#import <text/src/trim.h>
#import <text/src/utf16.h>
#import <text/src/utf8.h>
#import <settings/src/settings.h>
#import <oak/debug.h>
#import <editor/src/editor.h>
#import <editor/src/write.h>
#import <io/src/exec.h>
#import <Find/src/Find.h>

int32_t const NSWrapColumnWindowWidth =  0;
int32_t const NSWrapColumnAskUser     = -1;

NSString* const kUserDefaultsWrapColumnPresetsKey  = @"wrapColumnPresets";
NSString* const kUserDefaultsFontSmoothingKey      = @"fontSmoothing";
NSString* const kUserDefaultsDisableTypingPairsKey = @"disableTypingPairs";
NSString* const kUserDefaultsScrollPastEndKey      = @"scrollPastEnd";

@interface OakAccessibleLink : NSObject
- (id)initWithTextView:(OakTextView*)textView range:(ng::range_t)range title:(NSString*)title URL:(NSString*)URL frame:(NSRect)frame;
@property (nonatomic, weak) OakTextView* textView;
@property (nonatomic) ng::range_t range;
@property (nonatomic) NSString* title;
@property (nonatomic) NSString* URL;
@property (nonatomic) NSRect frame;
@end

@implementation OakAccessibleLink
- (id)initWithTextView:(OakTextView*)textView range:(ng::range_t)range title:(NSString*)title URL:(NSString*)URL frame:(NSRect)frame
{
	if((self = [super init]))
	{
		_textView = textView;
		_range = range;
		_title = title;
		_URL = URL;
		_frame = frame;
	}
	return self;
}

- (NSString*)description
{
	return [NSString stringWithFormat:@"[%@](%@), range = %@, frame = %@", self.title, self.URL, [NSString stringWithCxxString:to_s(self.range)], NSStringFromRect(self.frame)];
}

- (BOOL)isEqual:(id)object
{
	if([object isKindOfClass:[OakAccessibleLink class]])
	{
		OakAccessibleLink* link = (OakAccessibleLink*)object;
		return self.range == link.range && [self.textView isEqual:link.textView];
	}
	return NO;
}

- (NSUInteger)hash
{
	return [self.textView hash] + _range.min().index + _range.max().index;
}

- (BOOL)accessibilityIsIgnored
{
	return NO;
}

- (NSSet*)myAccessibilityAttributeNames
{
	static NSSet* set = [NSSet setWithArray:@[
		NSAccessibilityRoleAttribute,
		NSAccessibilityRoleDescriptionAttribute,
		NSAccessibilitySubroleAttribute,
		NSAccessibilityParentAttribute,
		NSAccessibilityWindowAttribute,
		NSAccessibilityTopLevelUIElementAttribute,
		NSAccessibilityPositionAttribute,
		NSAccessibilitySizeAttribute,
		NSAccessibilityTitleAttribute,
		NSAccessibilityURLAttribute,
	]];
	return set;
}

- (NSArray*)accessibilityAttributeNames
{
	static NSArray* attributes = [[self myAccessibilityAttributeNames] allObjects];
	return attributes;
}

- (id)accessibilityAttributeValue:(NSString*)attribute
{
	id value = nil;

	if([attribute isEqualToString:NSAccessibilityRoleAttribute]) {
		value = NSAccessibilityLinkRole;
	} else if([attribute isEqualToString:NSAccessibilitySubroleAttribute]) {
		value = NSAccessibilityTextLinkSubrole;
	} else if([attribute isEqualToString:NSAccessibilityRoleDescriptionAttribute]) {
		value = NSAccessibilityRoleDescriptionForUIElement(self);
	} else if([attribute isEqualToString:NSAccessibilityParentAttribute]) {
		value = self.textView;
	} else if([attribute isEqualToString:NSAccessibilityWindowAttribute] || [attribute isEqualToString:NSAccessibilityTopLevelUIElementAttribute]) {
		value = [self.textView accessibilityAttributeValue:attribute];
	} else if([attribute isEqualToString:NSAccessibilityPositionAttribute] || [attribute isEqualToString:NSAccessibilitySizeAttribute]) {
		NSRect frame = NSAccessibilityFrameInView(self.textView, self.frame);
		if([attribute isEqualToString:NSAccessibilityPositionAttribute])
			value = [NSValue valueWithPoint:frame.origin];
		else
			value = [NSValue valueWithSize:frame.size];
	} else if([attribute isEqualToString:NSAccessibilityTitleAttribute]) {
		value = self.title;
	} else if([attribute isEqualToString:NSAccessibilityURLAttribute]) {
		value = self.URL;
	} else {
		@throw [NSException exceptionWithName:NSAccessibilityException reason:[NSString stringWithFormat:@"Getting accessibility attribute not supported: %@", attribute] userInfo:nil];
	}

	return value;
}

- (BOOL)accessibilityIsAttributeSettable:(NSString*)attribute
{
	if([[self myAccessibilityAttributeNames] containsObject:attribute])
		return NO;
	return [super accessibilityIsAttributeSettable:attribute];
}

- (void)accessibilitySetValue:(id)value forAttribute:(NSString*)attribute
{
	if([[self myAccessibilityAttributeNames] containsObject:attribute])
		@throw [NSException exceptionWithName:NSAccessibilityException reason:[NSString stringWithFormat:@"Setting accessibility attribute not supported: %@", attribute] userInfo:nil];
	[super accessibilitySetValue:value forAttribute:attribute];
}

- (NSArray*)accessibilityParameterizedAttributeNames
{
	return @[];
}

- (id)accessibilityAttributeValue:(NSString*)attribute forParameter:(id)parameter
{
	@throw [NSException exceptionWithName:NSAccessibilityException reason:[NSString stringWithFormat:@"Accessibility parameterized attribute not supported: %@", attribute] userInfo:nil];
}

- (NSArray*)accessibilityActionNames
{
	static NSArray* actions = nil;
	if(!actions)
	{
		actions = @[
			NSAccessibilityPressAction,
		];
	}
	return actions;
}

- (NSString*)accessibilityActionDescription:(NSString*)action
{
	return NSAccessibilityActionDescription(action);
}

- (void)accessibilityPerformAction:(NSString*)action
{
	if([action isEqualToString:NSAccessibilityPressAction])
	{
		// TODO
	}
	else
	{
		@throw [NSException exceptionWithName:NSAccessibilityException reason:[NSString stringWithFormat:@"Accessibility action not supported: %@", action] userInfo:nil];
	}
}

- (id)accessibilityHitTest:(NSPoint)point
{
	return self;
}

- (id)accessibilityFocusedUIElement
{
	return NSAccessibilityUnignoredAncestor(self.textView);
}
@end

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

	std::multimap<size_t, std::pair<std::string, std::string>> all_marks () const
	{
		ng::buffer_t const& buf = [_document_editor buffer];
		return buf.get_marks(0, buf.size());
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
	void draw (ng::context_t const& context, CGRect rectangle, bool isFlipped, ng::ranges_t const& selection, ng::ranges_t const& highlightRanges = ng::ranges_t(), bool drawBackground = true, std::function<void()> const& drawBackgroundDecorations = {}) { _layout->draw(context, rectangle, isFlipped, selection, highlightRanges, drawBackground, drawBackgroundDecorations); }
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

@interface OakTextView () <NSTextInputClient, NSDraggingSource, NSIgnoreMisspelledWords, NSChangeSpelling, NSTextFieldDelegate, NSTouchBarDelegate, NSAccessibilityCustomRotorItemSearchDelegate, OakUserDefaultsObserver, NSMenuItemValidation>
{
	std::shared_ptr<document_view_t> documentView;
	ng::callback_t* callback;

	BOOL hideCaret;
	NSTimer* blinkCaretTimer;

	NSImage* spellingDotImage;
	NSImage* foldingDotsImage;

	// ======================
	// = Diagnostic Overlay =
	// ======================

	struct diagnostic_info_t
	{
		std::string type;    // error / warning / note
		std::string message; // message with length prefix and fix payload stripped
		std::string fixJSON; // empty when the mark carries no fix
		size_t index;        // buffer index of the mark
	};
	std::map<size_t, std::vector<diagnostic_info_t>> diagnosticsByLine;
	std::map<size_t, std::string> diagnosticLineTint;
	std::vector<std::pair<ng::range_t, std::string>> diagnosticSquiggles;
	std::map<size_t, NSRect> diagnosticPillRects;   // line → clickable icon capsule
	std::map<size_t, NSRect> diagnosticBannerRects; // line → full banner; padding clicks are swallowed
	std::map<size_t, NSTextField*> diagnosticBannerTextFields;
	NSPopover* diagnosticsPopover;
	NSMutableArray<NSDictionary*>* diagnosticFixQueue;

	// =================
	// = Mouse Support =
	// =================

	NSPoint mouseDownPos;
	ng::index_t mouseDownIndex;
	NSInteger mouseDownModifierFlags;
	NSInteger mouseDownClickCount;

	BOOL ignoreMouseDown;  // set when the mouse down is the same event which caused becomeFirstResponder:
	BOOL delayMouseDown; // set when mouseUp: should process lastMouseDownEvent

	// ===============
	// = Drag’n’drop =
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

	// =================
	// = Accessibility =
	// =================

	links_ptr _links;
}
- (void)ensureSelectionIsInVisibleArea:(id)sender;
- (void)updateChoiceMenu:(id)sender;
- (void)resetBlinkCaretTimer;
- (void)updateSelection;
- (void)updateSymbol;
- (void)updateMarkedRanges;
- (void)redisplayFrom:(size_t)from to:(size_t)to;
- (NSImage*)imageForRanges:(ng::ranges_t const&)ranges imageRect:(NSRect*)outRect;
@property (nonatomic, readonly) ng::ranges_t markedRanges;
@property (nonatomic) NSDate* lastFlagsChangeDate;
@property (nonatomic) NSUInteger lastFlags;
@property (nonatomic) OakFlagsState flagsState;
@property (nonatomic) NSTimer* initiateDragTimer;
@property (nonatomic) NSTimer* dragScrollTimer;
@property (nonatomic) BOOL showDragCursor;
@property (nonatomic) BOOL showColumnSelectionCursor;
@property (nonatomic) OakChoiceMenu* choiceMenu;
@property (nonatomic) LiveSearchView* liveSearchView;
@property (nonatomic, copy) NSString* liveSearchString;
@property (nonatomic) ng::ranges_t liveSearchRanges;
@property (nonatomic, readonly) links_ptr links;
@property (nonatomic) BOOL needsEnsureSelectionIsInVisibleArea;
@property (nonatomic, readwrite) NSString* symbol;
@property (nonatomic) scm::status::type scmStatus;
@end

static std::vector<bundles::item_ptr> items_for_tab_expansion (std::shared_ptr<document_view_t> const& documentView, ng::ranges_t const& ranges, std::string const& scopeAttributes, ng::range_t* range)
{
	size_t caret = ranges.last().min().index;
	size_t line  = documentView->convert(caret).line;
	size_t bol   = documentView->begin(line);

	bool lastWasWordChar           = false;
	std::string lastCharacterClass = ng::kCharacterClassUnknown;

	scope::scope_t const rightScope = ng::scope(*documentView, ng::ranges_t(caret), scopeAttributes).right;
	for(size_t i = bol; i < caret; i += (*documentView)[i].size())
	{
		// we don’t use text::is_word_char because that function treats underscores as word characters, which is undesired, see <issue://157>.
		bool isWordChar = CFCharacterSetIsLongCharacterMember(CFCharacterSetGetPredefined(kCFCharacterSetAlphaNumeric), utf8::to_ch((*documentView)[i]));
		std::string characterClass = ng::character_class(*documentView, i);

		if(i == bol || lastWasWordChar != isWordChar || lastCharacterClass != characterClass || !isWordChar)
		{
			std::vector<bundles::item_ptr> const& items = bundles::query(bundles::kFieldTabTrigger, documentView->substr(i, caret), scope::context_t(ng::scope(*documentView, ng::ranges_t(i), scopeAttributes).left, rightScope));
			if(!items.empty())
			{
				if(range)
					*range = ng::range_t(i, caret);
				return items;
			}
		}

		lastWasWordChar    = isWordChar;
		lastCharacterClass = characterClass;
	}

	return std::vector<bundles::item_ptr>();
}

static ng::ranges_t merge (ng::ranges_t lhs, ng::ranges_t const& rhs)
{
	for(auto const& range : rhs)
		lhs.push_back(range);
	return lhs;
}

static NSRect TextViewportRect (OakTextView* textView)
{
	if(NSScrollView* scrollView = [textView enclosingScrollView])
	{
		NSRect res = scrollView.contentView.bounds;
		res.size = scrollView.contentView.frame.size;
		// With a vertical ruler, AppKit reports a content-view frame that still
		// includes the ruler. Soft wrapping must use the remaining text width.
		if(scrollView.rulersVisible && scrollView.hasVerticalRuler)
			res.size.width = std::max<CGFloat>(0, NSWidth(res) - scrollView.verticalRulerView.requiredThickness);
		return res;
	}
	return textView.visibleRect;
}

struct refresh_helper_t
{
	refresh_helper_t (OakTextView* self, std::shared_ptr<document_view_t> const& documentView) : _self(self), _document_view(documentView)
	{
		if(documentView->begin_change_grouping())
		{
			_revision  = documentView->revision();
			_selection = documentView->ranges();
			documentView->begin_refresh_cycle(merge(_selection, [_self markedRanges]), [_self liveSearchRanges]);
		}
	}

	~refresh_helper_t ()
	{
		if(auto documentView = _document_view.lock())
		{
			if(documentView->end_change_grouping())
			{
				if(_revision == documentView->revision())
				{
					for(auto const& range : ng::highlight_ranges_for_movement(*documentView, _selection, documentView->ranges()))
					{
						NSRect imageRect;
						NSImage* image = [_self imageForRanges:range imageRect:&imageRect];
						imageRect = [[_self window] convertRectToScreen:[_self convertRect:imageRect toView:nil]];
						OakShowPopOutAnimation(_self, imageRect, image);
					}
				}

				if(_revision != documentView->revision() || _selection != documentView->ranges())
				{
					[_self updateMarkedRanges];
					[_self updateSelection];
					[_self updateSymbol];
				}

				NSRect r = TextViewportRect(_self);
				documentView->set_viewport(r);
				auto damagedRects = documentView->end_refresh_cycle(merge(documentView->ranges(), [_self markedRanges]), [_self visibleRect], [_self liveSearchRanges]);

				NSSize newSize = NSMakeSize(std::max(NSWidth(r), documentView->width()), std::max(NSHeight(r), documentView->height()));
				if(!NSEqualSizes([_self frame].size, newSize))
					[_self setFrameSize:newSize];

				NSView* gutterView = find_gutter_view([[_self enclosingScrollView] superview]);
				for(auto const& rect : damagedRects)
				{
					[_self setNeedsDisplayInRect:rect];
					if(gutterView)
					{
						NSRect r = rect;
						r.origin.x = 0;
						r.size.width = NSWidth([gutterView frame]);
						[gutterView setNeedsDisplayInRect:r];
					}
				}

				if(_revision != documentView->revision() || _selection != documentView->ranges() || _self.needsEnsureSelectionIsInVisibleArea)
				{
					[_self ensureSelectionIsInVisibleArea:nil];
					[_self resetBlinkCaretTimer];
					[_self updateChoiceMenu:nil];
				}
			}
		}
	}

private:
	static NSView* find_gutter_view (NSView* view)
	{
		for(NSView* candidate in [view subviews])
		{
			if([candidate isKindOfClass:NSClassFromString(@"GutterView")])
				return candidate;
			else if(NSView* res = find_gutter_view(candidate))
				return res;
		}
		return nil;
	}

	OakTextView* _self;
	std::weak_ptr<document_view_t> _document_view;
	size_t _revision;
	ng::ranges_t _selection;
};

#define AUTO_REFRESH refresh_helper_t _dummy(self, documentView)

static std::string shell_quote (std::vector<std::string> paths)
{
	std::transform(paths.begin(), paths.end(), paths.begin(), &path::escape);
	return text::join(paths, " ");
}

// =============================
// = OakTextView’s Find Server =
// =============================

@interface OakTextViewFindServer : NSObject <OakFindServerProtocol>
@property (nonatomic) OakTextView*     textView;
@property (nonatomic) find_operation_t findOperation;
@property (nonatomic) find::options_t  findOptions;
@end

@implementation OakTextViewFindServer
+ (id)findServerWithTextView:(OakTextView*)aTextView operation:(find_operation_t)anOperation options:(find::options_t)someOptions
{
	OakTextViewFindServer* res = [OakTextViewFindServer new];
	res.textView      = aTextView;
	res.findOperation = anOperation;
	res.findOptions   = someOptions;
	return res;
}

- (NSString*)findString      { return [OakPasteboard.findPasteboard current].string;    }
- (NSString*)replaceString   { return [OakPasteboard.replacePasteboard current].string; }

- (void)showToolTip:(NSString*)aToolTip
{
	OakShowToolTip(aToolTip, [self.textView positionForWindowUnderCaret]);
	NSAccessibilityPostNotificationWithUserInfo(self.textView, NSAccessibilityAnnouncementRequestedNotification, @{ NSAccessibilityAnnouncementKey: aToolTip });
}

- (void)didFind:(NSUInteger)aNumber occurrencesOf:(NSString*)aFindString atPosition:(text::pos_t const&)aPosition wrapped:(BOOL)didWrap
{
	NSString* format = nil;
	switch(aNumber)
	{
		case 0:  format = @"No more %@ “%@”.";                break;
		case 1:  format = didWrap ? @"Search wrapped." : nil; break;
		default: format = @"%3$@ %@ “%@”.";                   break;
	}

	NSString* classifier = (self.findOptions & find::regular_expression) ? @"matches for" : @"occurrences of";
	if(format)
		[self showToolTip:[NSString stringWithFormat:format, classifier, aFindString, [NSNumberFormatter localizedStringFromNumber:@(aNumber) numberStyle:NSNumberFormatterDecimalStyle]]];
}

- (void)didReplace:(NSUInteger)aNumber occurrencesOf:(NSString*)aFindString with:(NSString*)aReplacementString
{
	static NSString* const formatStrings[2][3] = {
		{ @"Nothing replaced (no occurrences of “%@”).", @"Replaced one occurrence of “%@”.", @"Replaced %2$@ occurrences of “%@”." },
		{ @"Nothing replaced (no matches for “%@”).",    @"Replaced one match of “%@”.",      @"Replaced %2$@ matches of “%@”."     }
	};
	NSString* format = formatStrings[(self.findOptions & find::regular_expression) ? 1 : 0][aNumber > 2 ? 2 : aNumber];
	[self showToolTip:[NSString stringWithFormat:format, aFindString, [NSNumberFormatter localizedStringFromNumber:@(aNumber) numberStyle:NSNumberFormatterDecimalStyle]]];
}
@end

@implementation OakTextView
// =================================
// = OakTextView Delegate Wrappers =
// =================================

- (NSString*)scopeAttributes
{
	if([self.delegate respondsToSelector:@selector(scopeAttributes)])
		return [self.delegate scopeAttributes];
	return @"";
}

// =================================

- (NSImage*)imageForRanges:(ng::ranges_t const&)ranges imageRect:(NSRect*)outRect
{
	NSRect srcRect = NSZeroRect, visibleRect = [self visibleRect];
	for(auto const& range : ranges)
		srcRect = NSUnionRect(srcRect, NSIntersectionRect(visibleRect, documentView->rect_for_range(range.min().index, range.max().index, true)));

	NSBezierPath* clip = [NSBezierPath bezierPath];
	for(auto const& rect : documentView->rects_for_ranges(ranges))
		[clip appendBezierPath:[NSBezierPath bezierPathWithRect:NSOffsetRect(rect, -NSMinX(srcRect), -NSMinY(srcRect))]];

	NSImage* image = [[NSImage alloc] initWithSize:NSMakeSize(std::max<CGFloat>(NSWidth(srcRect), 1), std::max<CGFloat>(NSHeight(srcRect), 1))];
	[image lockFocusFlipped:[self isFlipped]];
	[clip addClip];

	CGContextRef context = NSGraphicsContext.currentContext.CGContext;
	CGContextTranslateCTM(context, -NSMinX(srcRect), -NSMinY(srcRect));

	NSRectClip(srcRect);
	documentView->draw(context, srcRect, [self isFlipped], ng::ranges_t(), ng::ranges_t(), false);

	[image unlockFocus];

	if(outRect)
		*outRect = srcRect;

	return image;
}

- (void)highlightRanges:(ng::ranges_t const&)ranges
{
	if(ranges.empty())
		return;

	for(auto const& range : ranges)
		documentView->remove_enclosing_folds(range.min().index, range.max().index);
	[self ensureSelectionIsInVisibleArea:self];

	BOOL firstRange = YES;
	for(auto const& range : ranges)
	{
		NSRect imageRect;
		NSImage* image = [self imageForRanges:range imageRect:&imageRect];
		imageRect = [[self window] convertRectToScreen:[self convertRect:imageRect toView:nil]];
		OakShowPopOutAnimation(self, imageRect, image, firstRange);
		firstRange = NO;
	}
}

- (void)scrollIndexToFirstVisible:(ng::index_t const&)visibleIndex
{
	if(documentView && visibleIndex && visibleIndex.index < documentView->size())
	{
		documentView->update_metrics(CGRectMake(0, CGRectGetMinY(documentView->rect_at_index(visibleIndex)), CGFLOAT_MAX, NSHeight([self visibleRect])));
		[self reflectDocumentSize];

		CGRect rect = documentView->rect_at_index(visibleIndex);
		if(CGRectGetMinX(rect) <= documentView->margin().left)
			rect.origin.x = 0;
		if(CGRectGetMinY(rect) <= documentView->margin().top)
			rect.origin.y = 0;
		rect.size = [self visibleRect].size;

		[self scrollRectToVisible:CGRectIntegral(rect)];
	}
}

- (void)updateDocumentMetadata
{
	if(_document && documentView)
		_document.visibleIndex = documentView->index_at_point([self visibleRect].origin);
}

- (NSString*)effectiveThemeUUID
{
	settings_t const settings = settings_for_path(to_s(_document.virtualPath ?: _document.path), to_s(_document.fileType), to_s(_document.directory ?: [_document.path stringByDeletingLastPathComponent]));
	std::string const scopedThemeUUID = settings.get(kSettingsThemeKey);
	if(scopedThemeUUID != NULL_STR)
		return to_ns(scopedThemeUUID);

	NSString* appearance = [NSUserDefaults.standardUserDefaults stringForKey:@"themeAppearance"];
	BOOL darkMode = [appearance isEqualToString:@"dark"];
	if(@available(macOS 10.14, *))
	{
		if(!darkMode && ![appearance isEqualToString:@"light"]) // If it is not ‘light’ then assume ‘auto’
			darkMode = [[self.effectiveAppearance bestMatchFromAppearancesWithNames:@[ NSAppearanceNameAqua, NSAppearanceNameDarkAqua ]] isEqualToString:NSAppearanceNameDarkAqua];
	}

	return [NSUserDefaults.standardUserDefaults stringForKey:darkMode ? @"darkModeThemeUUID" : @"universalThemeUUID"];
}

- (void)setThemeUUID:(NSString*)newThemeUUID
{
	if(_themeUUID && [_themeUUID isEqualToString:newThemeUUID])
		return;
	_themeUUID = newThemeUUID;

	if(bundles::item_ptr const& themeItem = bundles::lookup(to_s(_themeUUID)))
		self.theme = parse_theme(themeItem);
}

- (void)setDocument:(OakDocument*)aDocument
{
	if(aDocument && [_document isEqual:aDocument])
	{
		if(_document.selection)
		{
			ng::ranges_t ranges = ng::convert(*documentView, to_s(_document.selection));
			documentView->set_ranges(ranges);
			for(auto const& range : ranges)
				documentView->remove_enclosing_folds(range.min().index, range.max().index);

			[self ensureSelectionIsInVisibleArea:self];
			[self updateSelection];
			[self updateSymbol];
		}
		[self resetBlinkCaretTimer];
		return;
	}

	CGFloat fontScaleFactor = 1;
	if(documentView)
	{
		fontScaleFactor = documentView->font_scale_factor();

		[NSNotificationCenter.defaultCenter removeObserver:self name:OakDocumentWillSaveNotification object:_document];
		[NSNotificationCenter.defaultCenter removeObserver:self name:OakDocumentDidSaveNotification object:_document];
		[NSNotificationCenter.defaultCenter removeObserver:self name:OakDocumentWillReloadNotification object:_document];
		[NSNotificationCenter.defaultCenter removeObserver:self name:OakDocumentDidReloadNotification object:_document];
		[NSNotificationCenter.defaultCenter removeObserver:self name:OakDocumentMarksDidChangeNotification object:_document];

		[self updateDocumentMetadata];

		documentView->remove_callback(callback);
		delete callback;
		callback = NULL;

		delete documentView->delegate();
		documentView->set_delegate(NULL);

		self.choiceMenu = nil;
		choiceVector.clear();

		documentView.reset();
	}

	if((_document = aDocument))
	{
		_scmStatus = scm::status::unknown;

		[self willChangeValueForKey:@"themeUUID"];
		_themeUUID = self.effectiveThemeUUID;
		[self didChangeValueForKey:@"themeUUID"];

		documentView = std::make_shared<document_view_t>(_document, _themeUUID, to_s(self.scopeAttributes), self.scrollPastEnd, fontScaleFactor);
		documentView->set_command_runner([self](bundle_command_t const& cmd, ng::buffer_api_t const& buffer, ng::ranges_t const& selection, std::map<std::string, std::string> const& variables){
			[self executeBundleCommand:cmd buffer:buffer selection:selection variables:variables];
		});

		BOOL hasFocus = (self.keyState & (OakViewViewIsFirstResponderMask|OakViewWindowIsKeyMask|OakViewApplicationIsActiveMask)) == (OakViewViewIsFirstResponderMask|OakViewWindowIsKeyMask|OakViewApplicationIsActiveMask);
		documentView->set_draw_as_key(hasFocus);

		struct buffer_refresh_callback_t : ng::callback_t
		{
			buffer_refresh_callback_t (OakTextView* textView) : textView(textView) { }
			void did_parse (size_t from, size_t to)                                { [textView redisplayFrom:from to:to]; [textView updateSymbol]; }
			void did_replace (size_t from, size_t to, char const* buf, size_t len) { NSAccessibilityPostNotification(textView, NSAccessibilityValueChangedNotification); }

		private:
			__weak OakTextView* textView;
		};

		callback = new buffer_refresh_callback_t(self);

		struct textview_delegate_t : ng::editor_delegate_t
		{
			textview_delegate_t (OakTextView* textView) : _self(textView) { }

			std::map<std::string, std::string> variables_for_bundle_item (bundles::item_ptr item)
			{
				return [_self variablesForBundleItem:item];
			}

			OakTextView* _self;
		};

		documentView->set_delegate(new textview_delegate_t(self));

		ng::index_t visibleIndex = _document.visibleIndex;
		if(_document.selection)
		{
			ng::ranges_t ranges = ng::convert(*documentView, to_s(_document.selection));
			documentView->set_ranges(ranges);
			for(auto const& range : ranges)
				documentView->remove_enclosing_folds(range.min().index, range.max().index);
		}

		[self reflectDocumentSize];
		[self updateSelection];
		[self updateSymbol];

		if(visibleIndex && visibleIndex.index < documentView->size())
				[self scrollIndexToFirstVisible:visibleIndex];
		else	[self ensureSelectionIsInVisibleArea:self];

		documentView->add_callback(callback);

		// TODO Pre and post save actions should be handled by OakDocument once we have OakDocumentEditor
		[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(documentWillSave:) name:OakDocumentWillSaveNotification object:_document];
		[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(documentDidSave:) name:OakDocumentDidSaveNotification object:_document];
		[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(documentWillReload:) name:OakDocumentWillReloadNotification object:_document];
		[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(documentDidReload:) name:OakDocumentDidReloadNotification object:_document];
		[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(documentMarksDidChange:) name:OakDocumentMarksDidChangeNotification object:_document];

		[self resetBlinkCaretTimer];
		[self setNeedsDisplay:YES];
		_links.reset();
		NSAccessibilityPostNotification(self, NSAccessibilityValueChangedNotification);

		if(hasFocus)
			[NSFontManager.sharedFontManager setSelectedFont:self.font isMultiple:NO];
	}
}

- (id)initWithFrame:(NSRect)aRect
{
	if(self = [super initWithFrame:aRect])
	{
		settings_t const& settings = settings_for_path();

		_showInvisibles = settings.get(kSettingsShowInvisiblesKey, false);
		_scrollPastEnd  = [NSUserDefaults.standardUserDefaults boolForKey:kUserDefaultsScrollPastEndKey];
		_antiAlias      = ![NSUserDefaults.standardUserDefaults boolForKey:kUserDefaultsDisableAntiAliasKey];
		_fontSmoothing  = (OTVFontSmoothing)[NSUserDefaults.standardUserDefaults integerForKey:kUserDefaultsFontSmoothingKey];

		spellingDotImage = [NSImage imageNamed:@"SpellingDot" inSameBundleAsClass:[self class]];
		foldingDotsImage = [NSImage imageNamed:@"FoldingDots Template" inSameBundleAsClass:[self class]];

		[self registerForDraggedTypes:[[self class] dropTypes]];

		[self bind:@"scmStatus" toObject:self withKeyPath:@"document.scmStatus" options:nil];
		OakObserveUserDefaults(self);
	}
	return self;
}

- (void)setScmStatus:(scm::status::type)newStatus
{
	if(_scmStatus == newStatus)
		return;

	BOOL notifyHooks = _scmStatus != scm::status::unknown || newStatus != scm::status::none;
	_scmStatus = newStatus;
	if(notifyHooks)
		[self performSelector:@selector(runDidChangeSCMStatusCallbacks:) withObject:self afterDelay:0];
}

- (void)runDidChangeSCMStatusCallbacks:(id)sender
{
	for(auto const& item : bundles::query(bundles::kFieldSemanticClass, "callback.document.did-change-scm-status", [self scopeContext], bundles::kItemTypeMost, oak::uuid_t(), false))
		[self performBundleItem:item];
}

- (void)setNilValueForKey:(NSString*)key
{
	// scmStatus can be nil because we bind to self.document.scmStatus
	if(![key isEqualToString:@"scmStatus"])
		[super setNilValueForKey:key];
}

- (void)dealloc
{
	[NSNotificationCenter.defaultCenter removeObserver:self];
	[self unbind:@"scmStatus"];
	[self setDocument:nil];
}

- (void)documentWillSave:(NSNotification*)aNotification
{
	for(auto const& item : bundles::query(bundles::kFieldSemanticClass, "callback.document.will-save", [self scopeContext], bundles::kItemTypeMost, oak::uuid_t(), false))
		[self performBundleItem:item];

	[self updateDocumentMetadata];
}

- (void)documentDidSave:(NSNotification*)aNotification
{
	for(auto const& item : bundles::query(bundles::kFieldSemanticClass, "callback.document.did-save", [self scopeContext], bundles::kItemTypeMost, oak::uuid_t(), false))
		[self performBundleItem:item];
}

- (void)documentWillReload:(NSNotification*)aNotification
{
	for(auto const& item : bundles::query(bundles::kFieldSemanticClass, "callback.document.will-reload", [self scopeContext], bundles::kItemTypeMost, oak::uuid_t(), false))
		[self performBundleItem:item];
}

- (void)documentDidReload:(NSNotification*)aNotification
{
	for(auto const& item : bundles::query(bundles::kFieldSemanticClass, "callback.document.did-reload", [self scopeContext], bundles::kItemTypeMost, oak::uuid_t(), false))
		[self performBundleItem:item];
}

- (void)documentMarksDidChange:(NSNotification*)aNotification
{
	[self setNeedsDisplay:YES];
}

- (void)viewDidMoveToSuperview
{
	[super viewDidMoveToSuperview];
	[NSNotificationCenter.defaultCenter removeObserver:self name:NSViewBoundsDidChangeNotification object:nil];
	if(NSClipView* clipView = [[self enclosingScrollView] contentView])
	{
		clipView.postsBoundsChangedNotifications = YES;
		[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(diagnosticScrollBoundsDidChange:) name:NSViewBoundsDidChangeNotification object:clipView];
	}
}

- (void)diagnosticScrollBoundsDidChange:(NSNotification*)aNotification
{
	if(diagnosticPillRects.empty())
		return;

	// The message pills anchor to the visible rect’s right edge, so scrolling
	// must redraw the horizontal band of every marked line — both to erase the
	// pill at its old position and to draw it at the new one.
	NSRect const visible = self.visibleRect;
	for(auto const& pair : diagnosticPillRects)
	{
		NSRect band = pair.second;
		band.origin.x   = NSMinX(visible);
		band.size.width = NSWidth(visible);
		[self setNeedsDisplayInRect:band];
	}
}

- (void)reflectDocumentSize
{
	if(documentView && [self enclosingScrollView])
	{
		NSRect r = TextViewportRect(self);
		documentView->set_viewport(r);
		NSSize newSize = NSMakeSize(std::max(NSWidth(r), documentView->width()), std::max(NSHeight(r), documentView->height()));
		if(!NSEqualSizes([self frame].size, newSize))
			[self setFrameSize:newSize];
	}
}

- (void)resizeWithOldSuperviewSize:(NSSize)oldBoundsSize
{
	if(documentView)
			[self reflectDocumentSize];
	else	[super resizeWithOldSuperviewSize:oldBoundsSize];
}

- (void)centerSelectionInVisibleArea:(id)sender
{
	[self recordSelector:_cmd withArgument:nil];

	CGRect r = documentView->rect_at_index(documentView->ranges().last().last);
	CGFloat w = NSWidth([self visibleRect]), h = NSHeight([self visibleRect]);

	CGFloat x = r.origin.x < w ? 0 : r.origin.x - w/2;
	CGFloat y = std::clamp(r.origin.y - (h-r.size.height)/2, NSMinY(self.frame), NSHeight(self.frame) - h);

	[self scrollRectToVisible:CGRectMake(round(x), round(y), w, h)];
}

- (void)ensureSelectionIsInVisibleArea:(id)sender
{
	self.needsEnsureSelectionIsInVisibleArea = NO;
	if([[self.window currentEvent] type] == NSEventTypeLeftMouseDragged) // User is drag-selecting
		return;

	ng::range_t range = documentView->ranges().last();
	CGRect r = documentView->rect_at_index(range.last);
	CGRect s = [self visibleRect];

	CGFloat x = NSMinX(s), w = NSWidth(s);
	CGFloat y = NSMinY(s), h = NSHeight(s);

	if(range.unanchored)
	{
		CGRect a = documentView->rect_at_index(range.first);
		CGFloat top = NSMinY(a), bottom = NSMaxY(r);
		if(bottom < top)
		{
			top = NSMinY(r);
			bottom = NSMaxY(a);
		}

		// If top or bottom of selection is outside viewport we center selection
		if(bottom - top < h && (top < y || y + h < bottom))
		{
			y = top - 0.5 * (h - (bottom - top));
			goto doScroll;
		}

		// If selection is taller than viewport then we don’t do anything
		if(bottom - top > h)
			return;
	}

	if(x + w - 2*r.size.width < r.origin.x)
		x = r.origin.x + 5*r.size.width - w;
	else if(r.origin.x < x + 2*r.size.width)
		x = r.origin.x < w/2 ? 0 : r.origin.x - 5*r.size.width;

	if(std::clamp<CGFloat>(r.origin.y, y + h - 1.5*r.size.height, y + h + 1.5*r.size.height) == r.origin.y) // scroll down
		y = r.origin.y + 1.5*r.size.height - h;
	else if(std::clamp<CGFloat>(r.origin.y, y - 3*r.size.height, y + 0.5*r.size.height) == r.origin.y) // scroll up
		y = r.origin.y - 0.5*r.size.height;
	else if(std::clamp(r.origin.y, y, y + h) != r.origin.y) // center y
		y = r.origin.y - (h-r.size.height)/2;

doScroll:
	CGRect b = [self bounds];
	x = std::clamp(x, NSMinX(b), NSMaxX(b) - w);
	y = std::clamp(y, NSMinY(b), NSMaxY(b) - h);

	NSClipView* contentView = [[self enclosingScrollView] contentView];
	if([contentView respondsToSelector:@selector(_extendNextScrollRelativeToCurrentPosition)])
		[contentView performSelector:@selector(_extendNextScrollRelativeToCurrentPosition)]; // Workaround for <rdar://9295929>
	[self scrollRectToVisible:CGRectMake(round(x), round(y), w, h)];
}

- (void)updateChoiceMenu:(id)sender
{
	if(choiceVector == documentView->choices())
		return;

	self.choiceMenu = nil;
	choiceVector    = documentView->choices();

	if(!choiceVector.empty())
	{
		_choiceMenu = [OakChoiceMenu new];
		_choiceMenu.font = [NSFont fontWithName:self.font.fontName size:self.font.pointSize * documentView->font_scale_factor()];
		_choiceMenu.choices = (__bridge NSArray*)((CFArrayRef)cf::wrap(choiceVector));

		std::string const& currentChoice = documentView->placeholder_content();
		for(size_t i = choiceVector.size(); i-- > 0; )
		{
			if(choiceVector[i] == currentChoice)
				_choiceMenu.choiceIndex = i;
		}

		[_choiceMenu showAtTopLeftPoint:[self positionForWindowUnderCaret] forView:self];
	}
}

// ======================
// = Generic view stuff =
// ======================

+ (BOOL)isCompatibleWithResponsiveScrolling
{
	return [NSUserDefaults.standardUserDefaults boolForKey:@"enableResponsiveScroll"];
}

- (BOOL)acceptsFirstResponder       { return YES; }
- (BOOL)isFlipped                   { return YES; }
- (BOOL)isOpaque                    { return YES; }

- (void)redisplayFrom:(size_t)from to:(size_t)to
{
	AUTO_REFRESH;
	documentView->did_update_scopes(from, to);
	_links.reset();
}

- (BOOL)clipsToBounds {
	return YES;
}

// ====================
// = Diagnostic Marks =
// ====================

static NSColor* OTVColorForDiagnosticMark (std::string const& markType, CGFloat alpha)
{
	if(markType == "error")
		return [NSColor colorWithSRGBRed:0.90 green:0.23 blue:0.20 alpha:alpha];
	else if(markType == "warning")
		return [NSColor colorWithSRGBRed:0.95 green:0.68 blue:0.09 alpha:alpha];
	else if(markType == "note")
		return [NSColor colorWithSRGBRed:0.33 green:0.60 blue:0.90 alpha:alpha];
	return nil;
}

static size_t OTVDiagnosticSeverity (std::string const& markType)
{
	if(markType == "error")   return 3;
	if(markType == "warning") return 2;
	if(markType == "note")    return 1;
	return 0;
}

// Mark content format: “<length>|<message>” optionally followed by U+001F and
// a JSON fix payload. A legacy payload is one { "message": …, "edits": … }
// object; newer payloads use { "fixes": [ … ] } to offer multiple actions.
// Edit rows/columns are 1-based.
// The length (in UTF-8 bytes) sizes the squiggle; without it we underline to
// the end of the word at the mark’s position.
static size_t OTVParseDiagnosticContent (std::string const& content, std::string& message, std::string& fixJSON)
{
	size_t len = 0, i = 0;
	while(i < content.size() && isdigit(content[i]))
		len = len * 10 + (content[i++] - '0');
	if(!(i && i < content.size() && content[i] == '|'))
		i = len = 0;
	else
		++i;

	std::string::size_type sep = content.find('\x1F', i);
	message = content.substr(i, sep == std::string::npos ? std::string::npos : sep - i);
	fixJSON = sep == std::string::npos ? std::string() : content.substr(sep + 1);
	return len;
}

static NSArray<NSDictionary*>* OTVDiagnosticFixes (std::string const& fixJSON, BOOL includeOptionFixes)
{
	if(fixJSON.empty())
		return @[];

	NSData* data = [NSData dataWithBytes:fixJSON.data() length:fixJSON.size()];
	id object = [NSJSONSerialization JSONObjectWithData:data options:0 error:nil];
	if(![object isKindOfClass:NSDictionary.class])
		return @[];

	NSDictionary* payload = object;
	NSArray* fixes = [payload[@"fixes"] isKindOfClass:NSArray.class] ? payload[@"fixes"] : ([payload[@"edits"] isKindOfClass:NSArray.class] ? @[ payload ] : @[]);
	NSMutableArray<NSDictionary*>* result = [NSMutableArray array];
	for(id fix in fixes)
	{
		if([fix isKindOfClass:NSDictionary.class] && (includeOptionFixes || ![fix[@"requiresOption"] boolValue]))
			[result addObject:fix];
	}
	return result;
}

static NSString* OTVPyrightConfigByDisablingRule (NSString* contents, NSString* rule)
{
	if(!contents || ![rule hasPrefix:@"report"] || [rule rangeOfCharacterFromSet:NSCharacterSet.alphanumericCharacterSet.invertedSet].location != NSNotFound)
		return nil;

	NSString* escapedRule = [NSRegularExpression escapedPatternForString:rule];
	NSString* keyPattern = [NSString stringWithFormat:@"\\\"%@\\\"\\s*:", escapedRule];
	NSRegularExpression* keyExpression = [NSRegularExpression regularExpressionWithPattern:keyPattern options:0 error:nil];
	if([keyExpression firstMatchInString:contents options:0 range:NSMakeRange(0, contents.length)])
	{
		NSString* valuePattern = [NSString stringWithFormat:@"(\\\"%@\\\"\\s*:\\s*)(?:\\\"(?:none|information|warning|error)\\\"|true|false)", escapedRule];
		NSRegularExpression* valueExpression = [NSRegularExpression regularExpressionWithPattern:valuePattern options:0 error:nil];
		NSTextCheckingResult* match = [valueExpression firstMatchInString:contents options:0 range:NSMakeRange(0, contents.length)];
		if(!match)
			return nil;
		NSMutableString* result = [contents mutableCopy];
		[result replaceCharactersInRange:[match rangeAtIndex:0] withString:[NSString stringWithFormat:@"%@\"none\"", [contents substringWithRange:[match rangeAtIndex:1]]]];
		return result;
	}

	NSRange openingBrace = [contents rangeOfString:@"{"];
	NSRange closingBrace = [contents rangeOfString:@"}" options:NSBackwardsSearch];
	if(openingBrace.location == NSNotFound || closingBrace.location == NSNotFound || openingBrace.location > closingBrace.location)
		return nil;

	NSString* propertyPattern = @"(?m)^([ \\t]+)\\\"[^\\\"]+\\\"[ \\t]*:";
	NSRegularExpression* propertyExpression = [NSRegularExpression regularExpressionWithPattern:propertyPattern options:0 error:nil];
	NSRange const objectRange = NSMakeRange(openingBrace.location + 1, closingBrace.location - openingBrace.location - 1);
	NSTextCheckingResult* property = [propertyExpression firstMatchInString:contents options:0 range:objectRange];
	NSRegularExpression* anyPropertyExpression = [NSRegularExpression regularExpressionWithPattern:@"\\\"[^\\\"]+\\\"\\s*:" options:0 error:nil];
	BOOL const hasProperties = [anyPropertyExpression firstMatchInString:contents options:0 range:objectRange] != nil;
	NSString* indent = property ? [contents substringWithRange:[property rangeAtIndex:1]] : @"    ";
	NSString* newline = [contents containsString:@"\r\n"] ? @"\r\n" : @"\n";
	NSString* insertion = [NSString stringWithFormat:@"%@%@\"%@\": \"none\"%@", newline, indent, rule, hasProperties ? @"," : @""];
	return [contents stringByReplacingCharactersInRange:NSMakeRange(openingBrace.location + 1, 0) withString:insertion];
}

- (void)updateDiagnosticMarks
{
	diagnosticsByLine.clear();
	diagnosticLineTint.clear();
	diagnosticSquiggles.clear();

	for(auto const& pair : documentView->all_marks())
	{
		size_t const index         = documentView->sanitize_index(pair.first);
		std::string const& type    = pair.second.first;
		std::string const& content = pair.second.second;

		size_t const severity = OTVDiagnosticSeverity(type);
		if(severity == 0)
			continue;

		size_t const line = documentView->convert(index).line;
		auto tint = diagnosticLineTint.find(line);
		if(tint == diagnosticLineTint.end() || OTVDiagnosticSeverity(tint->second) < severity)
			diagnosticLineTint[line] = type;

		diagnostic_info_t info;
		info.type  = type;
		info.index = index;
		size_t markLen = OTVParseDiagnosticContent(content, info.message, info.fixJSON);
		auto& lineInfos = diagnosticsByLine[line];
		auto insertAt = std::find_if(lineInfos.begin(), lineInfos.end(), [&type](diagnostic_info_t const& rhs){ return OTVDiagnosticSeverity(rhs.type) < OTVDiagnosticSeverity(type); });
		lineInfos.insert(insertAt, info);

		size_t const eol = documentView->eol(line);
		size_t to = index + std::min(markLen, eol - index);
		if(to == index) // no length given: extend to end of word
		{
			std::string const rest = documentView->substr(index, eol);
			while(to - index < rest.size() && (isalnum((unsigned char)rest[to - index]) || rest[to - index] == '_'))
				++to;
		}
		to = documentView->sanitize_index(std::clamp(to, index, eol));
		if(to <= index && index < eol)
			to = std::min(index + (*documentView)[index].size(), eol);
		if(index < to)
			diagnosticSquiggles.emplace_back(ng::range_t(ng::index_t(index), ng::index_t(to)), type);
	}
}

- (void)drawDiagnosticBackgroundInGutterRect:(NSRect)dirtyRect bounds:(NSRect)bounds
{
	[self updateDiagnosticMarks];

	for(auto const& pair : diagnosticLineTint)
	{
		CGRect r = documentView->rect_for_range(documentView->begin(pair.first), documentView->eol(pair.first));
		r.origin.x   = NSMinX(bounds);
		r.size.width = NSWidth(bounds);
		if(!NSIntersectsRect(r, dirtyRect))
			continue;
		[OTVColorForDiagnosticMark(pair.second, 0.12) set];
		NSRectFillUsingOperation(NSIntersectionRect(r, dirtyRect), NSCompositingOperationSourceOver);
	}
}

- (void)drawDiagnosticBackgroundInRect:(NSRect)aRect
{
	for(auto const& pair : diagnosticLineTint)
	{
		CGRect r = documentView->rect_for_range(documentView->begin(pair.first), documentView->eol(pair.first));
		r.origin.x   = NSMinX(self.bounds);
		r.size.width = NSWidth(self.bounds);
		if(!NSIntersectsRect(r, aRect))
			continue;
		[OTVColorForDiagnosticMark(pair.second, 0.12) set];
		NSRectFillUsingOperation(NSIntersectionRect(r, aRect), NSCompositingOperationSourceOver);
	}
}

- (void)drawDiagnosticForegroundInRect:(NSRect)aRect
{
	diagnosticPillRects.clear();
	diagnosticBannerRects.clear();
	std::set<size_t> visibleBannerTextFields;

	for(auto const& pair : diagnosticSquiggles)
	{
		NSColor* color = OTVColorForDiagnosticMark(pair.second, 0.85);
		for(CGRect rect : documentView->rects_for_ranges(pair.first))
		{
			if(!NSIntersectsRect(NSInsetRect(rect, -2, -2), aRect))
				continue;

			CGFloat const amplitude  = 0.7;
			CGFloat const wavelength = 6;
			CGFloat const y = NSMaxY(rect) - amplitude * 3;

			NSBezierPath* path = [NSBezierPath bezierPath];
			[path moveToPoint:NSMakePoint(NSMinX(rect), y)];
			CGFloat x = NSMinX(rect);
			BOOL up = YES;
			while(x < NSMaxX(rect))
			{
				CGFloat nextX = std::min(x + wavelength/2, NSMaxX(rect));
				[path curveToPoint:NSMakePoint(nextX, y)
					 controlPoint1:NSMakePoint(x + (nextX-x)/2, y + (up ? -amplitude*2 : amplitude*2))
					 controlPoint2:NSMakePoint(x + (nextX-x)/2, y + (up ? -amplitude*2 : amplitude*2))];
				up = !up;
				x = nextX;
			}
			path.lineWidth = 1;
			[color set];
			[path stroke];
		}
	}

	// Xcode-style issue banner per marked line, right-aligned and never
	// overlapping the code: a solid icon capsule (with issue count when a line
	// has several) plus a tinted message section that truncates — or vanishes,
	// leaving only the icon — when the code runs long. The message is selectable,
	// while clicking the icon opens the diagnostics popover (see mouseDown:).
	// Rects are stored for hit-testing even when outside the dirty rect.
	NSRect const visible = self.visibleRect;
	NSFont* pillFont = [NSFont systemFontOfSize:documentView->font().pointSize * documentView->font_scale_factor()]; // match the editor’s effective text size
	BOOL const isDark = self.theme->is_dark();
	NSColor* messageColor = [NSColor colorWithCGColor:self.theme->foreground()] ?: (isDark ? NSColor.whiteColor : NSColor.blackColor);

	for(auto const& pair : diagnosticsByLine)
	{
		size_t const line = pair.first;
		auto const& infos = pair.second;
		std::string const& type = infos.front().type;

		CGRect const lineRect = documentView->rect_for_range(documentView->begin(line), documentView->eol(line));

		CGFloat const pillH     = NSHeight(lineRect); // banner and icon share the full height of the line bar
		CGFloat const iconW     = std::max<CGFloat>(round(pillH), 20);
		CGFloat const padX      = 7;
		CGFloat const rightEdge = NSMaxX(visible) - 8;
		CGFloat const codeGap   = 16; // minimum distance kept between end of code and banner

		// Space available to the right of the code on this line
		CGFloat const available = rightEdge - (NSMaxX(lineRect) + codeGap);

		NSMutableParagraphStyle* pStyle = [NSMutableParagraphStyle new];
		pStyle.lineBreakMode = NSLineBreakByTruncatingTail;
		NSDictionary* attrs = @{
			NSFontAttributeName: pillFont,
			NSForegroundColorAttributeName: messageColor,
			NSParagraphStyleAttributeName: pStyle
		};
		NSAttributedString* str = [[NSAttributedString alloc] initWithString:[NSString stringWithUTF8String:infos.front().message.c_str()] ?: @"" attributes:attrs];

		CGFloat textWidth = ceil(str.size.width) + 2 * padX;

		// Shrink the message into the free space; drop it entirely when tight.
		if(available < iconW + 40)
			textWidth = 0; // icon-only capsule
		else if(iconW + textWidth > available)
			textWidth = available - iconW;

		NSRect pill;
		pill.size.width  = iconW + textWidth;
		pill.size.height = pillH;
		pill.origin.x    = rightEdge - pill.size.width;
		pill.origin.y    = NSMinY(lineRect);

		NSRect const iconRect = NSMakeRect(NSMinX(pill), NSMinY(pill), iconW, pillH);
		diagnosticPillRects[line]   = iconRect; // only the icon capsule opens the popover
		diagnosticBannerRects[line] = pill;     // padding outside the selectable text swallows clicks

		if(textWidth > 0 && NSIntersectsRect(pill, visible))
		{
			NSTextField* textField = diagnosticBannerTextFields[line];
			if(!textField)
			{
				textField = [NSTextField labelWithAttributedString:str];
				textField.selectable = YES;
				textField.focusRingType = NSFocusRingTypeNone;
				textField.lineBreakMode = NSLineBreakByTruncatingTail;
				textField.usesSingleLineMode = YES;
				[self addSubview:textField];
				diagnosticBannerTextFields[line] = textField;
			}
			if(![textField.attributedStringValue isEqualToAttributedString:str])
				textField.attributedStringValue = str;
			textField.frame = NSMakeRect(NSMinX(pill) + iconW + padX, round(NSMidY(pill) - str.size.height/2) - 1, textWidth - 2*padX, str.size.height);
			visibleBannerTextFields.insert(line);
		}

		if(!NSIntersectsRect(pill, aRect))
			continue;

		CGFloat const radius = std::min<CGFloat>(5, pillH / 2);
		NSBezierPath* capsule = [NSBezierPath bezierPathWithRoundedRect:pill xRadius:radius yRadius:radius];

		[NSGraphicsContext saveGraphicsState];
		[capsule addClip];

		// opaque base so the banner stays readable over anything beneath it
		//[(isDark ? [NSColor colorWithWhite:0.12 alpha:1] : [NSColor colorWithWhite:1 alpha:1]) set];
		//NSRectFill(pill);
		[OTVColorForDiagnosticMark(type, 0.16) set];
		NSRectFillUsingOperation(pill, NSCompositingOperationSourceOver);

		// solid icon section
		[OTVColorForDiagnosticMark(type, 0.7) set];
		//NSRectFill(iconRect);
		NSRectFillUsingOperation(iconRect, NSCompositingOperationSourceOver);

		[NSGraphicsContext restoreGraphicsState];

		// icon glyph — issue count when the line has more than one diagnostic
		std::string glyph = infos.size() > 1 ? std::to_string(infos.size()) : (type == "error" ? "✕" : (type == "warning" ? "!" : "i"));
		NSAttributedString* glyphStr = [[NSAttributedString alloc] initWithString:[NSString stringWithUTF8String:glyph.c_str()] ?: @"" attributes:@{ NSFontAttributeName: [NSFont boldSystemFontOfSize:pillFont.pointSize], NSForegroundColorAttributeName: NSColor.whiteColor }];
		NSSize const glyphSize = glyphStr.size;
		[glyphStr drawAtPoint:NSMakePoint(round(NSMidX(iconRect) - glyphSize.width/2), round(NSMidY(iconRect) - glyphSize.height/2) - 1)];

	}

	for(auto it = diagnosticBannerTextFields.begin(); it != diagnosticBannerTextFields.end();)
	{
		if(visibleBannerTextFields.count(it->first))
			++it;
		else
		{
			[it->second removeFromSuperview];
			it = diagnosticBannerTextFields.erase(it);
		}
	}
}

- (void)showDiagnosticsPopoverForLine:(size_t)line fromRect:(NSRect)aRect includeOptionFixes:(BOOL)includeOptionFixes
{
	auto it = diagnosticsByLine.find(line);
	if(it == diagnosticsByLine.end())
		return;

	[diagnosticsPopover close];
	diagnosticFixQueue = [NSMutableArray array];

	NSStackView* stack = [NSStackView new];
	stack.orientation = NSUserInterfaceLayoutOrientationVertical;
	stack.alignment   = NSLayoutAttributeWidth; // rows stretch so Apply buttons right-align
	stack.spacing     = 8;
	stack.edgeInsets  = NSEdgeInsetsMake(12, 12, 12, 12);

	// Wrapping labels report no intrinsic width, so the popover would collapse
	// unless we measure the content and pin the stack to an explicit width.
	NSFont* messageFont = [NSFont systemFontOfSize:documentView->font().pointSize * documentView->font_scale_factor()]; // match the editor’s effective text size
	CGFloat maxMessageWidth = 0;
	CGFloat maxFixWidth = 0;
	BOOL anyFix = NO;
	for(auto const& info : it->second)
	{
		NSString* message = [NSString stringWithUTF8String:info.message.c_str()] ?: @"";
		NSSize const size = [[[NSAttributedString alloc] initWithString:[@"● " stringByAppendingString:message] attributes:@{ NSFontAttributeName: messageFont }] size];
		maxMessageWidth = std::max(maxMessageWidth, std::ceil(size.width));
		for(NSDictionary* fix in OTVDiagnosticFixes(info.fixJSON, includeOptionFixes))
		{
			NSString* description = [fix[@"message"] isKindOfClass:NSString.class] ? fix[@"message"] : ([fix[@"title"] isKindOfClass:NSString.class] ? fix[@"title"] : @"Apply fix");
			NSSize const fixSize = [description sizeWithAttributes:@{ NSFontAttributeName: [NSFont systemFontOfSize:NSFont.smallSystemFontSize] }];
			maxFixWidth = std::max(maxFixWidth, std::ceil(fixSize.width));
			anyFix = YES;
		}
	}
	CGFloat const buttonWidth = anyFix ? 64 : 0;
	CGFloat const fixIndent = anyFix ? 18 : 0;
	CGFloat const contentWidth = std::max(maxMessageWidth, maxFixWidth + fixIndent + buttonWidth + 12);
	CGFloat const stackWidth  = std::clamp<CGFloat>(contentWidth + 24, 280, 760);
	CGFloat const labelWidth  = stackWidth - 24;
	CGFloat const fixLabelWidth = labelWidth - fixIndent - buttonWidth - 12;

	for(auto const& info : it->second)
	{
		NSString* message = [NSString stringWithUTF8String:info.message.c_str()] ?: @"";
		NSMutableAttributedString* str = [[NSMutableAttributedString alloc] initWithString:@"● " attributes:@{ NSForegroundColorAttributeName: OTVColorForDiagnosticMark(info.type, 1), NSFontAttributeName: messageFont }];
		[str appendAttributedString:[[NSAttributedString alloc] initWithString:message attributes:@{ NSForegroundColorAttributeName: NSColor.labelColor, NSFontAttributeName: messageFont }]];

		NSTextField* label = [NSTextField labelWithAttributedString:str];
		label.selectable = YES;
		label.lineBreakMode = NSLineBreakByWordWrapping;
		label.preferredMaxLayoutWidth = labelWidth;
		[label setContentHuggingPriority:NSLayoutPriorityDefaultLow forOrientation:NSLayoutConstraintOrientationHorizontal];
		[label setContentCompressionResistancePriority:NSLayoutPriorityDefaultLow forOrientation:NSLayoutConstraintOrientationHorizontal];
		[label.widthAnchor constraintLessThanOrEqualToConstant:labelWidth].active = YES;

		NSStackView* diagnostic = [NSStackView new];
		diagnostic.orientation = NSUserInterfaceLayoutOrientationVertical;
		diagnostic.alignment = NSLayoutAttributeWidth;
		diagnostic.spacing = 4;
		[diagnostic addArrangedSubview:label];

		NSArray<NSDictionary*>* fixes = OTVDiagnosticFixes(info.fixJSON, includeOptionFixes);
		for(NSDictionary* fix in fixes)
		{
			BOOL const hasEdits = [fix[@"edits"] isKindOfClass:NSArray.class];
			BOOL const disablesPyrightRule = [fix[@"action"] isEqualToString:@"disablePyrightRule"];
			if(!hasEdits && !disablesPyrightRule)
				continue;

			NSString* description = [fix[@"message"] isKindOfClass:NSString.class] ? fix[@"message"] : ([fix[@"title"] isKindOfClass:NSString.class] ? fix[@"title"] : @"Apply fix");
			NSTextField* fixLabel = [NSTextField labelWithString:description];
			fixLabel.textColor = NSColor.secondaryLabelColor;
			fixLabel.font = [NSFont systemFontOfSize:NSFont.smallSystemFontSize];
			fixLabel.lineBreakMode = NSLineBreakByTruncatingTail;
			fixLabel.toolTip = description;
			[fixLabel.widthAnchor constraintLessThanOrEqualToConstant:fixLabelWidth].active = YES;

			NSButton* button = [NSButton buttonWithTitle:@"Apply" target:self action:@selector(applyDiagnosticFix:)];
			button.controlSize = NSControlSizeSmall;
			button.toolTip = description;
			button.tag = diagnosticFixQueue.count;
			[button.widthAnchor constraintEqualToConstant:buttonWidth].active = YES;
			[button setContentHuggingPriority:NSLayoutPriorityRequired forOrientation:NSLayoutConstraintOrientationHorizontal];

			NSMutableDictionary* queuedFix = [fix mutableCopy];
			queuedFix[@"type"] = [NSString stringWithUTF8String:info.type.c_str()] ?: @"";
			queuedFix[@"index"] = @(info.index);
			[diagnosticFixQueue addObject:queuedFix];

			NSStackView* fixRow = [NSStackView new];
			fixRow.orientation = NSUserInterfaceLayoutOrientationHorizontal;
			fixRow.alignment = NSLayoutAttributeCenterY;
			fixRow.spacing = 12;
			fixRow.edgeInsets = NSEdgeInsetsMake(0, fixIndent, 0, 0);
			[fixRow addView:fixLabel inGravity:NSStackViewGravityLeading];
			[fixRow addView:button inGravity:NSStackViewGravityTrailing];
			[diagnostic addArrangedSubview:fixRow];
		}

		[stack addArrangedSubview:diagnostic];
	}

	[stack.widthAnchor constraintEqualToConstant:stackWidth].active = YES;

	NSViewController* vc = [NSViewController new];
	vc.view = stack;

	diagnosticsPopover = [NSPopover new];
	diagnosticsPopover.behavior = NSPopoverBehaviorTransient;
	diagnosticsPopover.contentViewController = vc;
	[diagnosticsPopover showRelativeToRect:aRect ofView:self preferredEdge:NSRectEdgeMaxY];
}

- (void)applyDiagnosticFix:(NSButton*)sender
{
	if(!documentView || sender.tag < 0 || (NSUInteger)sender.tag >= diagnosticFixQueue.count)
		return;

	NSDictionary* info = diagnosticFixQueue[sender.tag];
	if([info[@"action"] isEqualToString:@"disablePyrightRule"])
	{
		NSString* path = [info[@"path"] isKindOfClass:NSString.class] ? info[@"path"] : nil;
		NSString* rule = [info[@"rule"] isKindOfClass:NSString.class] ? info[@"rule"] : nil;
		if(!path.isAbsolutePath || !rule)
			return NSBeep();

		[diagnosticsPopover close];
		OakDocument* configDocument = [OakDocumentController.sharedInstance documentWithPath:path.stringByStandardizingPath];
		[configDocument loadModalForWindow:self.window completionHandler:^(OakDocumentIOResult result, NSString* errorMessage, oak::uuid_t const& filterUUID){
			if(result != OakDocumentIOResultSuccess)
			{
				NSError* error = [NSError errorWithDomain:@"OakDiagnosticFix" code:1 userInfo:@{ NSLocalizedDescriptionKey: errorMessage ?: @"Could not open the Pyright configuration." }];
				[self presentError:error];
				return;
			}

			NSString* updated = OTVPyrightConfigByDisablingRule(configDocument.content, rule);
			if(!updated)
			{
				NSError* error = [NSError errorWithDomain:@"OakDiagnosticFix" code:2 userInfo:@{ NSLocalizedDescriptionKey: [NSString stringWithFormat:@"Could not disable %@ in %@.", rule, path.lastPathComponent] }];
				[self presentError:error];
				[configDocument close];
				return;
			}

			configDocument.content = updated;
			size_t markIndex = [info[@"index"] unsignedLongValue];
			[self.document removeMarkOfType:info[@"type"] atPosition:documentView->convert(markIndex)];
			[OakDocumentController.sharedInstance showDocument:configDocument andSelect:text::range_t::undefined inProject:nil bringToFront:YES];
			[configDocument close];
			[self setNeedsDisplay:YES];
		}];
		return;
	}

	NSArray* edits = [info[@"edits"] sortedArrayUsingComparator:^NSComparisonResult(NSDictionary* lhs, NSDictionary* rhs){
		NSComparisonResult res = [rhs[@"location"][@"row"] compare:lhs[@"location"][@"row"]];
		return res != NSOrderedSame ? res : [rhs[@"location"][@"column"] compare:lhs[@"location"][@"column"]];
	}];

	// Remove the mark before edits shift positions around.
	size_t markIndex = [info[@"index"] unsignedLongValue];
	[self.document removeMarkOfType:info[@"type"] atPosition:documentView->convert(markIndex)];

	AUTO_REFRESH;
	documentView->begin_change_grouping();
	for(NSDictionary* edit in edits) // bottom-up so earlier positions stay valid
	{
		size_t from = documentView->convert(text::pos_t([edit[@"location"][@"row"] unsignedLongValue] - 1, [edit[@"location"][@"column"] unsignedLongValue] - 1));
		size_t to   = documentView->convert(text::pos_t([edit[@"end_location"][@"row"] unsignedLongValue] - 1, [edit[@"end_location"][@"column"] unsignedLongValue] - 1));
		documentView->set_ranges(ng::range_t(ng::index_t(std::min(from, to)), ng::index_t(std::max(from, to))));
		documentView->insert(to_s((NSString*)edit[@"content"] ?: @""));
	}
	documentView->end_change_grouping();

	[diagnosticsPopover close];
	[self setNeedsDisplay:YES];
}

- (void)drawRect:(NSRect)aRect
{
	if(!documentView || !self.theme)
	{
		NSEraseRect(aRect);
		//NSEraseRect(NSIntersectionRect(aRect, self.bounds));
		return;
	}

	if(self.theme->is_transparent())
	{
		[NSColor.clearColor set];
		NSRectFill(aRect);
		//NSRectFill(NSIntersectionRect(aRect, self.bounds));
	}

	CGContextRef context = NSGraphicsContext.currentContext.CGContext;
	if(!self.antiAlias)
		CGContextSetShouldAntialias(context, false);

	BOOL disableFontSmoothing = NO;
	switch(self.fontSmoothing) {
		case OTVFontSmoothingDisabled:
			disableFontSmoothing = YES;
			break;
		case OTVFontSmoothingDisabledForDark:
			disableFontSmoothing = self.theme->is_dark();
			break;
		case OTVFontSmoothingDisabledForDarkHiDPI:
			disableFontSmoothing = self.theme->is_dark() && [[self window] backingScaleFactor] == 2;
			break;
		case OTVFontSmoothingEnabled:
			break;
	}

	if(disableFontSmoothing)
		CGContextSetShouldSmoothFonts(context, false);

	NSImage* pdfImage = foldingDotsImage;
	auto foldingDotsFactory = [&pdfImage](double width, double height) -> CGImageRef
	{
		NSRect rect = NSMakeRect(0, 0, width, height);
		if(CGImageRef img = [pdfImage CGImageForProposedRect:&rect context:[NSGraphicsContext currentContext] hints:nil])
			return CGImageRetain(img);

		NSLog(@"Unable to create CGImage (%.1f × %.1f) from %@", width, height, pdfImage);
		return NULL;
	};

	[self updateDiagnosticMarks];
	void (^decorationDrawingBlock)(NSRect, OTVDecorationLayer) = self.decorationDrawingBlock;
	documentView->draw(ng::context_t(context, _showInvisibles ? documentView->invisibles_map : NULL_STR, [spellingDotImage CGImageForProposedRect:NULL context:[NSGraphicsContext currentContext] hints:nil], foldingDotsFactory), aRect, [self isFlipped], merge(documentView->ranges(), [self markedRanges]), _liveSearchRanges, true, [self, decorationDrawingBlock, aRect] {
		[self drawDiagnosticBackgroundInRect:aRect];
		if(decorationDrawingBlock)
			decorationDrawingBlock(aRect, OTVDecorationLayerBackground);
	});

	if(decorationDrawingBlock)
		decorationDrawingBlock(aRect, OTVDecorationLayerForeground);
	[self drawDiagnosticForegroundInRect:aRect];
}

// =====================
// = NSTextInputClient =
// =====================

- (NSRange)nsRangeForRange:(ng::range_t const&)range
{
	//TODO this and the next method could use some optimization using an interval tree
	//     similar to basic_tree_t for conversion between UTF-8 and UTF-16 indexes.
	//     Currently poor performance for large documents (O(N)) would then get to O(log(N))
	//     Also currently copy of whole text is created here, which is not optimal

	size_t to = std::min(range.max().index, documentView->size());
	if(to == 0)
		return NSMakeRange(0, 0);

	std::string const text = documentView->substr(0, to);
	size_t from = std::min(range.min().index, text.size());

	crash_reporter_info_t info("%s %s, actual %zu-%zu", sel_getName(_cmd), to_s(range).c_str(), from, to);

	NSUInteger location = utf16::distance(text.data(), text.data() + from);
	NSUInteger length   = utf16::distance(text.data() + from, text.data() + text.size());
	return NSMakeRange(location, length);
}

- (ng::range_t)rangeForNSRange:(NSRange)nsRange
{
	std::string const text = documentView->substr();
	char const* base = text.data();
	ng::index_t from = utf16::advance(base, nsRange.location, base + text.size()) - base;
	ng::index_t to   = utf16::advance(base + from.index, nsRange.length, base + text.size()) - base;
	return ng::range_t(from, to);
}

- (ng::ranges_t)rangesForReplacementRange:(NSRange)aRange
{
	ng::range_t r = [self rangeForNSRange:aRange];
	if(documentView->ranges().size() == 1)
		return r;

	size_t adjustLeft = 0, adjustRight = 0;
	for(auto const& range : documentView->ranges())
	{
		if(range.min() <= r.max() && r.min() <= range.max())
		{
			adjustLeft  = r.min() < range.min() ? range.min().index - r.min().index : 0;
			adjustRight = range.max() < r.max() ? r.max().index - range.max().index : 0;
		}
	}

	ng::ranges_t res;
	for(auto const& range : documentView->ranges())
	{
		size_t from = adjustLeft > range.min().index ? 0 : range.min().index - adjustLeft;
		size_t to   = range.max().index + adjustRight;
		res.push_back(ng::range_t(documentView->sanitize_index(from), documentView->sanitize_index(to)));
	}
	return res;
}

- (void)setMarkedText:(id)aString selectedRange:(NSRange)aRange replacementRange:(NSRange)replacementRange
{
	if(!documentView)
		return;

	AUTO_REFRESH;
	if(replacementRange.location != NSNotFound)
		documentView->set_ranges([self rangesForReplacementRange:replacementRange]);
	else if(!_markedRanges.empty())
		documentView->set_ranges(_markedRanges);

	_markedRanges = ng::ranges_t();
	documentView->insert(to_s(aString), true);
	if([aString length] != 0)
		_markedRanges = documentView->ranges();
	pendingMarkedRanges = _markedRanges;

	ng::ranges_t sel;
	for(auto const& range : documentView->ranges())
	{
		std::string const str = documentView->substr(range.min().index, range.max().index);
		char const* base = str.data();
		size_t from = utf16::advance(base, aRange.location, base + str.size()) - base;
		size_t to   = utf16::advance(base, NSMaxRange(aRange), base + str.size()) - base;
		sel.push_back(ng::range_t(range.min() + from, range.min() + to));
	}
	documentView->set_ranges(sel);
}

- (NSRange)selectedRange
{
	if(!documentView)
		return { NSNotFound, 0 };

	NSRange res = [self nsRangeForRange:documentView->ranges().last()];
	return res;
}

- (NSRange)markedRange
{
	if(!documentView || _markedRanges.empty())
		return NSMakeRange(NSNotFound, 0);
	return [self nsRangeForRange:_markedRanges.last()];
}

- (void)unmarkText
{
	AUTO_REFRESH;
	_markedRanges = pendingMarkedRanges = ng::ranges_t();
}

- (BOOL)hasMarkedText
{
	return !_markedRanges.empty();
}

- (NSArray*)validAttributesForMarkedText
{
	return [NSArray array];
}

- (void)updateMarkedRanges
{
	if(!_markedRanges.empty() && pendingMarkedRanges.empty())
		[self.inputContext discardMarkedText];

	_markedRanges = pendingMarkedRanges;
	pendingMarkedRanges = ng::ranges_t();
}

- (NSUInteger)characterIndexForPoint:(NSPoint)thePoint
{
	if(!documentView)
		return NSNotFound;

	NSPoint p = [self convertPoint:[[self window] convertRectFromScreen:(NSRect){ thePoint, NSZeroSize }].origin fromView:nil];
	std::string const text = documentView->substr();
	size_t index = documentView->index_at_point(p).index;
	return utf16::distance(text.data(), text.data() + index);
}

- (NSAttributedString*)attributedSubstringForProposedRange:(NSRange)theRange actualRange:(NSRangePointer)actualRange
{
	if(!documentView || !self.theme)
		return nil;

	ng::range_t const& r = [self rangeForNSRange:theRange];
	size_t from = r.min().index, to = r.max().index;

	if(CFMutableAttributedStringRef res = CFAttributedStringCreateMutable(kCFAllocatorDefault, 0))
	{
		std::map<size_t, scope::scope_t> scopes = documentView->scopes(from, to);
		for(auto pair = scopes.begin(); pair != scopes.end(); )
		{
			styles_t const& styles = self.theme->styles_for_scope(pair->second);

			size_t i = from + pair->first;
			size_t j = ++pair != scopes.end() ? from + pair->first : to;

			if(CFMutableAttributedStringRef str = CFAttributedStringCreateMutable(kCFAllocatorDefault, 0))
			{
				CFAttributedStringReplaceString(str, CFRangeMake(0, 0), cf::wrap(documentView->substr(i, j)));
				CFAttributedStringSetAttribute(str, CFRangeMake(0, CFAttributedStringGetLength(str)), kCTFontAttributeName, styles.font());
				CFAttributedStringSetAttribute(str, CFRangeMake(0, CFAttributedStringGetLength(str)), kCTForegroundColorAttributeName, styles.foreground());
				if(styles.underlined())
					CFAttributedStringSetAttribute(str, CFRangeMake(0, CFAttributedStringGetLength(str)), kCTUnderlineStyleAttributeName, cf::wrap(kCTUnderlineStyleSingle));
				if(styles.strikethrough())
					CFAttributedStringSetAttribute(str, CFRangeMake(0, CFAttributedStringGetLength(str)), (CFStringRef)NSStrikethroughStyleAttributeName, cf::wrap(kCTUnderlineStyleSingle));
				CFAttributedStringReplaceAttributedString(res, CFRangeMake(CFAttributedStringGetLength(res), 0), str);

				CFRelease(str);
			}
		}

		if(actualRange)
			*actualRange = [self nsRangeForRange:ng::range_t(from, to)];

		return (NSAttributedString*)CFBridgingRelease(res);
	}
	return nil;
}

- (NSRect)firstRectForCharacterRange:(NSRange)theRange actualRange:(NSRangePointer)actualRange
{
	if(!documentView)
		return NSZeroRect;

	ng::range_t const& r = [self rangeForNSRange:theRange];
	if(actualRange)
		*actualRange = [self nsRangeForRange:r];

	NSRect rect = [[self window] convertRectToScreen:[self convertRect:documentView->rect_at_index(r.min()) toView:nil]];
	return rect;
}

- (void)doCommandBySelector:(SEL)aSelector
{
	AUTO_REFRESH;
	if(![self tryToPerform:aSelector with:self])
		NSBeep();
}

- (NSInteger)windowLevel
{
	return self.window.level;
}

- (BOOL)respondsToSelector:(SEL)aSelector
{
	// Do not handle cancelOperation: (as complete:) when caret is not on a word (instead give next responder a chance)
	if(aSelector == @selector(cancelOperation:) && ng::word_at(*documentView, documentView->ranges().last()).empty())
		return NO;
	return [super respondsToSelector:aSelector];
}

- (void)cancelOperation:(id)sender
{
	[self complete:sender];
}

// =============================
// = NSAccessibilityStaticText =
// =============================

- (NSAttributedString*)accessibilityAttributedStringForRange:(NSRange)aRange
{
	if(!documentView || !self.theme)
		return nil;
	ng::range_t const range = [self rangeForNSRange:aRange];
	size_t const from = range.min().index, to = range.max().index;
	std::string const text = documentView->substr(from, to);
	NSMutableAttributedString* res = [[NSMutableAttributedString alloc] initWithString:[NSString stringWithCxxString:text]];

	// Add style
	std::map<size_t, scope::scope_t> scopes = documentView->scopes(from, to);
	NSRange runRange = NSMakeRange(0, 0);
	for(auto pair = scopes.begin(); pair != scopes.end(); )
	{
		styles_t const& styles = self.theme->styles_for_scope(pair->second);

		size_t i = pair->first;
		size_t j = ++pair != scopes.end() ? pair->first : to - from;

		runRange.location += runRange.length;
		runRange.length = utf16::distance(text.data() + i, text.data() + j);
		NSFont* font = (__bridge NSFont*)styles.font();
		NSMutableDictionary* attributes = [NSMutableDictionary dictionaryWithCapacity:4];
		[attributes addEntriesFromDictionary:@{
			NSAccessibilityFontTextAttribute: @{
				NSAccessibilityFontNameKey:    font.fontName,
				NSAccessibilityFontFamilyKey:  font.familyName,
				NSAccessibilityVisibleNameKey: font.displayName,
				NSAccessibilityFontSizeKey:    @(font.pointSize),
			},
			NSAccessibilityForegroundColorTextAttribute: (__bridge id)styles.foreground(),
			NSAccessibilityBackgroundColorTextAttribute: (__bridge id)styles.background(),
		}];
		if(styles.underlined())
			attributes[NSAccessibilityUnderlineTextAttribute] = @(NSUnderlineStyleSingle | NSUnderlinePatternSolid); // TODO is this always so?
		if(styles.strikethrough())
			attributes[NSAccessibilityStrikethroughTextAttribute] = @YES;

		[res setAttributes:attributes range:runRange];
	}

	// Add links
	links_ptr const links = self.links;
	auto lbegin = links->upper_bound(from);
	auto lend   = links->lower_bound(to);
	if(lend != links->end() && to >= lend->second.range.min().index)
		++lend;

	std::for_each(lbegin, lend, [=](links_t::iterator::value_type const& pair){
		ng::range_t range = pair.second.range;
		range.first = std::clamp(range.min(), ng::index_t(from), ng::index_t(to));
		range.last  = std::clamp(range.max(), ng::index_t(from), ng::index_t(to));
		if(!range.empty())
		{
			range.first.index -= from;
			range.last.index  -= from;
			NSRange linkRange;
			linkRange.location = utf16::distance(text.data(), text.data() + range.first.index);
			linkRange.length   = utf16::distance(text.data() + range.first.index, text.data() + range.last.index);
			[res addAttribute:NSAccessibilityLinkTextAttribute value:pair.second range:linkRange];
		}
	});

	// Add misspellings
	std::map<size_t, bool> misspellings = documentView->misspellings(from, to);
	auto pair = misspellings.begin();
	auto const end = misspellings.end();
	ASSERT((pair == end) || pair->second);
	runRange = NSMakeRange(0, 0);
	if(pair != end)
		runRange.length = utf16::distance(text.data(), text.data() + pair->first);
	while(pair != end)
	{
		ASSERT(pair->second);

		size_t const i = pair->first;
		size_t const j = (++pair != end) ? pair->first : to - from;
		ASSERT((pair == end) || (!pair->second));
		runRange.location += runRange.length;
		runRange.length = utf16::distance(text.data() + i, text.data() + j);

		[res addAttribute:NSAccessibilityMisspelledTextAttribute value:@(true) range:runRange];
		[res addAttribute:@"AXMarkedMisspelled" value:@(true) range:runRange];

		if((pair != end) && (++pair != end))
		{
			ASSERT(pair->second);
			size_t const k = pair->first;
			runRange.location += runRange.length;
			runRange.length = utf16::distance(text.data() + j, text.data() + k);
		}
	}

	// Add text language
	NSString* lang = [NSString stringWithCxxString:documentView->spelling_language()];
	[res addAttribute:@"AXNaturalLanguageText" value:lang range:NSMakeRange(0, [res length])];

	return res;
}

- (NSString*)accessibilityValue
{
	if(!documentView)
		return nil;
	return [NSString stringWithCxxString:documentView->substr()];
}

- (NSRange)accessibilityVisibleCharacterRange
{
	if(!documentView)
		return NSMakeRange(0, 0);
	NSRect visibleRect = [self visibleRect];
	CGPoint startPoint = NSMakePoint(NSMinX(visibleRect), NSMinY(visibleRect));
	CGPoint endPoint   = NSMakePoint(NSMaxX(visibleRect), NSMaxY(visibleRect));
	ng::range_t visibleRange(documentView->index_at_point(startPoint), documentView->index_at_point(endPoint));
	visibleRange = ng::extend(*documentView, visibleRange, kSelectionExtendToEndOfSoftLine).last();
	return [self nsRangeForRange:visibleRange];
}

// ======================================
// = NSAccessibilityNavigableStaticText =
// ======================================

- (NSString*)accessibilityStringForRange:(NSRange)nsRange
{
	if(!documentView || !self.theme)
		return nil;
	ng::range_t range = [self rangeForNSRange:nsRange];
	return [NSString stringWithCxxString:documentView->substr(range.min().index, range.max().index)];
}

- (NSInteger)accessibilityLineForIndex:(NSInteger)index
{
	if(!documentView || !self.theme)
		return 0;
	index = [self rangeForNSRange:NSMakeRange(index, 0)].min().index;
	size_t line = documentView->softline_for_index(index);
	return line;
}

- (NSRange)accessibilityRangeForLine:(NSInteger)lineNumber
{
	if(!documentView || !self.theme)
		return NSMakeRange(0, 0);
	ng::range_t const range = documentView->range_for_softline(lineNumber);
	return [self nsRangeForRange:range];
}

- (NSRect)accessibilityFrameForRange:(NSRange)nsRange
{
	if(!documentView || !self.theme)
		return NSZeroRect;
	ng::range_t range = [self rangeForNSRange:nsRange];
	NSRect rect = documentView->rect_for_range(range.min().index, range.max().index, true);
	return NSAccessibilityFrameInView(self, rect);
}

// ===================
// = NSAccessibility =
// ===================

- (NSAccessibilityRole)accessibilityRole
{
	return NSAccessibilityTextAreaRole;
}

- (NSInteger)accessibilityInsertionPointLineNumber
{
	if(!documentView)
		return 0;
	return documentView->softline_for_index(documentView->ranges().last().min());
}

- (NSInteger)accessibilityNumberOfCharacters
{
	if(!documentView)
		return 0;
	return [self nsRangeForRange:ng::range_t(0, documentView->size())].length;
}

- (NSString*)accessibilitySelectedText
{
	if(!documentView)
		return nil;
	ng::range_t const selection = documentView->ranges().last();
	std::string const text = documentView->substr(selection.min().index, selection.max().index);
	return [NSString stringWithCxxString:text];
}

- (NSRange)accessibilitySelectedTextRange
{
	if(!documentView)
		return NSMakeRange(0, 0);
	return [self nsRangeForRange:documentView->ranges().last()];
}

- (NSArray*)accessibilitySelectedTextRanges
{
	if(!documentView)
		return nil;
	ng::ranges_t const ranges = documentView->ranges();
	NSMutableArray* nsRanges = [NSMutableArray arrayWithCapacity:ranges.size()];
	for(auto const& range : ranges)
		[nsRanges addObject:[NSValue valueWithRange:[self nsRangeForRange:range]]];
	return nsRanges;
}

- (NSArray*)accessibilityChildren
{
	if(!documentView)
		return nil;
	NSMutableArray* links = [NSMutableArray array];
	std::shared_ptr<links_t> links_ = self.links;
	for(auto const& pair : *links_)
		[links addObject:pair.second];
	return links;
}

- (void)setAccessibilityValue:(NSString*)value
{
	if(!documentView)
		return;
	AUTO_REFRESH;
	_document.content = value;
}

- (void)setAccessibilitySelectedText:(NSString*)selectedText
{
	if(!documentView)
		return;
	AUTO_REFRESH;
	documentView->insert(to_s(selectedText));
}

- (void)setAccessibilitySelectedTextRange:(NSRange)range
{
	if(!documentView)
		return;
	[self setAccessibilitySelectedTextRanges:@[ [NSValue valueWithRange:range] ]];
}

- (void)setAccessibilitySelectedTextRanges:(NSArray*)nsRanges
{
	if(!documentView)
		return;
	ng::ranges_t ranges;
	for(NSValue* nsRangeValue in nsRanges)
		ranges.push_back([self rangeForNSRange:[nsRangeValue rangeValue]]);
	AUTO_REFRESH;
	documentView->set_ranges(ranges);
}

- (NSRange)accessibilityRangeForPosition:(NSPoint)point
{
	if(!documentView || !self.theme)
		return NSMakeRange(0, 0);
	point = [[self window] convertRectFromScreen:(NSRect){ point, NSZeroSize }].origin;
	point = [self convertPoint:point fromView:nil];
	size_t index = documentView->index_at_point(point).index;
	index = documentView->sanitize_index(index);
	size_t const length = (*documentView)[index].length();
	return [self nsRangeForRange:ng::range_t(index, index + length)];
}

- (NSRange)accessibilityRangeForIndex:(NSInteger)index
{
	if(!documentView || !self.theme)
		return NSMakeRange(0, 0);
	index = [self rangeForNSRange:NSMakeRange(index, 0)].min().index;
	index = documentView->sanitize_index(index);
	size_t const length = (*documentView)[index].length();
	return [self nsRangeForRange:ng::range_t(index, index + length)];
}

- (NSArray*)accessibilityCustomRotors API_AVAILABLE(macos(10.13))
{
	return @[
		[[NSAccessibilityCustomRotor alloc] initWithLabel:@"Symbols" itemSearchDelegate:self],
	];
}

- (NSUInteger)accessibilityArrayAttributeCount:(NSString*)attribute
{
	if([attribute isEqualToString:NSAccessibilityChildrenAttribute])
		return self.links->size();

	return [super accessibilityArrayAttributeCount:attribute];
}

- (NSArray*)accessibilityArrayAttributeValues:(NSString*)attribute index:(NSUInteger)index maxCount:(NSUInteger)maxCount
{
	if([attribute isEqualToString:NSAccessibilityChildrenAttribute])
	{
		links_ptr const links = self.links;
		NSMutableArray* values = [NSMutableArray arrayWithCapacity:maxCount];
		for(auto it = links->nth(index); maxCount && it != links->end(); ++it, --maxCount)
			[values addObject:it->second];
		return values;
	}

	return [super accessibilityArrayAttributeValues:attribute index:index maxCount:maxCount];
}

- (NSUInteger)accessibilityIndexOfChild:(id)child
{
	if([child isKindOfClass:[OakAccessibleLink class]])
	{
		OakAccessibleLink* link = (OakAccessibleLink* )child;
		links_ptr const links = self.links;
		auto it = links->find(link.range.max().index);
		return it != links->end() ? it.index() : NSNotFound;
	}

	return [super accessibilityIndexOfChild:child];
}

- (id)accessibilityHitTest:(NSPoint)screenPoint
{
	if(!documentView)
		return self;

	NSPoint point = [self convertRect:[self.window convertRectFromScreen:NSMakeRect(screenPoint.x, screenPoint.y, 0, 0)] fromView:nil].origin;
	ng::index_t index = documentView->index_at_point(point);
	const links_ptr links = self.links;
	auto it = links->lower_bound(index.index);
	if(it != links->end() && it->second.range.min() <= index)
	{
		OakAccessibleLink* link = it->second;
		if(NSMouseInRect(point, link.frame, YES))
			return [link accessibilityHitTest:screenPoint];
	}
	return self;
}

- (links_ptr)links
{
	if(!_links)
	{
		links_ptr links(new links_t());
		scope::selector_t linkSelector = "markup.underline.link";
		std::map<size_t, scope::scope_t> scopes = documentView->scopes(0, documentView->size());
		for(auto pair = scopes.begin(); pair != scopes.end(); )
		{
			if(!linkSelector.does_match(pair->second))
			{
				++pair;
				continue;
			}
			size_t i = pair->first;
			size_t j = ++pair != scopes.end() ? pair->first : documentView->size();
			NSString* title = [NSString stringWithCxxString:documentView->substr(i, j)];
			NSRect frame = NSRectFromCGRect(documentView->rect_for_range(i, j));
			ng::range_t range(i, j);
			OakAccessibleLink* link = [[OakAccessibleLink alloc] initWithTextView:self range:range title:title URL:nil frame:frame];
			links->set(j, link);
		}
		_links = links;
	}
	return _links;
}

// ================================================
// = NSAccessibilityCustomRotorItemSearchDelegate =
// ================================================

- (NSAccessibilityCustomRotorItemResult*)rotor:(NSAccessibilityCustomRotor*)rotor resultForSearchParameters:(NSAccessibilityCustomRotorSearchParameters*)searchParameters API_AVAILABLE(macos(10.13))
{
	auto const symbols = documentView->symbols();

	std::string const filterString = searchParameters.filterString ? text::lowercase(to_s(searchParameters.filterString)) : "";

	auto const substringMatcher = [&filterString](const std::pair<size_t, std::string>& symbolPair){
		std::string const symbol = text::lowercase(symbolPair.second);
		return symbol.find(filterString) != std::string::npos;
	};

	NSAccessibilityCustomRotorItemResult* currentItem = searchParameters.currentItem;
	NSUInteger location = currentItem.targetRange.location;

	// Contrary to what is implied in NSAccessibilityCustomRotor.h, the first call does not
	// set ‘location’ to NSNotFound nor ‘currentItem’ to nil (macOS 10.14.2).
	if(!currentItem.targetElement && location == 0 && currentItem.targetRange.length == 0)
		location = NSNotFound;

	auto it = symbols.end();
	switch(searchParameters.searchDirection)
	{
		case NSAccessibilityCustomRotorSearchDirectionNext:
		{
			if(location == NSNotFound)
			{
				it = symbols.begin();
			}
			else
			{
				ng::index_t	const currentIndex = [self rangeForNSRange:NSMakeRange(location, 0)].min();
				it = symbols.upper_bound(currentIndex.index);
			}
			it = std::find_if(it, symbols.end(), substringMatcher);
		}
		break;

		case NSAccessibilityCustomRotorSearchDirectionPrevious:
		{
			if(location == NSNotFound)
			{
				it = symbols.end();
			}
			else
			{
				ng::index_t	const currentIndex = [self rangeForNSRange:NSMakeRange(location, 0)].min();
				it = symbols.lower_bound(currentIndex.index);
			}
			auto rit = std::make_reverse_iterator(it);
			rit = std::find_if(rit, symbols.rend(), substringMatcher);
			if(rit == symbols.rend())
					it = symbols.end();
			else	it = (++rit).base();
		}
		break;
	}

	if(it == symbols.end())
		return nil;

	NSAccessibilityCustomRotorItemResult* result = [[NSAccessibilityCustomRotorItemResult alloc] initWithTargetElement:self];

	ng::index_t const resultIndex = it->first;
	text::pos_t const pos = documentView->convert(resultIndex.index);
	size_t const end = documentView->end(pos.line);
	result.targetRange = [self nsRangeForRange:ng::range_t(resultIndex, end)];
	result.customLabel = to_ns(it->second);

	return result;
}

- (void)updateZoom:(id)sender
{
	if(!documentView)
		return;

	size_t const index = documentView->ranges().last().min().index;
	NSRect selectedRect = NSAccessibilityFrameInView(self, documentView->rect_at_index(index, false));
	NSRect viewRect = NSAccessibilityFrameInView(self, [self visibleRect]);
	viewRect.origin.y = [[NSScreen mainScreen] frame].size.height - (viewRect.origin.y + viewRect.size.height);
	selectedRect.origin.y = [[NSScreen mainScreen] frame].size.height - (selectedRect.origin.y + selectedRect.size.height);
	UAZoomChangeFocus(&viewRect, &selectedRect, kUAZoomFocusTypeInsertionPoint);
}

// ================
// = Bundle Items =
// ================

- (std::map<std::string, std::string>)variablesForBundleItem:(bundles::item_ptr const&)item
{
	std::map<std::string, std::string> res = oak::basic_environment();
	if(!documentView || !self.theme)
		return res;

	res << documentView->variables(to_s([self scopeAttributes]));
	if(item)
		res << item->bundle_variables();

	if(auto themeItem = bundles::lookup(self.theme->uuid()))
	{
		if(!themeItem->paths().empty())
			res["TM_CURRENT_THEME_PATH"] = themeItem->paths().back();
	}

	if([self.delegate respondsToSelector:@selector(variables)])
		res << [self.delegate variables];

	res = bundles::scope_variables(res, [self scopeContext]);
	res = variables_for_path(res, documentView->logical_path(), [self scopeContext].right, path::parent(documentView->path()));
	return res;
}

- (std::map<std::string, std::string>)variables
{
	return [self variablesForBundleItem:bundles::item_ptr()];
}

- (void)performBundleItem:(bundles::item_ptr)item
{
	crash_reporter_info_t info("%s %s", sel_getName(_cmd), item->name_with_bundle().c_str());
	switch(item->kind())
	{
		case bundles::kItemTypeSnippet:
		{
			[self recordSelector:@selector(insertSnippetWithOptions:) withArgument:ns::to_dictionary(item->plist())];
			AUTO_REFRESH;
			documentView->snippet_dispatch(item->plist(), [self variablesForBundleItem:item]);
		}
		break;

		case bundles::kItemTypeCommand:
		{
			// Macro command names are dispatched by the editor, not Objective-C methods.
			[self recordSelector:NSSelectorFromString(@"executeCommandWithOptions:") withArgument:ns::to_dictionary(item->plist())];

			auto command = parse_command(item);
			command.name = name_with_selection(item, self.hasSelection);
			[self executeBundleCommand:command variables:item->bundle_variables()];
		}
		break;

		case bundles::kItemTypeMacro:
		{
			[self recordSelector:NSSelectorFromString(@"playMacroWithOptions:") withArgument:ns::to_dictionary(item->plist())];
			AUTO_REFRESH;
			documentView->macro_dispatch(item->plist(), [self variablesForBundleItem:item]);
		}
		break;

		case bundles::kItemTypeGrammar:
		{
			documentView->set_file_type(item->value_for_field(bundles::kFieldGrammarScope));
			file::set_type(documentView->logical_path(), item->value_for_field(bundles::kFieldGrammarScope));
		}
		break;
	}
}

- (void)applicationDidBecomeActiveNotification:(NSNotification*)aNotification
{
	for(auto const& item : bundles::query(bundles::kFieldSemanticClass, "callback.application.did-activate", [self scopeContext], bundles::kItemTypeMost, oak::uuid_t(), false))
		[self performBundleItem:item];
}

- (void)applicationDidResignActiveNotification:(NSNotification*)aNotification
{
	for(auto const& item : bundles::query(bundles::kFieldSemanticClass, "callback.application.did-deactivate", [self scopeContext], bundles::kItemTypeMost, oak::uuid_t(), false))
		[self performBundleItem:item];
}

// ============
// = Key Down =
// ============

static plist::dictionary_t KeyBindings;
static std::set<std::string> LocalBindings;

static plist::any_t normalize_potential_dictionary (plist::any_t const& action)
{
	if(plist::dictionary_t const* dict = boost::get<plist::dictionary_t>(&action))
	{
		plist::dictionary_t res;
		for(auto const& pair : *dict)
			res.emplace(ns::normalize_event_string(pair.first), normalize_potential_dictionary(pair.second));
		return res;
	}
	return action;
}

static void update_menu_key_equivalents (NSMenu* menu, std::multimap<std::string, std::string> const& actionToKey)
{
	for(NSMenuItem* item in [menu itemArray])
	{
		SEL action = [item action];
		auto it = actionToKey.find(sel_getName(action));
		if(it != actionToKey.end() && OakIsEmptyString([item keyEquivalent]))
			[item setKeyEquivalentCxxString:it->second];

		update_menu_key_equivalents([item submenu], actionToKey);
	}
}

+ (void)initialize
{
	static dispatch_once_t onceToken = 0;
	dispatch_once(&onceToken, ^{
		static struct { std::string path; bool local = false; } KeyBindingLocations[] =
		{
			{ oak::application_t::support("KeyBindings.dict"), true                            },
			{ oak::application_t::path("Contents/Resources/KeyBindings.dict"), true            },
			{ path::join(path::home(), "Library/KeyBindings/DefaultKeyBinding.dict")            },
			{ "/Library/KeyBindings/DefaultKeyBinding.dict"                                     },
			{ "/System/Library/Frameworks/AppKit.framework/Resources/StandardKeyBinding.dict"   },
		};

		for(auto const& info : KeyBindingLocations)
		{
			for(auto const& pair : plist::load(info.path))
			{
				if(info.local || boost::get<plist::dictionary_t>(&pair.second))
					LocalBindings.insert(ns::normalize_event_string(pair.first));
				KeyBindings.emplace(ns::normalize_event_string(pair.first), normalize_potential_dictionary(pair.second));
			}
		}

		std::multimap<std::string, std::string> actionToKey;
		for(auto const& pair : KeyBindings)
		{
			if(std::string const* selector = boost::get<std::string>(&pair.second))
				actionToKey.emplace(*selector, pair.first);
		}

		update_menu_key_equivalents([NSApp mainMenu], actionToKey);

		[NSUserDefaults.standardUserDefaults registerDefaults:@{
			kUserDefaultsFontSmoothingKey:     @(OTVFontSmoothingDisabledForDarkHiDPI),
			kUserDefaultsWrapColumnPresetsKey: @[ @40, @80 ],
		}];
	});

	[NSApp registerServicesMenuSendTypes:@[ NSPasteboardTypeString ] returnTypes:@[ NSPasteboardTypeString ]];
}

// ======================
// = NSServicesRequests =
// ======================

- (id)validRequestorForSendType:(NSString*)sendType returnType:(NSString*)returnType
{
	if([sendType isEqualToString:NSPasteboardTypeString] && [self hasSelection] && !macroRecordingArray)
		return self;
	if(!sendType && [returnType isEqualToString:NSPasteboardTypeString] && !macroRecordingArray)
		return self;
	return [super validRequestorForSendType:sendType returnType:returnType];
}

- (BOOL)writeSelectionToPasteboard:(NSPasteboard*)pboard types:(NSArray*)types
{
	if(![self hasSelection])
		return NO;

	std::vector<std::string> v;
	ng::ranges_t const ranges = ng::dissect_columnar(*documentView, documentView->ranges());
	for(auto const& range : ranges)
		v.push_back(documentView->substr(range.min().index, range.max().index));

	[pboard clearContents];
	return [pboard writeObjects:@[ to_ns(text::join(v, "\n")) ]];
}

- (BOOL)readSelectionFromPasteboard:(NSPasteboard*)pboard
{
	if(NSString* str = [pboard stringForType:[pboard availableTypeFromArray:@[ NSPasteboardTypeString ]]])
	{
		AUTO_REFRESH;
		documentView->insert(to_s(str));
		return YES;
	}
	return NO;
}

// ======================

- (void)handleKeyBindingAction:(plist::any_t const&)anAction
{
	AUTO_REFRESH;
	if(std::string const* selector = boost::get<std::string>(&anAction))
	{
		[self doCommandBySelector:NSSelectorFromString([NSString stringWithCxxString:*selector])];
	}
	else if(plist::array_t const* actions = boost::get<plist::array_t>(&anAction))
	{
		std::vector<std::string> selectors;
		for(auto const& it : *actions)
		{
			if(std::string const* selector = boost::get<std::string>(&it))
				selectors.push_back(*selector);
		}

		for(size_t i = 0; i < selectors.size(); ++i)
		{
			if(selectors[i] == "insertText:" && i+1 < selectors.size())
					[self insertText:[NSString stringWithCxxString:selectors[++i]]];
			else	[self doCommandBySelector:NSSelectorFromString([NSString stringWithCxxString:selectors[i]])];
		}
	}
	else if(plist::dictionary_t const* nested = boost::get<plist::dictionary_t>(&anAction))
	{
		__block id eventMonitor;
		eventMonitor = [NSEvent addLocalMonitorForEventsMatchingMask:NSEventMaskKeyDown handler:^NSEvent*(NSEvent* event){
			plist::dictionary_t::const_iterator pair = nested->find(to_s(event));
			if(pair != nested->end())
			{
				[self handleKeyBindingAction:pair->second];
				event = nil;
			}
			[NSEvent removeMonitor:eventMonitor];
			eventMonitor = nil;
			return event;
		}];
	}
}

- (BOOL)performKeyEquivalent:(NSEvent*)anEvent
{
	BOOL hasKey = (self.keyState & (OakViewViewIsFirstResponderMask|OakViewWindowIsKeyMask|OakViewApplicationIsActiveMask)) == (OakViewViewIsFirstResponderMask|OakViewWindowIsKeyMask|OakViewApplicationIsActiveMask);
	BOOL otherTextViewHasKey = [self.window.firstResponder isKindOfClass:[self class]];
	BOOL recordingShortcut = [self.window.firstResponder isKindOfClass:NSClassFromString(@"OakKeyEquivalentView")];
	BOOL noCommandFlag = (anEvent.modifierFlags & NSEventModifierFlagCommand) != NSEventModifierFlagCommand;
	if(!hasKey && (otherTextViewHasKey || recordingShortcut || noCommandFlag))
		return NO;

	std::string const eventString = to_s(anEvent);

	std::vector<bundles::item_ptr> const& items = bundles::query(bundles::kFieldKeyEquivalent, eventString, [self scopeContext]);
	if(!items.empty())
	{
		if(bundles::item_ptr item = [self showMenuForBundleItems:items])
			[self performBundleItem:item];
		return YES;
	}

	static std::string const kBackwardDelete = "\x7F";
	static std::string const kForwardDelete  = "\uF728";
	static std::string const kUpArrow        = "\uF700";
	static std::string const kDownArrow      = "\uF701";
	static std::string const kLeftArrow      = "\uF702";
	static std::string const kRightArrow     = "\uF703";

	// these never reach ‘keyDown:’ (tested on 10.5.8)
	static std::set<std::string> const SpecialKeys =
	{
		"^" + kBackwardDelete, "^" + kForwardDelete,
		"^"   + kUpArrow, "^"   + kDownArrow, "^"   + kLeftArrow, "^"   + kRightArrow,
		"^$"  + kUpArrow, "^$"  + kDownArrow, "^$"  + kLeftArrow, "^$"  + kRightArrow,
		"^~"  + kUpArrow, "^~"  + kDownArrow, "^~"  + kLeftArrow, "^~"  + kRightArrow,
		"^~$" + kUpArrow, "^~$" + kDownArrow, "^~$" + kLeftArrow, "^~$" + kRightArrow,
	};

	if(SpecialKeys.find(eventString) != SpecialKeys.end())
	{
		plist::dictionary_t::const_iterator pair = KeyBindings.find(eventString);
		if(pair != KeyBindings.end())
		{
			[self handleKeyBindingAction:pair->second];
			return YES;
		}
	}

	return NO;
}

- (void)oldKeyDown:(NSEvent*)anEvent
{
	std::vector<bundles::item_ptr> const& items = bundles::query(bundles::kFieldKeyEquivalent, to_s(anEvent), [self scopeContext]);
	if(bundles::item_ptr item = [self showMenuForBundleItems:items])
	{
		[self performBundleItem:item];
	}
	else if(items.empty())
	{
		if(LocalBindings.find(to_s(anEvent)) != LocalBindings.end())
				[self handleKeyBindingAction:KeyBindings[to_s(anEvent)]];
		else	[self.inputContext handleEvent:anEvent];
	}

	[NSCursor setHiddenUntilMouseMoves:YES];
	[NSNotificationCenter.defaultCenter postNotificationName:OakCursorDidHideNotification object:nil];
}

- (void)keyDown:(NSEvent*)anEvent
{
	crash_reporter_info_t info("%s %s", sel_getName(_cmd), to_s(anEvent).c_str());
	try {
		[self realKeyDown:anEvent];
	}
	catch(std::exception const& e) {
		info << text::format("C++ Exception: %s", e.what());
		abort();
	}
}

- (void)realKeyDown:(NSEvent*)anEvent
{
	AUTO_REFRESH;
	if(!_choiceMenu)
		return [self oldKeyDown:anEvent];

	ng::range_t oldSelection;
	std::string oldContent = documentView->placeholder_content(&oldSelection);
	std::string oldPrefix  = oldSelection ? oldContent.substr(0, oldSelection.min().index) : "";

	NSUInteger event = [_choiceMenu didHandleKeyEvent:anEvent];
	if(event == OakChoiceMenuKeyUnused)
	{
		[self oldKeyDown:anEvent];

		ng::range_t newSelection;
		std::string const& newContent = documentView->placeholder_content(&newSelection);
		std::string const newPrefix   = newSelection ? newContent.substr(0, newSelection.min().index) : "";

		std::vector<std::string> newChoices = documentView->choices();
		newChoices.erase(std::remove_if(newChoices.begin(), newChoices.end(), [&newPrefix](std::string const& str) { return str.find(newPrefix) != 0; }), newChoices.end());
		_choiceMenu.choices = (__bridge NSArray*)((CFArrayRef)cf::wrap(newChoices));

		bool didEdit   = oldPrefix != newPrefix;
		bool didDelete = didEdit && oldPrefix.find(newPrefix) == 0;

		if(didEdit && !didDelete)
		{
			NSUInteger choiceIndex = NSNotFound;
			if(std::find(newChoices.begin(), newChoices.end(), oldContent) != newChoices.end() && oldContent.find(newContent) == 0)
			{
				choiceIndex = std::find(newChoices.begin(), newChoices.end(), oldContent) - newChoices.begin();
			}
			else
			{
				for(size_t i = 0; i < newChoices.size(); ++i)
				{
					if(newChoices[i].find(newContent) != 0)
						continue;
					choiceIndex = i;
					break;
				}
			}

			_choiceMenu.choiceIndex = choiceIndex;
			if(choiceIndex != NSNotFound && newContent != newChoices[choiceIndex])
				documentView->set_placeholder_content(newChoices[choiceIndex], newPrefix.size());
		}
		else if(oldContent != newContent)
		{
			_choiceMenu.choiceIndex = NSNotFound;
		}
	}
	else if(event == OakChoiceMenuKeyMovement)
	{
		std::string const choice = to_s(_choiceMenu.selectedChoice);
		if(choice != NULL_STR && choice != oldContent)
			documentView->set_placeholder_content(choice, choice.find(oldPrefix) == 0 ? oldPrefix.size() : 0);
	}
	else
	{
		self.choiceMenu = nil;

		if(event != OakChoiceMenuKeyCancel)
		{
			documentView->perform(ng::kInsertTab, [self indentCorrections], to_s([self scopeAttributes]));
			choiceVector.clear();
		}
	}
}

- (BOOL)hasSelection                     { return documentView->has_selection(); }

- (void)flagsChanged:(NSEvent*)anEvent
{
	NSInteger modifiers  = [anEvent modifierFlags] & (NSEventModifierFlagOption | NSEventModifierFlagControl | NSEventModifierFlagCommand | NSEventModifierFlagShift);
	BOOL isHoldingOption = modifiers & NSEventModifierFlagOption ? YES : NO;

	self.showColumnSelectionCursor = isHoldingOption;
	if(([NSEvent pressedMouseButtons] & 1))
	{
		if(documentView->has_selection() && documentView->ranges().last().columnar != isHoldingOption)
			[self toggleColumnSelection:self];
	}
	else if(modifiers != _lastFlags)
	{
		BOOL tapThreshold     = [[NSDate date] timeIntervalSinceDate:_lastFlagsChangeDate] < 0.18;

		BOOL didPressShift    = modifiers == NSEventModifierFlagShift && _lastFlags == 0;
		BOOL didReleaseShift  = modifiers == 0 && _lastFlags == NSEventModifierFlagShift;

		BOOL didPressOption   = (modifiers & ~NSEventModifierFlagShift) == NSEventModifierFlagOption && (_lastFlags & ~NSEventModifierFlagShift) == 0;
		BOOL didReleaseOption = (modifiers & ~NSEventModifierFlagShift) == 0 && (_lastFlags & ~NSEventModifierFlagShift) == NSEventModifierFlagOption;

		OakFlagsState newFlagsState = OakFlagsStateClear;
		if(didPressOption)
			newFlagsState = OakFlagsStateOptionDown;
		else if(didReleaseOption && tapThreshold && _flagsState == OakFlagsStateOptionDown)
			[self toggleColumnSelection:self];
		else if(didPressShift)
			newFlagsState = _flagsState == OakFlagsStateShiftTapped && tapThreshold ? OakFlagsStateSecondShiftDown : OakFlagsStateShiftDown;
		else if(didReleaseShift && tapThreshold && _flagsState == OakFlagsStateSecondShiftDown)
			[self deselectLast:self];
		else if(didReleaseShift && tapThreshold)
			newFlagsState = OakFlagsStateShiftTapped;

		self.lastFlags           = modifiers;
		self.lastFlagsChangeDate = [NSDate date];
		self.flagsState          = newFlagsState;
	}
}

- (void)insertText:(id)aString
{
	[self insertText:aString replacementRange:NSMakeRange(NSNotFound, 0)];
}

- (void)insertText:(id)aString replacementRange:(NSRange)aRange
{
	AUTO_REFRESH;
	if(!_markedRanges.empty())
	{
		documentView->set_ranges(_markedRanges);
		[self delete:nil];
		_markedRanges = ng::ranges_t();
	}
	pendingMarkedRanges = ng::ranges_t();

	if(aRange.location != NSNotFound)
	{
		documentView->set_ranges([self rangesForReplacementRange:aRange]);
		[self delete:nil];
	}

	std::string const str = to_s(aString);
	[self recordSelector:@selector(insertText:) withArgument:[NSString stringWithCxxString:str]];
	bool autoPairing = !macroRecordingArray && ![NSUserDefaults.standardUserDefaults boolForKey:kUserDefaultsDisableTypingPairsKey];
	documentView->insert_with_pairing(str, [self indentCorrections], autoPairing, to_s([self scopeAttributes]));
}

- (IBAction)toggleCurrentFolding:(id)sender
{
	AUTO_REFRESH;
	if(documentView->ranges().size() == 1 && !documentView->ranges().last().empty() && !documentView->ranges().last().columnar)
	{
		documentView->fold(documentView->ranges().last().min().index, documentView->ranges().last().max().index);
	}
	else
	{
		size_t line = documentView->convert(documentView->ranges().last().first.index).line;
		documentView->toggle_fold_at_line(line, false);
	}
	[NSNotificationCenter.defaultCenter postNotificationName:GVColumnDataSourceDidChange object:[[self enclosingScrollView] superview]];
}

- (IBAction)toggleFoldingAtLine:(NSUInteger)lineNumber recursive:(BOOL)flag
{
	AUTO_REFRESH;
	documentView->toggle_fold_at_line(lineNumber, flag);
}

- (IBAction)takeLevelToFoldFrom:(id)sender
{
	AUTO_REFRESH;
	documentView->toggle_all_folds_at_level([sender tag]);
	[NSNotificationCenter.defaultCenter postNotificationName:GVColumnDataSourceDidChange object:[[self enclosingScrollView] superview]];
}

- (NSPoint)positionForWindowUnderCaret
{
	CGRect r1 = documentView->rect_at_index(documentView->ranges().last().normalized().first);
	CGRect r2 = documentView->rect_at_index(documentView->ranges().last().normalized().last);
	CGRect r = r1.origin.y == r2.origin.y && r1.origin.x < r2.origin.x ? r1 : r2;
	NSPoint p = NSMakePoint(CGRectGetMinX(r), CGRectGetMaxY(r)+4);
	if(NSPointInRect(p, [self visibleRect]))
			{ p = [[self window] convertRectToScreen:[self convertRect:(NSRect){ p, NSZeroSize } toView:nil]].origin; }
	else	{ p = [NSEvent mouseLocation]; p.y -= 16; }

	return p;
}

- (bundles::item_ptr)showMenuForBundleItems:(std::vector<bundles::item_ptr> const&)items
{
	NSPoint pos = [self positionForWindowUnderCaret];
	pos = [self convertPoint:[self.window convertRectFromScreen:(NSRect){ pos, NSZeroSize }].origin fromView:nil];
	return OakShowMenuForBundleItems(items, self, pos);
}

- (NSMenu*)checkSpellingMenuForRanges:(ng::ranges_t const&)someRanges
{
	NSMenu* menu = [[NSMenu alloc] initWithTitle:@""];
	if(someRanges.size() != 1)
		return menu;

	ng::range_t const range     = someRanges.first();
	ng::range_t const wordRange = range.empty() ? ng::extend(*documentView, range.first, kSelectionExtendToWord).last() : range;
	std::string const candidate = documentView->substr(wordRange.min().index, wordRange.max().index);

	if(candidate.find_first_of(" \n\t") != std::string::npos)
		return menu;

	NSString* word = [NSString stringWithCxxString:candidate];
	if([NSSpellChecker.sharedSpellChecker hasLearnedWord:word])
	{
		NSMenuItem* item = [menu addItemWithTitle:[NSString stringWithFormat:@"Unlearn “%@”", word] action:@selector(contextMenuPerformUnlearnSpelling:) keyEquivalent:@""];
		[item setRepresentedObject:word];
		[menu addItem:[NSMenuItem separatorItem]];
	}
	else if(ns::is_misspelled(candidate, documentView->spelling_language(), documentView->spelling_tag()))
	{
		AUTO_REFRESH;
		documentView->set_ranges(wordRange);

		[NSSpellChecker.sharedSpellChecker updateSpellingPanelWithMisspelledWord:word];

		size_t bol = documentView->begin(documentView->convert(wordRange.min().index).line);
		size_t eol = documentView->eol(documentView->convert(wordRange.max().index).line);
		std::string const line = documentView->substr(bol, eol);
		NSUInteger location = utf16::distance(line.data(), line.data() + (wordRange.min().index - bol));
		NSUInteger length   = utf16::distance(line.data() + (wordRange.min().index - bol), line.data() + (wordRange.max().index - bol));

		char key = 0;
		NSMenuItem* item = nil;
		for(NSString* guess in [NSSpellChecker.sharedSpellChecker guessesForWordRange:NSMakeRange(location, length) inString:[NSString stringWithCxxString:line] language:[NSString stringWithCxxString:documentView->spelling_language()] inSpellDocumentWithTag:documentView->spelling_tag()])
		{
			item = [menu addItemWithTitle:guess action:@selector(contextMenuPerformCorrectWord:) keyEquivalent:key < 10 ? [NSString stringWithFormat:@"%c", '0' + (++key % 10)] : @""];
			[item setKeyEquivalentModifierMask:0];
			[item setRepresentedObject:guess];
		}

		if([menu numberOfItems] == 0)
			[menu addItemWithTitle:@"No Guesses Found" action:nil keyEquivalent:@""];

		[menu addItem:[NSMenuItem separatorItem]];
		item = [menu addItemWithTitle:@"Ignore Spelling" action:@selector(contextMenuPerformIgnoreSpelling:) keyEquivalent:@"-"];
		[item setKeyEquivalentModifierMask:0];
		[item setRepresentedObject:word];
		item = [menu addItemWithTitle:@"Learn Spelling" action:@selector(contextMenuPerformLearnSpelling:) keyEquivalent:@"="];
		[item setKeyEquivalentModifierMask:0];
		[item setRepresentedObject:word];
		[menu addItem:[NSMenuItem separatorItem]];
	}

	return menu;
}

- (NSMenu*)contextMenuForRanges:(ng::ranges_t const&)someRanges
{
	static struct { NSString* title; SEL action; } const items[] =
	{
		{ @"Cut",                     @selector(cut:)                           },
		{ @"Copy",                    @selector(copy:)                          },
		{ @"Paste",                   @selector(paste:)                         },
		{ nil,                        nil                                       },
		{ @"Fold/Unfold",             @selector(toggleCurrentFolding:)          },
		{ @"Filter Through Command…", @selector(orderFrontRunCommandWindow:)    },
	};

	NSMenu* menu = [self checkSpellingMenuForRanges:someRanges];
	std::vector<bundles::item_ptr> const& bundleItems = bundles::query(bundles::kFieldSemanticClass, "callback.context-menu", [self scopeContext], bundles::kItemTypeCommand, oak::uuid_t(), false);
	if(!bundleItems.empty())
	{
		for(auto const& item : bundleItems)
		{
			NSMenuItem* menuItem = [menu addItemWithTitle:[NSString stringWithCxxString:item->name()] action:@selector(performBundleItemWithUUIDStringFrom:) keyEquivalent:@""];
			[menuItem setRepresentedObject:[NSString stringWithCxxString:item->uuid()]];
		}
		[menu addItem:[NSMenuItem separatorItem]];
	}

	for(auto const& item : items)
	{
		if(item.title)
				[menu addItemWithTitle:item.title action:item.action keyEquivalent:@""];
		else	[menu addItem:[NSMenuItem separatorItem]];
	}
	return menu;
}

- (NSMenu*)menuForEvent:(NSEvent*)anEvent
{
	NSPoint point = [self convertPoint:[anEvent locationInWindow] fromView:nil];
	ng::index_t index = documentView->index_at_point(point);
	bool clickInSelection = false;
	for(auto const& range : documentView->ranges())
		clickInSelection = clickInSelection || (range.min() <= index && index <= range.max());
	if(!clickInSelection)
	{
		AUTO_REFRESH;
		documentView->set_ranges(ng::range_t(index.index));
	}
	return [self contextMenuForRanges:documentView->ranges()];
}

- (void)showMenu:(NSMenu*)aMenu
{
	NSWindow* win = [self window];
	NSEvent* anEvent = [NSApp currentEvent];
	NSEvent* fakeEvent = [NSEvent
		mouseEventWithType:NSEventTypeLeftMouseDown
		location:[win convertRectFromScreen:(NSRect){ [self positionForWindowUnderCaret], NSZeroSize }].origin
		modifierFlags:0
		timestamp:[anEvent timestamp]
		windowNumber:[win windowNumber]
		context:nil
		eventNumber:0
		clickCount:1
		pressure:1];

	[NSMenu popUpContextMenu:aMenu withEvent:fakeEvent forView:self];
	[win performSelector:@selector(invalidateCursorRectsForView:) withObject:self afterDelay:0]; // with option used as modifier, the cross-hair cursor will stick
}

- (void)showContextMenu:(id)sender
{
	// Since contextMenuForRanges: may change selection and showMenu: is blocking the event loop, we need to allow for refreshing the display before showing the context menu.
	[self performSelector:@selector(showMenu:) withObject:[self contextMenuForRanges:documentView->ranges()] afterDelay:0];
}

- (void)contextMenuPerformCorrectWord:(NSMenuItem*)menuItem
{
	AUTO_REFRESH;
	documentView->insert(to_s([menuItem representedObject]));
	if(NSSpellChecker.sharedSpellCheckerExists)
		[NSSpellChecker.sharedSpellChecker updateSpellingPanelWithMisspelledWord:[menuItem representedObject]];
}

- (void)contextMenuPerformIgnoreSpelling:(id)sender
{
	[self ignoreSpelling:[sender representedObject]];
}

- (void)contextMenuPerformLearnSpelling:(id)sender
{
	[NSSpellChecker.sharedSpellChecker learnWord:[sender representedObject]];

	documentView->recheck_spelling(0, documentView->size());
	[self setNeedsDisplay:YES];
}

- (void)contextMenuPerformUnlearnSpelling:(id)sender
{
	[NSSpellChecker.sharedSpellChecker unlearnWord:[sender representedObject]];

	documentView->recheck_spelling(0, documentView->size());
	[self setNeedsDisplay:YES];
}

- (void)ignoreSpelling:(id)sender
{
	NSString* word = nil;
	if([sender respondsToSelector:@selector(selectedCell)])
		word = [[sender selectedCell] stringValue];
	else if([sender isKindOfClass:[NSString class]])
		word = sender;

	if(word)
	{
		[NSSpellChecker.sharedSpellChecker ignoreWord:word inSpellDocumentWithTag:documentView->spelling_tag()];
		documentView->recheck_spelling(0, documentView->size());
		[self setNeedsDisplay:YES];
	}
}

- (void)changeSpelling:(id)sender
{
	if([sender respondsToSelector:@selector(selectedCell)])
	{
		AUTO_REFRESH;
		documentView->insert(to_s([[sender selectedCell] stringValue]));
	}
}

// =========================
// = Find Protocol: Client =
// =========================

- (void)performFindOperation:(id <OakFindServerProtocol>)aFindServer
{
	[NSNotificationCenter.defaultCenter postNotificationName:@"OakTextViewWillPerformFindOperation" object:self];

	if(![aFindServer isKindOfClass:[OakTextViewFindServer class]])
	{
		NSMutableDictionary* dict = [NSMutableDictionary dictionary];

		dict[@"findString"]    = aFindServer.findString;
		dict[@"replaceString"] = aFindServer.replaceString;

		static find_operation_t const inSelectionActions[] = { kFindOperationFindInSelection, kFindOperationReplaceAllInSelection };
		if(oak::contains(std::begin(inSelectionActions), std::end(inSelectionActions), aFindServer.findOperation))
			dict[@"replaceAllScope"] = @"selection";

		find::options_t options = aFindServer.findOptions;

		if(options & find::ignore_case)
			dict[@"ignoreCase"]        = @YES;
		if(options & find::ignore_whitespace)
			dict[@"ignoreWhitespace"]  = @YES;
		if(options & find::regular_expression)
			dict[@"regularExpression"] = @YES;
		if(options & find::wrap_around)
			dict[@"wrapAround"]        = @YES;

		switch(aFindServer.findOperation)
		{
			case kFindOperationFind:
			case kFindOperationFindInSelection:
			{
				if(options & find::all_matches)
					dict[@"action"] = @"findAll";
				else if(options & find::backwards)
					dict[@"action"] = @"findPrevious";
				else
					dict[@"action"] = @"findNext";
			}
			break;

			case kFindOperationReplaceAll:
			case kFindOperationReplaceAllInSelection: dict[@"action"] = @"replaceAll";     break;
			case kFindOperationReplace:               dict[@"action"] = @"replace";        break;
			case kFindOperationReplaceAndFind:        dict[@"action"] = @"replaceAndFind"; break;
		}

		if(dict[@"action"])
			[self recordSelector:NSSelectorFromString(@"findWithOptions:") withArgument:dict];
	}

	AUTO_REFRESH;

	find_operation_t findOperation = aFindServer.findOperation;
	if(findOperation == kFindOperationReplace || findOperation == kFindOperationReplaceAndFind)
	{
		std::string replacement = to_s(aFindServer.replaceString);
		if(NSDictionary* captures = _document.matchCaptures)
		{
			std::map<std::string, std::string> variables;
			for(NSString* key in [captures allKeys])
				variables.emplace(to_s(key), to_s(captures[key]));
			replacement = format_string::expand(replacement, variables);
		}
		documentView->insert(replacement, true);

		if(findOperation == kFindOperationReplaceAndFind)
			findOperation = kFindOperationFind;
	}

	bool onlyInSelection = false;
	switch(findOperation)
	{
		case kFindOperationFindInSelection:
		case kFindOperationCountInSelection: onlyInSelection = documentView->has_selection();
		case kFindOperationFind:
		case kFindOperationCount:
		{
			_document.matchCaptures = nil;
			bool isCounting = findOperation == kFindOperationCount || findOperation == kFindOperationCountInSelection;

			std::string const findStr = to_s(aFindServer.findString);
			find::options_t options   = aFindServer.findOptions;

			NSArray<FindMatch*>* findMatches = Find.sharedInstance.findMatches;
			if(findMatches && findMatches.count > 1)
				options &= ~find::wrap_around;

			bool didWrap = false;
			auto allMatches = ng::find(*documentView, documentView->ranges(), findStr, options, onlyInSelection ? documentView->ranges() : ng::ranges_t(), &didWrap);

			ng::ranges_t res;
			std::transform(allMatches.begin(), allMatches.end(), std::back_inserter(res), [](auto const& p){ return p.first; });
			if(onlyInSelection && res.sorted() == documentView->ranges().sorted())
			{
				res = ng::ranges_t();
				allMatches = ng::find(*documentView, documentView->ranges(), findStr, options, ng::ranges_t());
				std::transform(allMatches.begin(), allMatches.end(), std::back_inserter(res), [](auto const& p){ return p.first; });
			}

			if(res.empty() && !isCounting && findMatches && findMatches.count > 1)
			{
				for(NSUInteger i = 0; i < findMatches.count; ++i)
				{
					NSUUID* uuid = findMatches[i].UUID;
					if(oak::uuid_t(to_s(uuid)) == documentView->identifier())
					{
						// ====================================================
						// = Update our document’s matches on Find pasteboard =
						// ====================================================

						NSMutableArray<FindMatch*>* newFindMatches = [findMatches mutableCopy];
						auto newFirstMatch = ng::find(*documentView, ng::ranges_t(0), findStr, (find::options_t)(options & ~find::backwards));
						if(newFirstMatch.empty())
						{
							[newFindMatches removeObjectAtIndex:i];
						}
						else
						{
							auto newLastMatch = ng::find(*documentView, ng::ranges_t(0), findStr, (find::options_t)(options | find::backwards | find::wrap_around));
							auto to_range = [&](auto it) { return text::range_t(documentView->convert(it->first.min().index), documentView->convert(it->first.max().index)); };
							auto newFindMatch = [[FindMatch alloc] initWithUUID:uuid firstRange:to_range(newFirstMatch.begin()) lastRange:to_range((newLastMatch.empty() ? newFirstMatch : newLastMatch).begin())];
							[newFindMatches replaceObjectAtIndex:i withObject:newFindMatch];
						}
						Find.sharedInstance.findMatches = newFindMatches;

						// ====================================================

						FindMatch* findMatch = findMatches[(i + ((options & find::backwards) ? findMatches.count - 1 : 1)) % findMatches.count];
						if(OakDocument* doc = [OakDocumentController.sharedInstance findDocumentWithIdentifier:findMatch.UUID])
						{
							if(!doc.isOpen)
								doc.recentTrackingDisabled = YES;

							text::range_t range = (options & find::backwards) ? findMatch.lastRange : findMatch.firstRange;
							[OakDocumentController.sharedInstance showDocument:doc andSelect:range inProject:nil bringToFront:YES];
							return;
						}
					}
				}
			}

			if(isCounting)
			{
				[aFindServer didFind:res.size() occurrencesOf:aFindServer.findString atPosition:res.size() == 1 ? documentView->convert(res.last().min().index) : text::pos_t::undefined wrapped:NO];
			}
			else
			{
				std::set<ng::range_t> alreadySelected;
				for(auto const& range : documentView->ranges())
					alreadySelected.insert(range);

				ng::ranges_t newSelection;
				for(auto range : res)
				{
					if(alreadySelected.find(range.sorted()) == alreadySelected.end())
						newSelection.push_back(range.sorted());
				}

				if(!res.empty())
				{
					documentView->set_ranges(res);
					if(res.size() == 1 && (options & find::regular_expression))
					{
						NSMutableDictionary* captures = [NSMutableDictionary dictionary];
						for(auto pair : allMatches[res.last()])
							captures[[NSString stringWithCxxString:pair.first]] = [NSString stringWithCxxString:pair.second];
						_document.matchCaptures = captures;
					}
				}

				// If wrap_around is enabled but the only result is already selected, highlight it again.
				if(newSelection.empty() && (options & find::wrap_around) && alreadySelected.size() == 1)
				{
					auto selection = *alreadySelected.begin();
					auto tmp = ng::find_all(*documentView, findStr, options, { selection });
					if(tmp.size() == 1 && tmp.begin()->first.sorted() == selection)
						newSelection.push_back(selection);
				}

				[self highlightRanges:newSelection];
				[aFindServer didFind:newSelection.size() occurrencesOf:aFindServer.findString atPosition:res.size() == 1 ? documentView->convert(res.last().min().index) : text::pos_t::undefined wrapped:didWrap];
			}
		}
		break;

		case kFindOperationReplaceAll:
		case kFindOperationReplaceAllInSelection:
		{
			std::string const findStr    = to_s(aFindServer.findString);
			std::string const replaceStr = to_s(aFindServer.replaceString);
			find::options_t options      = aFindServer.findOptions;

			ng::ranges_t const res = documentView->replace_all(findStr, replaceStr, options, findOperation == kFindOperationReplaceAllInSelection);
			[aFindServer didReplace:res.size() occurrencesOf:aFindServer.findString with:aFindServer.replaceString];
		}
		break;
	}
}

- (void)recordSelector:(SEL)aSelector andPerform:(find_operation_t)findOperation withOptions:(find::options_t)extraOptions
{
	[self recordSelector:aSelector withArgument:nil];
	[self performFindOperation:[OakTextViewFindServer findServerWithTextView:self operation:findOperation options:[OakPasteboard.findPasteboard current].findOptions | extraOptions]];
}

- (void)setShowLiveSearch:(BOOL)flag
{
	OakDocumentView* docView = (OakDocumentView*)[[self enclosingScrollView] superview];
	if(flag)
	{
		liveSearchAnchor = documentView->ranges();

		if(!self.liveSearchView)
		{
			self.liveSearchView = [[LiveSearchView alloc] initWithFrame:NSZeroRect];
			[docView addAuxiliaryView:self.liveSearchView atEdge:NSMinYEdge];
			self.liveSearchView.nextResponder = self;
		}

		NSTextField* textField = self.liveSearchView.textField;
		[textField setDelegate:self];
		[textField setStringValue:self.liveSearchString ?: @""];

		[[self window] makeFirstResponder:textField];
	}
	else if(self.liveSearchView)
	{
		[docView removeAuxiliaryView:self.liveSearchView];
		[[self window] makeFirstResponder:self];
		self.liveSearchView = nil;
		_liveSearchRanges = ng::ranges_t();
	}
}

- (void)setLiveSearchRanges:(ng::ranges_t)ranges
{
	AUTO_REFRESH;

	ng::ranges_t const oldRanges = ng::move(*documentView, _liveSearchRanges, kSelectionMoveToBeginOfSelection);
	_liveSearchRanges = ranges;
	if(!_liveSearchRanges.empty())
	{
		documentView->set_ranges(_liveSearchRanges);
		if(oldRanges != ng::move(*documentView, _liveSearchRanges, kSelectionMoveToBeginOfSelection))
			[self highlightRanges:_liveSearchRanges];
	}
	else if(!oldRanges.empty())
	{
		NSBeep();
	}
}

- (BOOL)control:(NSControl*)aControl textView:(NSTextView*)aTextView doCommandBySelector:(SEL)aCommand
{
	if(aCommand == @selector(insertNewline:) || aCommand == @selector(cancelOperation:))
	{
		[self setShowLiveSearch:NO];
		return YES;
	}
	if(aCommand == @selector(insertTab:))
	{
		[self findNext:self];
		return YES;
	}
	if(aCommand == @selector(insertBacktab:))
	{
		[self findPrevious:self];
		return YES;
	}
	return NO;
}

- (void)controlTextDidChange:(NSNotification*)aNotification
{
	NSTextView* searchField = [[aNotification userInfo] objectForKey:@"NSFieldEditor"];
	self.liveSearchString = [searchField string];

	ng::ranges_t res;
	for(auto const& pair : ng::find(*documentView, liveSearchAnchor, to_s(_liveSearchString), find::ignore_case|find::ignore_whitespace|find::wrap_around))
		res.push_back(pair.first);
	[self setLiveSearchRanges:res];
}

- (IBAction)incrementalSearch:(id)sender
{
	if(self.liveSearchView)
			[self findNext:self];
	else	[self setShowLiveSearch:YES];
}

- (IBAction)incrementalSearchPrevious:(id)sender
{
	if(self.liveSearchView)
			[self findPrevious:self];
	else	[self setShowLiveSearch:YES];
}

- (find::options_t)incrementalSearchOptions
{
	BOOL ignoreCase = self.liveSearchView.ignoreCaseCheckBox.state == NSControlStateValueOn;
	BOOL wrapAround = self.liveSearchView.wrapAroundCheckBox.state == NSControlStateValueOn;
	return (ignoreCase ? find::ignore_case : find::none) | (wrapAround ? find::wrap_around : find::none) | find::ignore_whitespace;
}

- (IBAction)findNext:(id)sender
{
	if(self.liveSearchView)
	{
		ng::ranges_t tmp;
		for(auto const& pair : ng::find(*documentView, ng::move(*documentView, _liveSearchRanges.empty() ? liveSearchAnchor : _liveSearchRanges, kSelectionMoveToEndOfSelection), to_s(_liveSearchString), self.incrementalSearchOptions))
			tmp.push_back(pair.first);
		[self setLiveSearchRanges:tmp];
		if(!tmp.empty())
			liveSearchAnchor = ng::move(*documentView, tmp, kSelectionMoveToBeginOfSelection);
	}
	else
	{
		[self recordSelector:_cmd andPerform:kFindOperationFind withOptions:find::none];
	}
}

- (IBAction)findPrevious:(id)sender
{
	if(self.liveSearchView)
	{
		ng::ranges_t tmp;
		for(auto const& pair : ng::find(*documentView, ng::move(*documentView, _liveSearchRanges.empty() ? liveSearchAnchor : _liveSearchRanges, kSelectionMoveToBeginOfSelection), to_s(_liveSearchString), find::backwards|self.incrementalSearchOptions))
			tmp.push_back(pair.first);
		[self setLiveSearchRanges:tmp];
		if(!tmp.empty())
			liveSearchAnchor = ng::move(*documentView, tmp, kSelectionMoveToBeginOfSelection);
	}
	else
	{
		[self recordSelector:_cmd andPerform:kFindOperationFind withOptions:find::backwards];
	}
}

- (IBAction)findNextAndModifySelection:(id)sender     { [self recordSelector:_cmd andPerform:kFindOperationFind                  withOptions:find::extend_selection]; }
- (IBAction)findPreviousAndModifySelection:(id)sender { [self recordSelector:_cmd andPerform:kFindOperationFind                  withOptions:find::extend_selection | find::backwards]; }

- (IBAction)findAll:(id)sender                        { [self recordSelector:_cmd andPerform:kFindOperationFind                  withOptions:find::all_matches]; }
- (IBAction)findAllInSelection:(id)sender             { [self recordSelector:_cmd andPerform:kFindOperationFindInSelection       withOptions:find::all_matches]; }

- (IBAction)replace:(id)sender                        { [self recordSelector:_cmd andPerform:kFindOperationReplace               withOptions:find::none]; }
- (IBAction)replaceAndFind:(id)sender                 { [self recordSelector:_cmd andPerform:kFindOperationReplaceAndFind        withOptions:find::none]; }

- (IBAction)replaceAll:(id)sender                     { [self recordSelector:_cmd andPerform:kFindOperationReplaceAll            withOptions:find::all_matches]; }
- (IBAction)replaceAllInSelection:(id)sender          { [self recordSelector:_cmd andPerform:kFindOperationReplaceAllInSelection withOptions:find::all_matches]; }

// ============================
// = Bookmark Related Actions =
// ============================

- (IBAction)toggleCurrentBookmark:(id)sender
{
	if(!documentView)
		return;
	documentView->toggle_current_bookmark();
}

- (IBAction)goToNextBookmark:(id)sender
{
	if(!documentView)
		return;
	AUTO_REFRESH;
	documentView->jump_to_next_bookmark(to_s(OakDocumentBookmarkIdentifier));
}

- (IBAction)goToPreviousBookmark:(id)sender
{
	if(!documentView)
		return;
	AUTO_REFRESH;
	documentView->jump_to_previous_bookmark(to_s(OakDocumentBookmarkIdentifier));
}

- (IBAction)jumpToNextMark:(id)sender
{
	if(!documentView)
		return;
	AUTO_REFRESH;
	documentView->jump_to_next_bookmark();
}

- (IBAction)jumpToPreviousMark:(id)sender
{
	if(!documentView)
		return;
	AUTO_REFRESH;
	documentView->jump_to_previous_bookmark();
}

// ============================

// =============
// = Touch Bar =
// =============

static NSTouchBarItemIdentifier kOTVTouchBarCustomizationIdentifier          = @"com.macromates.TextMate.otv.customizationIdentifer";
static NSTouchBarItemIdentifier kOTVTouchBarItemIdentifierNavigateBookmarks  = @"com.macromates.TextMate.otv.navigateBookmarks";
static NSTouchBarItemIdentifier kOTVTouchBarItemIdentifierAddRemoveBookmark  = @"com.macromates.TextMate.otv.addRemoveBookmark";

- (NSTouchBar*)makeTouchBar
{
	NSTouchBar* touchBar = [NSTouchBar new];
	touchBar.delegate = self;
	touchBar.defaultItemIdentifiers = @[ kOTVTouchBarItemIdentifierAddRemoveBookmark, kOTVTouchBarItemIdentifierNavigateBookmarks, ];
	touchBar.customizationIdentifier = kOTVTouchBarCustomizationIdentifier;
	touchBar.customizationAllowedItemIdentifiers = @[ kOTVTouchBarItemIdentifierAddRemoveBookmark, kOTVTouchBarItemIdentifierNavigateBookmarks, ];

	return touchBar;
}

- (NSTouchBarItem*)touchBar:(NSTouchBar*)touchBar makeItemForIdentifier:(NSTouchBarItemIdentifier)identifier
{
	if([identifier isEqualToString:kOTVTouchBarItemIdentifierAddRemoveBookmark])
	{
		NSImage* bookmarkImage = [NSImage imageNamed:@"RemoveBookmarkTemplate" inSameBundleAsClass:[self class]];
		bookmarkImage.accessibilityDescription = @"add or remove bookmark";

		NSCustomTouchBarItem* bookmarkButtonTouchBarItem = [[NSCustomTouchBarItem alloc] initWithIdentifier:kOTVTouchBarItemIdentifierAddRemoveBookmark];
		bookmarkButtonTouchBarItem.view = [NSButton buttonWithImage:bookmarkImage target:self action:@selector(toggleCurrentBookmark:)];
		bookmarkButtonTouchBarItem.visibilityPriority = NSTouchBarItemPriorityHigh;
		bookmarkButtonTouchBarItem.customizationLabel = @"Add/Remove Bookmark";

		return bookmarkButtonTouchBarItem;
	}
	else if([identifier isEqualToString:kOTVTouchBarItemIdentifierNavigateBookmarks])
	{
		NSSegmentedControl* navigateMarkerSegmentedControl = [NSSegmentedControl new];
		navigateMarkerSegmentedControl.segmentCount = 2;
		navigateMarkerSegmentedControl.target       = self;
		navigateMarkerSegmentedControl.action       = @selector(performNavigateBookmarksSegmentAction:);
		navigateMarkerSegmentedControl.trackingMode = NSSegmentSwitchTrackingMomentary;
		navigateMarkerSegmentedControl.segmentStyle = NSSegmentStyleSeparated;

		NSImage* goUpImage = [NSImage imageNamed:NSImageNameTouchBarGoUpTemplate];
		goUpImage.accessibilityDescription = @"previous bookmark";
		NSImage* goDownImage = [NSImage imageNamed:NSImageNameTouchBarGoDownTemplate];
		goDownImage.accessibilityDescription = @"next bookmark";

		[navigateMarkerSegmentedControl setImage:goUpImage forSegment:0];
		[navigateMarkerSegmentedControl setImage:goDownImage forSegment:1];

		NSCustomTouchBarItem* markersTouchBarItem = [[NSCustomTouchBarItem alloc] initWithIdentifier:kOTVTouchBarItemIdentifierNavigateBookmarks];
		markersTouchBarItem.view = navigateMarkerSegmentedControl;
		markersTouchBarItem.visibilityPriority = NSTouchBarItemPriorityHigh;
		markersTouchBarItem.customizationLabel = @"Previous/Next Bookmark";

		return markersTouchBarItem;
	}

	return nil;
}

- (void)performNavigateBookmarksSegmentAction:(id)sender
{
	switch([sender selectedSegment])
	{
		case 0: [self goToPreviousBookmark:self]; break;
		case 1: [self goToNextBookmark:self];     break;
	}
}

// =============

- (void)insertSnippetWithOptions:(NSDictionary*)someOptions // For Dialog popup
{
	AUTO_REFRESH;
	[self recordSelector:_cmd withArgument:someOptions];
	documentView->snippet_dispatch(plist::convert((__bridge CFDictionaryRef)someOptions), [self variables]);
}

- (void)undo:(id)anArgument // MACRO?
{
	AUTO_REFRESH;
	if(!documentView->can_undo())
		return;
	documentView->undo();
}

- (void)redo:(id)anArgument // MACRO?
{
	AUTO_REFRESH;
	if(!documentView->can_redo())
		return;
	documentView->redo();
}

- (BOOL)expandTabTrigger:(id)sender
{
	if(documentView->disallow_tab_expansion())
		return NO;

	AUTO_REFRESH;
	ng::range_t range;
	std::vector<bundles::item_ptr> const& items = items_for_tab_expansion(documentView, documentView->ranges(), to_s([self scopeAttributes]), &range);
	if(bundles::item_ptr item = [self showMenuForBundleItems:items])
	{
		[self recordSelector:NSSelectorFromString(@"deleteTabTrigger:") withArgument:[NSString stringWithCxxString:documentView->substr(range.first.index, range.last.index)]];
		documentView->delete_tab_trigger(documentView->substr(range.first.index, range.last.index));
		[self performBundleItem:item];
	}
	return !items.empty();
}

- (void)insertTab:(id)sender
{
	AUTO_REFRESH;
	if(![self expandTabTrigger:sender])
	{
		[self recordSelector:_cmd withArgument:nil];
		documentView->perform(ng::kInsertTab, [self indentCorrections], to_s([self scopeAttributes]));
	}
}

static char const* kOakMenuItemTitle = "OakMenuItemTitle";

- (BOOL)validateMenuItem:(NSMenuItem*)aMenuItem
{
	NSString* title = objc_getAssociatedObject(aMenuItem, kOakMenuItemTitle) ?: aMenuItem.title;
	objc_setAssociatedObject(aMenuItem, kOakMenuItemTitle, title, OBJC_ASSOCIATION_RETAIN);
	[aMenuItem updateTitle:[NSString stringWithCxxString:format_string::replace(to_s(title), "\\b(\\w+) / (Selection)\\b", [self hasSelection] ? "$2" : "$1")]];

	if([aMenuItem action] == @selector(cut:))
		[aMenuItem setDynamicTitle:@"Cut"];
	else if([aMenuItem action] == @selector(copy:))
		[aMenuItem setDynamicTitle:@"Copy"];

	static auto const RequiresSelection = new std::set<SEL>{ @selector(cut:), @selector(copy:), @selector(delete:), @selector(copySelectionToFindPboard:) };
	if(RequiresSelection->find([aMenuItem action]) != RequiresSelection->end())
		return [self hasSelection];
	else if([aMenuItem action] == @selector(toggleMacroRecording:))
		[aMenuItem setTitle:self.isRecordingMacro ? @"Stop Recording" : @"Start Recording"];
	else if([aMenuItem action] == @selector(toggleShowInvisibles:))
		[aMenuItem setTitle:self.showInvisibles ? @"Hide Invisible Characters" : @"Show Invisible Characters"];
	else if([aMenuItem action] == @selector(toggleSoftWrap:))
		[aMenuItem setTitle:self.softWrap ? @"Disable Soft Wrap" : @"Enable Soft Wrap"];
	else if([aMenuItem action] == @selector(toggleScrollPastEnd:))
		[aMenuItem setTitle:self.scrollPastEnd ? @"Disallow Scroll Past End" : @"Allow Scroll Past End"];
	else if([aMenuItem action] == @selector(toggleShowWrapColumn:))
		[aMenuItem setTitle:(documentView && documentView->draw_wrap_column()) ? @"Hide Wrap Column" : @"Show Wrap Column"];
	else if([aMenuItem action] == @selector(toggleShowIndentGuides:))
		[aMenuItem setTitle:(documentView && documentView->draw_indent_guides()) ? @"Hide Indent Guides" : @"Show Indent Guides"];
	else if([aMenuItem action] == @selector(toggleContinuousSpellChecking:))
		[aMenuItem setState:documentView->live_spelling() ? NSControlStateValueOn : NSControlStateValueOff];
	else if([aMenuItem action] == @selector(takeSpellingLanguageFrom:))
		[aMenuItem setState:[[NSString stringWithCxxString:documentView->spelling_language()] isEqualToString:[aMenuItem representedObject]] ? NSControlStateValueOn : NSControlStateValueOff];
	else if([aMenuItem action] == @selector(takeWrapColumnFrom:))
		[aMenuItem setState:(documentView && documentView->wrap_column() == [aMenuItem tag]) ? NSControlStateValueOn : NSControlStateValueOff];
	else if([aMenuItem action] == @selector(undo:))
	{
		[aMenuItem setTitle:@"Undo"];
		return documentView->can_undo();
	}
	else if([aMenuItem action] == @selector(redo:))
	{
		[aMenuItem setTitle:@"Redo"];
		return documentView->can_redo();
	}
	else if([aMenuItem action] == @selector(toggleCurrentBookmark:))
		[aMenuItem setTitle:documentView && documentView->current_line_has_marks(to_s(OakDocumentBookmarkIdentifier)) ? @"Remove Bookmark" : @"Set Bookmark"];
	else if([aMenuItem action] == @selector(goToNextBookmark:) || [aMenuItem action] == @selector(goToPreviousBookmark:))
		return documentView && documentView->has_marks(to_s(OakDocumentBookmarkIdentifier));
	else if([aMenuItem action] == @selector(jumpToNextMark:) || [aMenuItem action] == @selector(jumpToPreviousMark:))
		return documentView && documentView->has_marks();
	else if([aMenuItem action] == @selector(performBundleItemWithUUIDStringFrom:))
	{
		if(bundles::item_ptr bundleItem = bundles::lookup(to_s(aMenuItem.representedObject)))
		{
			std::string name = bundleItem->name();
			if(regexp::match_t const m = regexp::search("^(\\w+) / (\\w+) (.*)", name))
			{
				auto command = parse_command(bundleItem);
				if(command.auto_refresh != auto_refresh::never)
				{
					BOOL shouldTeardown = NO;
					if(OakCommandRefresher* refresher = [self existingRefresherForCommand:command])
						shouldTeardown = !refresher.command.htmlOutputView;
					[aMenuItem updateTitle:to_ns(format_string::expand(shouldTeardown ? "$2 $3" : "$1 $3", m.captures()))];
				}
			}
		}
	}
	return YES;
}

// ==================
// = Caret Blinking =
// ==================

- (NSTimer*)blinkCaretTimer
{
	return blinkCaretTimer;
}

- (void)setBlinkCaretTimer:(NSTimer*)aValue
{
	[blinkCaretTimer invalidate];
	blinkCaretTimer = aValue;
}

- (void)resetBlinkCaretTimer
{
	BOOL hasFocus = (self.keyState & (OakViewViewIsFirstResponderMask|OakViewWindowIsKeyMask|OakViewApplicationIsActiveMask)) == (OakViewViewIsFirstResponderMask|OakViewWindowIsKeyMask|OakViewApplicationIsActiveMask);
	if(hasFocus && documentView)
	{
		AUTO_REFRESH;
		documentView->set_draw_caret(true);
		hideCaret = NO;

		self.blinkCaretTimer = [NSTimer scheduledTimerWithTimeInterval:[NSEvent caretBlinkInterval] target:self selector:@selector(toggleCaretVisibility:) userInfo:nil repeats:YES];
	}
}

- (void)toggleCaretVisibility:(id)sender
{
	if(!documentView)
		return;

	AUTO_REFRESH;
	documentView->set_draw_caret(hideCaret);
	hideCaret = !hideCaret;

	// The column selection cursor may get stuck if e.g. using ⌥F2 to bring up a menu: We see the initial “option down” but never the “option release” that would normally reset the column selection cursor state.
	if(([NSEvent modifierFlags] & NSEventModifierFlagOption) == 0)
		self.showColumnSelectionCursor = NO;
}

- (void)setShowColumnSelectionCursor:(BOOL)flag
{
	if(flag != _showColumnSelectionCursor)
	{
		_showColumnSelectionCursor = flag;
		[[self window] invalidateCursorRectsForView:self];
	}
}

// ==============
// = Public API =
// ==============

- (theme_ptr)theme            { return documentView ? documentView->theme() : theme_ptr(); }
- (NSFont*)font               { return documentView ? documentView->font() : [NSFont userFixedPitchFontOfSize:0]; }
- (CGFloat)fontScaleFactor    { return documentView ? documentView->font_scale_factor() : 1; }
- (size_t)tabSize             { return documentView ? documentView->tab_size() : 2; }
- (BOOL)softTabs              { return documentView ? documentView->soft_tabs() : NO; }
- (BOOL)softWrap              { return documentView && documentView->soft_wrap(); }

- (ng::indent_correction_t)indentCorrections
{
	plist::any_t indentCorrections = bundles::value_for_setting("disableIndentCorrections", [self scopeContext]);

	if(std::string const* str = boost::get<std::string>(&indentCorrections))
	{
		if(*str == "emptyLines")
			return ng::kIndentCorrectNonEmptyLines;
	}

	if(plist::is_true(indentCorrections))
		return ng::kIndentCorrectNever;

	return ng::kIndentCorrectAlways;
}

- (void)setTheme:(theme_ptr)newTheme
{
	if(!documentView)
		return;

	AUTO_REFRESH;
	documentView->set_theme(newTheme);
}

- (void)setFont:(NSFont*)newFont
{
	if(documentView)
	{
		AUTO_REFRESH;
		ng::index_t visibleIndex = documentView->index_at_point([self visibleRect].origin);
		documentView->set_font(newFont);
		[self scrollIndexToFirstVisible:documentView->begin(documentView->convert(visibleIndex.index).line)];
	}
}

- (void)setFontScaleFactor:(CGFloat)newFontScaleFactor
{
	if(documentView)
	{
		AUTO_REFRESH;
		ng::index_t visibleIndex = documentView->index_at_point([self visibleRect].origin);
		documentView->set_font_scale_factor(newFontScaleFactor);
		[self scrollIndexToFirstVisible:documentView->begin(documentView->convert(visibleIndex.index).line)];
	}

	[OTVHUD showHudForView:self withText:[NSString stringWithFormat:@"%.f%%", newFontScaleFactor * 100]];
}

- (void)setTabSize:(size_t)newTabSize
{
	if(!documentView || documentView->tab_size() == newTabSize)
		return;
	AUTO_REFRESH;
	documentView->set_tab_size(newTabSize);
}

- (void)setShowInvisibles:(BOOL)flag
{
	if(_showInvisibles == flag)
		return;
	_showInvisibles = flag;
	settings_t::set(kSettingsShowInvisiblesKey, (bool)flag, documentView->file_type());
	[self setNeedsDisplay:YES];
}

- (void)setScrollPastEnd:(BOOL)flag
{
	if(_scrollPastEnd == flag)
		return;
	_scrollPastEnd = flag;
	[NSUserDefaults.standardUserDefaults setBool:flag forKey:kUserDefaultsScrollPastEndKey];
	if(documentView)
	{
		AUTO_REFRESH;
		documentView->set_scroll_past_end(flag);
	}
}

- (void)setSoftWrap:(BOOL)flag
{
	if(!documentView || documentView->soft_wrap() == flag)
		return;

	AUTO_REFRESH;
	ng::index_t visibleIndex = documentView->index_at_point([self visibleRect].origin);
	documentView->set_wrapping(flag, documentView->wrap_column());
	[self scrollIndexToFirstVisible:documentView->begin(documentView->convert(visibleIndex.index).line)];
}

- (void)setSoftTabs:(BOOL)flag
{
	if(!documentView || documentView->soft_tabs() == flag)
		return;
	documentView->set_soft_tabs(flag);
}

- (void)setWrapColumn:(NSInteger)newWrapColumn
{
	if(!documentView || documentView->wrap_column() == newWrapColumn)
		return;

	if(newWrapColumn != NSWrapColumnWindowWidth)
	{
		NSInteger const kWrapColumnPresetsHistorySize = 5;

		NSMutableArray* presets = [[NSUserDefaults.standardUserDefaults arrayForKey:kUserDefaultsWrapColumnPresetsKey] mutableCopy];
		[presets removeObject:@(newWrapColumn)];
		[presets addObject:@(newWrapColumn)];
		if(presets.count > kWrapColumnPresetsHistorySize)
			[presets removeObjectsInRange:NSMakeRange(0, presets.count - kWrapColumnPresetsHistorySize)];
		[NSUserDefaults.standardUserDefaults setObject:presets forKey:kUserDefaultsWrapColumnPresetsKey];
	}

	AUTO_REFRESH;
	documentView->set_wrapping(self.softWrap, newWrapColumn);
	settings_t::set(kSettingsWrapColumnKey, (int32_t)newWrapColumn);
}

- (void)takeWrapColumnFrom:(id)sender
{
	ASSERT([sender respondsToSelector:@selector(tag)]);
	if(!documentView || documentView->wrap_column() == [sender tag])
		return;

	if([sender tag] == NSWrapColumnAskUser)
	{
		NSTextField* textField = [[NSTextField alloc] initWithFrame:NSZeroRect];
		[textField setIntegerValue:documentView->wrap_column() == NSWrapColumnWindowWidth ? 80 : documentView->wrap_column()];
		[textField sizeToFit];
		[textField setFrameSize:NSMakeSize(200, NSHeight([textField frame]))];

		NSAlert* alert = [NSAlert tmAlertWithMessageText:@"Set Wrap Column" informativeText:@"Specify what column text should wrap at:" buttons:@"OK", @"Cancel", nil];
		[alert setAccessoryView:textField];

		if(NSWindow* alertWindow = alert.window)
		{
			[alert layout];
			alertWindow.initialFirstResponder = textField;
			[alertWindow recalculateKeyViewLoop];
		}

		[alert beginSheetModalForWindow:self.window completionHandler:^(NSModalResponse returnCode){
			if(returnCode == NSAlertFirstButtonReturn)
				[self setWrapColumn:std::max<NSInteger>([textField integerValue], 10)];
		}];
	}
	else
	{
		[self setWrapColumn:[sender tag]];
	}
}

- (BOOL)hasMultiLineSelection { return ng::multiline(*documentView, documentView->ranges()); }

- (IBAction)toggleShowInvisibles:(id)sender
{
	self.showInvisibles = !self.showInvisibles;
}

- (IBAction)toggleScrollPastEnd:(id)sender
{
	self.scrollPastEnd = !self.scrollPastEnd;
}

- (IBAction)toggleSoftWrap:(id)sender
{
	BOOL const softWrap = !self.softWrap;
	self.softWrap = softWrap;
	settings_t::set(kSettingsSoftWrapKey, (bool)softWrap, documentView->file_type());
}

- (IBAction)toggleShowWrapColumn:(id)sender
{
	if(documentView)
	{
		AUTO_REFRESH;
		bool flag = !documentView->draw_wrap_column();
		documentView->set_draw_wrap_column(flag);
		settings_t::set(kSettingsShowWrapColumnKey, flag);
	}
}

-(IBAction)toggleShowIndentGuides:(id)sender
{
	if(documentView)
	{
		AUTO_REFRESH;
		bool flag = !documentView->draw_indent_guides();
		documentView->set_draw_indent_guides(flag);
		settings_t::set(kSettingsShowIndentGuidesKey, flag);
	}
}

- (void)checkSpelling:(id)sender
{
	NSSpellChecker* speller = NSSpellChecker.sharedSpellChecker;

	NSString* lang = [NSString stringWithCxxString:documentView->spelling_language()];
	if([[speller spellingPanel] isVisible])
	{
		if(![[speller language] isEqualToString:lang])
		{
			documentView->set_spelling_language(to_s([speller language]));
			[self setNeedsDisplay:YES];
		}
	}
	else
	{
		[speller setLanguage:lang];
	}

	if(!documentView->live_spelling())
	{
		documentView->set_live_spelling(true);
		[self setNeedsDisplay:YES];
	}

	ng::index_t caret = documentView->ranges().last().last;
	if(!documentView->has_selection())
	{
		ng::range_t wordRange = ng::extend(*documentView, caret, kSelectionExtendToWord).last();
		if(caret <= wordRange.max())
			caret = wordRange.min();
	}

	auto nextMisspelling = documentView->next_misspelling(caret.index);
	if(nextMisspelling.first != nextMisspelling.second)
	{
		if([[speller spellingPanel] isVisible])
		{
			AUTO_REFRESH;
			documentView->set_ranges(ng::range_t(nextMisspelling.first, nextMisspelling.second));
			[speller updateSpellingPanelWithMisspelledWord:[NSString stringWithCxxString:documentView->substr(nextMisspelling.first, nextMisspelling.second)]];
		}
		else
		{
			NSMenu* menu = [self checkSpellingMenuForRanges:ng::range_t(nextMisspelling.first, nextMisspelling.second)];
			[menu addItemWithTitle:@"Find Next" action:@selector(checkSpelling:) keyEquivalent:@";"];
			[self showMenu:menu];
		}
	}
	else
	{
		[speller updateSpellingPanelWithMisspelledWord:@""];
	}
}

- (void)toggleContinuousSpellChecking:(id)sender
{
	bool flag = !documentView->live_spelling();
	documentView->set_live_spelling(flag);
	settings_t::set(kSettingsSpellCheckingKey, flag, documentView->file_type(), documentView->path());

	[self setNeedsDisplay:YES];
}

- (void)takeSpellingLanguageFrom:(id)sender
{
	NSString* lang = (NSString*)[sender representedObject];
	[NSSpellChecker.sharedSpellChecker setLanguage:lang];
	documentView->set_spelling_language(to_s(lang));
	settings_t::set(kSettingsSpellingLanguageKey, to_s(lang), "", documentView->path());
	if(documentView->path() != NULL_STR)
		settings_t::set(kSettingsSpellingLanguageKey, to_s(lang), NULL_STR, path::join(path::parent(documentView->path()), "**"));

	[self setNeedsDisplay:YES];
}

- (scope::context_t)scopeContext
{
	return documentView ? ng::scope(*documentView, documentView->ranges(), to_s([self scopeAttributes])) : scope::context_t();
}

- (NSString*)scopeAsString // Used by https://github.com/emmetio/Emmet.tmplugin
{
	return [NSString stringWithCxxString:to_s([self scopeContext].right)];
}

- (void)setSelectionString:(NSString*)aSelectionString
{
	if([aSelectionString isEqualToString:selectionString])
		return;

	selectionString = [aSelectionString copy];
	NSAccessibilityPostNotification(self, NSAccessibilitySelectedTextChangedNotification);
	if(UAZoomEnabled())
		[self performSelector:@selector(updateZoom:) withObject:self afterDelay:0];
	if(isUpdatingSelection)
		return;

	if(documentView)
	{
		AUTO_REFRESH;
		ng::ranges_t ranges = ng::convert(*documentView, to_s(aSelectionString));
		documentView->set_ranges(ranges);
		for(auto const& range : ranges)
			documentView->remove_enclosing_folds(range.min().index, range.max().index);
	}
}

- (NSString*)selectionString
{
	return selectionString;
}

- (void)updateSelection
{
	text::selection_t withoutCarry;
	for(auto const& range : documentView->ranges())
	{
		text::pos_t from = documentView->convert(range.first.index);
		text::pos_t to   = documentView->convert(range.last.index);
		if(range.freehanded || range.columnar)
		{
			from.offset = range.first.carry;
			to.offset   = range.last.carry;
			withoutCarry.push_back(text::range_t(from, to, range.columnar));
		}
		else
		{
			withoutCarry.push_back(text::range_t(from, to, range.columnar));
		}
	}

	isUpdatingSelection = YES;
	[self setSelectionString:[NSString stringWithCxxString:withoutCarry]];
	isUpdatingSelection = NO;
}

- (void)updateSymbol
{
	NSString* newSymbol = to_ns(documentView->symbol());
	if(![_symbol isEqualToString:newSymbol])
		self.symbol = newSymbol;
}

- (folding_state_t)foldingStateForLine:(NSUInteger)lineNumber
{
	if(documentView)
	{
		if(documentView->is_line_folded(lineNumber))
			return kFoldingCollapsed;
		else if(documentView->is_line_fold_start_marker(lineNumber))
			return kFoldingTop;
		else if(documentView->is_line_fold_stop_marker(lineNumber))
			return kFoldingBottom;
	}
	return kFoldingNone;
}

- (GVLineRecord)lineRecordForPosition:(CGFloat)yPos
{
	if(!documentView)
		return GVLineRecord();
	auto record = documentView->line_record_for(yPos);
	return GVLineRecord(record.line, record.softline, record.top, record.bottom, record.baseline);
}

- (GVLineRecord)lineFragmentForLine:(NSUInteger)aLine column:(NSUInteger)aColumn
{
	if(!documentView)
		return GVLineRecord();
	auto record = documentView->line_record_for(text::pos_t(aLine, aColumn));
	return GVLineRecord(record.line, record.softline, record.top, record.bottom, record.baseline);
}

- (NSRect)rectForLine:(NSUInteger)lineNumber byteColumnRange:(NSRange)byteColumnRange
{
	if(!documentView || lineNumber >= documentView->lines())
		return NSZeroRect;

	size_t const first = documentView->convert(text::pos_t(lineNumber, byteColumnRange.location));
	size_t const last = documentView->convert(text::pos_t(lineNumber, NSMaxRange(byteColumnRange)));
	return documentView->rect_for_range(first, last);
}

- (NSRect)caretRectForLine:(NSUInteger)lineNumber byteColumn:(NSUInteger)byteColumn
{
	if(!documentView || lineNumber >= documentView->lines())
		return NSZeroRect;

	size_t const index = documentView->convert(text::pos_t(lineNumber, byteColumn));
	return documentView->rect_at_index(index);
}

- (NSColor*)backgroundColorForLine:(NSUInteger)lineNumber
{
	if(!documentView || !self.theme || lineNumber == NSNotFound || lineNumber >= documentView->lines())
		return nil;

	CGColorRef editorBackground = self.theme->background(documentView->file_type());
	CGColorRef lineBackground   = self.theme->styles_for_scope(documentView->scope(documentView->begin(lineNumber)).right).background();
	return lineBackground && !CFEqual(editorBackground, lineBackground) ? [NSColor colorWithCGColor:lineBackground] : nil;
}

- (BOOL)filterDocumentThroughCommand:(NSString*)commandString input:(input::type)inputUnit output:(output::type)outputUnit
{
	bundle_command_t command = {
		.name           = "Filter Through Command",
		.uuid           = oak::uuid_t().generate(),
		.command        = "#!/bin/sh\n" + to_s(commandString),
		.input          = inputUnit,
		.input_fallback = input::entire_document,
		.output         = outputUnit,
	};
	[self executeBundleCommand:command variables:{ }];
	return YES;
}

- (NSString*)string
{
	// This is used by the Emmet plug-in (with no “respondsToSelector:” check)
	return [NSString stringWithCxxString:documentView->substr()];
}

// ===================
// = Macro Recording =
// ===================

- (BOOL)isRecordingMacro                    { return macroRecordingArray != nil; }
- (IBAction)toggleMacroRecording:(id)sender { self.recordingMacro = !self.isRecordingMacro; }

- (void)setRecordingMacro:(BOOL)flag
{
	if(self.isRecordingMacro == flag)
		return;

	if(macroRecordingArray)
	{
		[NSUserDefaults.standardUserDefaults setObject:[macroRecordingArray copy] forKey:@"OakMacroManagerScratchMacro"];
		macroRecordingArray = nil;
	}
	else
	{
		macroRecordingArray = [NSMutableArray new];
	}
	OakPlayUISound(flag ? OakSoundDidBeginRecordingUISound : OakSoundDidEndRecordingUISound);
}

- (IBAction)playScratchMacro:(id)anArgument
{
	AUTO_REFRESH;
	if(NSArray* scratchMacro = [NSUserDefaults.standardUserDefaults arrayForKey:@"OakMacroManagerScratchMacro"])
			documentView->macro_dispatch(plist::convert((__bridge CFDictionaryRef)@{ @"commands": scratchMacro }), [self variables]);
	else	NSBeep();
}

- (IBAction)saveScratchMacro:(id)sender
{
	if(NSArray* scratchMacro = [NSUserDefaults.standardUserDefaults arrayForKey:@"OakMacroManagerScratchMacro"])
	{
		bundles::item_ptr bundle;
		if([BundlesManager.sharedInstance findBundleForInstall:&bundle])
		{
			oak::uuid_t uuid = oak::uuid_t().generate();

			plist::dictionary_t plist = plist::convert((__bridge CFDictionaryRef)@{ @"commands": scratchMacro });
			plist[bundles::kFieldUUID] = to_s(uuid);
			plist[bundles::kFieldName] = std::string("untitled");

			auto item = std::make_shared<bundles::item_t>(uuid, bundle, bundles::kItemTypeMacro);
			item->set_plist(plist);
			bundles::add_item(item);

			[NSApp sendAction:@selector(editBundleItemWithUUIDString:) to:nil from:[NSString stringWithCxxString:uuid]];
		}
	}
}

- (void)recordSelector:(SEL)aSelector withArgument:(id)anArgument
{
	if(!macroRecordingArray)
		return;

	[macroRecordingArray addObject:[NSDictionary dictionaryWithObjectsAndKeys:NSStringFromSelector(aSelector), @"command", anArgument, @"argument", nil]];
}

// ================
// = Drop Support =
// ================

+ (NSArray*)dropTypes
{
	return @[ NSPasteboardTypeColor, NSPasteboardTypeFileURL,
		@"WebURLsWithTitlesPboardType", NSPasteboardTypeURL, @"public.url-name",
		NSPasteboardTypeString ];
}

- (void)setDropMarkAtPoint:(NSPoint)aPoint
{
	ASSERT(documentView);
	AUTO_REFRESH;
	dropPosition = NSEqualPoints(aPoint, NSZeroPoint) ? ng::index_t() : documentView->index_at_point(aPoint).index;
	documentView->set_drop_marker(dropPosition);
}

- (void)dropFiles:(NSArray*)someFiles
{
	std::set<bundles::item_ptr> allHandlers;
	std::map<oak::uuid_t, std::vector<std::string> > handlerToFiles;

	scope::context_t scope = [self scopeContext];
	for(NSString* path in someFiles)
	{
		for(auto const& item : bundles::drag_commands_for_path(to_s(path), scope))
		{
			handlerToFiles[item->uuid()].push_back(to_s(path));
			allHandlers.insert(item);
		}
	}

	if(allHandlers.empty())
	{
		bool binary = false;
		std::string merged = "";
		for(NSString* path in someFiles)
		{
			std::string const& content = path::content(to_s(path));
			if(!utf8::is_valid(content.begin(), content.end()))
			{
				binary = true;
			}
			else
			{
				NSAlert* alert        = [[NSAlert alloc] init];
				alert.messageText     = @"Inserting Large File";
				alert.informativeText = [NSString stringWithFormat: @"The file “%@” has a size of %.1f MB. Are you sure you want to insert this as a text file?", [path stringByAbbreviatingWithTildeInPath], content.size() / SQ(1024.0)];
				[alert addButtons:@"Insert File", @"Cancel", nil];

				if(content.size() < SQ(1024) || [alert runModal] == NSAlertFirstButtonReturn) // larger than 1 MB?
					merged += content;
			}
		}

		if(binary)
		{
			std::vector<std::string> paths;
			for(NSString* path in someFiles)
				paths.push_back(to_s(path));
			merged = text::join(paths, "\n");
		}

		AUTO_REFRESH;
		documentView->insert(merged, true);
	}
	else if(bundles::item_ptr handler = [self showMenuForBundleItems:std::vector<bundles::item_ptr>(allHandlers.begin(), allHandlers.end())])
	{
		static struct { NSUInteger mask; std::string name; } const qualNames[] =
		{
			{ NSEventModifierFlagShift,     "SHIFT"    },
			{ NSEventModifierFlagControl,   "CONTROL"  },
			{ NSEventModifierFlagOption,    "OPTION"   },
			{ NSEventModifierFlagCommand,   "COMMAND"  }
		};

		auto env = handler->bundle_variables();
		auto const pwd = documentView->path() != NULL_STR ? path::parent(documentView->path()) : documentView->directory();

		std::vector<std::string> files, paths = handlerToFiles[handler->uuid()];
		std::transform(paths.begin(), paths.end(), back_inserter(files), [&pwd](std::string const& path){ return path::relative_to(path, pwd); });

		env["TM_DROPPED_FILE"]     = files.front();
		env["TM_DROPPED_FILEPATH"] = paths.front();

		if(files.size() > 1)
		{
			env["TM_DROPPED_FILES"]     = shell_quote(files);
			env["TM_DROPPED_FILEPATHS"] = shell_quote(paths);
		}

		NSUInteger state = [NSEvent modifierFlags];
		std::vector<std::string> flagNames;
		for(auto const& qualifier : qualNames)
		{
			if(state & qualifier.mask)
				flagNames.push_back(qualifier.name);
		}
		env["TM_MODIFIER_FLAGS"] = text::join(flagNames, "|");

		[self executeBundleCommand:parse_drag_command(handler) variables:env];
	}
}

// ===============
// = Drag Source =
// ===============

- (NSDragOperation)draggingSession:(NSDraggingSession*)session sourceOperationMaskForDraggingContext:(NSDraggingContext)context
{
	return context == NSDraggingContextWithinApplication ? (NSDragOperationCopy|NSDragOperationMove) : (NSDragOperationCopy|NSDragOperationGeneric);
}

// ====================
// = Drag Destination =
// ====================

- (BOOL)isPointInSelection:(NSPoint)aPoint
{
	BOOL res = NO;
	for(auto const& rect : documentView->rects_for_ranges(documentView->ranges(), kRectsIncludeSelections))
		res = res || CGRectContainsPoint(rect, aPoint);
	return res;
}

- (NSDragOperation)dragOperationForInfo:(id <NSDraggingInfo>)info
{
	if(macroRecordingArray || [self isHiddenOrHasHiddenAncestor])
		return NSDragOperationNone;

	NSDragOperation mask = [info draggingSourceOperationMask];

	NSDragOperation res;
	if([info draggingSource] == self)
	{
		BOOL hoveringSelection = [self isPointInSelection:[self convertPoint:[info draggingLocation] fromView:nil]];
		res = hoveringSelection ? NSDragOperationNone : ((mask & NSDragOperationMove) ?: (mask & NSDragOperationCopy));
	}
	else if([[info draggingPasteboard] canReadObjectForClasses:@[ [NSURL class] ] options:@{ NSPasteboardURLReadingFileURLsOnlyKey: @YES }])
	{
		res = (mask & NSDragOperationCopy) ?: (mask & NSDragOperationLink);
	}
	else
	{
		res = (mask & NSDragOperationCopy);
	}
	return res;
}

- (NSDragOperation)draggingEntered:(id <NSDraggingInfo>)info
{
	NSDragOperation flag = [self dragOperationForInfo:info];
	[self setDropMarkAtPoint:flag == NSDragOperationNone ? NSZeroPoint : [self convertPoint:[info draggingLocation] fromView:nil]];
	return flag;
}

- (NSDragOperation)draggingUpdated:(id <NSDraggingInfo>)info
{
	NSDragOperation flag = [self dragOperationForInfo:info];
	[self setDropMarkAtPoint:flag == NSDragOperationNone ? NSZeroPoint : [self convertPoint:[info draggingLocation] fromView:nil]];
	return flag;
}

- (void)draggingExited:(id <NSDraggingInfo>)info
{
	[self setDropMarkAtPoint:NSZeroPoint];
}

- (BOOL)performDragOperation:(id <NSDraggingInfo>)info
{
	ASSERT(dropPosition);

	BOOL res = YES;
	NSPasteboard* pboard  = [info draggingPasteboard];
	NSArray* types        = [pboard types];
	NSString* type DB_VAR = [pboard availableTypeFromArray:[[self class] dropTypes]];
	BOOL shouldMove       = ([info draggingSource] == self) && ([info draggingSourceOperationMask] & NSDragOperationMove);
	BOOL shouldLink       = ([info draggingSource] != self) && ([info draggingSourceOperationMask] == NSDragOperationLink);

	crash_reporter_info_t crashInfo("local %s, should move %s, type %s, all types %s", BSTR([info draggingSource] == self), BSTR(shouldMove), [type UTF8String], [[types description] UTF8String]);

	AUTO_REFRESH;
	ng::index_t pos = dropPosition;
	documentView->set_drop_marker(dropPosition = ng::index_t());

	NSArray<NSURL*>* fileURLs = [pboard readObjectsForClasses:@[ [NSURL class] ] options:@{ NSPasteboardURLReadingFileURLsOnlyKey: @YES }];
	NSMutableArray<NSString*>* files = fileURLs ? [NSMutableArray arrayWithCapacity:fileURLs.count] : nil;
	for(NSURL* url in fileURLs)
		[files addObject:url.path];
	if(shouldLink && files)
	{
		std::vector<std::string> paths;
		for(NSString* path in files)
			paths.push_back(to_s(path));

		documentView->set_ranges(ng::range_t(pos));
		documentView->insert(text::join(paths, "\n"));
	}
	else if(NSString* text = [pboard stringForType:[pboard availableTypeFromArray:@[ NSPasteboardTypeString ]]])
	{
		if(shouldMove && documentView->has_selection())
		{
			crashInfo << text::format("buffer size: %zu, move selection (%s) to %s", documentView->size(), to_s(documentView->ranges()).c_str(), to_s(pos).c_str());
			documentView->move_selection_to(pos);
			crashInfo << text::format("new selection %s", to_s(documentView->ranges()).c_str());
		}
		else
		{
			std::string str = to_s(text);
			str.erase(text::convert_line_endings(str.begin(), str.end(), text::estimate_line_endings(str.begin(), str.end())), str.end());
			str.erase(utf8::remove_malformed(str.begin(), str.end()), str.end());

			documentView->set_ranges(ng::range_t(pos));
			documentView->insert(str);
		}
	}
	else if(files)
	{
		documentView->set_ranges(ng::range_t(pos));
		[self performSelector:@selector(dropFiles:) withObject:files afterDelay:0.05]; // we use “afterDelay” so that slow commands won’t trigger a timeout of the drop event
	}
	else
	{
		os_log_error(OS_LOG_DEFAULT, "No known type for drop: %{public}@", [types description]);
		res = NO;
	}
	return res;
}

// ==================
// = Cursor Support =
// ==================

- (void)setIbeamCursor:(NSCursor*)aCursor
{
	if(_ibeamCursor != aCursor)
	{
		_ibeamCursor = aCursor;
		[[self window] invalidateCursorRectsForView:self];
	}
}

- (void)resetCursorRects
{
	[self addCursorRect:[self visibleRect] cursor:_showDragCursor ? [NSCursor arrowCursor] : (_showColumnSelectionCursor ? [NSCursor crosshairCursor] : [self ibeamCursor])];
}

- (void)setShowDragCursor:(BOOL)flag
{
	if(flag != _showDragCursor)
	{
		_showDragCursor = flag;
		[[self window] invalidateCursorRectsForView:self];
	}
}

// =================
// = User Defaults =
// =================

- (void)userDefaultsDidChange:(id)sender
{
	self.antiAlias     = ![NSUserDefaults.standardUserDefaults boolForKey:kUserDefaultsDisableAntiAliasKey];
	self.fontSmoothing = (OTVFontSmoothing)[NSUserDefaults.standardUserDefaults integerForKey:kUserDefaultsFontSmoothingKey];
	self.scrollPastEnd = [NSUserDefaults.standardUserDefaults boolForKey:kUserDefaultsScrollPastEndKey];
	self.themeUUID     = self.effectiveThemeUUID;
}

- (void)viewDidChangeEffectiveAppearance
{
	self.themeUUID = self.effectiveThemeUUID;
}

// =================
// = Mouse Support =
// =================

- (void)actOnMouseDown
{
	bool optionDown  = mouseDownModifierFlags & NSEventModifierFlagOption;
	bool shiftDown   = mouseDownModifierFlags & NSEventModifierFlagShift;
	bool commandDown = mouseDownModifierFlags & NSEventModifierFlagCommand;

	ng::ranges_t s = documentView->ranges();

	ng::index_t index = documentView->index_at_point(mouseDownPos);
	if(!optionDown)
		index.carry = 0;

	ng::index_t min = s.last().min(), max = s.last().max();
	mouseDownIndex = shiftDown ? (index <= min ? max : (max <= index ? min : s.last().first)) : index;
	ng::ranges_t range(ng::range_t(mouseDownIndex, index));

	switch(mouseDownClickCount)
	{
		case 2: range = ng::extend(*documentView, range, kSelectionExtendToWordOrTypingPair); break;
		case 3: range = ng::extend(*documentView, range, kSelectionExtendToLine); break;
	}

	if(optionDown)
	{
		if(shiftDown)
				range.last().columnar = true;
		else	range.last().freehanded = true;
	}

	if(commandDown)
	{
		if(mouseDownClickCount == 1)
		{
			ng::index_t click = range.last().min();

			bool didModify = false;
			ng::ranges_t newSel;
			for(auto const& cur : s)
			{
				if(cur.min() <= click && click <= cur.max())
					didModify = true;
				else
					newSel.push_back(cur);
			}

			s = newSel;
			if(!didModify || s.empty())
				s.push_back(range.last());
		}
		else
		{
			ng::ranges_t newSel;
			for(auto const& cur : s)
			{
				bool overlap = range.last().min() <= cur.min() && cur.max() <= range.last().max();
				if(!overlap)
					newSel.push_back(cur);
			}

			s = newSel;
			s.push_back(range.last());
		}
	}
	else if(shiftDown)
		s.last() = range.last();
	else
		s = range.last();

	documentView->set_ranges(s);
}

- (void)actOnMouseDragged:(NSEvent*)anEvent
{
	NSPoint mouseCurrentPos = [self convertPoint:[anEvent locationInWindow] fromView:nil];
	ng::ranges_t range(ng::range_t(mouseDownIndex, documentView->index_at_point(mouseCurrentPos)));
	switch(mouseDownClickCount)
	{
		case 2: range = ng::extend(*documentView, range, kSelectionExtendToWord); break;
		case 3: range = ng::extend(*documentView, range, kSelectionExtendToLine); break;
	}

	NSUInteger currentModifierFlags = [anEvent modifierFlags];
	if(currentModifierFlags & NSEventModifierFlagOption)
		range.last().columnar = true;

	ng::ranges_t s = documentView->ranges();
	s.last() = range.last();
	documentView->set_ranges(s);

	[self autoscroll:anEvent];
}

- (void)startDragForEvent:(NSEvent*)anEvent
{
	ASSERT(documentView);

	NSRect srcRect;
	ng::ranges_t const ranges = ng::dissect_columnar(*documentView, documentView->ranges());
	NSImage* srcImage = [self imageForRanges:ranges imageRect:&srcRect];

	NSImage* image = [[NSImage alloc] initWithSize:srcImage.size];
	[image lockFocus];
	[srcImage drawAtPoint:NSZeroPoint fromRect:NSZeroRect operation:NSCompositingOperationCopy fraction:0.5];
	[image unlockFocus];

	std::vector<std::string> v;
	for(auto const& range : ranges)
		v.push_back(documentView->substr(range.min().index, range.max().index));

	NSDraggingItem* dragItem = [[NSDraggingItem alloc] initWithPasteboardWriter:[NSString stringWithCxxString:text::join(v, "\n")]];
	[dragItem setDraggingFrame:srcRect contents:image];
	[self beginDraggingSessionWithItems:@[ dragItem ] event:anEvent source:self];

	self.showDragCursor = NO;
}

- (BOOL)acceptsFirstMouse:(NSEvent*)anEvent
{
	BOOL res = [self isPointInSelection:[self convertPoint:[anEvent locationInWindow] fromView:nil]];
	return res;
}

- (BOOL)shouldDelayWindowOrderingForEvent:(NSEvent*)anEvent
{
	BOOL res = [self isPointInSelection:[self convertPoint:[anEvent locationInWindow] fromView:nil]];
	return res;
}

- (void)changeToDragPointer:(NSTimer*)aTimer
{
	self.initiateDragTimer = nil;
	delayMouseDown         = NO;
	self.showDragCursor    = YES;
}

- (int)dragDelay
{
	id dragDelayObj = [NSUserDefaults.standardUserDefaults objectForKey:@"NSDragAndDropTextDelay"];
	return [dragDelayObj respondsToSelector:@selector(intValue)] ? [dragDelayObj intValue] : 150;
}

- (void)preparePotentialDrag:(NSEvent*)anEvent
{
	if([self dragDelay] != 0 && ([[self window] isKeyWindow] || ([anEvent modifierFlags] & NSEventModifierFlagCommand)))
			self.initiateDragTimer = [NSTimer scheduledTimerWithTimeInterval:(0.001 * [self dragDelay]) target:self selector:@selector(changeToDragPointer:) userInfo:nil repeats:NO];
	else	[self changeToDragPointer:nil];
	delayMouseDown = [[self window] isKeyWindow];
}

static scope::context_t add_modifiers_to_scope (scope::context_t scope, NSUInteger modifiers)
{
	static struct { NSUInteger modifier; char const* scope; } const map[] =
	{
		{ NSEventModifierFlagShift,      "dyn.modifier.shift"   },
		{ NSEventModifierFlagControl,    "dyn.modifier.control" },
		{ NSEventModifierFlagOption,     "dyn.modifier.option"  },
		{ NSEventModifierFlagCommand,    "dyn.modifier.command" }
	};

	for(auto const& it : map)
	{
		if(modifiers & it.modifier)
		{
			scope.left.push_scope(it.scope);
			scope.right.push_scope(it.scope);
		}
	}

	return scope;
}

- (void)quickLookWithEvent:(NSEvent*)anEvent
{
	ng::index_t index = documentView->index_at_point([self convertPoint:[anEvent locationInWindow] fromView:nil]);
	ng::range_t range = ng::extend(*documentView, index, kSelectionExtendToWord).first();

	if([self isPointInSelection:[self convertPoint:[anEvent locationInWindow] fromView:nil]] && documentView->ranges().size() == 1)
		range = documentView->ranges().first();

	NSRect rect = documentView->rect_at_index(range.min(), false, true);
	NSPoint pos = NSMakePoint(NSMinX(rect), NSMaxY(rect));

	NSAttributedString* str = [self attributedSubstringForProposedRange:[self nsRangeForRange:range] actualRange:nullptr];
	if(str && str.length > 0)
		[self showDefinitionForAttributedString:str atPoint:pos];
}

- (void)pressureChangeWithEvent:(NSEvent*)anEvent
{
	id forceClickFlag = [NSUserDefaults.standardUserDefaults objectForKey:@"com.apple.trackpad.forceClick"];
	if(forceClickFlag && ![forceClickFlag boolValue])
		return;

	static NSInteger oldStage = 0;
	if(oldStage < anEvent.stage && anEvent.stage == 2)
		[self quickLookWithEvent:anEvent];
	oldStage = anEvent.stage;
}

- (void)mouseDown:(NSEvent*)anEvent
{
	if([self.inputContext handleEvent:anEvent] || !documentView || [anEvent type] != NSEventTypeLeftMouseDown || ignoreMouseDown)
		return (void)(ignoreMouseDown = NO);

	NSPoint const clickPos = [self convertPoint:[anEvent locationInWindow] fromView:nil];
	for(auto const& pair : diagnosticPillRects)
	{
		if(NSMouseInRect(clickPos, pair.second, [self isFlipped]))
			return [self showDiagnosticsPopoverForLine:pair.first fromRect:pair.second includeOptionFixes:([anEvent modifierFlags] & NSEventModifierFlagOption) != 0];
	}
	for(auto const& pair : diagnosticBannerRects)
	{
		if(NSMouseInRect(clickPos, pair.second, [self isFlipped]))
			return; // don’t let clicks on the banner text reach the code beneath
	}

	if(ng::range_t r = documentView->folded_range_at_point([self convertPoint:[anEvent locationInWindow] fromView:nil]))
	{
		AUTO_REFRESH;
		documentView->unfold(r.min().index, r.max().index);
		return;
	}

	if(macroRecordingArray && [anEvent type] == NSEventTypeLeftMouseDown)
	{
		NSAlert* alert        = [[NSAlert alloc] init];
		alert.messageText     = @"You are recording a macro";
		alert.informativeText = @"While recording macros it is not possible to select text or reposition the caret using your mouse.";
		[alert addButtons:@"Continue", @"Stop Recording", nil];
		if([alert runModal] == NSAlertSecondButtonReturn) // "Stop Macro Recording"
			self.recordingMacro = NO;
		return;
	}

	std::string callbackName;
	switch(([anEvent clickCount]-1) % 3)
	{
		case 0: callbackName = "callback.single-click"; break;
		case 1: callbackName = "callback.double-click"; break;
		case 2: callbackName = "callback.triple-click"; break;
	}

	std::vector<bundles::item_ptr> const& items = bundles::query(bundles::kFieldSemanticClass, callbackName, add_modifiers_to_scope(ng::scope(*documentView, documentView->index_at_point([self convertPoint:[anEvent locationInWindow] fromView:nil]), to_s([self scopeAttributes])), [anEvent modifierFlags]));
	if(!items.empty())
	{
		if(bundles::item_ptr item = [self showMenuForBundleItems:items])
		{
			AUTO_REFRESH;
			documentView->set_ranges(ng::range_t(documentView->index_at_point([self convertPoint:[anEvent locationInWindow] fromView:nil]).index));
			[self performBundleItem:item];
		}
		return;
	}

	AUTO_REFRESH;
	mouseDownPos           = [self convertPoint:[anEvent locationInWindow] fromView:nil];
	mouseDownClickCount    = [anEvent clickCount];
	mouseDownModifierFlags = [anEvent modifierFlags];

	BOOL hasFocus = (self.keyState & (OakViewViewIsFirstResponderMask|OakViewWindowIsKeyMask|OakViewApplicationIsActiveMask)) == (OakViewViewIsFirstResponderMask|OakViewWindowIsKeyMask|OakViewApplicationIsActiveMask);
	if(!hasFocus)
		mouseDownModifierFlags &= ~NSEventModifierFlagCommand;

	if(!(mouseDownModifierFlags & NSEventModifierFlagShift) && [self isPointInSelection:[self convertPoint:[anEvent locationInWindow] fromView:nil]] && [anEvent clickCount] == 1 && [self dragDelay] >= 0 && !([anEvent modifierFlags] & (NSEventModifierFlagShift | NSEventModifierFlagControl | NSEventModifierFlagOption | NSEventModifierFlagCommand)))
			[self preparePotentialDrag:anEvent];
	else	[self actOnMouseDown];
}

- (void)mouseDragged:(NSEvent*)anEvent
{
	if([self.inputContext handleEvent:anEvent] || !documentView || macroRecordingArray)
		return;

	NSPoint mouseCurrentPos = [self convertPoint:[anEvent locationInWindow] fromView:nil];
	if(hypot(mouseDownPos.x - mouseCurrentPos.x, mouseDownPos.y - mouseCurrentPos.y) < 2.5)
		return;

	delayMouseDown = NO;
	if(_showDragCursor)
	{
		[self startDragForEvent:anEvent];
	}
	else if(_initiateDragTimer) // delayed reaction to mouseDown
	{
		[self.initiateDragTimer invalidate];
		self.initiateDragTimer = nil;

		AUTO_REFRESH;
		[self actOnMouseDown];
	}
	else
	{
		if(!_dragScrollTimer && [self autoscroll:[NSApp currentEvent]] == YES)
			self.dragScrollTimer = [NSTimer scheduledTimerWithTimeInterval:(1.0/25.0) target:self selector:@selector(dragScrollTimerFired:) userInfo:nil repeats:YES];

		AUTO_REFRESH;
		[self actOnMouseDragged:anEvent];
	}
}

- (void)dragScrollTimerFired:(id)sender
{
	if(([NSEvent pressedMouseButtons] & 1) && documentView)
	{
		AUTO_REFRESH;
		[self actOnMouseDragged:[NSApp currentEvent]];
	}
	else
	{
		[_dragScrollTimer invalidate];
		_dragScrollTimer = nil;
	}
}

- (void)mouseUp:(NSEvent*)anEvent
{
	if([self.inputContext handleEvent:anEvent] || !documentView || macroRecordingArray)
		return;

	AUTO_REFRESH;
	if(delayMouseDown)
		[self actOnMouseDown];
	delayMouseDown = NO;

	[self.initiateDragTimer invalidate];
	self.initiateDragTimer = nil;
	[self.dragScrollTimer invalidate];
	self.dragScrollTimer   = nil;
	self.showDragCursor    = NO;
}

// ===================
// = Change in Focus =
// ===================

- (void)setKeyState:(NSUInteger)newState
{
	BOOL didHaveFocus  = (self.keyState & (OakViewViewIsFirstResponderMask|OakViewWindowIsKeyMask|OakViewApplicationIsActiveMask)) == (OakViewViewIsFirstResponderMask|OakViewWindowIsKeyMask|OakViewApplicationIsActiveMask);
	[super setKeyState:newState];
	BOOL doesHaveFocus = (self.keyState & (OakViewViewIsFirstResponderMask|OakViewWindowIsKeyMask|OakViewApplicationIsActiveMask)) == (OakViewViewIsFirstResponderMask|OakViewWindowIsKeyMask|OakViewApplicationIsActiveMask);

	if(didHaveFocus == doesHaveFocus)
		return;

	if(doesHaveFocus)
	{
		[NSFontManager.sharedFontManager setSelectedFont:self.font isMultiple:NO];
		[self setShowLiveSearch:NO];
	}
	else
	{
		self.showColumnSelectionCursor = _showDragCursor = NO;
		[[self window] invalidateCursorRectsForView:self];
	}

	if(documentView)
	{
		AUTO_REFRESH;
		documentView->set_draw_caret(doesHaveFocus);
		documentView->set_draw_as_key(doesHaveFocus);
		hideCaret = !doesHaveFocus;
	}

	self.blinkCaretTimer = doesHaveFocus ? [NSTimer scheduledTimerWithTimeInterval:[NSEvent caretBlinkInterval] target:self selector:@selector(toggleCaretVisibility:) userInfo:nil repeats:YES] : nil;
}

// ===========
// = Actions =
// ===========

- (void)handleAction:(ng::action_t)anAction forSelector:(SEL)aSelector
{
	AUTO_REFRESH;
	[self recordSelector:aSelector withArgument:nil];
	try {
		documentView->perform(anAction, [self indentCorrections], to_s([self scopeAttributes]));

		static std::set<ng::action_t> const SilentActions = { ng::kCopy, ng::kCopySelectionToFindPboard, ng::kCopySelectionToReplacePboard, ng::kCopySelectionToYankPboard, ng::kAppendSelectionToYankPboard, ng::kPrependSelectionToYankPboard, ng::kSetMark, ng::kNop };
		if(SilentActions.find(anAction) == SilentActions.end())
			self.needsEnsureSelectionIsInVisibleArea = YES;
	}
	catch(std::exception const& e) {
		crash_reporter_info_t info("Performing @selector(%s)\nC++ Exception: %s", sel_getName(aSelector), e.what());
		abort();
	}
}

#define ACTION(NAME)      (void)NAME:(id)sender { [self handleAction:ng::to_action(#NAME ":") forSelector:@selector(NAME:)]; }
#define ALIAS(NAME, REAL) (void)NAME:(id)sender { [self handleAction:ng::to_action(#REAL ":") forSelector:sel_registerName(#REAL ":")]; }

// =========================
// = Scroll Action Methods =
// =========================
- (void)scrollLineUp:(id)sender                { [self recordSelector:_cmd withArgument:nil]; [self scrollRectToVisible:NSOffsetRect([self visibleRect], 0, -17)]; } // TODO Query layout for scroll increments
- (void)scrollLineDown:(id)sender              { [self recordSelector:_cmd withArgument:nil]; [self scrollRectToVisible:NSOffsetRect([self visibleRect], 0, +17)]; } // TODO Query layout for scroll increments
- (void)scrollColumnLeft:(id)sender            { [self recordSelector:_cmd withArgument:nil]; [self scrollRectToVisible:NSOffsetRect([self visibleRect],  -7, 0)]; } // TODO Query layout for scroll increments
- (void)scrollColumnRight:(id)sender           { [self recordSelector:_cmd withArgument:nil]; [self scrollRectToVisible:NSOffsetRect([self visibleRect],  +7, 0)]; } // TODO Query layout for scroll increments
- (void)scrollPageUp:(id)sender                { [self recordSelector:_cmd withArgument:nil]; [self scrollRectToVisible:NSOffsetRect([self visibleRect], 0, -NSHeight([self visibleRect]))]; }
- (void)scrollPageDown:(id)sender              { [self recordSelector:_cmd withArgument:nil]; [self scrollRectToVisible:NSOffsetRect([self visibleRect], 0, +NSHeight([self visibleRect]))]; }
- (void)scrollToBeginningOfDocument:(id)sender { [self recordSelector:_cmd withArgument:nil]; [self scrollRectToVisible:(NSRect){ NSZeroPoint, [self visibleRect].size }]; }
- (void)scrollToEndOfDocument:(id)sender       { [self recordSelector:_cmd withArgument:nil]; [self scrollRectToVisible:(NSRect){ { 0, NSMaxY([self bounds]) - NSHeight([self visibleRect]) }, [self visibleRect].size }]; }

// ========
// = Move =
// ========
- ACTION(moveBackward);
- ACTION(moveBackwardAndModifySelection);
- ACTION(moveDown);
- ACTION(moveDownAndModifySelection);
- ACTION(moveForward);
- ACTION(moveForwardAndModifySelection);
- ACTION(moveParagraphBackwardAndModifySelection);
- ACTION(moveParagraphForwardAndModifySelection);
- ACTION(moveSubWordLeft);
- ACTION(moveSubWordLeftAndModifySelection);
- ACTION(moveSubWordRight);
- ACTION(moveSubWordRightAndModifySelection);
- ACTION(moveToBeginningOfColumn);
- ACTION(moveToBeginningOfColumnAndModifySelection);
- ACTION(moveToBeginningOfDocument);
- ACTION(moveToBeginningOfDocumentAndModifySelection);
- ACTION(moveToBeginningOfIndentedLine);
- ACTION(moveToBeginningOfIndentedLineAndModifySelection);
- ACTION(moveToBeginningOfLine);
- ACTION(moveToBeginningOfLineAndModifySelection);
- ACTION(moveToBeginningOfParagraph);
- ACTION(moveToBeginningOfParagraphAndModifySelection);
- ACTION(moveToBeginningOfBlock);
- ACTION(moveToBeginningOfBlockAndModifySelection);
- ACTION(moveToEndOfColumn);
- ACTION(moveToEndOfColumnAndModifySelection);
- ACTION(moveToEndOfDocument);
- ACTION(moveToEndOfDocumentAndModifySelection);
- ACTION(moveToEndOfIndentedLine);
- ACTION(moveToEndOfIndentedLineAndModifySelection);
- ACTION(moveToEndOfLine);
- ACTION(moveToEndOfLineAndModifySelection);
- ACTION(moveToEndOfParagraph);
- ACTION(moveToEndOfParagraphAndModifySelection);
- ACTION(moveToEndOfBlock);
- ACTION(moveToEndOfBlockAndModifySelection);
- ACTION(moveUp);
- ACTION(moveUpAndModifySelection);
- ACTION(moveWordBackward);
- ACTION(moveWordBackwardAndModifySelection);
- ACTION(moveWordForward);
- ACTION(moveWordForwardAndModifySelection);

- ALIAS(moveLeft,                                moveBackward);
- ALIAS(moveRight,                               moveForward);
- ALIAS(moveLeftAndModifySelection,              moveBackwardAndModifySelection);
- ALIAS(moveRightAndModifySelection,             moveForwardAndModifySelection);
- ALIAS(moveWordLeft,                            moveWordBackward);
- ALIAS(moveWordLeftAndModifySelection,          moveWordBackwardAndModifySelection);
- ALIAS(moveWordRight,                           moveWordForward);
- ALIAS(moveWordRightAndModifySelection,         moveWordForwardAndModifySelection);
- ALIAS(moveToLeftEndOfLine,                     moveToBeginningOfLine);
- ALIAS(moveToLeftEndOfLineAndModifySelection,   moveToBeginningOfLineAndModifySelection);
- ALIAS(moveToRightEndOfLine,                    moveToEndOfLine);
- ALIAS(moveToRightEndOfLineAndModifySelection,  moveToEndOfLineAndModifySelection);

- ACTION(pageDown);
- ACTION(pageDownAndModifySelection);
- ACTION(pageUp);
- ACTION(pageUpAndModifySelection);

// ==========
// = Select =
// ==========
- ACTION(toggleColumnSelection);
- ACTION(selectAll);
- ACTION(selectCurrentScope);
- ACTION(selectBlock);
- ACTION(selectHardLine);
- ACTION(selectLine);
- ACTION(selectParagraph);
- ACTION(selectWord);
- ACTION(deselectLast);

// ==========
// = Delete =
// ==========
- ALIAS(delete, deleteSelection);

- ACTION(deleteBackward);
- ACTION(deleteForward);
- ACTION(deleteSubWordLeft);
- ACTION(deleteSubWordRight);
- ACTION(deleteToBeginningOfIndentedLine);
- ACTION(deleteToBeginningOfLine);
- ACTION(deleteToBeginningOfParagraph);
- ACTION(deleteToEndOfIndentedLine);
- ACTION(deleteToEndOfLine);
- ACTION(deleteToEndOfParagraph);
- ACTION(deleteWordBackward);
- ACTION(deleteWordForward);

- ACTION(deleteBackwardByDecomposingPreviousCharacter);

// =============
// = Clipboard =
// =============
- ACTION(cut);
- ACTION(copy);
- ACTION(copySelectionToFindPboard);
- ACTION(copySelectionToReplacePboard);
- ACTION(paste);
- ACTION(pastePrevious);
- ACTION(pasteNext);
- ACTION(pasteWithoutReindent);
- ACTION(yank);

// =============
// = Transform =
// =============
- ACTION(capitalizeWord);
- ACTION(changeCaseOfLetter);
- ACTION(changeCaseOfWord);
- ACTION(lowercaseWord);
- ACTION(reformatText);
- ACTION(reformatTextAndJustify);
- ACTION(shiftLeft);
- ACTION(shiftRight);
- ACTION(transpose);
- ACTION(transposeWords);
- ACTION(unwrapText);
- ACTION(uppercaseWord);

// =========
// = Marks =
// =========
- ACTION(setMark);
- ACTION(deleteToMark);
- ACTION(selectToMark);
- ACTION(swapWithMark);

// ==============
// = Completion =
// ==============
- ACTION(complete);
- ACTION(nextCompletion);
- ACTION(previousCompletion);

// =============
// = Insertion =
// =============
- ACTION(insertBacktab);
- ACTION(insertTabIgnoringFieldEditor);
- ACTION(insertNewline);
- ACTION(insertNewlineIgnoringFieldEditor);

// ===========
// = Complex =
// ===========
- ACTION(indent);

- ACTION(moveSelectionUp);
- ACTION(moveSelectionDown);
- ACTION(moveSelectionLeft);
- ACTION(moveSelectionRight);

// ===============
// = Run Command =
// ===============

- (OakCommandRefresher*)existingRefresherForCommand:(bundle_command_t const&)aBundleCommand
{
	OakDocument* doc = aBundleCommand.auto_refresh & (auto_refresh::on_document_change|auto_refresh::on_document_close) ? _document : nil;
	return [OakCommandRefresher findRefresherForCommandUUID:[[NSUUID alloc] initWithUUIDString:to_ns(aBundleCommand.uuid)] document:doc window:self.window];
}

- (void)executeBundleCommand:(bundle_command_t const&)aBundleCommand variables:(std::map<std::string, std::string> const&)initialVariables
{
	[self executeBundleCommand:aBundleCommand buffer:*documentView selection:documentView->ranges() variables:initialVariables];
}

- (void)executeBundleCommand:(bundle_command_t)aBundleCommand buffer:(ng::buffer_api_t const&)buffer selection:(ng::ranges_t const&)selection variables:(std::map<std::string, std::string> const&)initialVariables
{
	if(OakCommandRefresher* refresher = [self existingRefresherForCommand:aBundleCommand])
	{
		if(refresher.command.htmlOutputView)
				[refresher bringHTMLOutputToFront:self];
		else	[refresher teardown];
		return;
	}

	std::map<std::string, std::string> variables = initialVariables;

	int stdinRead, stdinWrite;
	std::tie(stdinRead, stdinWrite) = io::create_pipe();

	bool inputWasSelection = false;
	ng::ranges_t const inputRanges = ng::write_unit_to_fd(buffer, selection, buffer.indent().tab_size(), stdinWrite, aBundleCommand.input, aBundleCommand.input_fallback, aBundleCommand.input_format, aBundleCommand.scope_selector, variables, &inputWasSelection);

	OakCommand* command = [[OakCommand alloc] initWithBundleCommand:aBundleCommand];

	auto variablesByValue = initialVariables;

	command.modalEventLoopRunner = ^(OakCommand* command, BOOL* didTerminate){
		NSMutableArray* queuedEvents = [NSMutableArray array];
		while(*didTerminate == NO)
		{
			// We use CFRunLoopRunInMode() to handle dispatch queues and nextEventMatchingMask:… to catcn ⌃C
			CFRunLoopRunInMode(kCFRunLoopDefaultMode, 5, true);
			if(NSEvent* event = [NSApp nextEventMatchingMask:NSEventMaskAny untilDate:nil inMode:NSDefaultRunLoopMode dequeue:YES])
			{
				static NSEventType const events[] = { NSEventTypeLeftMouseDown, NSEventTypeLeftMouseUp, NSEventTypeRightMouseDown, NSEventTypeRightMouseUp, NSEventTypeOtherMouseDown, NSEventTypeOtherMouseUp, NSEventTypeLeftMouseDragged, NSEventTypeRightMouseDragged, NSEventTypeOtherMouseDragged, NSEventTypeKeyDown, NSEventTypeKeyUp, NSEventTypeFlagsChanged };
				if(!oak::contains(std::begin(events), std::end(events), [event type]))
				{
					[NSApp sendEvent:event];
				}
				else if([event type] == NSEventTypeKeyDown && (([[event charactersIgnoringModifiers] isEqualToString:@"c"] && ([event modifierFlags] & (NSEventModifierFlagShift|NSEventModifierFlagControl|NSEventModifierFlagOption|NSEventModifierFlagCommand)) == NSEventModifierFlagControl) || ([[event charactersIgnoringModifiers] isEqualToString:@"."] && ([event modifierFlags] & (NSEventModifierFlagShift|NSEventModifierFlagControl|NSEventModifierFlagOption|NSEventModifierFlagCommand)) == NSEventModifierFlagCommand)))
				{
					NSAlert* alert        = [[NSAlert alloc] init];
					alert.messageText     = [NSString stringWithFormat:@"Stop “%@”", to_ns(aBundleCommand.name)];
					alert.informativeText = @"Would you like to kill the current shell command?";
					[alert addButtons:@"Kill Command", @"Cancel", nil];
					if([alert runModal]== NSAlertFirstButtonReturn) // "Kill Command"
						[command terminate];
				}
				else
				{
					[queuedEvents addObject:event];
				}
			}
		}

		for(NSEvent* event in queuedEvents)
			[NSApp postEvent:event atStart:NO];
	};

	command.terminationHandler = ^(OakCommand* command, BOOL normalExit){
		if(normalExit && aBundleCommand.auto_refresh != auto_refresh::never)
		{
			OakCommandRefresherOptions options = 0;
			if(aBundleCommand.auto_refresh & auto_refresh::on_document_change)
				options |= OakCommandRefresherDocumentDidChange;
			if(aBundleCommand.auto_refresh & auto_refresh::on_document_save)
				options |= OakCommandRefresherDocumentDidSave;
			if(aBundleCommand.auto_refresh & auto_refresh::on_document_close)
				options |= OakCommandRefresherDocumentDidClose;
			if(aBundleCommand.input == input::entire_document)
				options |= OakCommandRefresherDocumentAsInput;
			[OakCommandRefresher scheduleRefreshForCommand:command document:_document window:self.window options:options variables:variables];
		}
	};

	command.firstResponder = self;
	[command executeWithInput:[[NSFileHandle alloc] initWithFileDescriptor:stdinRead closeOnDealloc:YES] variables:variables outputHandler:^(std::string const& out, output::type placement, output_format::type format, output_caret::type outputCaret, std::map<std::string, std::string> const& environment){
		if(outputCaret == output_caret::heuristic)
		{
			if(aBundleCommand.input == input::selection && inputWasSelection)
				outputCaret = output_caret::select_output;
			else if(aBundleCommand.input == input::selection && (aBundleCommand.input_fallback == input::line || aBundleCommand.input_fallback == input::word || aBundleCommand.input_fallback == input::scope))
				outputCaret = output_caret::interpolate_by_char;
			else if(aBundleCommand.input == input::selection && (aBundleCommand.input_fallback == input::entire_document))
				outputCaret = output_caret::interpolate_by_line;
			else
				outputCaret = output_caret::after_output;
		}

		if(!inputRanges)
		{
			switch(placement)
			{
				case output::after_input:   placement = output::at_caret;          break;
				case output::replace_input: placement = output::replace_selection; break;
			}
		}

		AUTO_REFRESH;
		documentView->handle_result(out, placement, format, outputCaret, inputRanges, environment);
	}];
}

- (void)updateEnvironment:(std::map<std::string, std::string>&)res
{
	if(!documentView || !self.theme)
		return;

	res << documentView->variables(to_s([self scopeAttributes]));
	if(auto themeItem = bundles::lookup(self.theme->uuid()))
	{
		if(!themeItem->paths().empty())
			res["TM_CURRENT_THEME_PATH"] = themeItem->paths().back();
	}

	res = bundles::scope_variables(res, [self scopeContext]);
	res = variables_for_path(res, documentView->logical_path(), [self scopeContext].right, path::parent(documentView->path()));
}

- (void)showToolTip:(NSString*)aToolTip
{
	OakShowToolTip(aToolTip, [self positionForWindowUnderCaret]);
}

- (BOOL)presentError:(NSError*)anError
{
	[self.window presentError:anError modalForWindow:self.window delegate:nil didPresentSelector:NULL contextInfo:nullptr];
	return NO;
}
@end
