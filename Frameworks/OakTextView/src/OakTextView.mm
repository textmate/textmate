#import "OakTextView.h"
#import "OakPasteboardWrapper.h"
#import "OakChoiceMenu.h"
#import "OakDocumentView.h" // addAuxiliaryView:atEdge: signature
#import "LiveSearchView.h"
#import "OTVHUD.h"
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/NSEvent Additions.h>
#import <OakAppKit/NSColor Additions.h>
#import <OakAppKit/NSImage Additions.h>
#import <OakAppKit/NSMenuItem Additions.h>
#import <OakAppKit/OakPasteboard.h>
#import <OakAppKit/OakPopOutAnimation.h>
#import <OakAppKit/OakToolTip.h>
#import <OakFoundation/NSString Additions.h>
#import <OakFoundation/OakFoundation.h>
#import <OakFoundation/OakFindProtocol.h>
#import <OakFoundation/OakTimer.h>
#import <OakSystem/application.h>
#import <crash/info.h>
#import <buffer/indexed_map.h>
#import <BundleMenu/BundleMenu.h>
#import <BundlesManager/BundlesManager.h>
#import <Preferences/Keys.h>
#import <bundles/bundles.h>
#import <cf/cf.h>
#import <command/runner.h>
#import <document/collection.h>
#import <file/type.h>
#import <layout/layout.h>
#import <ns/ns.h>
#import <ns/spellcheck.h>
#import <text/classification.h>
#import <text/format.h>
#import <text/newlines.h>
#import <text/trim.h>
#import <text/utf16.h>
#import <text/utf8.h>
#import <oak/debug.h>
#import <editor/write.h>
#import <io/exec.h>

OAK_DEBUG_VAR(OakTextView_TextInput);
OAK_DEBUG_VAR(OakTextView_Accessibility);
OAK_DEBUG_VAR(OakTextView_Spelling);
OAK_DEBUG_VAR(OakTextView_ViewRect);
OAK_DEBUG_VAR(OakTextView_NSView);
OAK_DEBUG_VAR(OakTextView_DragNDrop);
OAK_DEBUG_VAR(OakTextView_MouseEvents);
OAK_DEBUG_VAR(OakTextView_Macros);

int32_t const NSWrapColumnWindowWidth =  0;
int32_t const NSWrapColumnAskUser     = -1;

NSString* const kUserDefaultsWrapColumnPresetsKey  = @"wrapColumnPresets";
NSString* const kUserDefaultsFontSmoothingKey      = @"fontSmoothing";
NSString* const kUserDefaultsDisableTypingPairsKey = @"disableTypingPairs";
NSString* const kUserDefaultsScrollPastEndKey      = @"scrollPastEnd";

struct buffer_refresh_callback_t;

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
		NSRect frame = self.frame;
		frame = [self.textView convertRect:frame toView:nil];
		frame = [self.textView.window convertRectToScreen:frame];
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
	document_view_t (document::document_ptr const& document, theme_ptr const& theme, bool softWrap, bool wrapColumn, bool scrollPastEnd) : document(document)
	{
		_editor = ng::editor_for_document(document);
		_layout = std::make_shared<ng::layout_t>(document->buffer(), theme, softWrap, scrollPastEnd, wrapColumn, document->folded());
	}

	// ==========
	// = Buffer =
	// ==========

	iterator begin () const { return document->buffer().begin(); }
	iterator end () const { return document->buffer().end(); }

	size_t size () const { return document->buffer().size(); }
	size_t revision () const { return document->buffer().revision(); }
	std::string operator[] (size_t i) const { return document->buffer()[i]; }
	std::string substr (size_t from, size_t to) const { return document->buffer().substr(from, to); }
	std::string xml_substr (size_t from = 0, size_t to = SIZE_T_MAX) const { return document->buffer().xml_substr(from, to); }
	size_t begin (size_t n) const { return document->buffer().begin(n); }
	size_t eol (size_t n) const { return document->buffer().eol(n); }
	size_t end (size_t n) const { return document->buffer().end(n); }
	size_t lines () const { return document->buffer().lines(); }
	size_t sanitize_index (size_t i) const { return document->buffer().sanitize_index(i); }
	size_t convert (text::pos_t const& p) const { return document->buffer().convert(p); }
	text::pos_t convert (size_t i) const { return document->buffer().convert(i); }
	text::indent_t const& indent () const { return document->buffer().indent(); }
	void set_indent (text::indent_t const& indent) { document->set_indent(indent); }
	scope::context_t scope (size_t i, bool includeDynamic = true) const { return document->buffer().scope(i, includeDynamic); }
	std::map<size_t, scope::scope_t> scopes (size_t from, size_t to) const { return document->buffer().scopes(from, to); }
	void set_live_spelling (bool flag) { document->buffer().set_live_spelling(flag); }
	bool live_spelling () const { return document->buffer().live_spelling(); }
	void set_spelling_language (std::string const& lang) { document->buffer().set_spelling_language(lang); }
	std::string const& spelling_language () const { return document->buffer().spelling_language(); }
	std::map<size_t, bool> misspellings (size_t from, size_t to) const { return document->buffer().misspellings(from, to); }
	std::pair<size_t, size_t> next_misspelling (size_t from) const { return document->buffer().next_misspelling(from); }
	ns::spelling_tag_t spelling_tag () const { return document->buffer().spelling_tag(); }
	void recheck_spelling (size_t from, size_t to) { document->buffer().recheck_spelling(from, to); }
	void add_callback (ng::callback_t* callback) { document->buffer().add_callback(callback); }
	void remove_callback (ng::callback_t* callback) { document->buffer().remove_callback(callback); }

	// ================
	// = Undo Manager =
	// ================

	bool can_undo () const { return document->undo_manager().can_undo(); }
	bool can_redo () const { return document->undo_manager().can_redo(); }
	void begin_undo_group (ng::ranges_t const& ranges) { document->undo_manager().begin_undo_group(ranges); }
	void end_undo_group (ng::ranges_t const& ranges, bool force = false) { document->undo_manager().end_undo_group(ranges, force); }
	ng::ranges_t undo () { return document->undo_manager().undo(); }
	ng::ranges_t redo () { return document->undo_manager().redo(); }

	// ==========
	// = Editor =
	// ==========

	ng::editor_delegate_t* delegate () const { return _editor->delegate(); }
	void set_delegate (ng::editor_delegate_t* delegate) { _editor->set_delegate(delegate); }
	void perform (ng::action_t action, ng::indent_correction_t indentCorrections = ng::kIndentCorrectAlways, std::string const& scopeAttributes = NULL_STR) { _editor->perform(action, _layout.get(), indentCorrections, scopeAttributes); }
	bool disallow_tab_expansion () const { return _editor->disallow_tab_expansion(); }
	void insert (std::string const& str, bool selectInsertion = false) { _editor->insert(str, selectInsertion); }
	void insert_with_pairing (std::string const& str, ng::indent_correction_t indentCorrections, bool autoPairing, std::string const& scopeAttributes = NULL_STR) { _editor->insert_with_pairing(str, indentCorrections, autoPairing, scopeAttributes); }
	void move_selection_to (ng::index_t const& index, bool selectInsertion = true) { _editor->move_selection_to(index, selectInsertion); }
	ng::ranges_t replace_all (std::string const& searchFor, std::string const& replaceWith, find::options_t options = find::none, bool searchOnlySelection = false) { return _editor->replace_all(searchFor, replaceWith, options, searchOnlySelection); }
	void delete_tab_trigger (std::string const& str) { _editor->delete_tab_trigger(str); }
	void macro_dispatch (plist::dictionary_t const& args, std::map<std::string, std::string> const& variables) { _editor->macro_dispatch(args, variables); }
	void snippet_dispatch (plist::dictionary_t const& args, std::map<std::string, std::string> const& variables) { _editor->snippet_dispatch(args, variables); }
	scope::context_t scope (std::string const& scopeAttributes) const { return _editor->scope(scopeAttributes); }
	std::map<std::string, std::string> editor_variables (std::string const& scopeAttributes) const { return _editor->editor_variables(scopeAttributes); }
	std::vector<std::string> const& choices () const { return _editor->choices(); }
	std::string placeholder_content (ng::range_t* placeholderSelection = NULL) const { return _editor->placeholder_content(placeholderSelection); }
	void set_placeholder_content (std::string const& str, size_t selectFrom) { _editor->set_placeholder_content(str, selectFrom); }
	ng::ranges_t ranges () const { return _editor->ranges(); }
	void set_selections (ng::ranges_t const& r) { _editor->set_selections(r); }
	bool has_selection () const { return _editor->has_selection(); }
	std::string as_string (size_t from = 0, size_t to = SIZE_T_MAX) const { return _editor->as_string(from, to); }
	bool handle_result (std::string const& out, output::type placement, output_format::type format, output_caret::type outputCaret, ng::ranges_t const& inputRanges, std::map<std::string, std::string> environment) { return _editor->handle_result(out, placement, format, outputCaret, inputRanges, environment); }
	void clear_snippets () { _editor->clear_snippets(); }
	void set_clipboard (clipboard_ptr cb) { _editor->set_clipboard(cb); }
	void set_find_clipboard (clipboard_ptr cb) { _editor->set_find_clipboard(cb); }
	void set_replace_clipboard (clipboard_ptr cb) { _editor->set_replace_clipboard(cb); }

	// ==========
	// = Layout =
	// ==========

	void set_theme (theme_ptr const& theme) { _layout->set_theme(theme); }
	void set_font (std::string const& fontName, CGFloat fontSize) { _layout->set_font(fontName, fontSize); }
	void set_wrapping (bool softWrap, size_t wrapColumn) { _layout->set_wrapping(softWrap, wrapColumn); }
	void set_scroll_past_end (bool scrollPastEnd) { _layout->set_scroll_past_end(scrollPastEnd); }
	ng::layout_t::margin_t const& margin () const { return _layout->margin(); }
	bool wrapping () const { return _layout->wrapping(); }
	void set_is_key (bool isKey) { _layout->set_is_key(isKey); }
	void set_draw_caret (bool drawCaret) { _layout->set_draw_caret(drawCaret); }
	void set_draw_wrap_column (bool drawWrapColumn) { _layout->set_draw_wrap_column(drawWrapColumn); }
	void set_draw_indent_guides (bool drawIndentGuides) { _layout->set_draw_indent_guides(drawIndentGuides); }
	void set_drop_marker (ng::index_t dropMarkerIndex) { _layout->set_drop_marker(dropMarkerIndex); }
	void set_viewport_size (CGSize size) { _layout->set_viewport_size(size); }
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
	ng::index_t index_below (ng::index_t const& index) const { return _layout->index_below(index); }
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

	// ==========

	document::document_ptr document;
private:
	ng::editor_ptr _editor;
	std::shared_ptr<ng::layout_t> _layout;
};

@interface OakTextView () <NSTextInputClient, NSDraggingSource, NSIgnoreMisspelledWords, NSChangeSpelling, NSTextFieldDelegate>
{
	OBJC_WATCH_LEAKS(OakTextView);

	document::document_ptr document;
	theme_ptr theme;
	std::string fontName;
	CGFloat fontSize;
	std::shared_ptr<document_view_t> documentView;
	NSUInteger refreshNestCount;
	buffer_refresh_callback_t* callback;

	int32_t wrapColumn;
	std::string invisiblesMap;

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

	OakTimer* initiateDragTimer;
	OakTimer* dragScrollTimer;
	BOOL showDragCursor;
	BOOL showColumnSelectionCursor;
	BOOL ignoreMouseDown;  // set when the mouse down is the same event which caused becomeFirstResponder:
	BOOL delayMouseDown; // set when mouseUp: should process lastMouseDownEvent

	// ===============
	// = Drag’n’drop =
	// ===============

	ng::index_t dropPosition;
	ng::ranges_t markedRanges;
	ng::ranges_t pendingMarkedRanges;

	NSString* selectionString;
	BOOL isUpdatingSelection;

	NSMutableArray* macroRecordingArray;

	// ======================
	// = Incremental Search =
	// ======================

	NSString* liveSearchString;
	ng::ranges_t liveSearchAnchor;
	ng::ranges_t liveSearchRanges;

	// ===================
	// = Snippet Choices =
	// ===================

	OakChoiceMenu* choiceMenu;
	std::vector<std::string> choiceVector;

	// =================
	// = Accessibility =
	// =================

	links_ptr _links;
}
- (void)deselectLast:(id)sender;
- (void)ensureSelectionIsInVisibleArea:(id)sender;
- (void)updateChoiceMenu:(id)sender;
- (void)resetBlinkCaretTimer;
- (void)updateSelection;
- (void)updateMarkedRanges;
- (void)redisplayFrom:(size_t)from to:(size_t)to;
- (NSImage*)imageForRanges:(ng::ranges_t const&)ranges imageRect:(NSRect*)outRect;
@property (nonatomic, readonly) ng::ranges_t const& markedRanges;
@property (nonatomic) NSDate* lastFlagsChangeDate;
@property (nonatomic) NSUInteger lastFlags;
@property (nonatomic) OakFlagsState flagsState;
@property (nonatomic) OakTimer* initiateDragTimer;
@property (nonatomic) OakTimer* dragScrollTimer;
@property (nonatomic) BOOL showDragCursor;
@property (nonatomic) BOOL showColumnSelectionCursor;
@property (nonatomic) OakChoiceMenu* choiceMenu;
@property (nonatomic) NSUInteger refreshNestCount;
@property (nonatomic) LiveSearchView* liveSearchView;
@property (nonatomic, copy) NSString* liveSearchString;
@property (nonatomic) ng::ranges_t const& liveSearchRanges;
@property (nonatomic, readonly) links_ptr links;
@property (nonatomic) NSDictionary* matchCaptures; // Captures from last regexp match
@property (nonatomic) BOOL needsEnsureSelectionIsInVisibleArea;
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

struct refresh_helper_t
{
	refresh_helper_t (OakTextView* self, document::document_ptr const& document, std::shared_ptr<document_view_t> const& documentView) : _self(self), _document(document), _document_view(documentView)
	{
		if(++_self.refreshNestCount == 1)
		{
			_document->sync_open();

			_revision  = documentView->revision();
			_selection = documentView->ranges();
			documentView->begin_undo_group(_selection);
			documentView->begin_refresh_cycle(merge(_selection, [_self markedRanges]), [_self liveSearchRanges]);
		}
	}

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

	~refresh_helper_t ()
	{
		if(--_self.refreshNestCount == 0)
		{
			if(auto documentView = _document_view.lock())
			{
				documentView->end_undo_group(documentView->ranges());
				if(_revision == documentView->revision())
				{
					for(auto const& range : ng::highlight_ranges_for_movement(*documentView, _selection, documentView->ranges()))
					{
						NSRect imageRect;
						NSImage* image = [_self imageForRanges:range imageRect:&imageRect];
						imageRect = [[_self window] convertRectToScreen:[_self convertRect:imageRect toView:nil]];
						OakShowPopOutAnimation(imageRect, image);
					}
				}

				if(_revision != documentView->revision() || _selection != documentView->ranges())
				{
					[_self updateMarkedRanges];
					[_self updateSelection];
				}

				auto damagedRects = documentView->end_refresh_cycle(merge(documentView->ranges(), [_self markedRanges]), [_self visibleRect], [_self liveSearchRanges]);

				NSRect r = [[_self enclosingScrollView] documentVisibleRect];
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
					if(_revision != documentView->revision()) // FIXME document_t needs to skip work in set_revision if nothing changed.
						_document->set_revision(documentView->revision());

					[_self ensureSelectionIsInVisibleArea:nil];
					[_self resetBlinkCaretTimer];
					[_self updateChoiceMenu:nil];
				}
			}

			_document->close();
		}
	}

private:
	OakTextView* _self;
	document::document_ptr _document;
	std::weak_ptr<document_view_t> _document_view;
	size_t _revision;
	ng::ranges_t _selection;
};

#define AUTO_REFRESH refresh_helper_t _dummy(self, document, documentView)

struct buffer_refresh_callback_t : ng::callback_t
{
	buffer_refresh_callback_t (OakTextView* textView) : textView(textView) { }
	void did_parse (size_t from, size_t to);
	void did_replace (size_t from, size_t to, std::string const& str);
private:
	__weak OakTextView* textView;
};

void buffer_refresh_callback_t::did_parse (size_t from, size_t to)
{
	[textView redisplayFrom:from to:to];
}

void buffer_refresh_callback_t::did_replace (size_t, size_t, std::string const&)
{
	NSAccessibilityPostNotification(textView, NSAccessibilityValueChangedNotification);
}

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

- (NSString*)findString      { return [[OakPasteboard pasteboardWithName:NSFindPboard] current].string;    }
- (NSString*)replaceString   { return [[OakPasteboard pasteboardWithName:OakReplacePboard] current].string; }

- (void)showToolTip:(NSString*)aToolTip
{
	OakShowToolTip(aToolTip, [self.textView positionForWindowUnderCaret]);
	NSAccessibilityPostNotificationWithUserInfo(self.textView, NSAccessibilityAnnouncementRequestedNotification, @{ NSAccessibilityAnnouncementKey : aToolTip });
}

- (void)didFind:(NSUInteger)aNumber occurrencesOf:(NSString*)aFindString atPosition:(text::pos_t const&)aPosition wrapped:(BOOL)didWrap
{
	NSString* format = nil;
	switch(aNumber)
	{
		case 0:  format = @"No more %@ “%@”.";                break;
		case 1:  format = didWrap ? @"Search wrapped." : nil; break;
		default: format = @"%3$ld %@ “%@”.";                  break;
	}

	NSString* classifier = (self.findOptions & find::regular_expression) ? @"matches for" : @"occurrences of";
	if(format)
		[self showToolTip:[NSString stringWithFormat:format, classifier, aFindString, aNumber]];
}

- (void)didReplace:(NSUInteger)aNumber occurrencesOf:(NSString*)aFindString with:(NSString*)aReplacementString
{
	static NSString* const formatStrings[2][3] = {
		{ @"Nothing replaced (no occurrences of “%@”).", @"Replaced one occurrence of “%@”.", @"Replaced %2$ld occurrences of “%@”." },
		{ @"Nothing replaced (no matches for “%@”).",    @"Replaced one match of “%@”.",      @"Replaced %2$ld matches of “%@”."     }
	};
	NSString* format = formatStrings[(self.findOptions & find::regular_expression) ? 1 : 0][aNumber > 2 ? 2 : aNumber];
	[self showToolTip:[NSString stringWithFormat:format, aFindString, aNumber]];
}
@end

@implementation OakTextView
@synthesize initiateDragTimer, dragScrollTimer, showColumnSelectionCursor, showDragCursor, choiceMenu;
@synthesize markedRanges;
@synthesize refreshNestCount;
@synthesize liveSearchString, liveSearchRanges;

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
		srcRect = NSUnionRect(srcRect, NSIntersectionRect(visibleRect, documentView->rect_for_range(range.min().index, range.max().index)));

	NSBezierPath* clip = [NSBezierPath bezierPath];
	for(auto const& rect : documentView->rects_for_ranges(ranges))
		[clip appendBezierPath:[NSBezierPath bezierPathWithRect:NSOffsetRect(rect, -NSMinX(srcRect), -NSMinY(srcRect))]];

	NSImage* image = [[NSImage alloc] initWithSize:NSMakeSize(std::max<CGFloat>(NSWidth(srcRect), 1), std::max<CGFloat>(NSHeight(srcRect), 1))];
	[image lockFocusFlipped:[self isFlipped]];
	[clip addClip];

	CGContextRef context = (CGContextRef)[[NSGraphicsContext currentContext] graphicsPort];
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

	for(auto const& range : ranges)
	{
		NSRect imageRect;
		NSImage* image = [self imageForRanges:range imageRect:&imageRect];
		imageRect = [[self window] convertRectToScreen:[self convertRect:imageRect toView:nil]];
		OakShowPopOutAnimation(imageRect, image);
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
	if(document && documentView)
	{
		document->set_folded(documentView->folded_as_string());
		document->set_visible_index(documentView->index_at_point([self visibleRect].origin));
	}
}

- (void)setDocument:(document::document_ptr const&)aDocument
{
	if(document && aDocument && *document == *aDocument)
	{
		if(document->selection() != NULL_STR)
		{
			ng::ranges_t ranges = ng::convert(*documentView, document->selection());
			documentView->set_selections(ranges);
			for(auto const& range : ranges)
				documentView->remove_enclosing_folds(range.min().index, range.max().index);

			[self ensureSelectionIsInVisibleArea:self];
			[self updateSelection];
		}
		[self resetBlinkCaretTimer];
		return;
	}

	if(documentView)
	{
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

	if(document = aDocument)
	{
		settings_t const settings = settings_for_path(document->virtual_path(), document->file_type() + " " + to_s(self.scopeAttributes), path::parent(document->path()));

		wrapColumn = settings.get(kSettingsWrapColumnKey, wrapColumn);
		invisiblesMap = settings.get(kSettingsInvisiblesMapKey, "");
		documentView = std::make_shared<document_view_t>(document, theme, settings.get(kSettingsSoftWrapKey, false), wrapColumn, self.scrollPastEnd);
		if(settings.get(kSettingsShowWrapColumnKey, false))
			documentView->set_draw_wrap_column(true);

		if(settings.get(kSettingsShowIndentGuidesKey, false))
			documentView->set_draw_indent_guides(true);

		BOOL hasFocus = (self.keyState & (OakViewViewIsFirstResponderMask|OakViewWindowIsKeyMask|OakViewApplicationIsActiveMask)) == (OakViewViewIsFirstResponderMask|OakViewWindowIsKeyMask|OakViewApplicationIsActiveMask);
		documentView->set_is_key(hasFocus);

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

		documentView->set_clipboard(get_clipboard(NSGeneralPboard));
		documentView->set_find_clipboard(get_clipboard(NSFindPboard));
		documentView->set_replace_clipboard(get_clipboard(OakReplacePboard));

		ng::index_t visibleIndex = document->visible_index();
		if(document->selection() != NULL_STR)
		{
			ng::ranges_t ranges = ng::convert(*documentView, document->selection());
			documentView->set_selections(ranges);
			for(auto const& range : ranges)
				documentView->remove_enclosing_folds(range.min().index, range.max().index);
		}

		[self reflectDocumentSize];
		[self updateSelection];

		if(visibleIndex && visibleIndex.index < documentView->size())
				[self scrollIndexToFirstVisible:visibleIndex];
		else	[self ensureSelectionIsInVisibleArea:self];

		documentView->add_callback(callback);

		[self resetBlinkCaretTimer];
		[self setNeedsDisplay:YES];
		_links.reset();
		NSAccessibilityPostNotification(self, NSAccessibilityValueChangedNotification);
	}
}

- (id)initWithFrame:(NSRect)aRect
{
	if(self = [super initWithFrame:aRect])
	{
		_fontScaleFactor = 100;

		settings_t const& settings = settings_for_path();

		theme          = parse_theme(bundles::lookup(settings.get(kSettingsThemeKey, NULL_STR)));
		fontName       = settings.get(kSettingsFontNameKey, NULL_STR);
		fontSize       = settings.get(kSettingsFontSizeKey, 11.0);
		theme          = theme->copy_with_font_name_and_size(fontName, fontSize * _fontScaleFactor / 100);

		_showInvisibles = settings.get(kSettingsShowInvisiblesKey, false);
		_scrollPastEnd  = [[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsScrollPastEndKey];
		_antiAlias      = ![[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsDisableAntiAliasKey];
		_fontSmoothing  = (OTVFontSmoothing)[[NSUserDefaults standardUserDefaults] integerForKey:kUserDefaultsFontSmoothingKey];

		spellingDotImage = [NSImage imageNamed:@"SpellingDot" inSameBundleAsClass:[self class]];
		foldingDotsImage = [NSImage imageNamed:@"FoldingDots Template" inSameBundleAsClass:[self class]];

		[self registerForDraggedTypes:[[self class] dropTypes]];

		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(documentWillSave:) name:@"OakDocumentNotificationWillSave" object:nil];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(documentDidSave:) name:@"OakDocumentNotificationDidSave" object:nil];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(userDefaultsDidChange:) name:NSUserDefaultsDidChangeNotification object:[NSUserDefaults standardUserDefaults]];
	}
	return self;
}

- (void)dealloc
{
	[[NSNotificationCenter defaultCenter] removeObserver:self];
	[self setDocument:document::document_ptr()];
}

- (void)documentWillSave:(NSNotification*)aNotification
{
	NSWindow* window = [[aNotification userInfo] objectForKey:@"window"];
	if(window != self.window)
		return;

	for(auto const& item : bundles::query(bundles::kFieldSemanticClass, "callback.document.will-save", [self scopeContext], bundles::kItemTypeMost, oak::uuid_t(), false))
		[self performBundleItem:item];

	[self updateDocumentMetadata];
}

- (void)documentDidSave:(NSNotification*)aNotification
{
	NSWindow* window = [[aNotification userInfo] objectForKey:@"window"];
	if(window != self.window || document->path() == NULL_STR)
		return;

	for(auto const& item : bundles::query(bundles::kFieldSemanticClass, "callback.document.did-save", [self scopeContext], bundles::kItemTypeMost, oak::uuid_t(), false))
		[self performBundleItem:item];
}

- (void)reflectDocumentSize
{
	if(document && documentView && [self enclosingScrollView])
	{
		NSRect r = [[self enclosingScrollView] documentVisibleRect];
		documentView->set_viewport_size(r.size);
		NSSize newSize = NSMakeSize(std::max(NSWidth(r), documentView->width()), std::max(NSHeight(r), documentView->height()));
		if(!NSEqualSizes([self frame].size, newSize))
			[self setFrameSize:newSize];
	}
}

- (void)resizeWithOldSuperviewSize:(NSSize)oldBoundsSize
{
	if(document && documentView)
			[self reflectDocumentSize];
	else	[super resizeWithOldSuperviewSize:oldBoundsSize];
}

- (void)centerSelectionInVisibleArea:(id)sender
{
	[self recordSelector:_cmd withArgument:nil];

	CGRect r = documentView->rect_at_index(documentView->ranges().last().last);
	CGFloat w = NSWidth([self visibleRect]), h = NSHeight([self visibleRect]);

	CGFloat x = r.origin.x < w ? 0 : r.origin.x - w/2;
	CGFloat y = oak::cap(NSMinY([self frame]), r.origin.y - (h-r.size.height)/2, NSHeight([self frame]) - h);

	[self scrollRectToVisible:CGRectMake(round(x), round(y), w, h)];
}

- (void)ensureSelectionIsInVisibleArea:(id)sender
{
	self.needsEnsureSelectionIsInVisibleArea = NO;
	if([[self.window currentEvent] type] == NSLeftMouseDragged) // User is drag-selecting
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
	{
		D(DBF_OakTextView_ViewRect, bug("scroll right\n"););
		x = r.origin.x + 5*r.size.width - w;
	}
	else if(r.origin.x < x + 2*r.size.width)
	{
		D(DBF_OakTextView_ViewRect, bug("scroll left\n"););
		x = r.origin.x < w/2 ? 0 : r.origin.x - 5*r.size.width;
	}

	if(oak::cap<CGFloat>(y + h - 1.5*r.size.height, r.origin.y, y + h + 1.5*r.size.height) == r.origin.y) // scroll down
	{
		D(DBF_OakTextView_ViewRect, bug("scroll down\n"););
		y = r.origin.y + 1.5*r.size.height - h;
	}
	else if(oak::cap<CGFloat>(y - 3*r.size.height, r.origin.y, y + 0.5*r.size.height) == r.origin.y) // scroll up
	{
		D(DBF_OakTextView_ViewRect, bug("scroll up\n"););
		y = r.origin.y - 0.5*r.size.height;
	}
	else if(oak::cap(y, r.origin.y, y + h) != r.origin.y) // center y
	{
		y = r.origin.y - (h-r.size.height)/2;
	}

doScroll:
	CGRect b = [self bounds];
	x = oak::cap(NSMinX(b), x, NSMaxX(b) - w);
	y = oak::cap(NSMinY(b), y, NSMaxY(b) - h);

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
		choiceMenu = [OakChoiceMenu new];
		choiceMenu.choices = (__bridge NSArray*)((CFArrayRef)cf::wrap(choiceVector));

		std::string const& currentChoice = documentView->placeholder_content();
		for(size_t i = choiceVector.size(); i-- > 0; )
		{
			if(choiceVector[i] == currentChoice)
				choiceMenu.choiceIndex = i;
		}

		[choiceMenu showAtTopLeftPoint:[self positionForWindowUnderCaret] forView:self];
	}
}

// ======================
// = Generic view stuff =
// ======================

+ (BOOL)isCompatibleWithResponsiveScrolling
{
	return NO;
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

- (void)drawRect:(NSRect)aRect
{
	if(!documentView || !theme)
	{
		NSEraseRect(aRect);
		return;
	}

	if(theme->is_transparent())
	{
		[[NSColor clearColor] set];
		NSRectFill(aRect);
	}

	CGContextRef context = (CGContextRef)[[NSGraphicsContext currentContext] graphicsPort];
	if(!self.antiAlias)
		CGContextSetShouldAntialias(context, false);

	BOOL disableFontSmoothing = NO;
	switch(self.fontSmoothing)
	{
		case OTVFontSmoothingDisabled:             disableFontSmoothing = YES;                                                         break;
		case OTVFontSmoothingDisabledForDark:      disableFontSmoothing = theme->is_dark();                                            break;
		case OTVFontSmoothingDisabledForDarkHiDPI: disableFontSmoothing = theme->is_dark() && [[self window] backingScaleFactor] == 2; break;
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

	documentView->draw(ng::context_t(context, _showInvisibles ? invisiblesMap : NULL_STR, [spellingDotImage CGImageForProposedRect:NULL context:[NSGraphicsContext currentContext] hints:nil], foldingDotsFactory), aRect, [self isFlipped], merge(documentView->ranges(), [self markedRanges]), liveSearchRanges);
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
	std::string const text = documentView->substr(0, range.max().index);
	char const* base = text.data();
	NSUInteger location = utf16::distance(base, base + range.min().index);
	NSUInteger length   = utf16::distance(base + range.min().index, base + range.max().index);
	return NSMakeRange(location, length);
}

- (ng::range_t)rangeForNSRange:(NSRange)nsRange
{
	std::string const text = documentView->as_string();
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
	D(DBF_OakTextView_TextInput, bug("‘%s’ %s\n", to_s([aString description]).c_str(), [NSStringFromRange(aRange) UTF8String]););
	if(!documentView)
		return;

	AUTO_REFRESH;
	if(replacementRange.location != NSNotFound)
		documentView->set_selections([self rangesForReplacementRange:replacementRange]);
	else if(!markedRanges.empty())
		documentView->set_selections(markedRanges);

	markedRanges = ng::ranges_t();
	documentView->insert(to_s(aString), true);
	if([aString length] != 0)
		markedRanges = documentView->ranges();
	pendingMarkedRanges = markedRanges;

	ng::ranges_t sel;
	for(auto const& range : documentView->ranges())
	{
		std::string const str = documentView->substr(range.min().index, range.max().index);
		char const* base = str.data();
		size_t from = utf16::advance(base, aRange.location, base + str.size()) - base;
		size_t to   = utf16::advance(base, NSMaxRange(aRange), base + str.size()) - base;
		sel.push_back(ng::range_t(range.min() + from, range.min() + to));
	}
	documentView->set_selections(sel);
}

- (NSRange)selectedRange
{
	if(!documentView)
		return { NSNotFound, 0 };

	NSRange res = [self nsRangeForRange:documentView->ranges().last()];
	D(DBF_OakTextView_TextInput, bug("%s\n", [NSStringFromRange(res) UTF8String]););
	return res;
}

- (NSRange)markedRange
{
	D(DBF_OakTextView_TextInput, bug("%s\n", to_s(markedRanges).c_str()););
	if(!documentView || markedRanges.empty())
		return NSMakeRange(NSNotFound, 0);
	return [self nsRangeForRange:markedRanges.last()];
}

- (void)unmarkText
{
	D(DBF_OakTextView_TextInput, bug("\n"););
	AUTO_REFRESH;
	markedRanges = pendingMarkedRanges = ng::ranges_t();
}

- (BOOL)hasMarkedText
{
	D(DBF_OakTextView_TextInput, bug("%s\n", BSTR(!markedRanges.empty())););
	return !markedRanges.empty();
}

- (NSArray*)validAttributesForMarkedText
{
	D(DBF_OakTextView_TextInput, bug("\n"););
	return [NSArray array];
}

- (void)updateMarkedRanges
{
	if(!markedRanges.empty() && pendingMarkedRanges.empty())
		[self.inputContext discardMarkedText];

	markedRanges = pendingMarkedRanges;
	pendingMarkedRanges = ng::ranges_t();
}

- (NSUInteger)characterIndexForPoint:(NSPoint)thePoint
{
	if(!documentView)
		return NSNotFound;

	NSPoint p = [self convertPoint:[[self window] convertRectFromScreen:(NSRect){ thePoint, NSZeroSize }].origin fromView:nil];
	std::string const text = documentView->as_string();
	size_t index = documentView->index_at_point(p).index;
	D(DBF_OakTextView_TextInput, bug("%s → %zu\n", [NSStringFromPoint(thePoint) UTF8String], index););
	return utf16::distance(text.data(), text.data() + index);
}

- (NSAttributedString*)attributedSubstringForProposedRange:(NSRange)theRange actualRange:(NSRangePointer)actualRange
{
	if(!documentView)
		return nil;

	ng::range_t const& r = [self rangeForNSRange:theRange];
	size_t from = r.min().index, to = r.max().index;

	if(CFMutableAttributedStringRef res = CFAttributedStringCreateMutable(kCFAllocatorDefault, 0))
	{
		std::map<size_t, scope::scope_t> scopes = documentView->scopes(from, to);
		for(auto pair = scopes.begin(); pair != scopes.end(); )
		{
			styles_t const& styles = theme->styles_for_scope(pair->second);

			size_t i = from + pair->first;
			size_t j = ++pair != scopes.end() ? from + pair->first : to;

			if(CFMutableAttributedStringRef str = CFAttributedStringCreateMutable(kCFAllocatorDefault, 0))
			{
				CFAttributedStringReplaceString(str, CFRangeMake(0, 0), cf::wrap(documentView->substr(i, j)));
				CFAttributedStringSetAttribute(str, CFRangeMake(0, CFAttributedStringGetLength(str)), kCTFontAttributeName, styles.font());
				CFAttributedStringSetAttribute(str, CFRangeMake(0, CFAttributedStringGetLength(str)), kCTForegroundColorAttributeName, styles.foreground());
				if(styles.underlined())
					CFAttributedStringSetAttribute(str, CFRangeMake(0, CFAttributedStringGetLength(str)), kCTUnderlineStyleAttributeName, cf::wrap(0x1|kCTUnderlinePatternSolid));
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
	D(DBF_OakTextView_TextInput, bug("%s → %s\n", [NSStringFromRange(theRange) UTF8String], [NSStringFromRect(rect) UTF8String]););
	return rect;
}

- (void)doCommandBySelector:(SEL)aSelector
{
	D(DBF_OakTextView_TextInput, bug("%s\n", sel_getName(aSelector)););
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

// =================
// = Accessibility =
// =================

- (BOOL)accessibilityIsIgnored
{
	return NO;
}

- (NSSet*)myAccessibilityAttributeNames
{
	static NSSet* set = [NSSet setWithArray:@[
		NSAccessibilityRoleAttribute,
		NSAccessibilityValueAttribute,
		NSAccessibilityInsertionPointLineNumberAttribute,
		NSAccessibilityNumberOfCharactersAttribute,
		NSAccessibilitySelectedTextAttribute,
		NSAccessibilitySelectedTextRangeAttribute,
		NSAccessibilitySelectedTextRangesAttribute,
		NSAccessibilityVisibleCharacterRangeAttribute,
		NSAccessibilityChildrenAttribute,
	]];
	return set;
}

- (NSArray*)accessibilityAttributeNames
{
	static NSArray* attributes = [[[self myAccessibilityAttributeNames] setByAddingObjectsFromArray:[super accessibilityAttributeNames]] allObjects];
	return attributes;
}

- (id)accessibilityAttributeValue:(NSString*)attribute
{
	D(DBF_OakTextView_Accessibility, bug("%s\n", to_s(attribute).c_str()););
	id ret = nil;

	if(false) {
	} else if([attribute isEqualToString:NSAccessibilityRoleAttribute]) {
		ret = NSAccessibilityTextAreaRole;
	} else if([attribute isEqualToString:NSAccessibilityValueAttribute]) {
		ret = [NSString stringWithCxxString:documentView->as_string()];
	} else if([attribute isEqualToString:NSAccessibilityInsertionPointLineNumberAttribute]) {
		ret = [NSNumber numberWithUnsignedLong:documentView->softline_for_index(documentView->ranges().last().min())];
	} else if([attribute isEqualToString:NSAccessibilityNumberOfCharactersAttribute]) {
		ret = [NSNumber numberWithUnsignedInteger:[self nsRangeForRange:ng::range_t(0, documentView->size())].length];
	} else if([attribute isEqualToString:NSAccessibilitySelectedTextAttribute]) {
		ng::range_t const selection = documentView->ranges().last();
		std::string const text = documentView->substr(selection.min().index, selection.max().index);
		ret = [NSString stringWithCxxString:text];
	} else if([attribute isEqualToString:NSAccessibilitySelectedTextRangeAttribute]) {
		ret = [NSValue valueWithRange:[self nsRangeForRange:documentView->ranges().last()]];
	} else if([attribute isEqualToString:NSAccessibilitySelectedTextRangesAttribute]) {
		ng::ranges_t const ranges = documentView->ranges();
		NSMutableArray* nsRanges = [NSMutableArray arrayWithCapacity:ranges.size()];
		for(auto const& range : ranges)
			[nsRanges addObject:[NSValue valueWithRange:[self nsRangeForRange:range]]];
		ret = nsRanges;
	} else if([attribute isEqualToString:NSAccessibilityVisibleCharacterRangeAttribute]) {
		NSRect visibleRect = [self visibleRect];
		CGPoint startPoint = NSMakePoint(NSMinX(visibleRect), NSMaxY(visibleRect));
		CGPoint   endPoint = NSMakePoint(NSMinX(visibleRect), NSMinY(visibleRect));
		ng::range_t visibleRange(documentView->index_at_point(startPoint), documentView->index_at_point(endPoint));
		visibleRange = visibleRange.sorted();
		visibleRange.last = documentView->index_below(visibleRange.last);
		return [NSValue valueWithRange:[self nsRangeForRange:visibleRange]];
	} else if([attribute isEqualToString:NSAccessibilityChildrenAttribute]) {
		NSMutableArray* links = [NSMutableArray array];
		std::shared_ptr<links_t> links_ = self.links;
		for(auto const& pair : *links_)
			[links addObject:pair.second];
		return links;
	} else {
		ret = [super accessibilityAttributeValue:attribute];
	}
	return ret;
}

- (BOOL)accessibilityIsAttributeSettable:(NSString*)attribute
{
	static NSArray* settable = @[
		NSAccessibilityValueAttribute,
		NSAccessibilitySelectedTextAttribute,
		NSAccessibilitySelectedTextRangeAttribute,
		NSAccessibilitySelectedTextRangesAttribute,
	];
	if([[self myAccessibilityAttributeNames] containsObject:attribute])
		return [settable containsObject:attribute];
	return [super accessibilityIsAttributeSettable:attribute];
}

- (void)accessibilitySetValue:(id)value forAttribute:(NSString*)attribute
{
	D(DBF_OakTextView_Accessibility, bug("%s <- %s\n", to_s(attribute).c_str(), to_s([value description]).c_str()););
	if(false) {
	} else if([attribute isEqualToString:NSAccessibilityValueAttribute]) {
		AUTO_REFRESH;
		document->set_content(to_s(value));
	} else if([attribute isEqualToString:NSAccessibilitySelectedTextAttribute]) {
		AUTO_REFRESH;
		documentView->insert(to_s(value));
	} else if([attribute isEqualToString:NSAccessibilitySelectedTextRangeAttribute]) {
		[self accessibilitySetValue:@[ value ] forAttribute:NSAccessibilitySelectedTextRangesAttribute];
	} else if([attribute isEqualToString:NSAccessibilitySelectedTextRangesAttribute]) {
		NSArray* nsRanges = (NSArray*)value;
		ng::ranges_t ranges;
		for(NSValue* nsRangeValue in nsRanges)
			ranges.push_back([self rangeForNSRange:[nsRangeValue rangeValue]]);
		AUTO_REFRESH;
		documentView->set_selections(ranges);
	} else {
		[super accessibilitySetValue:value forAttribute:attribute];
	}
}

- (NSUInteger)accessibilityArrayAttributeCount:(NSString* )attribute
{
	if([attribute isEqualToString:NSAccessibilityChildrenAttribute])
	{
		return self.links->size();
	}
	else
	{
		return [super accessibilityArrayAttributeCount:attribute];
	}
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
	else
	{
		return [super accessibilityArrayAttributeValues:attribute index:index maxCount:maxCount];
	}
}

- (NSUInteger)accessibilityIndexOfChild:(id)child
{
	if([child isKindOfClass:[OakAccessibleLink class]])
	{
		OakAccessibleLink* link = (OakAccessibleLink* )child;
		links_ptr const links = self.links;
		auto it = links->find(link.range.max().index);
		if(it != links->end())
			return it.index();
		else
			return NSNotFound;
	}
	else
	{
		return [super accessibilityIndexOfChild:child];
	}
}

- (NSArray*)accessibilityParameterizedAttributeNames
{
	static NSArray* attributes = nil;
	if(!attributes)
	{
		NSSet* set = [NSSet setWithArray:@[
			NSAccessibilityLineForIndexParameterizedAttribute,
			NSAccessibilityRangeForLineParameterizedAttribute,
			NSAccessibilityStringForRangeParameterizedAttribute,
			NSAccessibilityRangeForPositionParameterizedAttribute,
			NSAccessibilityRangeForIndexParameterizedAttribute,
			NSAccessibilityBoundsForRangeParameterizedAttribute,
			// NSAccessibilityRTFForRangeParameterizedAttribute,
			// NSAccessibilityStyleRangeForIndexParameterizedAttribute,
			NSAccessibilityAttributedStringForRangeParameterizedAttribute,
		]];

		attributes = [[set setByAddingObjectsFromArray:[super accessibilityParameterizedAttributeNames]] allObjects];
	}
	return attributes;
}

- (id)accessibilityAttributeValue:(NSString*)attribute forParameter:(id)parameter
{
	D(DBF_OakTextView_Accessibility, bug("%s(%s)\n", to_s(attribute).c_str(), to_s([parameter description]).c_str()););
	id ret = nil;
	if(false) {
	} else if([attribute isEqualToString:NSAccessibilityLineForIndexParameterizedAttribute]) {
		size_t index = [((NSNumber*)parameter) unsignedLongValue];
		index = [self rangeForNSRange:NSMakeRange(index, 0)].min().index;
		size_t line = documentView->softline_for_index(index);
		ret = [NSNumber numberWithUnsignedLong:line];
	} else if([attribute isEqualToString:NSAccessibilityRangeForLineParameterizedAttribute]) {
		size_t line = [((NSNumber*)parameter) unsignedLongValue];
		ng::range_t const range = documentView->range_for_softline(line);
		ret = [NSValue valueWithRange:[self nsRangeForRange:range]];
	} else if([attribute isEqualToString:NSAccessibilityStringForRangeParameterizedAttribute]) {
		ng::range_t range = [self rangeForNSRange:[((NSValue*)parameter) rangeValue]];
		ret = [NSString stringWithCxxString:documentView->as_string(range.min().index, range.max().index)];
	} else if([attribute isEqualToString:NSAccessibilityRangeForPositionParameterizedAttribute]) {
		NSPoint point = [((NSValue*)parameter) pointValue];
		point = [[self window] convertRectFromScreen:(NSRect){ point, NSZeroSize }].origin;
		point = [self convertPoint:point fromView:nil];
		size_t index = documentView->index_at_point(point).index;
		index = documentView->sanitize_index(index);
		size_t const length = (*documentView)[index].length();
		ret = [NSValue valueWithRange:[self nsRangeForRange:ng::range_t(index, index + length)]];
	} else if([attribute isEqualToString:NSAccessibilityRangeForIndexParameterizedAttribute]) {
		size_t index = [((NSNumber*)parameter) unsignedLongValue];
		index = [self rangeForNSRange:NSMakeRange(index, 0)].min().index;
		index = documentView->sanitize_index(index);
		size_t const length = (*documentView)[index].length();
		ret = [NSValue valueWithRange:[self nsRangeForRange:ng::range_t(index, index + length)]];
	} else if([attribute isEqualToString:NSAccessibilityBoundsForRangeParameterizedAttribute]) {
		ng::range_t range = [self rangeForNSRange:[((NSValue*)parameter) rangeValue]];
		NSRect rect = documentView->rect_for_range(range.min().index, range.max().index, true);
		rect = [self convertRect:rect toView:nil];
		rect = [[self window] convertRectToScreen:rect];
		ret = [NSValue valueWithRect:rect];
	// } else if([attribute isEqualToString:NSAccessibilityRTFForRangeParameterizedAttribute]) { // TODO
	// } else if([attribute isEqualToString:NSAccessibilityStyleRangeForIndexParameterizedAttribute]) { // TODO
	} else if([attribute isEqualToString:NSAccessibilityAttributedStringForRangeParameterizedAttribute]) {
		NSRange aRange = [((NSValue *)parameter) rangeValue];
		ng::range_t const range = [self rangeForNSRange:aRange];
		size_t const from = range.min().index, to = range.max().index;
		std::string const text = documentView->as_string(from, to);
		NSMutableAttributedString* res = [[NSMutableAttributedString alloc] initWithString:[NSString stringWithCxxString:text]];

		// Add style
		std::map<size_t, scope::scope_t> scopes = documentView->scopes(from, to);
		NSRange runRange = NSMakeRange(0, 0);
		for(auto pair = scopes.begin(); pair != scopes.end(); )
		{
			styles_t const& styles = theme->styles_for_scope(pair->second);

			size_t i = pair->first;
			size_t j = ++pair != scopes.end() ? pair->first : to - from;

			runRange.location += runRange.length;
			runRange.length = utf16::distance(text.data() + i, text.data() + j);
			NSFont* font = (__bridge NSFont *)styles.font();
			NSMutableDictionary* attributes = [NSMutableDictionary dictionaryWithCapacity:4];
			[attributes addEntriesFromDictionary:@{
				NSAccessibilityFontTextAttribute: @{
					NSAccessibilityFontNameKey: [font fontName],
					NSAccessibilityFontFamilyKey: [font familyName],
					NSAccessibilityVisibleNameKey: [font displayName],
					NSAccessibilityFontSizeKey: @([font pointSize]),
				},
				NSAccessibilityForegroundColorTextAttribute: (__bridge id)styles.foreground(),
				NSAccessibilityBackgroundColorTextAttribute: (__bridge id)styles.background(),
			}];
			if(styles.underlined())
				attributes[NSAccessibilityUnderlineTextAttribute] = @(NSUnderlineStyleSingle | NSUnderlinePatternSolid); // TODO is this always so?

			[res setAttributes:attributes range:runRange];
		}

		// Add links
		const links_ptr links = self.links;
		auto lbegin = links->upper_bound(from);
		auto lend   = links->lower_bound(to);
		if(lend != links->end() && to >= lend->second.range.min().index)
			++lend;

		std::for_each(lbegin, lend, [=](links_t::iterator::value_type const& pair){
			ng::range_t range = pair.second.range;
			range.first = oak::cap(ng::index_t(from), range.min(), ng::index_t(to));
			range.last  = oak::cap(ng::index_t(from), range.max(), ng::index_t(to));
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
			ASSERT(pair != end);
			ASSERT(pair->second);
			// assert(runRange.location + runRange.length ~ pair->first)
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
	} else {
		ret = [super accessibilityAttributeValue:attribute forParameter:parameter];
	}
	return ret;
}

- (id)accessibilityHitTest:(NSPoint)screenPoint
{
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

- (void)updateZoom:(id)sender
{
	size_t const index = documentView->ranges().last().min().index;
	NSRect selectedRect = documentView->rect_at_index(index, false);
	selectedRect = [self convertRect:selectedRect toView:nil];
	selectedRect = [[self window] convertRectToScreen:selectedRect];
	NSRect viewRect = [self convertRect:[self visibleRect] toView:nil];
	viewRect = [[self window] convertRectToScreen:viewRect];
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
	res << document->document_variables() << documentView->editor_variables(to_s([self scopeAttributes]));
	if(item)
		res << item->bundle_variables();

	if(auto themeItem = (theme ? bundles::lookup(theme->uuid()) : bundles::item_ptr()))
	{
		if(!themeItem->paths().empty())
			res["TM_CURRENT_THEME_PATH"] = themeItem->paths().back();
	}

	if([self.delegate respondsToSelector:@selector(variables)])
		res << [self.delegate variables];

	res = bundles::scope_variables(res, [self scopeContext]);
	res = variables_for_path(res, document->virtual_path(), [self scopeContext].right, path::parent(document->path()));
	return res;
}

- (std::map<std::string, std::string>)variables
{
	return [self variablesForBundleItem:bundles::item_ptr()];
}

- (void)performBundleItem:(bundles::item_ptr)item
{
	crash_reporter_info_t info(text::format("%s %s", sel_getName(_cmd), item->name_with_bundle().c_str()));
	// D(DBF_OakTextView_BundleItems, bug("%s\n", anItem->name_with_bundle().c_str()););
	AUTO_REFRESH;
	switch(item->kind())
	{
		case bundles::kItemTypeSnippet:
		{
			[self recordSelector:@selector(insertSnippetWithOptions:) withArgument:ns::to_dictionary(item->plist())];
			documentView->snippet_dispatch(item->plist(), [self variablesForBundleItem:item]);
		}
		break;

		case bundles::kItemTypeCommand:
		{
			[self recordSelector:@selector(executeCommandWithOptions:) withArgument:ns::to_dictionary(item->plist())];

			auto command = parse_command(item);
			command.name = name_with_selection(item, self.hasSelection);
			if([self.delegate respondsToSelector:@selector(bundleItemPreExec:completionHandler:)])
			{
				[self.delegate bundleItemPreExec:command.pre_exec completionHandler:^(BOOL success){
					if(success)
					{
						AUTO_REFRESH;
						document::run(command, *documentView, documentView->ranges(), document, [self variablesForBundleItem:item]);
					}
				}];
			}
			else
			{
				command.pre_exec = pre_exec::nop;
				document::run(command, *documentView, documentView->ranges(), document, [self variablesForBundleItem:item]);
			}
		}
		break;

		case bundles::kItemTypeMacro:
		{
			[self recordSelector:@selector(playMacroWithOptions:) withArgument:ns::to_dictionary(item->plist())];
			documentView->macro_dispatch(item->plist(), [self variablesForBundleItem:item]);
		}
		break;

		case bundles::kItemTypeGrammar:
		{
			document->set_file_type(item->value_for_field(bundles::kFieldGrammarScope));
			file::set_type(document->virtual_path(), item->value_for_field(bundles::kFieldGrammarScope));
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

		[[NSUserDefaults standardUserDefaults] registerDefaults:@{
			kUserDefaultsFontSmoothingKey     : @(OTVFontSmoothingDisabledForDarkHiDPI),
			kUserDefaultsWrapColumnPresetsKey : @[ @40, @80 ],
		}];
	});

	[NSApp registerServicesMenuSendTypes:@[ NSStringPboardType ] returnTypes:@[ NSStringPboardType ]];
}

// ======================
// = NSServicesRequests =
// ======================

- (id)validRequestorForSendType:(NSString*)sendType returnType:(NSString*)returnType
{
	if([sendType isEqualToString:NSStringPboardType] && [self hasSelection] && !macroRecordingArray)
		return self;
	if(!sendType && [returnType isEqualToString:NSStringPboardType] && !macroRecordingArray)
		return self;
	return [super validRequestorForSendType:sendType returnType:returnType];
}

- (BOOL)writeSelectionToPasteboard:(NSPasteboard*)pboard types:(NSArray*)types
{
	BOOL res = NO;
	if([self hasSelection] && [types containsObject:NSStringPboardType])
	{
		std::vector<std::string> v;
		ng::ranges_t const ranges = ng::dissect_columnar(*documentView, documentView->ranges());
		for(auto const& range : ranges)
			v.push_back(documentView->substr(range.min().index, range.max().index));

		[pboard declareTypes:@[ NSStringPboardType ] owner:nil];
		res = [pboard setString:[NSString stringWithCxxString:text::join(v, "\n")] forType:NSStringPboardType];
	}
	return res;
}

- (BOOL)readSelectionFromPasteboard:(NSPasteboard*)pboard
{
	if(NSString* str = [pboard stringForType:[pboard availableTypeFromArray:@[ NSStringPboardType ]]])
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
		eventMonitor = [NSEvent addLocalMonitorForEventsMatchingMask:NSKeyDownMask handler:^NSEvent*(NSEvent* event){
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
	BOOL hasFocus = (self.keyState & (OakViewViewIsFirstResponderMask|OakViewWindowIsKeyMask|OakViewApplicationIsActiveMask)) == (OakViewViewIsFirstResponderMask|OakViewWindowIsKeyMask|OakViewApplicationIsActiveMask);
	if(!hasFocus && ([[[self window] firstResponder] isKindOfClass:[self class]] || [[[self window] firstResponder] isKindOfClass:NSClassFromString(@"OakKeyEquivalentView")]))
		return NO;

	D(DBF_OakTextView_TextInput, bug("%s\n", [[anEvent description] UTF8String]););
	std::string const eventString = to_s(anEvent);

	std::vector<bundles::item_ptr> const& items = bundles::query(bundles::kFieldKeyEquivalent, eventString, [self scopeContext]);
	if(!items.empty())
	{
		if(bundles::item_ptr item = OakShowMenuForBundleItems(items, [self positionForWindowUnderCaret]))
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
			return [self handleKeyBindingAction:pair->second], YES;
	}

	return NO;
}

- (void)oldKeyDown:(NSEvent*)anEvent
{
	std::vector<bundles::item_ptr> const& items = bundles::query(bundles::kFieldKeyEquivalent, to_s(anEvent), [self scopeContext]);
	if(bundles::item_ptr item = OakShowMenuForBundleItems(items, [self positionForWindowUnderCaret]))
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
	[[NSNotificationCenter defaultCenter] postNotificationName:OakCursorDidHideNotification object:nil];
}

- (void)keyDown:(NSEvent*)anEvent
{
	D(DBF_OakTextView_TextInput, bug("%s\n", [[anEvent description] UTF8String]););
	crash_reporter_info_t info(text::format("%s %s", sel_getName(_cmd), to_s(anEvent).c_str()));
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
	if(!choiceMenu)
		return [self oldKeyDown:anEvent];

	ng::range_t oldSelection;
	std::string oldContent = documentView->placeholder_content(&oldSelection);
	std::string oldPrefix  = oldSelection ? oldContent.substr(0, oldSelection.min().index) : "";

	NSUInteger event = [choiceMenu didHandleKeyEvent:anEvent];
	if(event == OakChoiceMenuKeyUnused)
	{
		[self oldKeyDown:anEvent];

		ng::range_t newSelection;
		std::string const& newContent = documentView->placeholder_content(&newSelection);
		std::string const newPrefix   = newSelection ? newContent.substr(0, newSelection.min().index) : "";

		std::vector<std::string> newChoices = documentView->choices();
		newChoices.erase(std::remove_if(newChoices.begin(), newChoices.end(), [&newPrefix](std::string const& str) { return str.find(newPrefix) != 0; }), newChoices.end());
		choiceMenu.choices = (__bridge NSArray*)((CFArrayRef)cf::wrap(newChoices));

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

			choiceMenu.choiceIndex = choiceIndex;
			if(choiceIndex != NSNotFound && newContent != newChoices[choiceIndex])
				documentView->set_placeholder_content(newChoices[choiceIndex], newPrefix.size());
		}
		else if(oldContent != newContent)
		{
			choiceMenu.choiceIndex = NSNotFound;
		}
	}
	else if(event == OakChoiceMenuKeyMovement)
	{
		std::string const choice = to_s(choiceMenu.selectedChoice);
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
	NSInteger modifiers  = [anEvent modifierFlags] & (NSAlternateKeyMask | NSControlKeyMask | NSCommandKeyMask | NSShiftKeyMask);
	BOOL isHoldingOption = modifiers & NSAlternateKeyMask ? YES : NO;

	self.showColumnSelectionCursor = isHoldingOption;
	if(([NSEvent pressedMouseButtons] & 1))
	{
		if(documentView->has_selection() && documentView->ranges().last().columnar != isHoldingOption)
			[self toggleColumnSelection:self];
	}
	else if(modifiers != _lastFlags)
	{
		BOOL tapThreshold     = [[NSDate date] timeIntervalSinceDate:_lastFlagsChangeDate] < 0.18;

		BOOL didPressShift    = modifiers == NSShiftKeyMask && _lastFlags == 0;
		BOOL didReleaseShift  = modifiers == 0 && _lastFlags == NSShiftKeyMask;

		BOOL didPressOption   = (modifiers & ~NSShiftKeyMask) == NSAlternateKeyMask && (_lastFlags & ~NSShiftKeyMask) == 0;
		BOOL didReleaseOption = (modifiers & ~NSShiftKeyMask) == 0 && (_lastFlags & ~NSShiftKeyMask) == NSAlternateKeyMask;

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
	D(DBF_OakTextView_TextInput, bug("‘%s’, has marked %s\n", [[aString description] UTF8String], BSTR(!markedRanges.empty())););

	AUTO_REFRESH;
	if(!markedRanges.empty())
	{
		documentView->set_selections(markedRanges);
		[self delete:nil];
		markedRanges = ng::ranges_t();
	}
	pendingMarkedRanges = ng::ranges_t();

	if(aRange.location != NSNotFound)
	{
		documentView->set_selections([self rangesForReplacementRange:aRange]);
		[self delete:nil];
	}

	std::string const str = to_s(aString);
	[self recordSelector:@selector(insertText:) withArgument:[NSString stringWithCxxString:str]];
	bool autoPairing = !macroRecordingArray && ![[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsDisableTypingPairsKey];
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
	[[NSNotificationCenter defaultCenter] postNotificationName:GVColumnDataSourceDidChange object:[[self enclosingScrollView] superview]];
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
	[[NSNotificationCenter defaultCenter] postNotificationName:GVColumnDataSourceDidChange object:[[self enclosingScrollView] superview]];
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
	if([[NSSpellChecker sharedSpellChecker] hasLearnedWord:word])
	{
		NSMenuItem* item = [menu addItemWithTitle:[NSString stringWithFormat:@"Unlearn “%@”", word] action:@selector(contextMenuPerformUnlearnSpelling:) keyEquivalent:@""];
		[item setRepresentedObject:word];
		[menu addItem:[NSMenuItem separatorItem]];
	}
	else if(ns::is_misspelled(candidate, documentView->spelling_language(), documentView->spelling_tag()))
	{
		AUTO_REFRESH;
		documentView->set_selections(wordRange);

		[[NSSpellChecker sharedSpellChecker] updateSpellingPanelWithMisspelledWord:word];

		size_t bol = documentView->begin(documentView->convert(wordRange.min().index).line);
		size_t eol = documentView->eol(documentView->convert(wordRange.max().index).line);
		std::string const line = documentView->substr(bol, eol);
		NSUInteger location = utf16::distance(line.data(), line.data() + (wordRange.min().index - bol));
		NSUInteger length   = utf16::distance(line.data() + (wordRange.min().index - bol), line.data() + (wordRange.max().index - bol));

		char key = 0;
		NSMenuItem* item = nil;
		for(NSString* guess in [[NSSpellChecker sharedSpellChecker] guessesForWordRange:NSMakeRange(location, length) inString:[NSString stringWithCxxString:line] language:[NSString stringWithCxxString:documentView->spelling_language()] inSpellDocumentWithTag:documentView->spelling_tag()])
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
		clickInSelection = clickInSelection || range.min() <= index && index <= range.max();
	return [self contextMenuForRanges:(clickInSelection ? documentView->ranges() : index)];
}

- (void)showMenu:(NSMenu*)aMenu
{
	NSWindow* win = [self window];
	NSEvent* anEvent = [NSApp currentEvent];
	NSEvent* fakeEvent = [NSEvent
		mouseEventWithType:NSLeftMouseDown
		location:[win convertRectFromScreen:(NSRect){ [self positionForWindowUnderCaret], NSZeroSize }].origin
		modifierFlags:0
		timestamp:[anEvent timestamp]
		windowNumber:[win windowNumber]
		context:[anEvent context]
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
	D(DBF_OakTextView_Spelling, bug("%s\n", [[menuItem representedObject] UTF8String]););
	AUTO_REFRESH;
	documentView->insert(to_s([menuItem representedObject]));
	if([NSSpellChecker sharedSpellCheckerExists])
		[[NSSpellChecker sharedSpellChecker] updateSpellingPanelWithMisspelledWord:[menuItem representedObject]];
}

- (void)contextMenuPerformIgnoreSpelling:(id)sender
{
	D(DBF_OakTextView_Spelling, bug("%s\n", [[sender representedObject] UTF8String]););
	[self ignoreSpelling:[sender representedObject]];
}

- (void)contextMenuPerformLearnSpelling:(id)sender
{
	D(DBF_OakTextView_Spelling, bug("%s\n", [[sender representedObject] UTF8String]););
	[[NSSpellChecker sharedSpellChecker] learnWord:[sender representedObject]];

	documentView->recheck_spelling(0, documentView->size());
	[self setNeedsDisplay:YES];
}

- (void)contextMenuPerformUnlearnSpelling:(id)sender
{
	D(DBF_OakTextView_Spelling, bug("%s\n", [[sender representedObject] UTF8String]););
	[[NSSpellChecker sharedSpellChecker] unlearnWord:[sender representedObject]];

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

	D(DBF_OakTextView_Spelling, bug("%s → %s\n", [[sender description] UTF8String], [word UTF8String]););
	if(word)
	{
		[[NSSpellChecker sharedSpellChecker] ignoreWord:word inSpellDocumentWithTag:documentView->spelling_tag()];
		documentView->recheck_spelling(0, documentView->size());
		[self setNeedsDisplay:YES];
	}
}

- (void)changeSpelling:(id)sender
{
	D(DBF_OakTextView_Spelling, bug("%s\n", [[sender description] UTF8String]););
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
	[[NSNotificationCenter defaultCenter] postNotificationName:@"OakTextViewWillPerformFindOperation" object:self];

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
			[self recordSelector:@selector(findWithOptions:) withArgument:dict];
	}

	AUTO_REFRESH;

	find_operation_t findOperation = aFindServer.findOperation;
	if(findOperation == kFindOperationReplace || findOperation == kFindOperationReplaceAndFind)
	{
		std::string replacement = to_s(aFindServer.replaceString);
		if(NSDictionary* captures = self.matchCaptures)
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
			self.matchCaptures = nil;
			bool isCounting = findOperation == kFindOperationCount || findOperation == kFindOperationCountInSelection;

			std::string const findStr = to_s(aFindServer.findString);
			find::options_t options   = aFindServer.findOptions;

			NSArray* documents = [[OakPasteboard pasteboardWithName:NSFindPboard].auxiliaryOptionsForCurrent objectForKey:@"documents"];
			if(documents && [documents count] > 1)
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

			if(res.empty() && !isCounting && documents && [documents count] > 1)
			{
				for(NSUInteger i = 0; i < [documents count]; ++i)
				{
					NSString* uuid = [[documents objectAtIndex:i] objectForKey:@"identifier"];
					if(uuid && oak::uuid_t(to_s(uuid)) == document->identifier())
					{
						// ====================================================
						// = Update our document’s matches on Find pasteboard =
						// ====================================================

						NSMutableArray* newDocuments = [documents mutableCopy];
						auto newFirstMatch = ng::find(*documentView, ng::ranges_t(0), findStr, (find::options_t)(options & ~find::backwards));
						if(newFirstMatch.empty())
						{
							[newDocuments removeObjectAtIndex:i];
						}
						else
						{
							auto newLastMatch = ng::find(*documentView, ng::ranges_t(0), findStr, (find::options_t)(options | find::backwards | find::wrap_around));
							auto to_range = [&](auto it) { return text::range_t(documentView->convert(it->first.min().index), documentView->convert(it->first.max().index)); };
							[newDocuments replaceObjectAtIndex:i withObject:@{
								@"identifier"      : [NSString stringWithCxxString:document->identifier()],
								@"firstMatchRange" : [NSString stringWithCxxString:to_range(newFirstMatch.begin())],
								@"lastMatchRange"  : [NSString stringWithCxxString:to_range((newLastMatch.empty() ? newFirstMatch : newLastMatch).begin())]
							}];
						}
						[OakPasteboard pasteboardWithName:NSFindPboard].auxiliaryOptionsForCurrent = @{ @"documents" : newDocuments };

						// ====================================================

						NSDictionary* info = [documents objectAtIndex:(i + ((options & find::backwards) ? [documents count] - 1 : 1)) % [documents count]];
						document::document_ptr doc;
						if(NSString* path = [info objectForKey:@"path"])
							doc = document::create(to_s(path));
						else if(NSString* identifier = [info objectForKey:@"identifier"])
							doc = document::find(to_s(identifier));

						if(doc)
						{
							if(!doc->is_open())
								doc->set_recent_tracking(false);

							NSString* range = [info objectForKey:(options & find::backwards) ? @"lastMatchRange" : @"firstMatchRange"];
							document::show(doc, document::kCollectionAny, to_s(range));
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
					documentView->set_selections(res);
					if(res.size() == 1 && (options & find::regular_expression))
					{
						NSMutableDictionary* captures = [NSMutableDictionary dictionary];
						for(auto pair : allMatches[res.last()])
							captures[[NSString stringWithCxxString:pair.first]] = [NSString stringWithCxxString:pair.second];
						self.matchCaptures = captures;
					}
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
	[self performFindOperation:[OakTextViewFindServer findServerWithTextView:self operation:findOperation options:[[OakPasteboard pasteboardWithName:NSFindPboard] current].findOptions | extraOptions]];
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
		liveSearchRanges = ng::ranges_t();
	}
}

- (void)setLiveSearchRanges:(ng::ranges_t const&)ranges
{
	AUTO_REFRESH;

	ng::ranges_t const oldRanges = ng::move(*documentView, liveSearchRanges, kSelectionMoveToBeginOfSelection);
	liveSearchRanges = ranges;
	if(!liveSearchRanges.empty())
	{
		documentView->set_selections(liveSearchRanges);
		if(oldRanges != ng::move(*documentView, liveSearchRanges, kSelectionMoveToBeginOfSelection))
			[self highlightRanges:liveSearchRanges];
	}
	else if(!oldRanges.empty())
	{
		NSBeep();
	}
}

- (BOOL)control:(NSControl*)aControl textView:(NSTextView*)aTextView doCommandBySelector:(SEL)aCommand
{
	if(aCommand == @selector(insertNewline:) || aCommand == @selector(cancelOperation:))
		return [self setShowLiveSearch:NO], YES;
	if(aCommand == @selector(insertTab:))
		return [self findNext:self], YES;
	if(aCommand == @selector(insertBacktab:))
		return [self findPrevious:self], YES;
	return NO;
}

- (void)controlTextDidChange:(NSNotification*)aNotification
{
	NSTextView* searchField = [[aNotification userInfo] objectForKey:@"NSFieldEditor"];
	self.liveSearchString = [searchField string];

	ng::ranges_t res;
	for(auto const& pair : ng::find(*documentView, liveSearchAnchor, to_s(liveSearchString), find::ignore_case|find::ignore_whitespace|find::wrap_around))
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
	BOOL ignoreCase = self.liveSearchView.ignoreCaseCheckBox.state == NSOnState;
	BOOL wrapAround = self.liveSearchView.wrapAroundCheckBox.state == NSOnState;
	return (ignoreCase ? find::ignore_case : find::none) | (wrapAround ? find::wrap_around : find::none) | find::ignore_whitespace;
}

- (IBAction)findNext:(id)sender
{
	if(self.liveSearchView)
	{
		ng::ranges_t tmp;
		for(auto const& pair : ng::find(*documentView, ng::move(*documentView, liveSearchRanges.empty() ? liveSearchAnchor : liveSearchRanges, kSelectionMoveToEndOfSelection), to_s(liveSearchString), self.incrementalSearchOptions))
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
		for(auto const& pair : ng::find(*documentView, ng::move(*documentView, liveSearchRanges.empty() ? liveSearchAnchor : liveSearchRanges, kSelectionMoveToBeginOfSelection), to_s(liveSearchString), find::backwards|self.incrementalSearchOptions))
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
	documentView->clear_snippets();
	documentView->set_selections(documentView->undo());
}

- (void)redo:(id)anArgument // MACRO?
{
	AUTO_REFRESH;
	if(!documentView->can_redo())
		return;
	documentView->clear_snippets();
	documentView->set_selections(documentView->redo());
}

- (BOOL)expandTabTrigger:(id)sender
{
	if(documentView->disallow_tab_expansion())
		return NO;

	AUTO_REFRESH;
	ng::range_t range;
	std::vector<bundles::item_ptr> const& items = items_for_tab_expansion(documentView, documentView->ranges(), to_s([self scopeAttributes]), &range);
	if(bundles::item_ptr item = OakShowMenuForBundleItems(items, [self positionForWindowUnderCaret]))
	{
		[self recordSelector:@selector(deleteTabTrigger:) withArgument:[NSString stringWithCxxString:documentView->as_string(range.first.index, range.last.index)]];
		documentView->delete_tab_trigger(documentView->as_string(range.first.index, range.last.index));
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
		[aMenuItem setTitle:@"Cut"];
	else if([aMenuItem action] == @selector(copy:))
		[aMenuItem setTitle:@"Copy"];

	static auto const RequiresSelection = new std::set<SEL>{ @selector(cut:), @selector(copy:), @selector(delete:), @selector(copySelectionToFindPboard:) };
	if(RequiresSelection->find([aMenuItem action]) != RequiresSelection->end())
		return [self hasSelection];
	else if([aMenuItem action] == @selector(toggleMacroRecording:))
		[aMenuItem setTitle:self.isMacroRecording ? @"Stop Recording" : @"Start Recording"];
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
		[aMenuItem setState:documentView->live_spelling() ? NSOnState : NSOffState];
	else if([aMenuItem action] == @selector(takeSpellingLanguageFrom:))
		[aMenuItem setState:[[NSString stringWithCxxString:documentView->spelling_language()] isEqualToString:[aMenuItem representedObject]] ? NSOnState : NSOffState];
	else if([aMenuItem action] == @selector(takeWrapColumnFrom:))
		[aMenuItem setState:wrapColumn == [aMenuItem tag] ? NSOnState : NSOffState];
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

	// The column selection cursor may get stuck if e.g. using ⌥F2 to bring up a menu: We see the initial “option down” but newer the “option release” that would normally reset the column selection cursor state.
	if(([NSEvent modifierFlags] & NSAlternateKeyMask) == 0)
		self.showColumnSelectionCursor = NO;
}

- (void)setShowColumnSelectionCursor:(BOOL)flag
{
	D(DBF_OakTextView_TextInput, bug("%s → %s\n", BSTR(showColumnSelectionCursor), BSTR(flag)););
	if(flag != showColumnSelectionCursor)
	{
		showColumnSelectionCursor = flag;
		[[self window] invalidateCursorRectsForView:self];
	}
}

// ==============
// = Public API =
// ==============

- (theme_ptr const&)theme     { return theme; }
- (NSFont*)font               { return [NSFont fontWithName:[NSString stringWithCxxString:fontName] size:fontSize]; }
- (size_t)tabSize             { return documentView ? documentView->indent().tab_size() : 2; }
- (BOOL)softTabs              { return documentView ? documentView->indent().soft_tabs() : NO; }
- (BOOL)softWrap              { return documentView && documentView->wrapping(); }

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

- (void)setTheme:(theme_ptr const&)newTheme
{
	theme = newTheme->copy_with_font_name_and_size(fontName, fontSize * _fontScaleFactor / 100);
	if(documentView)
	{
		AUTO_REFRESH;
		documentView->set_theme(theme);
	}
}

- (void)setFont:(NSFont*)newFont
{
	fontName = to_s([newFont fontName]);
	fontSize = [newFont pointSize];
	_fontScaleFactor = 100;

	if(documentView)
	{
		AUTO_REFRESH;
		ng::index_t visibleIndex = documentView->index_at_point([self visibleRect].origin);
		documentView->set_font(fontName, fontSize * _fontScaleFactor / 100);
		[self scrollIndexToFirstVisible:documentView->begin(documentView->convert(visibleIndex.index).line)];
	}
}

- (void)setFontScaleFactor:(NSInteger)newFontScaleFactor
{
	if(_fontScaleFactor == newFontScaleFactor)
		return;
	_fontScaleFactor = newFontScaleFactor;

	if(theme)
		theme = theme->copy_with_font_name_and_size(fontName, fontSize * _fontScaleFactor / 100);

	if(documentView)
	{
		AUTO_REFRESH;
		ng::index_t visibleIndex = documentView->index_at_point([self visibleRect].origin);
		documentView->set_font(fontName, fontSize * _fontScaleFactor / 100);
		[self scrollIndexToFirstVisible:documentView->begin(documentView->convert(visibleIndex.index).line)];
	}

	[OTVHUD showHudForView:self withText:[NSString stringWithFormat:@"%ld%%", _fontScaleFactor]];
}

- (void)setTabSize:(size_t)newTabSize
{
	AUTO_REFRESH;
	if(documentView)
	{
		text::indent_t tmp = documentView->indent();
		tmp.set_tab_size(newTabSize);
		documentView->set_indent(tmp);
	}
}

- (void)setShowInvisibles:(BOOL)flag
{
	if(_showInvisibles == flag)
		return;
	_showInvisibles = flag;
	settings_t::set(kSettingsShowInvisiblesKey, (bool)flag, document->file_type());
	[self setNeedsDisplay:YES];
}

- (void)setScrollPastEnd:(BOOL)flag
{
	if(_scrollPastEnd == flag)
		return;
	_scrollPastEnd = flag;
	[[NSUserDefaults standardUserDefaults] setBool:flag forKey:kUserDefaultsScrollPastEndKey];
	if(documentView)
	{
		AUTO_REFRESH;
		documentView->set_scroll_past_end(flag);
	}
}

- (void)setSoftWrap:(BOOL)flag
{
	if(!documentView || documentView->wrapping() == flag)
		return;

	AUTO_REFRESH;
	ng::index_t visibleIndex = documentView->index_at_point([self visibleRect].origin);
	documentView->set_wrapping(flag, wrapColumn);
	[self scrollIndexToFirstVisible:documentView->begin(documentView->convert(visibleIndex.index).line)];
	settings_t::set(kSettingsSoftWrapKey, (bool)flag, document->file_type());
}

- (void)setSoftTabs:(BOOL)flag
{
	if(flag != self.softTabs)
	{
		text::indent_t tmp = documentView->indent();
		tmp.set_soft_tabs(flag);
		documentView->set_indent(tmp);
	}
}

- (void)setWrapColumn:(NSInteger)newWrapColumn
{
	if(wrapColumn == newWrapColumn)
		return;

	wrapColumn = newWrapColumn;
	settings_t::set(kSettingsWrapColumnKey, wrapColumn);

	if(wrapColumn != NSWrapColumnWindowWidth)
	{
		NSInteger const kWrapColumnPresetsHistorySize = 5;

		NSMutableArray* presets = [[[NSUserDefaults standardUserDefaults] arrayForKey:kUserDefaultsWrapColumnPresetsKey] mutableCopy];
		[presets removeObject:@(wrapColumn)];
		[presets addObject:@(wrapColumn)];
		if(presets.count > kWrapColumnPresetsHistorySize)
			[presets removeObjectsInRange:NSMakeRange(0, presets.count - kWrapColumnPresetsHistorySize)];
		[[NSUserDefaults standardUserDefaults] setObject:presets forKey:kUserDefaultsWrapColumnPresetsKey];
	}

	if(documentView)
	{
		AUTO_REFRESH;
		documentView->set_wrapping(self.softWrap, wrapColumn);
	}
}

- (void)takeWrapColumnFrom:(id)sender
{
	ASSERT([sender respondsToSelector:@selector(tag)]);
	if(wrapColumn == [sender tag])
		return;

	if([sender tag] == NSWrapColumnAskUser)
	{
		NSTextField* textField = [[NSTextField alloc] initWithFrame:NSZeroRect];
		[textField setIntegerValue:wrapColumn == NSWrapColumnWindowWidth ? 80 : wrapColumn];
		[textField sizeToFit];
		[textField setFrameSize:NSMakeSize(200, NSHeight([textField frame]))];

		NSAlert* alert = [NSAlert alertWithMessageText:@"Set Wrap Column" defaultButton:@"OK" alternateButton:@"Cancel" otherButton:nil informativeTextWithFormat:@"Specify what column text should wrap at:"];
		[alert setAccessoryView:textField];
		OakShowAlertForWindow(alert, [self window], ^(NSInteger returnCode){
			if(returnCode == NSAlertDefaultReturn)
				[self setWrapColumn:std::max<NSInteger>([textField integerValue], 10)];
		});
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
	self.softWrap = !self.softWrap;
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
	NSSpellChecker* speller = [NSSpellChecker sharedSpellChecker];

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
			documentView->set_selections(ng::range_t(nextMisspelling.first, nextMisspelling.second));
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
	settings_t::set(kSettingsSpellCheckingKey, flag, document->file_type(), document->path());

	[self setNeedsDisplay:YES];
}

- (void)takeSpellingLanguageFrom:(id)sender
{
	NSString* lang = (NSString*)[sender representedObject];
	[[NSSpellChecker sharedSpellChecker] setLanguage:lang];
	documentView->set_spelling_language(to_s(lang));
	settings_t::set(kSettingsSpellingLanguageKey, to_s(lang), "", document->path());
	if(document->path() != NULL_STR)
		settings_t::set(kSettingsSpellingLanguageKey, to_s(lang), NULL_STR, path::join(path::parent(document->path()), "**"));

	[self setNeedsDisplay:YES];
}

- (scope::context_t)scopeContext
{
	return documentView->scope(to_s([self scopeAttributes]));
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

	AUTO_REFRESH;
	ng::ranges_t ranges = ng::convert(*documentView, to_s(aSelectionString));
	documentView->set_selections(ranges);
	for(auto const& range : ranges)
		documentView->remove_enclosing_folds(range.min().index, range.max().index);
}

- (NSString*)selectionString
{
	return selectionString;
}

- (void)updateSelection
{
	text::selection_t ranges, withoutCarry;
	for(auto const& range : documentView->ranges())
	{
		text::pos_t from = documentView->convert(range.first.index);
		text::pos_t to   = documentView->convert(range.last.index);
		if(!range.freehanded && !range.columnar)
			withoutCarry.push_back(text::range_t(from, to, range.columnar));
		from.offset = range.first.carry;
		to.offset   = range.last.carry;
		if(range.freehanded || range.columnar)
			withoutCarry.push_back(text::range_t(from, to, range.columnar));
		ranges.push_back(text::range_t(from, to, range.columnar));
	}
	document->set_selection(ranges);

	isUpdatingSelection = YES;
	[self setSelectionString:[NSString stringWithCxxString:withoutCarry]];
	isUpdatingSelection = NO;
}

- (folding_state_t)foldingStateForLine:(NSUInteger)lineNumber
{
	if(document)
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

- (BOOL)filterDocumentThroughCommand:(NSString*)commandString input:(input::type)inputUnit output:(output::type)outputUnit
{
	BOOL res = NO;

	auto environment = [self variables];
	if(io::process_t process = io::spawn(std::vector<std::string>{ "/bin/sh", "-c", to_s(commandString) }, environment))
	{
		bool inputWasSelection = false;
		ng::ranges_t const inputRanges = ng::write_unit_to_fd(*documentView, documentView->ranges(), documentView->indent().tab_size(), process.in, inputUnit, input::entire_document, input_format::text, scope::selector_t(), environment, &inputWasSelection);

		__block int status = 0;
		__block std::string output, error;

		dispatch_group_t group = dispatch_group_create();
		dispatch_group_async(group, dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
			if(waitpid(process.pid, &status, 0) != process.pid)
				perror("waitpid");
		});
		dispatch_group_async(group, dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
			io::exhaust_fd(process.out, &output);
		});
		dispatch_group_async(group, dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
			io::exhaust_fd(process.err, &error);
		});
		dispatch_group_wait(group, DISPATCH_TIME_FOREVER);
		dispatch_release(group);

		if(res = WIFEXITED(status) && WEXITSTATUS(status) == 0)
		{
			if(outputUnit == output::tool_tip)
			{
				OakShowToolTip([NSString stringWithCxxString:text::trim(output)], [self positionForWindowUnderCaret]);
			}
			else if(outputUnit == output::new_window)
			{
				oak::uuid_t projectIdentifier = document::kCollectionAny;
				if([self.window.delegate respondsToSelector:@selector(identifier)]) // FIXME This should be a formal interface
					projectIdentifier = to_s([self.window.delegate performSelector:@selector(identifier)]);

				document::show(document::from_content(output, document->file_type()), projectIdentifier);
			}
			else
			{
				AUTO_REFRESH;
				documentView->handle_result(output, outputUnit, output_format::text, output_caret::after_output, inputRanges, environment);
			}
		}

		error = text::trim(error);
		if(error.empty() && !res)
		{
			if(WIFEXITED(status))
				error = text::format("Failed executing ‘%s’.\nCommand returned non-zero status code: %d.", [commandString UTF8String], WEXITSTATUS(status));
			else
				error = text::format("Failed executing ‘%s’.\nAbnormal exit: %d.", [commandString UTF8String], status);
		}

		if(!error.empty())
			OakShowToolTip([NSString stringWithCxxString:error], [self positionForWindowUnderCaret]);
	}
	return res;
}

- (NSString*)string
{
	// This is used by the Emmet plug-in (with no “respondsToSelector:” check)
	return [NSString stringWithCxxString:documentView->as_string()];
}

// ===================
// = Macro Recording =
// ===================

- (BOOL)isMacroRecording                    { return macroRecordingArray != nil; }
- (IBAction)toggleMacroRecording:(id)sender { self.isMacroRecording = !self.isMacroRecording; }

- (void)setIsMacroRecording:(BOOL)flag
{
	if(self.isMacroRecording == flag)
		return;

	D(DBF_OakTextView_Macros, bug("%s\n", BSTR(flag)););
	if(macroRecordingArray)
	{
		D(DBF_OakTextView_Macros, bug("%s\n", to_s(plist::convert((__bridge CFPropertyListRef)macroRecordingArray)).c_str()););
		[[NSUserDefaults standardUserDefaults] setObject:[macroRecordingArray copy] forKey:@"OakMacroManagerScratchMacro"];
		macroRecordingArray = nil;
	}
	else
	{
		macroRecordingArray = [NSMutableArray new];
	}
}

- (IBAction)playScratchMacro:(id)anArgument
{
	D(DBF_OakTextView_Macros, bug("%s\n", to_s(plist::convert((__bridge CFPropertyListRef)[[NSUserDefaults standardUserDefaults] arrayForKey:@"OakMacroManagerScratchMacro"])).c_str()););
	AUTO_REFRESH;
	if(NSArray* scratchMacro = [[NSUserDefaults standardUserDefaults] arrayForKey:@"OakMacroManagerScratchMacro"])
			documentView->macro_dispatch(plist::convert((__bridge CFDictionaryRef)@{ @"commands" : scratchMacro }), [self variables]);
	else	NSBeep();
}

- (IBAction)saveScratchMacro:(id)sender
{
	if(NSArray* scratchMacro = [[NSUserDefaults standardUserDefaults] arrayForKey:@"OakMacroManagerScratchMacro"])
	{
		bundles::item_ptr bundle;
		if([[BundlesManager sharedInstance] findBundleForInstall:&bundle])
		{
			oak::uuid_t uuid = oak::uuid_t().generate();

			plist::dictionary_t plist = plist::convert((__bridge CFDictionaryRef)@{ @"commands" : scratchMacro });
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

	D(DBF_OakTextView_Macros, bug("%s, %s\n", sel_getName(aSelector), [[anArgument description] UTF8String]););
	[macroRecordingArray addObject:[NSDictionary dictionaryWithObjectsAndKeys:NSStringFromSelector(aSelector), @"command", anArgument, @"argument", nil]];
}

// ================
// = Drop Support =
// ================

+ (NSArray*)dropTypes
{
	return @[ NSColorPboardType, NSFilenamesPboardType,
		@"WebURLsWithTitlesPboardType", (NSString*)kUTTypeURL, @"public.url-name", NSURLPboardType,
		@"public.plain-text" ];
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
	D(DBF_OakTextView_DragNDrop, bug("%s\n", [[someFiles description] UTF8String]););

	std::set<bundles::item_ptr> allHandlers;
	std::map<oak::uuid_t, std::vector<std::string> > handlerToFiles;

	scope::context_t scope = [self scopeContext];
	for(NSString* path in someFiles)
	{
		for(auto const& item : bundles::drag_commands_for_path(to_s(path), scope))
		{
			D(DBF_OakTextView_DragNDrop, bug("handler: %s\n", item->name_with_bundle().c_str()););
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
			D(DBF_OakTextView_DragNDrop, bug("insert as text: %s\n", [path UTF8String]););
			std::string const& content = path::content(to_s(path));
			if(!utf8::is_valid(content.begin(), content.end()))
				binary = true;
			else if(content.size() < SQ(1024) || NSAlertDefaultReturn == NSRunAlertPanel(@"Inserting Large File", @"The file “%@” has a size of %.1f MB. Are you sure you want to insert this as a text file?", @"Insert File", @"Cancel", nil, [path stringByAbbreviatingWithTildeInPath], content.size() / SQ(1024.0))) // larger than 1 MB?
				merged += content;
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
	else if(bundles::item_ptr handler = OakShowMenuForBundleItems(std::vector<bundles::item_ptr>(allHandlers.begin(), allHandlers.end()), [self positionForWindowUnderCaret]))
	{
		D(DBF_OakTextView_DragNDrop, bug("execute %s\n", handler->name_with_bundle().c_str()););

		static struct { NSUInteger mask; std::string name; } const qualNames[] =
		{
			{ NSShiftKeyMask,     "SHIFT"    },
			{ NSControlKeyMask,   "CONTROL"  },
			{ NSAlternateKeyMask, "OPTION"   },
			{ NSCommandKeyMask,   "COMMAND"  }
		};

		auto env = [self variablesForBundleItem:handler];
		auto const pwd = format_string::expand("${TM_DIRECTORY:-${TM_PROJECT_DIRECTORY:-$TMPDIR}}", env);

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

		AUTO_REFRESH;
		document::run(parse_drag_command(handler), *documentView, documentView->ranges(), document, env, pwd);
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
	else if([[info draggingPasteboard] availableTypeFromArray:@[ NSFilenamesPboardType ]])
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
	D(DBF_OakTextView_DragNDrop, bug("hidden: %s\n", BSTR([self isHiddenOrHasHiddenAncestor])););
	NSDragOperation flag = [self dragOperationForInfo:info];
	[self setDropMarkAtPoint:flag == NSDragOperationNone ? NSZeroPoint : [self convertPoint:[info draggingLocation] fromView:nil]];
	return flag;
}

- (NSDragOperation)draggingUpdated:(id <NSDraggingInfo>)info
{
	D(DBF_OakTextView_DragNDrop, bug("\n"););
	NSDragOperation flag = [self dragOperationForInfo:info];
	[self setDropMarkAtPoint:flag == NSDragOperationNone ? NSZeroPoint : [self convertPoint:[info draggingLocation] fromView:nil]];
	return flag;
}

- (void)draggingExited:(id <NSDraggingInfo>)info
{
	D(DBF_OakTextView_DragNDrop, bug("\n"););
	[self setDropMarkAtPoint:NSZeroPoint];
}

- (BOOL)performDragOperation:(id <NSDraggingInfo>)info
{
	D(DBF_OakTextView_DragNDrop, bug("\n"););
	ASSERT(dropPosition);
	AUTO_REFRESH;
	ng::index_t pos = dropPosition;
	documentView->set_drop_marker(dropPosition = ng::index_t());

	BOOL res = YES;
	NSPasteboard* pboard  = [info draggingPasteboard];
	NSArray* types        = [pboard types];
	NSString* type DB_VAR = [pboard availableTypeFromArray:[[self class] dropTypes]];
	BOOL shouldMove       = ([info draggingSource] == self) && ([info draggingSourceOperationMask] & NSDragOperationMove);
	BOOL shouldLink       = ([info draggingSource] != self) && ([info draggingSourceOperationMask] == NSDragOperationLink);

	D(DBF_OakTextView_DragNDrop, bug("local %s, should move %s, type %s, all types %s\n", BSTR([info draggingSource] == self), BSTR(shouldMove), [type UTF8String], [[types description] UTF8String]););

	NSArray* files = [pboard availableTypeFromArray:@[ NSFilenamesPboardType ]] ? [pboard propertyListForType:NSFilenamesPboardType] : nil;
	if(shouldLink && files)
	{
		std::vector<std::string> paths;
		for(NSString* path in files)
			paths.push_back(to_s(path));

		documentView->set_selections(ng::range_t(pos));
		documentView->insert(text::join(paths, "\n"));
	}
	else if(NSString* text = [pboard stringForType:[pboard availableTypeFromArray:@[ @"public.plain-text" ]]] ?: [pboard stringForType:NSStringPboardType])
	{
		D(DBF_OakTextView_DragNDrop, bug("plain text: %s\n", [text UTF8String]););
		if(shouldMove)
		{
			documentView->move_selection_to(pos);
		}
		else
		{
			std::string str = to_s(text);
			str.erase(text::convert_line_endings(str.begin(), str.end(), text::estimate_line_endings(str.begin(), str.end())), str.end());
			str.erase(utf8::remove_malformed(str.begin(), str.end()), str.end());

			documentView->set_selections(ng::range_t(pos));
			documentView->insert(str);
		}
	}
	else if(files)
	{
		documentView->set_selections(ng::range_t(pos));
		[self performSelector:@selector(dropFiles:) withObject:files afterDelay:0.05]; // we use “afterDelay” so that slow commands won’t trigger a timeout of the drop event
	}
	else
	{
		fprintf(stderr, "unknown drop: %s\n", [[types description] UTF8String]);
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
	D(DBF_OakTextView_MouseEvents, bug("drag: %s, column selection: %s\n", BSTR(showDragCursor), BSTR(showColumnSelectionCursor)););
	[self addCursorRect:[self visibleRect] cursor:showDragCursor ? [NSCursor arrowCursor] : (showColumnSelectionCursor ? [NSCursor crosshairCursor] : [self ibeamCursor])];
}

- (void)setShowDragCursor:(BOOL)flag
{
	if(flag != showDragCursor)
	{
		showDragCursor = flag;
		[[self window] invalidateCursorRectsForView:self];
	}
}

// =================
// = User Defaults =
// =================

- (void)userDefaultsDidChange:(id)sender
{
	self.antiAlias     = ![[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsDisableAntiAliasKey];
	self.fontSmoothing = (OTVFontSmoothing)[[NSUserDefaults standardUserDefaults] integerForKey:kUserDefaultsFontSmoothingKey];
	self.scrollPastEnd = [[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsScrollPastEndKey];
}

// =================
// = Mouse Support =
// =================

- (void)actOnMouseDown
{
	bool optionDown  = mouseDownModifierFlags & NSAlternateKeyMask;
	bool shiftDown   = mouseDownModifierFlags & NSShiftKeyMask;
	bool commandDown = mouseDownModifierFlags & NSCommandKeyMask;

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

	documentView->set_selections(s);
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
	if(currentModifierFlags & NSAlternateKeyMask)
		range.last().columnar = true;

	ng::ranges_t s = documentView->ranges();
	s.last() = range.last();
	documentView->set_selections(s);

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
	[srcImage drawAtPoint:NSZeroPoint fromRect:NSZeroRect operation:NSCompositeCopy fraction:0.5];
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
	D(DBF_OakTextView_MouseEvents, bug("%s\n", BSTR(res)););
	return res;
}

- (BOOL)shouldDelayWindowOrderingForEvent:(NSEvent*)anEvent
{
	BOOL res = [self isPointInSelection:[self convertPoint:[anEvent locationInWindow] fromView:nil]];
	D(DBF_OakTextView_MouseEvents, bug("%s\n", BSTR(res)););
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
	id dragDelayObj = [[NSUserDefaults standardUserDefaults] objectForKey:@"NSDragAndDropTextDelay"];
	return [dragDelayObj respondsToSelector:@selector(intValue)] ? [dragDelayObj intValue] : 150;
}

- (void)preparePotentialDrag:(NSEvent*)anEvent
{
	if([self dragDelay] != 0 && ([[self window] isKeyWindow] || ([anEvent modifierFlags] & NSCommandKeyMask)))
			self.initiateDragTimer = [OakTimer scheduledTimerWithTimeInterval:(0.001 * [self dragDelay]) target:self selector:@selector(changeToDragPointer:) repeats:NO];
	else	[self changeToDragPointer:nil];
	delayMouseDown = [[self window] isKeyWindow];
}

static scope::context_t add_modifiers_to_scope (scope::context_t scope, NSUInteger modifiers)
{
	static struct { NSUInteger modifier; char const* scope; } const map[] =
	{
		{ NSShiftKeyMask,      "dyn.modifier.shift"   },
		{ NSControlKeyMask,    "dyn.modifier.control" },
		{ NSAlternateKeyMask,  "dyn.modifier.option"  },
		{ NSCommandKeyMask,    "dyn.modifier.command" }
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
	id forceClickFlag = [[NSUserDefaults standardUserDefaults] objectForKey:@"com.apple.trackpad.forceClick"];
	if(forceClickFlag && ![forceClickFlag boolValue])
		return;

	static NSInteger oldStage = 0;
	if(oldStage < anEvent.stage && anEvent.stage == 2)
		[self quickLookWithEvent:anEvent];
	oldStage = anEvent.stage;
}

- (void)mouseDown:(NSEvent*)anEvent
{
	if([self.inputContext handleEvent:anEvent] || !documentView || [anEvent type] != NSLeftMouseDown || ignoreMouseDown)
		return (void)(ignoreMouseDown = NO);

	if(ng::range_t r = documentView->folded_range_at_point([self convertPoint:[anEvent locationInWindow] fromView:nil]))
	{
		AUTO_REFRESH;
		documentView->unfold(r.min().index, r.max().index);
		return;
	}

	if(macroRecordingArray && [anEvent type] == NSLeftMouseDown)
	{
		NSInteger choice = NSRunAlertPanel(@"You are recording a macro", @"While recording macros it is not possible to select text or reposition the caret using your mouse.", @"Continue", @"Stop Recording", nil);
		if(choice == NSAlertAlternateReturn) // "Stop Macro Recording"
			self.isMacroRecording = NO;
		return;
	}

	std::vector<bundles::item_ptr> const& items = bundles::query(bundles::kFieldSemanticClass, "callback.mouse-click", add_modifiers_to_scope(ng::scope(*documentView, documentView->index_at_point([self convertPoint:[anEvent locationInWindow] fromView:nil]), to_s([self scopeAttributes])), [anEvent modifierFlags]));
	if(!items.empty())
	{
		if(bundles::item_ptr item = OakShowMenuForBundleItems(items, [self positionForWindowUnderCaret]))
		{
			AUTO_REFRESH;
			documentView->set_selections(ng::range_t(documentView->index_at_point([self convertPoint:[anEvent locationInWindow] fromView:nil]).index));
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
		mouseDownModifierFlags &= ~NSCommandKeyMask;

	if(!(mouseDownModifierFlags & NSShiftKeyMask) && [self isPointInSelection:[self convertPoint:[anEvent locationInWindow] fromView:nil]] && [anEvent clickCount] == 1 && [self dragDelay] >= 0 && !([anEvent modifierFlags] & (NSShiftKeyMask | NSControlKeyMask | NSAlternateKeyMask | NSCommandKeyMask)))
			[self preparePotentialDrag:anEvent];
	else	[self actOnMouseDown];
}

- (void)mouseDragged:(NSEvent*)anEvent
{
	if([self.inputContext handleEvent:anEvent] || !documentView || macroRecordingArray)
		return;

	NSPoint mouseCurrentPos = [self convertPoint:[anEvent locationInWindow] fromView:nil];
	if(SQ(fabs(mouseDownPos.x - mouseCurrentPos.x)) + SQ(fabs(mouseDownPos.y - mouseCurrentPos.y)) < SQ(1))
		return; // we didn't even drag a pixel

	delayMouseDown = NO;
	if(showDragCursor)
	{
		[self startDragForEvent:anEvent];
	}
	else if(initiateDragTimer) // delayed reaction to mouseDown
	{
		self.initiateDragTimer = nil;

		AUTO_REFRESH;
		[self actOnMouseDown];
	}
	else
	{
		if(!dragScrollTimer && [self autoscroll:[NSApp currentEvent]] == YES)
			self.dragScrollTimer = [OakTimer scheduledTimerWithTimeInterval:(1.0/25.0) target:self selector:@selector(dragScrollTimerFired:) repeats:YES];

		AUTO_REFRESH;
		[self actOnMouseDragged:anEvent];
	}
}

- (void)dragScrollTimerFired:(id)sender
{
	AUTO_REFRESH;
	[self actOnMouseDragged:[NSApp currentEvent]];
}

- (void)mouseUp:(NSEvent*)anEvent
{
	if([self.inputContext handleEvent:anEvent] || !documentView || macroRecordingArray)
		return;

	AUTO_REFRESH;
	if(delayMouseDown)
		[self actOnMouseDown];
	delayMouseDown = NO;

	self.initiateDragTimer = nil;
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
		[[NSFontManager sharedFontManager] setSelectedFont:self.font isMultiple:NO];
		[self setShowLiveSearch:NO];
	}
	else
	{
		self.showColumnSelectionCursor = showDragCursor = NO;
		[[self window] invalidateCursorRectsForView:self];
	}

	if(documentView)
	{
		AUTO_REFRESH;
		documentView->set_draw_caret(doesHaveFocus);
		documentView->set_is_key(doesHaveFocus);
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
		crash_reporter_info_t info(text::format("Performing @selector(%s)\nC++ Exception: %s", sel_getName(aSelector), e.what()));
		abort();
	}
}

#define ACTION(NAME)      (void)NAME:(id)sender { [self handleAction:ng::to_action(#NAME ":") forSelector:@selector(NAME:)]; }
#define ALIAS(NAME, REAL) (void)NAME:(id)sender { [self handleAction:ng::to_action(#REAL ":") forSelector:@selector(REAL:)]; }

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
@end
