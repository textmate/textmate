#import "OakTextView.h"
#import "OakPasteboardWrapper.h"
#import "OakChoiceMenu.h"
#import "OakDocumentView.h" // addAuxiliaryView:atEdge: signature
#import "LiveSearchView.h"
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/NSEvent Additions.h>
#import <OakAppKit/NSColor Additions.h>
#import <OakAppKit/NSImage Additions.h>
#import <OakAppKit/NSMenuItem Additions.h>
#import <OakAppKit/OakPasteboard.h>
#import <OakAppKit/OakPopOutAnimation.h>
#import <OakAppKit/OakToolTip.h>
#import <OakFoundation/NSString Additions.h>
#import <OakFoundation/OakFindProtocol.h>
#import <OakFoundation/OakTimer.h>
#import <OakSystem/application.h>
#import <crash/info.h>
#import <BundleMenu/BundleMenu.h>
#import <BundlesManager/BundlesManager.h>
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

NSString* const kUserDefaultsFontSmoothingKey      = @"fontSmoothing";
NSString* const kUserDefaultsDisableAntiAliasKey   = @"disableAntiAlias";
NSString* const kUserDefaultsDisableTypingPairsKey = @"disableTypingPairs";
NSString* const kUserDefaultsScrollPastEndKey      = @"scrollPastEnd";

struct buffer_refresh_callback_t;

@interface OakTextView ()
{
	OBJC_WATCH_LEAKS(OakTextView);

	document::document_ptr document;
	theme_ptr theme;
	std::string fontName;
	CGFloat fontSize;
	ng::editor_ptr editor;
	std::shared_ptr<ng::layout_t> layout;
	NSUInteger refreshNestCount;
	buffer_refresh_callback_t* callback;

	int32_t wrapColumn;

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
	NSDate* optionDownDate;
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
}
+ (NSArray*)dropTypes;
- (void)ensureSelectionIsInVisibleArea:(id)sender;
- (NSPoint)positionForWindowUnderCaret;
- (void)toggleColumnSelection:(id)sender;
- (void)delete:(id)sender;
- (void)updateChoiceMenu:(id)sender;
- (void)resetBlinkCaretTimer;
- (void)reflectDocumentSize;
- (void)updateSelection;
- (void)updateMarkedRanges;
- (void)redisplayFrom:(size_t)from to:(size_t)to;
- (void)recordSelector:(SEL)aSelector withArgument:(id)anArgument;
- (NSImage*)imageForRanges:(ng::ranges_t const&)ranges imageRect:(NSRect*)outRect;
- (void)highlightRanges:(ng::ranges_t const&)ranges;
- (NSRange)nsRangeForRange:(ng::range_t const&)range;
- (ng::range_t)rangeForNSRange:(NSRange)nsRange;
@property (nonatomic, readonly) ng::ranges_t const& markedRanges;
@property (nonatomic) NSDate* optionDownDate;
@property (nonatomic) OakTimer* initiateDragTimer;
@property (nonatomic) OakTimer* dragScrollTimer;
@property (nonatomic) BOOL showDragCursor;
@property (nonatomic) BOOL showColumnSelectionCursor;
@property (nonatomic) OakChoiceMenu* choiceMenu;
@property (nonatomic) NSUInteger refreshNestCount;
@property (nonatomic) LiveSearchView* liveSearchView;
@property (nonatomic, copy) NSString* liveSearchString;
@property (nonatomic) ng::ranges_t const& liveSearchRanges;
@end

static std::vector<bundles::item_ptr> items_for_tab_expansion (ng::buffer_t const& buffer, ng::ranges_t const& ranges, std::string const& scopeAttributes, ng::range_t* range)
{
	size_t caret = ranges.last().min().index;
	size_t line  = buffer.convert(caret).line;
	size_t bol   = buffer.begin(line);

	bool lastWasWordChar           = false;
	std::string lastCharacterClass = ng::kCharacterClassUnknown;

	scope::scope_t const rightScope = ng::scope(buffer, ng::ranges_t(caret), scopeAttributes).right;
	for(size_t i = bol; i < caret; i += buffer[i].size())
	{
		// we don’t use text::is_word_char because that function treats underscores as word characters, which is undesired, see <issue://157>.
		bool isWordChar = CFCharacterSetIsLongCharacterMember(CFCharacterSetGetPredefined(kCFCharacterSetAlphaNumeric), utf8::to_ch(buffer[i]));
		std::string characterClass = character_class(buffer, i);

		if(i == bol || lastWasWordChar != isWordChar || lastCharacterClass != characterClass)
		{
			std::vector<bundles::item_ptr> const& items = bundles::query(bundles::kFieldTabTrigger, buffer.substr(i, caret), scope::context_t(ng::scope(buffer, ng::ranges_t(i), scopeAttributes).left, rightScope));
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
	iterate(range, rhs)
		lhs.push_back(*range);
	return lhs;
}

struct refresh_helper_t
{
	typedef std::shared_ptr<ng::layout_t> layout_ptr;

	refresh_helper_t (OakTextView* self, document::document_ptr document, ng::editor_ptr editor, layout_ptr theLayout) : _self(self), _document(document), _editor(editor), _layout(theLayout)
	{
		if(++_self.refreshNestCount == 1)
		{
			_document->open();

			_revision  = document->buffer().revision();
			_selection = editor->ranges();
			_document->undo_manager().begin_undo_group(_editor->ranges());
			if(layout_ptr layout = _layout.lock())
				layout->begin_refresh_cycle(merge(_editor->ranges(), [_self markedRanges]), [_self liveSearchRanges]);
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
			_document->undo_manager().end_undo_group(_editor->ranges());
			if(layout_ptr layout = _layout.lock())
			{
				if(_revision == _document->buffer().revision())
				{
					citerate(range, ng::highlight_ranges_for_movement(_document->buffer(), _selection, _editor->ranges()))
					{
						NSRect imageRect;
						NSImage* image = [_self imageForRanges:*range imageRect:&imageRect];
						imageRect = [_self convertRect:imageRect toView:nil];
						imageRect.origin = [[_self window] convertBaseToScreen:imageRect.origin];
						OakShowPopOutAnimation(imageRect, image);
					}
				}

				if(_revision != _document->buffer().revision() || _selection != _editor->ranges())
				{
					[_self updateMarkedRanges];
					[_self updateSelection];
				}

				auto damagedRects = layout->end_refresh_cycle(merge(_editor->ranges(), [_self markedRanges]), [_self visibleRect], [_self liveSearchRanges]);

				NSRect r = [[_self enclosingScrollView] documentVisibleRect];
				NSSize newSize = NSMakeSize(std::max(NSWidth(r), layout->width()), std::max(NSHeight(r), layout->height()));
				if(!NSEqualSizes([_self frame].size, newSize))
					[_self setFrameSize:newSize];

				NSView* gutterView = find_gutter_view([[_self enclosingScrollView] superview]);
				iterate(rect, damagedRects)
				{
					[_self setNeedsDisplayInRect:*rect];
					if(gutterView)
					{
						NSRect r = *rect;
						r.origin.x = 0;
						r.size.width = NSWidth([gutterView frame]);
						[gutterView setNeedsDisplayInRect:r];
					}
				}

				if(_revision != _document->buffer().revision() || _selection != _editor->ranges())
				{
					if(_revision != _document->buffer().revision()) // FIXME document_t needs to skip work in set_revision if nothing changed.
						_document->set_revision(_document->buffer().revision());

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
	size_t _revision;
	ng::editor_ptr _editor;
	ng::ranges_t _selection;
	std::weak_ptr<ng::layout_t> _layout;
};

#define AUTO_REFRESH refresh_helper_t _dummy(self, document, editor, layout)

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
	iterate(it, paths)
		*it = format_string::replace(*it, ".+", "'${0/'/'\\''/g}'");
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
- (NSString*)replaceString   { return [[OakPasteboard pasteboardWithName:NSReplacePboard] current].string; }

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
		OakShowToolTip([NSString stringWithFormat:format, classifier, aFindString, aNumber], [self.textView positionForWindowUnderCaret]);
}

- (void)didReplace:(NSUInteger)aNumber occurrencesOf:(NSString*)aFindString with:(NSString*)aReplacementString
{
	static NSString* const formatStrings[2][3] = {
		{ @"Nothing replaced (no occurrences of “%@”).", @"Replaced one occurrence of “%@”.", @"Replaced %2$ld occurrences of “%@”." },
		{ @"Nothing replaced (no matches for “%@”).",    @"Replaced one match of “%@”.",      @"Replaced %2$ld matches of “%@”."     }
	};
	NSString* format = formatStrings[(self.findOptions & find::regular_expression) ? 1 : 0][aNumber > 2 ? 2 : aNumber];
	OakShowToolTip([NSString stringWithFormat:format, aFindString, aNumber], [self.textView positionForWindowUnderCaret]);
}
@end

@implementation OakTextView
@synthesize initiateDragTimer, dragScrollTimer, optionDownDate, showColumnSelectionCursor, showDragCursor, choiceMenu;
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
	citerate(range, ranges)
		srcRect = NSUnionRect(srcRect, NSIntersectionRect(visibleRect, layout->rect_for_range(range->min().index, range->max().index)));

	NSBezierPath* clip = [NSBezierPath bezierPath];
	citerate(rect, layout->rects_for_ranges(ranges))
		[clip appendBezierPath:[NSBezierPath bezierPathWithRect:NSOffsetRect(*rect, -NSMinX(srcRect), -NSMinY(srcRect))]];

	NSImage* image = [[NSImage alloc] initWithSize:NSMakeSize(std::max<CGFloat>(NSWidth(srcRect), 1), std::max<CGFloat>(NSHeight(srcRect), 1))];
	[image setFlipped:[self isFlipped]];
	[image lockFocus];
	[clip addClip];

	CGContextRef context = (CGContextRef)[[NSGraphicsContext currentContext] graphicsPort];
	CGContextTranslateCTM(context, -NSMinX(srcRect), -NSMinY(srcRect));

	NSRectClip(srcRect);
	layout->draw(context, srcRect, [self isFlipped], false, ng::ranges_t(), ng::ranges_t(), false);

	[image unlockFocus];
	[image setFlipped:NO];

	if(outRect)
		*outRect = srcRect;

	return image;
}

- (void)highlightRanges:(ng::ranges_t const&)ranges
{
	if(ranges.empty())
		return;

	iterate(range, ranges)
		layout->remove_enclosing_folds(range->min().index, range->max().index);
	[self ensureSelectionIsInVisibleArea:self];

	citerate(range, ranges)
	{
		NSRect imageRect;
		NSImage* image = [self imageForRanges:*range imageRect:&imageRect];
		imageRect = [self convertRect:imageRect toView:nil];
		imageRect.origin = [[self window] convertBaseToScreen:imageRect.origin];
		OakShowPopOutAnimation(imageRect, image);
	}
}

- (void)scrollIndexToFirstVisible:(ng::index_t const&)visibleIndex
{
	if(layout && visibleIndex && visibleIndex.index < document->buffer().size())
	{
		layout->update_metrics(CGRectMake(0, CGRectGetMinY(layout->rect_at_index(visibleIndex)), CGFLOAT_MAX, NSHeight([self visibleRect])));
		[self reflectDocumentSize];

		CGRect rect = layout->rect_at_index(visibleIndex);
		if(CGRectGetMinX(rect) <= layout->margin().left)
			rect.origin.x = 0;
		if(CGRectGetMinY(rect) <= layout->margin().top)
			rect.origin.y = 0;
		rect.size = [self visibleRect].size;

		[self scrollRectToVisible:CGRectIntegral(rect)];
	}
}

- (void)setDocument:(document::document_ptr const&)aDocument
{
	if(document && aDocument && *document == *aDocument)
	{
		if(document->selection() != NULL_STR)
		{
			ng::ranges_t ranges = convert(document->buffer(), document->selection());
			editor->set_selections(ranges);
			iterate(range, ranges)
				layout->remove_enclosing_folds(range->min().index, range->max().index);

			[self ensureSelectionIsInVisibleArea:self];
			[self updateSelection];
		}
		[self resetBlinkCaretTimer];
		return;
	}

	if(editor)
	{
		document->buffer().remove_callback(callback);
		document->set_folded(layout->folded_as_string());
		document->set_visible_index(layout->index_at_point([self visibleRect].origin));

		delete callback;
		callback = NULL;

		delete editor->delegate();
		editor->set_delegate(NULL);

		editor.reset();
		layout.reset();

		self.choiceMenu = nil;
		choiceVector.clear();
	}

	if(document = aDocument)
	{
		settings_t const settings = settings_for_path(document->virtual_path(), document->file_type() + " " + to_s(self.scopeAttributes), path::parent(document->path()));

		editor = ng::editor_for_document(document);
		wrapColumn = settings.get(kSettingsWrapColumnKey, wrapColumn);
		layout = std::make_shared<ng::layout_t>(document->buffer(), theme, settings.get(kSettingsSoftWrapKey, false), self.scrollPastEnd, wrapColumn, document->folded());
		if(settings.get(kSettingsShowWrapColumnKey, false))
			layout->set_draw_wrap_column(true);

		BOOL hasFocus = (self.keyState & (OakViewViewIsFirstResponderMask|OakViewWindowIsKeyMask|OakViewApplicationIsActiveMask)) == (OakViewViewIsFirstResponderMask|OakViewWindowIsKeyMask|OakViewApplicationIsActiveMask);
		layout->set_is_key(hasFocus);

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

		editor->set_delegate(new textview_delegate_t(self));

		editor->set_clipboard(get_clipboard(NSGeneralPboard));
		editor->set_find_clipboard(get_clipboard(NSFindPboard));
		editor->set_replace_clipboard(get_clipboard(NSReplacePboard));

		ng::index_t visibleIndex = document->visible_index();
		if(document->selection() != NULL_STR)
		{
			ng::ranges_t ranges = convert(document->buffer(), document->selection());
			editor->set_selections(ranges);
			iterate(range, ranges)
				layout->remove_enclosing_folds(range->min().index, range->max().index);
		}

		[self reflectDocumentSize];
		[self updateSelection];

		if(visibleIndex && visibleIndex.index < document->buffer().size())
				[self scrollIndexToFirstVisible:visibleIndex];
		else	[self ensureSelectionIsInVisibleArea:self];

		document->buffer().add_callback(callback);

		[self resetBlinkCaretTimer];
		[self setNeedsDisplay:YES];
		NSAccessibilityPostNotification(self, NSAccessibilityValueChangedNotification);
	}
}

- (id)initWithFrame:(NSRect)aRect
{
	if(self = [super initWithFrame:aRect])
	{
		settings_t const& settings = settings_for_path();

		theme          = parse_theme(bundles::lookup(settings.get(kSettingsThemeKey, NULL_STR)));
		fontName       = settings.get(kSettingsFontNameKey, NULL_STR);
		fontSize       = settings.get(kSettingsFontSizeKey, 11);
		theme          = theme->copy_with_font_name_and_size(fontName, fontSize);

		_showInvisibles = settings.get(kSettingsShowInvisiblesKey, false);
		_scrollPastEnd  = [[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsScrollPastEndKey];
		_antiAlias      = ![[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsDisableAntiAliasKey];
		_fontSmoothing  = (OTVFontSmoothing)[[NSUserDefaults standardUserDefaults] integerForKey:kUserDefaultsFontSmoothingKey];

		spellingDotImage = [NSImage imageNamed:@"SpellingDot" inSameBundleAsClass:[self class]];
		foldingDotsImage = [NSImage imageNamed:@"FoldingDots" inSameBundleAsClass:[self class]];

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

	citerate(item, bundles::query(bundles::kFieldSemanticClass, "callback.document.will-save", [self scopeContext]))
		[self performBundleItem:*item];

	if(document && layout)
	{
		document->set_folded(layout->folded_as_string());
		document->set_visible_index(layout->index_at_point([self visibleRect].origin));
	}
}

- (void)documentDidSave:(NSNotification*)aNotification
{
	NSWindow* window = [[aNotification userInfo] objectForKey:@"window"];
	if(window != self.window)
		return;

	citerate(item, bundles::query(bundles::kFieldSemanticClass, "callback.document.did-save", [self scopeContext]))
		[self performBundleItem:*item];
}

- (void)reflectDocumentSize
{
	if(document && layout && [self enclosingScrollView])
	{
		NSRect r = [[self enclosingScrollView] documentVisibleRect];
		layout->set_viewport_size(r.size);
		NSSize newSize = NSMakeSize(std::max(NSWidth(r), layout->width()), std::max(NSHeight(r), layout->height()));
		if(!NSEqualSizes([self frame].size, newSize))
			[self setFrameSize:newSize];
	}
}

- (void)resizeWithOldSuperviewSize:(NSSize)oldBoundsSize
{
	if(document && layout)
			[self reflectDocumentSize];
	else	[super resizeWithOldSuperviewSize:oldBoundsSize];
}

- (void)centerSelectionInVisibleArea:(id)sender
{
	[self recordSelector:_cmd withArgument:nil];

	CGRect r = layout->rect_at_index(editor->ranges().last().last);
	CGFloat w = NSWidth([self visibleRect]), h = NSHeight([self visibleRect]);

	CGFloat x = r.origin.x < w ? 0 : r.origin.x - w/2;
	CGFloat y = oak::cap(NSMinY([self frame]), r.origin.y - (h-r.size.height)/2, NSHeight([self frame]) - h);

	[self scrollRectToVisible:CGRectMake(round(x), round(y), w, h)];
}

- (void)ensureSelectionIsInVisibleArea:(id)sender
{
	if(([NSEvent pressedMouseButtons] & 1) == 1) // User is drag-selecting
		return;

	ng::range_t range = editor->ranges().last();
	CGRect r = layout->rect_at_index(range.last);
	CGRect s = [self visibleRect];

	CGFloat x = NSMinX(s), w = NSWidth(s);
	CGFloat y = NSMinY(s), h = NSHeight(s);

	if(range.unanchored)
	{
		CGRect a = layout->rect_at_index(range.first);
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
	if(choiceVector == editor->choices())
		return;

	self.choiceMenu = nil;
	choiceVector    = editor->choices();

	if(!choiceVector.empty())
	{
		choiceMenu = [OakChoiceMenu new];
		choiceMenu.choices = (__bridge NSArray*)((CFArrayRef)cf::wrap(choiceVector));

		std::string const& currentChoice = editor->placeholder_content();
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

- (BOOL)acceptsFirstResponder       { return YES; }
- (BOOL)isFlipped                   { return YES; }
- (BOOL)isOpaque                    { return YES; }

- (void)redisplayFrom:(size_t)from to:(size_t)to
{
	AUTO_REFRESH;
	layout->did_update_scopes(from, to);
}

- (void)drawRect:(NSRect)aRect
{
	if(theme && theme->is_transparent())
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
		{
			if(CGImageRef res = CGImageMaskCreate(CGImageGetWidth(img), CGImageGetHeight(img), CGImageGetBitsPerComponent(img), CGImageGetBitsPerPixel(img), CGImageGetBytesPerRow(img), CGImageGetDataProvider(img), NULL, false))
				return res;
			NSLog(@"Unable to create CGImageMask (%zu × %zu) from CGImage", CGImageGetWidth(img), CGImageGetHeight(img));
		}
		else
		{
			NSLog(@"Unable to create CGImage (%.1f × %.1f) from %@", width, height, pdfImage);
		}
		return NULL;
	};

	layout->draw(ng::context_t(context, [spellingDotImage CGImageForProposedRect:NULL context:[NSGraphicsContext currentContext] hints:nil], foldingDotsFactory), aRect, [self isFlipped], self.showInvisibles, merge(editor->ranges(), [self markedRanges]), liveSearchRanges);
}

// ===============
// = NSTextInput =
// ===============

- (NSInteger)conversationIdentifier
{
	return (NSInteger)self;
}

// ==================
// = Accented input =
// ==================

- (NSRange)nsRangeForRange:(ng::range_t const&)range
{
	//TODO this and the next method could use some optimization using an interval tree
	//     similar to basic_tree_t for conversion between UTF-8 and UTF-16 indexes.
	//     Currently poor performance for large documents (O(N)) would then get to O(log(N))
	//     Also currently copy of whole text is created here, which is not optimal
	std::string const text = document->buffer().substr(0, range.max().index);
	char const* base = text.data();
	NSUInteger location = utf16::distance(base, base + range.min().index);
	NSUInteger length   = utf16::distance(base + range.min().index, base + range.max().index);
	return NSMakeRange(location, length);
}

- (ng::range_t)rangeForNSRange:(NSRange)nsRange
{
	std::string const text = editor->as_string();
	char const* base = text.data();
	ng::index_t from = utf16::advance(base, nsRange.location, base + text.size()) - base;
	ng::index_t to   = utf16::advance(base + from.index, nsRange.length, base + text.size()) - base;
	return ng::range_t(from, to);
}

- (void)setMarkedText:(id)aString selectedRange:(NSRange)aRange
{
	D(DBF_OakTextView_TextInput, bug("‘%s’ %s\n", to_s([aString description]).c_str(), [NSStringFromRange(aRange) UTF8String]););

	if(![aString isKindOfClass:[NSString class]])
	{
		if([aString respondsToSelector:@selector(string)])
			aString = [aString string];
		else if([aString respondsToSelector:@selector(description)])
			aString = [aString description];
		else
			aString = @"<ERROR>";
	}

	AUTO_REFRESH;
	if(!markedRanges.empty())
		editor->set_selections(markedRanges);
	markedRanges = ng::ranges_t();
	editor->insert(to_s([aString description]), true);
	if([aString length] != 0)
		markedRanges = editor->ranges();
	pendingMarkedRanges = markedRanges;

	ng::ranges_t sel;
	citerate(range, editor->ranges())
	{
		std::string const str = document->buffer().substr(range->min().index, range->max().index);
		char const* base = str.data();
		size_t from = utf16::advance(base, aRange.location, base + str.size()) - base;
		size_t to   = utf16::advance(base, aRange.location + aRange.length, base + str.size()) - base;
		sel.push_back(ng::range_t(range->min() + from, range->min() + to));
	}
	editor->set_selections(sel);
}

- (NSRange)selectedRange
{
	NSRange res = [self nsRangeForRange:editor->ranges().last()];
	D(DBF_OakTextView_TextInput, bug("%s\n", [NSStringFromRange(res) UTF8String]););
	return res;
}

- (NSRange)markedRange
{
	D(DBF_OakTextView_TextInput, bug("%s\n", to_s(markedRanges).c_str()););
	if(markedRanges.empty())
		return NSMakeRange(NSNotFound, 0);
	return [self nsRangeForRange:markedRanges.last()];
}

- (void)unmarkText
{
	D(DBF_OakTextView_TextInput, bug("\n"););
	AUTO_REFRESH;
	markedRanges = ng::ranges_t();
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
		[[NSInputManager currentInputManager] markedTextAbandoned:self];

	markedRanges = pendingMarkedRanges;
	pendingMarkedRanges = ng::ranges_t();
}

// =====================
// = Dictionary pop-up =
// =====================

- (NSString*)string
{
	D(DBF_OakTextView_TextInput, bug("\n"););
	return [NSString stringWithCxxString:editor->as_string()]; // While undocumented (<rdar://4178606>), this is required in Lion to work with the dictionary implementation (⌃⌘D)
}

- (NSUInteger)characterIndexForPoint:(NSPoint)thePoint
{
	NSPoint p = [self convertPoint:[[self window] convertScreenToBase:thePoint] fromView:nil];
	std::string const text = editor->as_string();
	size_t index = layout->index_at_point(p).index;
	D(DBF_OakTextView_TextInput, bug("%s → %zu\n", [NSStringFromPoint(thePoint) UTF8String], index););
	return utf16::distance(text.data(), text.data() + index);
}

- (NSAttributedString*)attributedSubstringFromRange:(NSRange)theRange
{
	ng::range_t const& r = [self rangeForNSRange:theRange];
	size_t from = r.min().index, to = r.max().index;

	CFMutableAttributedStringRef res = CFAttributedStringCreateMutable(kCFAllocatorDefault, 0);
	std::map<size_t, scope::scope_t> scopes = document->buffer().scopes(from, to);
	for(auto pair = scopes.begin(); pair != scopes.end(); )
	{
		styles_t const& styles = theme->styles_for_scope(pair->second);

		size_t i = from + pair->first;
		size_t j = ++pair != scopes.end() ? from + pair->first : to;

		CFMutableAttributedStringRef str = CFAttributedStringCreateMutable(kCFAllocatorDefault, 0);
		CFAttributedStringReplaceString(str, CFRangeMake(0, 0), cf::wrap(document->buffer().substr(i, j)));
		CFAttributedStringSetAttribute(str, CFRangeMake(0, CFAttributedStringGetLength(str)), kCTFontAttributeName, styles.font());
		CFAttributedStringSetAttribute(str, CFRangeMake(0, CFAttributedStringGetLength(str)), kCTForegroundColorAttributeName, styles.foreground());
		if(styles.underlined())
			CFAttributedStringSetAttribute(str, CFRangeMake(0, CFAttributedStringGetLength(str)), kCTUnderlineStyleAttributeName, cf::wrap(0x1|kCTUnderlinePatternSolid));
		CFAttributedStringReplaceAttributedString(res, CFRangeMake(CFAttributedStringGetLength(res), 0), str);
		CFRelease(str);
	}

	return (NSAttributedString*)CFBridgingRelease(res);
}

- (NSRect)firstRectForCharacterRange:(NSRange)theRange
{
	ng::range_t const& r = [self rangeForNSRange:theRange];
	NSRect rect = [self convertRect:layout->rect_at_index(r.min()) toView:nil];
	rect.origin = [[self window] convertBaseToScreen:rect.origin];
	D(DBF_OakTextView_TextInput, bug("%s → %s\n", [NSStringFromRange(theRange) UTF8String], [NSStringFromRect(rect) UTF8String]););
	return rect;
}

- (void)doCommandBySelector:(SEL)aSelector
{
	D(DBF_OakTextView_TextInput, bug("%s\n", sel_getName(aSelector)););
	AUTO_REFRESH;
	[self tryToPerform:aSelector with:self];
}

// =================
// = Accessibility =
// =================

- (BOOL)accessibilityIsIgnored
{
	return NO;
}

#define ATTR(attr) NSAccessibility##attr##Attribute
#define PATTR(attr) NSAccessibility##attr##ParameterizedAttribute
#define ATTREQ_(attribute_) [attribute isEqualToString:attribute_]
#define HANDLE_ATTR(attr) else if(ATTREQ_(ATTR(attr)))
#define HANDLE_PATTR(attr) else if(ATTREQ_(PATTR(attr)))

- (NSArray*)accessibilityAttributeNames
{
	static NSArray* attributes = nil;
	if(!attributes)
	{
		NSSet* set = [NSSet setWithArray:@[
			ATTR(Role),
			ATTR(Value),
			ATTR(InsertionPointLineNumber),
			ATTR(NumberOfCharacters),
			ATTR(SelectedText),
			ATTR(SelectedTextRange),
			ATTR(SelectedTextRanges),
			// ATTR(VisibleCharacterRange),
		]];

		attributes = [[set setByAddingObjectsFromArray:[super accessibilityAttributeNames]] allObjects];
	}
	return attributes;
}

- (id)accessibilityAttributeValue:(NSString*)attribute
{
	D(DBF_OakTextView_Accessibility, bug("%s\n", to_s(attribute).c_str()););
	id ret = nil;
	ng::buffer_t const& buffer = document->buffer();

	if(false) {
	} HANDLE_ATTR(Role) {
		ret = NSAccessibilityTextAreaRole;
	} HANDLE_ATTR(Value) {
		ret = [NSString stringWithCxxString:editor->as_string()];
	} HANDLE_ATTR(InsertionPointLineNumber) {
		ret = [NSNumber numberWithUnsignedLong:buffer.convert(editor->ranges().last().min().index).line];
	} HANDLE_ATTR(NumberOfCharacters) {
		ret = [NSNumber numberWithUnsignedInteger:[self nsRangeForRange:ng::range_t(0, buffer.size())].length];
	} HANDLE_ATTR(SelectedText) {
		ng::range_t const& selection = editor->ranges().last();
		std::string const& text = buffer.substr(selection.min().index, selection.max().index);
		ret = [NSString stringWithCxxString:text];
	} HANDLE_ATTR(SelectedTextRange) {
		ret = [NSValue valueWithRange:[self nsRangeForRange:editor->ranges().last()]];
	} HANDLE_ATTR(SelectedTextRanges) {
		ng::ranges_t const ranges = editor->ranges();
		NSMutableArray* nsRanges = [NSMutableArray arrayWithCapacity:ranges.size()];
		iterate(range, ranges)
			[nsRanges addObject:[NSValue valueWithRange:[self nsRangeForRange:(*range)]]];
		ret = nsRanges;
	// } HANDLE_ATTR(VisibleCharacterRange) { //TODO
	} else {
		ret = [super accessibilityAttributeValue:attribute];
	}
	return ret;
}

- (BOOL)accessibilityIsAttributeSettable:(NSString*)attribute
{
	NSArray* settable = @[ ATTR(Value), ATTR(SelectedText), ATTR(SelectedTextRange), ATTR(SelectedTextRanges) ];
	if([settable containsObject:attribute])
		return YES;
	return [super accessibilityIsAttributeSettable:attribute];
}

- (void)accessibilitySetValue:(id)value forAttribute:(NSString*)attribute
{
	D(DBF_OakTextView_Accessibility, bug("%s <- %s\n", to_s(attribute).c_str(), to_s([value description]).c_str()););
	if(false) {
	} HANDLE_ATTR(Value) {
		AUTO_REFRESH;
		document->set_content(to_s((NSString*)value));
	} HANDLE_ATTR(SelectedText) {
		AUTO_REFRESH;
		editor->insert(to_s((NSString*)value));
	} HANDLE_ATTR(SelectedTextRange) {
		[self accessibilitySetValue:@[ value ] forAttribute:NSAccessibilitySelectedTextRangesAttribute];
	} HANDLE_ATTR(SelectedTextRanges) {
		NSArray* nsRanges = (NSArray*)value;
		ng::ranges_t ranges;
		for(NSValue* nsRangeValue in nsRanges)
			ranges.push_back([self rangeForNSRange:[nsRangeValue rangeValue]]);
		AUTO_REFRESH;
		editor->set_selections(ranges);
	} else {
		[super accessibilitySetValue:value forAttribute:attribute];
	}
}

- (NSArray*)accessibilityParameterizedAttributeNames
{
	static NSArray* attributes = nil;
	if(!attributes)
	{
		NSSet* set = [NSSet setWithArray:@[
			PATTR(LineForIndex),
			PATTR(RangeForLine),
			PATTR(StringForRange),
			PATTR(RangeForPosition),
			PATTR(RangeForIndex),
			PATTR(BoundsForRange),
			// PATTR(RTFForRange),
			// PATTR(StyleRangeForIndex),
			// PATTR(AttributedStringForRange),
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
	} HANDLE_PATTR(LineForIndex) {
		size_t index = [((NSNumber*)parameter) unsignedLongValue];
		index = [self rangeForNSRange:NSMakeRange(index, 0)].min().index;
		text::pos_t pos = document->buffer().convert(index);
		ret = [NSNumber numberWithUnsignedLong:pos.line];
	} HANDLE_PATTR(RangeForLine) {
		size_t line = [((NSNumber*)parameter) unsignedLongValue];
		size_t begin = document->buffer().begin(line), end = document->buffer().end(line);
		ng::range_t const range(begin, end);
		ret = [NSValue valueWithRange:[self nsRangeForRange:range]];
	} HANDLE_PATTR(StringForRange) {
		ng::range_t range = [self rangeForNSRange:[((NSValue*)parameter) rangeValue]];
		ret = [NSString stringWithCxxString:editor->as_string(range.min().index, range.max().index)];
	} HANDLE_PATTR(RangeForPosition) {
		NSPoint point = [((NSValue*)parameter) pointValue];
		point = [[self window] convertScreenToBase:point];
		point = [self convertPoint:point fromView:nil];
		size_t index = layout->index_at_point(point).index;
		index = document->buffer().sanitize_index(index);
		size_t const length = document->buffer()[index].length();
		ret = [NSValue valueWithRange:[self nsRangeForRange:ng::range_t(index, index + length)]];
	} HANDLE_PATTR(RangeForIndex) {
		size_t index = [((NSNumber*)parameter) unsignedLongValue];
		index = [self rangeForNSRange:NSMakeRange(index, 0)].min().index;
		index = document->buffer().sanitize_index(index);
		size_t const length = document->buffer()[index].length();
		ret = [NSValue valueWithRange:[self nsRangeForRange:ng::range_t(index, index + length)]];
	} HANDLE_PATTR(BoundsForRange) {
		ng::range_t range = [self rangeForNSRange:[((NSValue*)parameter) rangeValue]];
		if(!range.empty()) // TODO ask accessibility-dev@lists.apple.com if there is a better approach for dealing with newlines
		{
			size_t const max = range.max().index;
			if(editor->as_string(max - 1, max) == "\n")
				range.max().index -= 1;
		}
		NSRect rect = layout->rect_for_range(range.min().index, range.max().index);
		rect = [self convertRect:rect toView:nil];
		rect = [[self window] convertRectToScreen:rect];
		ret = [NSValue valueWithRect:rect];
	// } HANDLE_PATTR(RTFForRange) { // TODO
	// } HANDLE_PATTR(StyleRangeForIndex) { // TODO
	// } HANDLE_PATTR(AttributedStringForRange) { // TODO
	} else {
		ret = [super accessibilityAttributeValue:attribute forParameter:parameter];
	}
	return ret;
}

#undef ATTR
#undef PATTR
#undef ATTREQ_
#undef HANDLE_ATTR
#undef HANDLE_PATTR

// ================
// = Bundle Items =
// ================

- (std::map<std::string, std::string>)variablesForBundleItem:(bundles::item_ptr const&)item
{
	std::map<std::string, std::string> res = oak::basic_environment();
	res << document->document_variables() << editor->editor_variables(to_s([self scopeAttributes]));
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
	crash_reporter_info_t info(text::format("%s %s", sel_getName(_cmd), item->full_name().c_str()));
	// D(DBF_OakTextView_BundleItems, bug("%s\n", anItem->full_name().c_str()););
	AUTO_REFRESH;
	switch(item->kind())
	{
		case bundles::kItemTypeSnippet:
		{
			[self recordSelector:@selector(insertSnippetWithOptions:) withArgument:ns::to_dictionary(item->plist())];
			editor->snippet_dispatch(item->plist(), [self variablesForBundleItem:item]);
		}
		break;

		case bundles::kItemTypeCommand:
		{
			[self recordSelector:@selector(executeCommandWithOptions:) withArgument:ns::to_dictionary(item->plist())];

			auto command = parse_command(item);
			if([self.delegate respondsToSelector:@selector(bundleItemPreExec:completionHandler:)])
			{
				[self.delegate bundleItemPreExec:command.pre_exec completionHandler:^(BOOL success){
					if(success)
					{
						AUTO_REFRESH;
						document::run(command, document->buffer(), editor->ranges(), document, [self variablesForBundleItem:item]);
					}
				}];
			}
			else
			{
				command.pre_exec = pre_exec::nop;
				document::run(command, document->buffer(), editor->ranges(), document, [self variablesForBundleItem:item]);
			}
		}
		break;

		case bundles::kItemTypeMacro:
		{
			[self recordSelector:@selector(playMacroWithOptions:) withArgument:ns::to_dictionary(item->plist())];
			editor->macro_dispatch(item->plist(), [self variablesForBundleItem:item]);
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
	citerate(item, bundles::query(bundles::kFieldSemanticClass, "callback.application.did-activate", [self scopeContext]))
		[self performBundleItem:*item];
}

- (void)applicationDidResignActiveNotification:(NSNotification*)aNotification
{
	citerate(item, bundles::query(bundles::kFieldSemanticClass, "callback.application.did-deactivate", [self scopeContext]))
		[self performBundleItem:*item];
}

// ============
// = Key Down =
// ============

static plist::dictionary_t KeyBindings;
static plist::dictionary_t const* KeyEventContext = &KeyBindings;

static plist::any_t normalize_potential_dictionary (plist::any_t const& action)
{
	if(plist::dictionary_t const* dict = boost::get<plist::dictionary_t>(&action))
	{
		plist::dictionary_t res;
		citerate(pair, *dict)
			res.insert(std::make_pair(ns::normalize_event_string(pair->first), normalize_potential_dictionary(pair->second)));
		return res;
	}
	return action;
}

typedef std::multimap<std::string, std::string> action_to_key_t;

static void update_menu_key_equivalents (NSMenu* menu, action_to_key_t const& actionToKey)
{
	for(NSMenuItem* item in [menu itemArray])
	{
		SEL action = [item action];
		action_to_key_t::const_iterator it = actionToKey.find(sel_getName(action));
		if(it != actionToKey.end() && [[item keyEquivalent] isEqualToString:@""])
			[item setKeyEquivalentCxxString:it->second];

		update_menu_key_equivalents([item submenu], actionToKey);
	}
}

+ (void)initialize
{
	static bool didLoad = false;
	if(!didLoad)
	{
		didLoad = true;

		static std::string const KeyBindingLocations[] =
		{
			oak::application_t::support("KeyBindings.dict"),
			oak::application_t::path("Contents/Resources/KeyBindings.dict"),
			path::join(path::home(), "Library/KeyBindings/DefaultKeyBinding.dict"),
			"/Library/KeyBindings/DefaultKeyBinding.dict",
			"/System/Library/Frameworks/AppKit.framework/Resources/StandardKeyBinding.dict",
		};

		iterate(path, KeyBindingLocations)
		{
			citerate(pair, plist::load(*path))
				KeyBindings.insert(std::make_pair(ns::normalize_event_string(pair->first), normalize_potential_dictionary(pair->second)));
		}

		action_to_key_t actionToKey;
		iterate(pair, KeyBindings)
		{
			if(std::string const* selector = boost::get<std::string>(&pair->second))
				actionToKey.insert(std::make_pair(*selector, pair->first));
		}

		update_menu_key_equivalents([NSApp mainMenu], actionToKey);

		[[NSUserDefaults standardUserDefaults] registerDefaults:@{
			kUserDefaultsFontSmoothingKey : @(OTVFontSmoothingDisabledForDarkHiDPI),
		}];
	}

	[NSApp registerServicesMenuSendTypes:@[ NSStringPboardType ] returnTypes:@[ NSStringPboardType ]];
}

// ======================
// = NSServicesRequests =
// ======================

- (id)validRequestorForSendType:(NSString*)sendType returnType:(NSString*)returnType
{
	if([sendType isEqual:NSStringPboardType] && [self hasSelection] && !macroRecordingArray)
		return self;
	if(!sendType && [returnType isEqual:NSStringPboardType] && !macroRecordingArray)
		return self;
	return [super validRequestorForSendType:sendType returnType:returnType];
}

- (BOOL)writeSelectionToPasteboard:(NSPasteboard*)pboard types:(NSArray*)types
{
	BOOL res = NO;
	if([self hasSelection] && [types containsObject:NSStringPboardType])
	{
		std::vector<std::string> v;
		ng::ranges_t const ranges = ng::dissect_columnar(document->buffer(), editor->ranges());
		iterate(range, ranges)
			v.push_back(document->buffer().substr(range->min().index, range->max().index));

		[pboard declareTypes:@[ NSStringPboardType ] owner:nil];
		res = [pboard setString:[NSString stringWithCxxString:text::join(v, "\n")] forType:NSStringPboardType];
	}
	return res;
}

- (BOOL)readSelectionFromPasteboard:(NSPasteboard*)pboard
{
	if(NSString* str = [pboard stringForType:[pboard availableTypeFromArray:@[ @"public.plain-text" ]]])
	{
		AUTO_REFRESH;
		editor->insert(to_s(str));
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
		KeyEventContext = &KeyBindings;
		[self doCommandBySelector:NSSelectorFromString([NSString stringWithCxxString:*selector])];
	}
	else if(plist::array_t const* actions = boost::get<plist::array_t>(&anAction))
	{
		KeyEventContext = &KeyBindings;
		std::vector<std::string> selectors;
		iterate(it, *actions)
		{
			if(std::string const* selector = boost::get<std::string>(&*it))
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
		KeyEventContext = nested;
	}
}

- (BOOL)performKeyEquivalent:(NSEvent*)anEvent
{
	BOOL hasFocus = (self.keyState & (OakViewViewIsFirstResponderMask|OakViewWindowIsKeyMask|OakViewApplicationIsActiveMask)) == (OakViewViewIsFirstResponderMask|OakViewWindowIsKeyMask|OakViewApplicationIsActiveMask);
	if(!hasFocus && ([[[self window] firstResponder] isKindOfClass:[self class]] || [[[self window] firstResponder] isKindOfClass:NSClassFromString(@"OakKeyEquivalentView")]))
		return NO;

	D(DBF_OakTextView_TextInput, bug("%s\n", [[anEvent description] UTF8String]););
	std::string const eventString = to_s(anEvent);

	if(KeyEventContext != &KeyBindings)
	{
		plist::dictionary_t::const_iterator pair = KeyEventContext->find(eventString);
		if(pair != KeyEventContext->end())
			return [self handleKeyBindingAction:pair->second], YES;
	}

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
		plist::dictionary_t::const_iterator pair = KeyEventContext->find(eventString);
		if(pair != KeyEventContext->end())
			return [self handleKeyBindingAction:pair->second], YES;
	}

	return NO;
}

- (void)interpretKeyEvents:(NSArray*)someEvents
{
	AUTO_REFRESH;
	if([self hasMarkedText])
		return [super interpretKeyEvents:someEvents];

	for(NSEvent* event in someEvents)
	{
		plist::dictionary_t::const_iterator pair = KeyEventContext->find(to_s(event));
		if(pair == KeyEventContext->end() && KeyEventContext != &KeyBindings)
		{
			KeyEventContext = &KeyBindings;
			pair = KeyEventContext->find(to_s(event));
		}

		if(pair == KeyEventContext->end())
				[super interpretKeyEvents:@[ event ]];
		else	[self handleKeyBindingAction:pair->second];
	}
}

- (void)oldKeyDown:(NSEvent*)anEvent
{
	std::vector<bundles::item_ptr> const& items = bundles::query(bundles::kFieldKeyEquivalent, to_s(anEvent), [self scopeContext]);
	if(bundles::item_ptr item = OakShowMenuForBundleItems(items, [self positionForWindowUnderCaret]))
		[self performBundleItem:item];
	else if(items.empty())
		[self interpretKeyEvents:@[ anEvent ]];

	[NSCursor setHiddenUntilMouseMoves:YES];
	[[NSNotificationCenter defaultCenter] postNotificationName:OakCursorDidHideNotification object:nil];
}

- (void)keyDown:(NSEvent*)anEvent
{
	D(DBF_OakTextView_TextInput, bug("%s\n", [[anEvent description] UTF8String]););
	crash_reporter_info_t info(text::format("%s %s", sel_getName(_cmd), to_s(anEvent).c_str()));
	AUTO_REFRESH;
	if(!choiceMenu)
		return [self oldKeyDown:anEvent];

	ng::range_t oldSelection;
	std::string oldContent = editor->placeholder_content(&oldSelection);
	std::string oldPrefix  = oldSelection ? oldContent.substr(0, oldSelection.min().index) : "";

	NSUInteger event = [choiceMenu didHandleKeyEvent:anEvent];
	if(event == OakChoiceMenuKeyUnused)
	{
		[self oldKeyDown:anEvent];

		ng::range_t newSelection;
		std::string const& newContent = editor->placeholder_content(&newSelection);
		std::string const newPrefix   = newSelection ? newContent.substr(0, newSelection.min().index) : "";

		std::vector<std::string> newChoices = editor->choices();
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
				editor->set_placeholder_content(newChoices[choiceIndex], newPrefix.size());
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
			editor->set_placeholder_content(choice, choice.find(oldPrefix) == 0 ? oldPrefix.size() : 0);
	}
	else
	{
		self.choiceMenu = nil;

		if(event != OakChoiceMenuKeyCancel)
		{
			editor->perform(ng::kInsertTab, layout.get(), [self continuousIndentCorrections], to_s([self scopeAttributes]));
			choiceVector.clear();
		}
	}
}

- (BOOL)hasSelection                     { return editor->has_selection(); }

- (void)flagsChanged:(NSEvent*)anEvent
{
	AUTO_REFRESH;
	NSInteger modifiers       = [anEvent modifierFlags] & (NSAlternateKeyMask | NSControlKeyMask | NSCommandKeyMask);
	BOOL isHoldingOption      = modifiers & NSAlternateKeyMask ? YES : NO;
	BOOL didPressOption       = modifiers == NSAlternateKeyMask;
	BOOL didReleaseOption     = modifiers == 0 && optionDownDate && [optionDownDate timeIntervalSinceNow] > -0.18;
	BOOL isSelectingWithMouse = ([NSEvent pressedMouseButtons] & 1) && editor->has_selection();

	D(DBF_OakTextView_TextInput, bug("press option %s, release option %s, is selecting with mouse %s\n", BSTR(didPressOption), BSTR(didReleaseOption), BSTR(isSelectingWithMouse)););
	self.showColumnSelectionCursor = isHoldingOption;
	self.optionDownDate            = nil;

	if(isSelectingWithMouse)
	{
		if(editor->ranges().last().columnar != isHoldingOption)
			[self toggleColumnSelection:self];
	}

	// this checks if the ‘flags changed’ is caused by left/right option — the virtual key codes aren’t documented anywhere and in theory they could correspond to other keys, but worst case user lose the ability to toggle column selection by single-clicking option
	if([anEvent keyCode] != 58 && [anEvent keyCode] != 61)
		return;

	if(didPressOption)
		self.optionDownDate = [NSDate date];
	else if(didReleaseOption)
		[self toggleColumnSelection:self];
}

- (void)insertText:(id)aString
{
	D(DBF_OakTextView_TextInput, bug("‘%s’, has marked %s\n", [[aString description] UTF8String], BSTR(!markedRanges.empty())););

	AUTO_REFRESH;
	if(!markedRanges.empty())
	{
		editor->set_selections(markedRanges);
		[self delete:nil];
		markedRanges = ng::ranges_t();
	}

	if(![aString isKindOfClass:[NSString class]])
	{
		if([aString respondsToSelector:@selector(string)])
			aString = [aString string];
		else if([aString respondsToSelector:@selector(description)])
			aString = [aString description];
		else
			aString = @"<ERROR>";
	}

	[self recordSelector:_cmd withArgument:[aString copy]];
	bool autoPairing = !macroRecordingArray && ![[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsDisableTypingPairsKey];
	editor->insert_with_pairing([aString UTF8String], [self continuousIndentCorrections], autoPairing, to_s([self scopeAttributes]));
}

- (IBAction)toggleCurrentFolding:(id)sender
{
	AUTO_REFRESH;
	if(editor->ranges().size() == 1 && !editor->ranges().last().empty() && !editor->ranges().last().columnar)
	{
		layout->fold(editor->ranges().last().min().index, editor->ranges().last().max().index);
	}
	else
	{
		size_t line = document->buffer().convert(editor->ranges().last().first.index).line;
		layout->toggle_fold_at_line(line, false);
	}
	[[NSNotificationCenter defaultCenter] postNotificationName:GVColumnDataSourceDidChange object:[[self enclosingScrollView] superview]];
}

- (IBAction)toggleFoldingAtLine:(NSUInteger)lineNumber recursive:(BOOL)flag
{
	AUTO_REFRESH;
	layout->toggle_fold_at_line(lineNumber, flag);
}

- (IBAction)takeLevelToFoldFrom:(id)sender
{
	AUTO_REFRESH;
	layout->toggle_all_folds_at_level([sender tag]);
	[[NSNotificationCenter defaultCenter] postNotificationName:GVColumnDataSourceDidChange object:[[self enclosingScrollView] superview]];
}

- (NSPoint)positionForWindowUnderCaret
{
	CGRect r1 = layout->rect_at_index(editor->ranges().last().first);
	CGRect r2 = layout->rect_at_index(editor->ranges().last().last);
	CGRect r = r1.origin.y == r2.origin.y && r1.origin.x < r2.origin.x ? r1 : r2;
	NSPoint p = NSMakePoint(CGRectGetMinX(r), CGRectGetMaxY(r)+4);
	if(NSPointInRect(p, [self visibleRect]))
			{ p = [[self window] convertBaseToScreen:[self convertPoint:p toView:nil]]; }
	else	{ p = [NSEvent mouseLocation]; p.y -= 16; }

	return p;
}

- (NSString*)selectAndReturnMisspelledWordAtIndex:(size_t)currnetIndex
{
	AUTO_REFRESH;
	NSString* word = nil;
	ng::buffer_t const& buf = document->buffer();
	if(!editor->has_selection())
	{
		ng::range_t wordRange = ng::extend(buf, ng::index_t(currnetIndex), kSelectionExtendToWord).last();
		if(ns::is_misspelled(buf.substr(wordRange.min().index, wordRange.max().index), buf.spelling_language(), buf.spelling_tag()))
		{
			editor->set_selections(wordRange);
			word = [NSString stringWithCxxString:buf.substr(wordRange.min().index, wordRange.max().index)];
		}
	}
	else
	{
		ng::ranges_t ranges = editor->ranges();
		if(ranges.size() == 1)
		{
			std::string const str = buf.substr(ranges.first().min().index, ranges.first().max().index);
			if(str.find_first_of(" \n\t") == std::string::npos && ns::is_misspelled(str, document->buffer().spelling_language(), document->buffer().spelling_tag()))
				word = [NSString stringWithCxxString:str];
		}
	}
	return word;
}

- (NSMenu*)contextMenuWithMisspelledWord:(NSString*)aWord
{
	NSMenu* menu = [[NSMenu alloc] initWithTitle:@""];
	NSMenuItem* item = nil;

	if(aWord)
	{
		char key = 0;
		[[NSSpellChecker sharedSpellChecker] updateSpellingPanelWithMisspelledWord:aWord];
		for(NSString* guess in [[NSSpellChecker sharedSpellChecker] guessesForWord:aWord])
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
		[item setRepresentedObject:aWord];
		item = [menu addItemWithTitle:@"Learn Spelling" action:@selector(contextMenuPerformLearnSpelling:) keyEquivalent:@"="];
		[item setKeyEquivalentModifierMask:0];
		[item setRepresentedObject:aWord];
		item = [menu addItemWithTitle:@"Show Next" action:@selector(checkSpelling:) keyEquivalent:@";"];
		[item setRepresentedObject:aWord];
		[menu addItem:[NSMenuItem separatorItem]];
	}

	static struct { NSString* title; SEL action; } const items[] =
	{
		{ @"Cut",                     @selector(cut:)                           },
		{ @"Copy",                    @selector(copy:)                          },
		{ @"Paste",                   @selector(paste:)                         },
		{ nil,                        nil                                       },
		{ @"Fold/Unfold",             @selector(toggleCurrentFolding:)          },
		{ @"Filter Through Command…", @selector(orderFrontRunCommandWindow:)    },
	};

	for(size_t i = 0; i < sizeofA(items); i++)
	{
		if(items[i].title)
				[menu addItemWithTitle:items[i].title action:items[i].action keyEquivalent:@""];
		else	[menu addItem:[NSMenuItem separatorItem]];
	}
	return menu;
}

- (NSMenu*)menuForEvent:(NSEvent*)anEvent
{
	NSPoint point = [self convertPoint:[anEvent locationInWindow] fromView:nil];
	ng::index_t const& click = layout->index_at_point(point);
	return [self contextMenuWithMisspelledWord:[self selectAndReturnMisspelledWordAtIndex:click.index]];
}

- (void)showContextMenu:(id)sender
{
	NSString* word = [self selectAndReturnMisspelledWordAtIndex:editor->ranges().last().last.index];
	NSMenu* menu = [self contextMenuWithMisspelledWord:word];

	NSWindow* win = [self window];
	NSEvent* anEvent = [NSApp currentEvent];
	NSEvent* fakeEvent = [NSEvent
		mouseEventWithType:NSLeftMouseDown
		location:[win convertScreenToBase:[self positionForWindowUnderCaret]]
		modifierFlags:0
		timestamp:[anEvent timestamp]
		windowNumber:[win windowNumber]
		context:[anEvent context]
		eventNumber:0
		clickCount:1
		pressure:1];

	[NSMenu popUpContextMenu:menu withEvent:fakeEvent forView:self];
	[win performSelector:@selector(invalidateCursorRectsForView:) withObject:self afterDelay:0.0]; // with option used as modifier, the cross-hair cursor will stick
}

- (void)contextMenuPerformCorrectWord:(NSMenuItem*)menuItem
{
	D(DBF_OakTextView_Spelling, bug("%s\n", [[menuItem representedObject] UTF8String]););
	AUTO_REFRESH;
	editor->insert(to_s((NSString*)[menuItem representedObject]));
	if([NSSpellChecker sharedSpellCheckerExists])
		[[NSSpellChecker sharedSpellChecker] updateSpellingPanelWithMisspelledWord:[menuItem representedObject]];
}

- (void)contextMenuPerformIgnoreSpelling:(id)sender
{
	D(DBF_OakTextView_Spelling, bug("%s\n", [[sender representedObject] UTF8String]););
	[[NSSpellChecker sharedSpellChecker] ignoreWord:[sender representedObject] inSpellDocumentWithTag:document->buffer().spelling_tag()];

	document->buffer().recheck_spelling(0, document->buffer().size());
	[self setNeedsDisplay:YES];
}

- (void)contextMenuPerformLearnSpelling:(id)sender
{
	D(DBF_OakTextView_Spelling, bug("%s\n", [[sender representedObject] UTF8String]););
	[[NSSpellChecker sharedSpellChecker] learnWord:[sender representedObject]];

	document->buffer().recheck_spelling(0, document->buffer().size());
	[self setNeedsDisplay:YES];
}

// =========================
// = Find Protocol: Client =
// =========================

- (void)performFindOperation:(id <OakFindServerProtocol>)aFindServer
{
	if(![aFindServer isKindOfClass:[OakTextViewFindServer class]])
	{
		NSMutableDictionary* dict = [NSMutableDictionary dictionary];

		dict[@"findString"] = aFindServer.findString;

		static find_operation_t const replaceActions[] = { kFindOperationReplaceAll, kFindOperationReplaceAllInSelection, kFindOperationReplace, kFindOperationReplaceAndFind };
		if(oak::contains(std::begin(replaceActions), std::end(replaceActions), aFindServer.findOperation))
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
		if(NSDictionary* captures = [OakPasteboard pasteboardWithName:NSReplacePboard].auxiliaryOptionsForCurrent)
		{
			std::map<std::string, std::string> variables;
			for(NSString* key in [captures allKeys])
				variables.insert(std::make_pair(to_s(key), to_s((NSString*)captures[key])));
			replacement = format_string::expand(replacement, variables);
		}
		editor->insert(replacement, true);

		if(findOperation == kFindOperationReplaceAndFind)
			findOperation = kFindOperationFind;
	}

	bool onlyInSelection = false;
	switch(findOperation)
	{
		case kFindOperationFindInSelection:
		case kFindOperationCountInSelection: onlyInSelection = editor->has_selection();
		case kFindOperationFind:
		case kFindOperationCount:
		{
			[OakPasteboard pasteboardWithName:NSReplacePboard].auxiliaryOptionsForCurrent = nil;
			bool isCounting = findOperation == kFindOperationCount || findOperation == kFindOperationCountInSelection;

			std::string const findStr = to_s(aFindServer.findString);
			find::options_t options   = aFindServer.findOptions;

			NSArray* documents = [[OakPasteboard pasteboardWithName:NSFindPboard].auxiliaryOptionsForCurrent objectForKey:@"documents"];
			if(documents && [documents count] > 1)
				options &= ~find::wrap_around;

			bool didWrap = false;
			auto allMatches = ng::find(document->buffer(), editor->ranges(), findStr, options, onlyInSelection ? editor->ranges() : ng::ranges_t(), &didWrap);

			ng::ranges_t res;
			std::transform(allMatches.begin(), allMatches.end(), std::back_inserter(res), [](decltype(allMatches)::value_type const& p){ return p.first; });
			if(onlyInSelection && res.sorted() == editor->ranges().sorted())
			{
				res = ng::ranges_t();
				allMatches = ng::find(document->buffer(), editor->ranges(), findStr, options, ng::ranges_t());
				std::transform(allMatches.begin(), allMatches.end(), std::back_inserter(res), [](decltype(allMatches)::value_type const& p){ return p.first; });
			}

			if(res.empty() && !isCounting && documents && [documents count] > 1)
			{
				for(NSUInteger i = 0; i < [documents count]; ++i)
				{
					NSString* uuid = [[documents objectAtIndex:i] objectForKey:@"identifier"];
					if(uuid && oak::uuid_t(to_s(uuid)) == document->identifier())
					{
						NSDictionary* info = [documents objectAtIndex:(i + ((options & find::backwards) ? [documents count] - 1 : 1)) % [documents count]];
						document::document_ptr doc;
						if(NSString* path = [info objectForKey:@"path"])
							doc = document::create(to_s(path));
						else if(NSString* identifier = [info objectForKey:@"identifier"])
							doc = document::find(to_s(identifier));

						if(doc)
						{
							NSString* range = [info objectForKey:(options & find::backwards) ? @"lastMatchRange" : @"firstMatchRange"];
							document::show(doc, document::kCollectionAny, to_s(range));
							return;
						}
					}
				}
			}

			if(isCounting)
			{
				[aFindServer didFind:res.size() occurrencesOf:aFindServer.findString atPosition:res.size() == 1 ? document->buffer().convert(res.last().min().index) : text::pos_t::undefined wrapped:NO];
			}
			else
			{
				std::set<ng::range_t> alreadySelected;
				citerate(range, editor->ranges())
					alreadySelected.insert(*range);

				ng::ranges_t newSelection;
				for(auto range : res)
				{
					if(alreadySelected.find(range.sorted()) == alreadySelected.end())
						newSelection.push_back(range.sorted());
				}

				if(!res.empty())
				{
					editor->set_selections(res);
					if(res.size() == 1 && (options & find::regular_expression))
					{
						NSMutableDictionary* captures = [NSMutableDictionary dictionary];
						for(auto pair : allMatches[res.last()])
							captures[[NSString stringWithCxxString:pair.first]] = [NSString stringWithCxxString:pair.second];
						[OakPasteboard pasteboardWithName:NSReplacePboard].auxiliaryOptionsForCurrent = captures;
					}
				}

				[self highlightRanges:newSelection];
				[aFindServer didFind:newSelection.size() occurrencesOf:aFindServer.findString atPosition:res.size() == 1 ? document->buffer().convert(res.last().min().index) : text::pos_t::undefined wrapped:didWrap];
			}
		}
		break;

		case kFindOperationReplaceAll:
		case kFindOperationReplaceAllInSelection:
		{
			std::string const findStr    = to_s(aFindServer.findString);
			std::string const replaceStr = to_s(aFindServer.replaceString);
			find::options_t options      = aFindServer.findOptions;

			ng::ranges_t const res = editor->replace_all(findStr, replaceStr, options, findOperation == kFindOperationReplaceAllInSelection);
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
	OakDocumentView* documentView = (OakDocumentView*)[[self enclosingScrollView] superview];
	if(flag)
	{
		liveSearchAnchor = editor->ranges();

		if(!self.liveSearchView)
		{
			self.liveSearchView = [[LiveSearchView alloc] initWithFrame:NSZeroRect];
			[documentView addAuxiliaryView:self.liveSearchView atEdge:NSMinYEdge];
			self.liveSearchView.nextResponder = self;
		}

		NSTextField* textField = self.liveSearchView.textField;
		[textField setDelegate:self];
		[textField setStringValue:self.liveSearchString ?: @""];

		[[self window] makeFirstResponder:textField];
	}
	else if(self.liveSearchView)
	{
		[documentView removeAuxiliaryView:self.liveSearchView];
		[[self window] makeFirstResponder:self];
		self.liveSearchView = nil;
		liveSearchRanges = ng::ranges_t();
	}
}

- (void)setLiveSearchRanges:(ng::ranges_t const&)ranges
{
	AUTO_REFRESH;

	ng::ranges_t const oldRanges = ng::move(document->buffer(), liveSearchRanges, kSelectionMoveToBeginOfSelection);
	liveSearchRanges = ranges;
	if(!liveSearchRanges.empty())
	{
		editor->set_selections(liveSearchRanges);
		if(oldRanges != ng::move(document->buffer(), liveSearchRanges, kSelectionMoveToBeginOfSelection))
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
	citerate(pair, ng::find(document->buffer(), liveSearchAnchor, to_s(liveSearchString), find::ignore_case|find::ignore_whitespace|find::wrap_around))
		res.push_back(pair->first);
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

- (IBAction)findNext:(id)sender
{
	if(self.liveSearchView)
	{
		ng::ranges_t tmp;
		citerate(pair, ng::find(document->buffer(), ng::move(document->buffer(), liveSearchRanges.empty() ? liveSearchAnchor : liveSearchRanges, kSelectionMoveToEndOfSelection), to_s(liveSearchString), find::ignore_case|find::ignore_whitespace))
			tmp.push_back(pair->first);
		[self setLiveSearchRanges:tmp];
		if(!tmp.empty())
			liveSearchAnchor = ng::move(document->buffer(), tmp, kSelectionMoveToBeginOfSelection);
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
		citerate(pair, ng::find(document->buffer(), ng::move(document->buffer(), liveSearchRanges.empty() ? liveSearchAnchor : liveSearchRanges, kSelectionMoveToBeginOfSelection), to_s(liveSearchString), find::backwards|find::ignore_case|find::ignore_whitespace))
			tmp.push_back(pair->first);
		[self setLiveSearchRanges:tmp];
		if(!tmp.empty())
			liveSearchAnchor = ng::move(document->buffer(), tmp, kSelectionMoveToBeginOfSelection);
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
	editor->snippet_dispatch(plist::convert((__bridge CFDictionaryRef)someOptions), [self variables]);
}

- (void)undo:(id)anArgument // MACRO?
{
	AUTO_REFRESH;
	if(!document->undo_manager().can_undo())
		return;
	editor->clear_snippets();
	editor->set_selections(document->undo_manager().undo());
}

- (void)redo:(id)anArgument // MACRO?
{
	AUTO_REFRESH;
	if(!document->undo_manager().can_redo())
		return;
	editor->clear_snippets();
	editor->set_selections(document->undo_manager().redo());
}

- (BOOL)expandTabTrigger:(id)sender
{
	if(editor->disallow_tab_expansion())
		return NO;

	AUTO_REFRESH;
	ng::range_t range;
	std::vector<bundles::item_ptr> const& items = items_for_tab_expansion(document->buffer(), editor->ranges(), to_s([self scopeAttributes]), &range);
	if(bundles::item_ptr item = OakShowMenuForBundleItems(items, [self positionForWindowUnderCaret]))
	{
		[self recordSelector:@selector(deleteTabTrigger:) withArgument:[NSString stringWithCxxString:editor->as_string(range.first.index, range.last.index)]];
		editor->delete_tab_trigger(editor->as_string(range.first.index, range.last.index));
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
		editor->perform(ng::kInsertTab, layout.get(), [self continuousIndentCorrections], to_s([self scopeAttributes]));
	}
}

// FIXME copy/paste from bundles/src/query.cc
static std::string format_bundle_item_title (std::string title, bool hasSelection)
{
	static std::string const kSelectionSubString = " / Selection";

	std::string::size_type pos = title.find(kSelectionSubString);
	if(pos == 0 || pos == std::string::npos)
		return title;

	if(hasSelection)
	{
		std::string::size_type from = title.rfind(' ', pos - 1);
		if(from == std::string::npos)
			return title.erase(0, pos + 3);
		return title.erase(from + 1, pos + 3 - from - 1);
	}
	return title.erase(pos, kSelectionSubString.size());
}

static char const* kOakMenuItemTitle = "OakMenuItemTitle";

- (BOOL)validateMenuItem:(NSMenuItem*)aMenuItem
{
	NSString* title = objc_getAssociatedObject(aMenuItem, kOakMenuItemTitle) ?: aMenuItem.title;
	objc_setAssociatedObject(aMenuItem, kOakMenuItemTitle, title, OBJC_ASSOCIATION_RETAIN);
	[aMenuItem updateTitle:[NSString stringWithCxxString:format_bundle_item_title(to_s(title), [self hasSelection])]];

	if([aMenuItem action] == @selector(cut:))
		[aMenuItem setTitle:@"Cut"];
	else if([aMenuItem action] == @selector(copy:))
		[aMenuItem setTitle:@"Copy"];

	static auto const RequiresSelection = new std::set<SEL>{ @selector(cut:), @selector(copy:), @selector(delete:), @selector(copySelectionToFindPboard:) };
	if(RequiresSelection->find([aMenuItem action]) != RequiresSelection->end())
		return [self hasSelection];
	else if([aMenuItem action] == @selector(toggleShowInvisibles:))
		[aMenuItem setState:self.showInvisibles ? NSOnState : NSOffState];
	else if([aMenuItem action] == @selector(toggleSoftWrap:))
		[aMenuItem setState:self.softWrap ? NSOnState : NSOffState];
	else if([aMenuItem action] == @selector(toggleScrollPastEnd:))
		[aMenuItem setState:self.scrollPastEnd ? NSOnState : NSOffState];
	else if([aMenuItem action] == @selector(toggleShowWrapColumn:))
		[aMenuItem setState:(layout && layout->draw_wrap_column()) ? NSOnState : NSOffState];
	else if([aMenuItem action] == @selector(toggleContinuousSpellChecking:))
		[aMenuItem setState:document->buffer().live_spelling() ? NSOnState : NSOffState];
	else if([aMenuItem action] == @selector(takeSpellingLanguageFrom:))
		[aMenuItem setState:[[NSString stringWithCxxString:document->buffer().spelling_language()] isEqualToString:[aMenuItem representedObject]] ? NSOnState : NSOffState];
	else if([aMenuItem action] == @selector(takeWrapColumnFrom:))
	{
		static std::set<NSInteger> const Presets = { NSWrapColumnWindowWidth, 40, 80 };
		[aMenuItem setState:wrapColumn == [aMenuItem tag] ? NSOnState : NSOffState];
		if([aMenuItem tag] == NSWrapColumnAskUser)
		{
			bool custom = Presets.find(wrapColumn) == Presets.end();
			[aMenuItem setTitle:custom ? [NSString stringWithFormat:@"Other (%d)…", wrapColumn] : @"Other…"];
			[aMenuItem setState:custom ? NSOnState : NSOffState];
		}
	}
	else if([aMenuItem action] == @selector(undo:))
	{
		[aMenuItem setTitle:@"Undo"];
		return document->undo_manager().can_undo();
	}
	else if([aMenuItem action] == @selector(redo:))
	{
		[aMenuItem setTitle:@"Redo"];
		return document->undo_manager().can_redo();
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
	if(hasFocus && layout)
	{
		AUTO_REFRESH;
		layout->set_draw_caret(true);
		hideCaret = NO;

		self.blinkCaretTimer = [NSTimer scheduledTimerWithTimeInterval:[NSEvent caretBlinkInterval] target:self selector:@selector(toggleCaretVisibility:) userInfo:nil repeats:YES];
	}
}

- (void)toggleCaretVisibility:(id)sender
{
	if(!layout)
		return;

	AUTO_REFRESH;
	layout->set_draw_caret(hideCaret);
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
- (size_t)tabSize             { return document ? document->indent().tab_size() : 2; }
- (BOOL)softTabs              { return document ? document->indent().soft_tabs() : NO; }
- (BOOL)softWrap              { return layout && layout->wrapping(); }

- (BOOL)continuousIndentCorrections
{
	return !plist::is_true(bundles::value_for_setting("disableIndentCorrections", [self scopeContext]));
}

- (void)setTheme:(theme_ptr const&)newTheme
{
	theme = newTheme;
	if(layout)
	{
		AUTO_REFRESH;
		layout->set_theme(newTheme);
	}
}

- (void)setFont:(NSFont*)newFont
{
	fontName = to_s([newFont fontName]);
	fontSize = [newFont pointSize];

	if(layout)
	{
		AUTO_REFRESH;
		ng::index_t visibleIndex = layout->index_at_point([self visibleRect].origin);
		layout->set_font(fontName, fontSize);
		[self scrollIndexToFirstVisible:document->buffer().begin(document->buffer().convert(visibleIndex.index).line)];
	}
}

- (void)setTabSize:(size_t)newTabSize
{
	AUTO_REFRESH;
	if(document)
	{
		text::indent_t tmp = document->indent();
		tmp.set_tab_size(newTabSize);
		document->set_indent(tmp);
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
	if(layout)
	{
		AUTO_REFRESH;
		layout->set_scroll_past_end(flag);
	}
}

- (void)setSoftWrap:(BOOL)flag
{
	if(!layout || layout->wrapping() == flag)
		return;

	AUTO_REFRESH;
	ng::index_t visibleIndex = layout->index_at_point([self visibleRect].origin);
	layout->set_wrapping(flag, wrapColumn);
	[self scrollIndexToFirstVisible:document->buffer().begin(document->buffer().convert(visibleIndex.index).line)];
	settings_t::set(kSettingsSoftWrapKey, (bool)flag, document->file_type());
}

- (void)setSoftTabs:(BOOL)flag
{
	if(flag != self.softTabs)
	{
		text::indent_t tmp = document->indent();
		tmp.set_soft_tabs(flag);
		document->set_indent(tmp);
	}
}

- (void)setWrapColumn:(NSInteger)newWrapColumn
{
	if(wrapColumn == newWrapColumn)
		return;

	wrapColumn = newWrapColumn;
	settings_t::set(kSettingsWrapColumnKey, wrapColumn);
	if(layout)
	{
		AUTO_REFRESH;
		layout->set_wrapping(self.softWrap, wrapColumn);
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

- (BOOL)hasMultiLineSelection { return multiline(document->buffer(), editor->ranges()); }

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
	if(layout)
	{
		AUTO_REFRESH;
		bool flag = !layout->draw_wrap_column();
		layout->set_draw_wrap_column(flag);
		settings_t::set(kSettingsShowWrapColumnKey, flag);
	}
}

- (void)checkSpelling:(id)sender
{
	NSSpellChecker* speller = [NSSpellChecker sharedSpellChecker];
	ng::buffer_t& buf = document->buffer();

	if(!buf.live_spelling())
	{
		buf.set_live_spelling(true);
		[self setNeedsDisplay:YES];
	}

	auto nextMisspelling = buf.next_misspelling(editor->ranges().last().last.index);
	if(nextMisspelling.first != nextMisspelling.second)
	{
		{
			AUTO_REFRESH;
			editor->set_selections(ng::range_t(nextMisspelling.first, nextMisspelling.second));
		}

		NSString* word = [NSString stringWithCxxString:buf.substr(nextMisspelling.first, nextMisspelling.second)];
		[speller updateSpellingPanelWithMisspelledWord:word];

		if(![[speller spellingPanel] isVisible])
			[self showContextMenu:sender];
	}
	else
	{
		[speller updateSpellingPanelWithMisspelledWord:@""];
	}
}

- (void)toggleContinuousSpellChecking:(id)sender
{
	bool flag = !document->buffer().live_spelling();
	document->buffer().set_live_spelling(flag);
	settings_t::set(kSettingsSpellCheckingKey, flag, document->file_type(), document->path());

	[self setNeedsDisplay:YES];
}

- (void)takeSpellingLanguageFrom:(id)sender
{
	NSString* lang = (NSString*)[sender representedObject];
	[[NSSpellChecker sharedSpellChecker] setLanguage:lang];
	document->buffer().set_spelling_language(to_s(lang));
	settings_t::set(kSettingsSpellingLanguageKey, to_s(lang), "", document->path());
	if(document->path() != NULL_STR)
		settings_t::set(kSettingsSpellingLanguageKey, to_s(lang), NULL_STR, path::join(path::parent(document->path()), "**"));

	[self setNeedsDisplay:YES];
}

- (scope::context_t)scopeContext
{
	return editor->scope(to_s([self scopeAttributes]));
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
	{
		NSRange selectedRange = [[self accessibilityAttributeValue:NSAccessibilitySelectedTextRangeAttribute] rangeValue];
		NSRect  selectedRect  = [[self accessibilityAttributeValue:NSAccessibilityBoundsForRangeParameterizedAttribute forParameter:[NSValue valueWithRange:selectedRange]] rectValue];
		NSRect  viewRect      = [self convertRect:[self visibleRect] toView:nil];
		viewRect = [[self window] convertRectToScreen:viewRect];
		viewRect.origin.y = [[NSScreen mainScreen] frame].size.height - (viewRect.origin.y + viewRect.size.height);
		selectedRect.origin.y = [[NSScreen mainScreen] frame].size.height - (selectedRect.origin.y + selectedRect.size.height);
		if(selectedRect.size.width == -1)
			selectedRect.size.width = 1;
		UAZoomChangeFocus(&viewRect, &selectedRect, kUAZoomFocusTypeInsertionPoint);
	}
	if(isUpdatingSelection)
		return;

	AUTO_REFRESH;
	ng::ranges_t ranges = convert(document->buffer(), to_s(aSelectionString));
	editor->set_selections(ranges);
	iterate(range, ranges)
		layout->remove_enclosing_folds(range->min().index, range->max().index);
}

- (NSString*)selectionString
{
	return selectionString;
}

- (void)updateSelection
{
	text::selection_t ranges, withoutCarry;
	citerate(range, editor->ranges())
	{
		text::pos_t from = document->buffer().convert(range->first.index);
		text::pos_t to   = document->buffer().convert(range->last.index);
		if(!range->freehanded && !range->columnar)
			withoutCarry.push_back(text::range_t(from, to, range->columnar));
		from.offset = range->first.carry;
		to.offset   = range->last.carry;
		if(range->freehanded || range->columnar)
			withoutCarry.push_back(text::range_t(from, to, range->columnar));
		ranges.push_back(text::range_t(from, to, range->columnar));
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
		if(layout->is_line_folded(lineNumber))
			return kFoldingCollapsed;
		else if(layout->is_line_fold_start_marker(lineNumber))
			return kFoldingTop;
		else if(layout->is_line_fold_stop_marker(lineNumber))
			return kFoldingBottom;
	}
	return kFoldingNone;
}

- (GVLineRecord)lineRecordForPosition:(CGFloat)yPos
{
	if(!layout)
		return GVLineRecord();
	auto record = layout->line_record_for(yPos);
	return GVLineRecord(record.line, record.softline, record.top, record.bottom, record.baseline);
}

- (GVLineRecord)lineFragmentForLine:(NSUInteger)aLine column:(NSUInteger)aColumn
{
	if(!layout)
		return GVLineRecord();
	auto record = layout->line_record_for(text::pos_t(aLine, aColumn));
	return GVLineRecord(record.line, record.softline, record.top, record.bottom, record.baseline);
}

- (BOOL)filterDocumentThroughCommand:(NSString*)commandString input:(input::type)inputUnit output:(output::type)outputUnit
{
	BOOL res = NO;

	auto environment = [self variables];
	if(io::process_t process = io::spawn(std::vector<std::string>{ "/bin/sh", "-c", to_s(commandString) }, environment))
	{
		bool inputWasSelection = false;
		text::range_t inputRange = ng::write_unit_to_fd(document->buffer(), editor->ranges().last(), document->buffer().indent().tab_size(), process.in, inputUnit, input::entire_document, input_format::text, scope::selector_t(), environment, &inputWasSelection);

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
			else
			{
				AUTO_REFRESH;
				editor->handle_result(output, outputUnit, output_format::text, output_caret::after_output, inputRange, environment);
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
		D(DBF_OakTextView_Macros, bug("%s\n", to_s(plist::convert((__bridge CFDictionaryRef)macroRecordingArray)).c_str()););
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
	D(DBF_OakTextView_Macros, bug("%s\n", to_s(plist::convert((__bridge CFDictionaryRef)[[NSUserDefaults standardUserDefaults] arrayForKey:@"OakMacroManagerScratchMacro"])).c_str()););
	AUTO_REFRESH;
	if(NSArray* scratchMacro = [[NSUserDefaults standardUserDefaults] arrayForKey:@"OakMacroManagerScratchMacro"])
			editor->macro_dispatch(plist::convert((__bridge CFDictionaryRef)@{ @"commands" : scratchMacro }), [self variables]);
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
	ASSERT(layout);
	AUTO_REFRESH;
	dropPosition = NSEqualPoints(aPoint, NSZeroPoint) ? ng::index_t() : layout->index_at_point(aPoint).index;
	layout->set_drop_marker(dropPosition);
}

- (void)dropFiles:(NSArray*)someFiles
{
	D(DBF_OakTextView_DragNDrop, bug("%s\n", [[someFiles description] UTF8String]););

	std::set<bundles::item_ptr> allHandlers;
	std::map<oak::uuid_t, std::vector<std::string> > handlerToFiles;

	scope::context_t scope = [self scopeContext];
	for(NSString* path in someFiles)
	{
		citerate(item, bundles::drag_commands_for_path(to_s(path), scope))
		{
			D(DBF_OakTextView_DragNDrop, bug("handler: %s\n", (*item)->full_name().c_str()););
			handlerToFiles[(*item)->uuid()].push_back(to_s(path));
			allHandlers.insert(*item);
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
		editor->insert(merged, true);
	}
	else if(bundles::item_ptr handler = OakShowMenuForBundleItems(std::vector<bundles::item_ptr>(allHandlers.begin(), allHandlers.end()), [self positionForWindowUnderCaret]))
	{
		D(DBF_OakTextView_DragNDrop, bug("execute %s\n", handler->full_name().c_str()););

		static struct { NSUInteger qual; std::string name; } const qualNames[] =
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
		for(size_t i = 0; i != sizeofA(qualNames); ++i)
		{
			if(state & qualNames[i].qual)
				flagNames.push_back(qualNames[i].name);
		}
		env["TM_MODIFIER_FLAGS"] = text::join(flagNames, "|");

		AUTO_REFRESH;
		document::run(parse_drag_command(handler), document->buffer(), editor->ranges(), document, env, pwd);
	}
}

// ===============
// = Drag Source =
// ===============

- (NSDragOperation)draggingSourceOperationMaskForLocal:(BOOL)isLocal
{
	return isLocal ? (NSDragOperationCopy | NSDragOperationMove) : NSDragOperationCopy;
}

// ====================
// = Drag Destination =
// ====================

- (BOOL)isPointInSelection:(NSPoint)aPoint
{
	BOOL res = NO;
	citerate(rect, layout->rects_for_ranges(editor->ranges(), kRectsIncludeSelections))
		res = res || CGRectContainsPoint(*rect, aPoint);
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
	layout->set_drop_marker(dropPosition = ng::index_t());

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

		editor->set_selections(ng::range_t(pos));
		editor->insert(text::join(paths, "\n"));
	}
	else if(NSString* text = [pboard stringForType:[pboard availableTypeFromArray:@[ @"public.plain-text" ]]] ?: [pboard stringForType:NSStringPboardType])
	{
		D(DBF_OakTextView_DragNDrop, bug("plain text: %s\n", [text UTF8String]););
		if(shouldMove)
		{
			editor->move_selection_to(pos);
		}
		else
		{
			editor->set_selections(ng::range_t(pos));
			editor->insert(to_s(text));
		}
	}
	else if(files)
	{
		editor->set_selections(ng::range_t(pos));
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

	ng::ranges_t s = editor->ranges();

	ng::index_t index = layout->index_at_point(mouseDownPos);
	if(!optionDown)
		index.carry = 0;

	ng::index_t min = s.last().min(), max = s.last().max();
	mouseDownIndex = shiftDown ? (index <= min ? max : (max <= index ? min : s.last().first)) : index;
	ng::ranges_t range(ng::range_t(mouseDownIndex, index));

	switch(mouseDownClickCount)
	{
		case 2: range = ng::extend(document->buffer(), range, kSelectionExtendToWord); break;
		case 3: range = ng::extend(document->buffer(), range, kSelectionExtendToLine); break;
	}

	if(optionDown)
	{
		if(shiftDown)
				range.last().columnar = true;
		else	range.last().freehanded = true;
	}

	if(commandDown && mouseDownClickCount == 1)
	{
		bool didToggle = false;
		ng::ranges_t newSel;
		citerate(cur, s)
		{
			if(*cur != range.last())
					newSel.push_back(*cur);
			else	didToggle = true;
		}
		s = newSel;

		if(s.empty() || !didToggle)
			s.push_back(range.last());
	}
	else if(shiftDown || (commandDown && mouseDownClickCount != 1))
		s.last() = range.last();
	else
		s = range.last();

	editor->set_selections(s);
}

- (void)actOnMouseDragged:(NSEvent*)anEvent
{
	NSPoint mouseCurrentPos = [self convertPoint:[anEvent locationInWindow] fromView:nil];
	ng::ranges_t range(ng::range_t(mouseDownIndex, layout->index_at_point(mouseCurrentPos)));
	switch(mouseDownClickCount)
	{
		case 2: range = ng::extend(document->buffer(), range, kSelectionExtendToWord); break;
		case 3: range = ng::extend(document->buffer(), range, kSelectionExtendToLine); break;
	}

	NSUInteger currentModifierFlags = [anEvent modifierFlags];
	if(currentModifierFlags & NSAlternateKeyMask)
		range.last().columnar = true;

	ng::ranges_t s = editor->ranges();
	s.last() = range.last();
	editor->set_selections(s);

	[self autoscroll:anEvent];
}

- (void)startDragForEvent:(NSEvent*)anEvent
{
	ASSERT(layout);

	NSRect srcRect;
	ng::ranges_t const ranges = ng::dissect_columnar(document->buffer(), editor->ranges());
	NSImage* srcImage = [self imageForRanges:ranges imageRect:&srcRect];

	NSImage* image = [[NSImage alloc] initWithSize:srcImage.size];
	[image lockFocus];
	[srcImage drawAtPoint:NSZeroPoint fromRect:NSZeroRect operation:NSCompositeCopy fraction:0.5];
	[image unlockFocus];

	std::vector<std::string> v;
	iterate(range, ranges)
		v.push_back(document->buffer().substr(range->min().index, range->max().index));

	NSPasteboard* pboard = [NSPasteboard pasteboardWithName:NSDragPboard];
	[pboard declareTypes:@[ NSStringPboardType ] owner:self];
	[pboard setString:[NSString stringWithCxxString:text::join(v, "\n")] forType:NSStringPboardType];

	[self dragImage:image at:NSMakePoint(NSMinX(srcRect), NSMaxY(srcRect)) offset:NSMakeSize(0, 0) event:anEvent pasteboard:pboard source:self slideBack:YES];
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

- (void)mouseDown:(NSEvent*)anEvent
{
	if(!layout || [anEvent type] != NSLeftMouseDown || ignoreMouseDown)
		return (void)(ignoreMouseDown = NO);

	if(macroRecordingArray && [anEvent type] == NSLeftMouseDown)
		return (void)NSRunAlertPanel(@"You are recording a macro", @"While recording macros it is not possible to select text or reposition the caret using your mouse.\nYou can stop macro recording from the Edit → Macros menu.", @"Continue", nil, nil);

	std::vector<bundles::item_ptr> const& items = bundles::query(bundles::kFieldSemanticClass, "callback.mouse-click", add_modifiers_to_scope(ng::scope(document->buffer(), layout->index_at_point([self convertPoint:[anEvent locationInWindow] fromView:nil]), to_s([self scopeAttributes])), [anEvent modifierFlags]));
	if(!items.empty())
	{
		if(bundles::item_ptr item = OakShowMenuForBundleItems(items, [self positionForWindowUnderCaret]))
		{
			AUTO_REFRESH;
			editor->set_selections(ng::range_t(layout->index_at_point([self convertPoint:[anEvent locationInWindow] fromView:nil]).index));
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
	if(!layout || macroRecordingArray)
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
	if(!layout || macroRecordingArray)
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

	if(layout)
	{
		AUTO_REFRESH;
		layout->set_draw_caret(doesHaveFocus);
		layout->set_is_key(doesHaveFocus);
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
		editor->perform(anAction, layout.get(), [self continuousIndentCorrections], to_s([self scopeAttributes]));
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
