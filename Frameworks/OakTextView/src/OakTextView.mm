#import "OakTextView.h"
#import "OakPasteboardWrapper.h"
#import "OakChoiceMenu.h"
#import "OakDocumentView.h" // addAuxiliaryView:atEdge: signature
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
#import <OakFoundation/OakTimer.h>
#import <OakSystem/application.h>
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
#import <text/utf16.h>
#import <text/utf8.h>
#import <oak/CocoaSTL.h>
#import <oak/debug.h>

OAK_DEBUG_VAR(OakTextView_TextInput);
OAK_DEBUG_VAR(OakTextView_Spelling);
OAK_DEBUG_VAR(OakTextView_ViewRect);
OAK_DEBUG_VAR(OakTextView_NSView);
OAK_DEBUG_VAR(OakTextView_DragNDrop);
OAK_DEBUG_VAR(OakTextView_MouseEvents);
OAK_DEBUG_VAR(OakTextView_Macros);

int32_t const NSWrapColumnWindowWidth =  0;
int32_t const NSWrapColumnAskUser     = -1;

NSString* const kUserDefaultsDisableAntiAliasKey = @"disableAntiAlias";

@interface OakTextView ()
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
@property (nonatomic, assign) ng::ranges_t const& markedRanges;
@property (nonatomic, retain) NSDate* optionDownDate;
@property (nonatomic, retain) OakTimer* initiateDragTimer;
@property (nonatomic, retain) OakTimer* dragScrollTimer;
@property (nonatomic, assign) BOOL showDragCursor;
@property (nonatomic, assign) BOOL showColumnSelectionCursor;
@property (nonatomic, readonly) BOOL hasSelection;
@property (nonatomic, retain) OakChoiceMenu* choiceMenu;
@property (nonatomic, assign) NSUInteger refreshNestCount;
@property (nonatomic, retain) NSViewController* liveSearchViewController;
@property (nonatomic, copy) NSString* liveSearchString;
@property (nonatomic, assign) ng::ranges_t const& liveSearchRanges;
@end

static std::vector<bundles::item_ptr> items_for_tab_expansion (ng::buffer_t const& buffer, ng::ranges_t const& ranges, std::string const& scopeAttributes, ng::range_t* range)
{
	size_t caret = ranges.last().min().index;
	size_t line  = buffer.convert(caret).line;
	size_t bol   = buffer.begin(line);

	bool lastWasWordChar           = false;
	std::string lastCharacterClass = ng::kCharacterClassUnknown;

	for(size_t i = bol; i < caret; i += buffer[i].size())
	{
		// we don’t use text::is_word_char because that function treats underscores as word characters, which is undesired, see <issue://157>.
		bool isWordChar = CFCharacterSetIsLongCharacterMember(CFCharacterSetGetPredefined(kCFCharacterSetAlphaNumeric), utf8::to_ch(buffer[i]));
		std::string characterClass = character_class(buffer, i);

		if(i == bol || lastWasWordChar != isWordChar || lastCharacterClass != characterClass)
		{
			std::vector<bundles::item_ptr> const& items = bundles::query(bundles::kFieldTabTrigger, buffer.substr(i, caret), ng::scope(buffer, ng::ranges_t(i), scopeAttributes));
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
					[_self updateMarkedRanges];

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

					[_self updateSelection];
					[_self ensureSelectionIsInVisibleArea:nil];
					[_self resetBlinkCaretTimer];
					[_self updateChoiceMenu:nil];
				}
			}
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
private:
	OakTextView* textView;
};

void buffer_refresh_callback_t::did_parse (size_t from, size_t to)
{
	[textView redisplayFrom:from to:to];
}

static std::string shell_quote (std::vector<std::string> paths)
{
	iterate(it, paths)
		*it = format_string::replace(*it, ".*", "'${0/'/'\\''/g}'");
	return text::join(paths, " ");
}

// =============================
// = OakTextView’s Find Server =
// =============================

@interface OakTextViewFindServer : NSObject <OakFindServerProtocol>
{
	OakTextView*     textView;
	find_operation_t findOperation;
	find::options_t  findOptions;
}
@property (nonatomic, retain)   OakTextView*     textView;
@property (nonatomic, readonly) find_operation_t findOperation;
@property (nonatomic, readonly) find::options_t  findOptions;
@end

@implementation OakTextViewFindServer
@synthesize textView, findOperation, findOptions;

- (id)initWithTextView:(OakTextView*)aTextView operation:(find_operation_t)anOperation options:(find::options_t)someOptions
{
	if((self = [super init]))
	{
		self.textView = aTextView;
		findOperation = anOperation;
		findOptions   = someOptions;
	}
	return self;
}

- (void)dealloc
{
	[textView release];
	[super dealloc];
}

+ (id)findServerWithTextView:(OakTextView*)aTextView operation:(find_operation_t)anOperation options:(find::options_t)someOptions
{
	return [[[self alloc] initWithTextView:aTextView operation:anOperation options:someOptions] autorelease];
}

- (NSString*)findString      { return [[OakPasteboard pasteboardWithName:NSFindPboard] current].string;    }
- (NSString*)replaceString   { return [[OakPasteboard pasteboardWithName:NSReplacePboard] current].string; }

- (void)didFind:(NSUInteger)aNumber occurrencesOf:(NSString*)aFindString atPosition:(text::pos_t const&)aPosition
{
	static NSString* const formatStrings[2][3] = {
		{ @"No more occurrences of “%@”.", nil, @"%2$ld occurrences of “%@”." },
		{ @"No more matches for “%@”.",    nil, @"%2$ld matches for “%@”."    },
	};
	if(NSString* format = formatStrings[(findOptions & find::regular_expression) ? 1 : 0][aNumber > 2 ? 2 : aNumber])
		OakShowToolTip([NSString stringWithFormat:format, aFindString, aNumber], [textView positionForWindowUnderCaret]);
}

- (void)didReplace:(NSUInteger)aNumber occurrencesOf:(NSString*)aFindString with:(NSString*)aReplacementString
{
	static NSString* const formatStrings[2][3] = {
		{ @"Nothing replaced (no occurrences of “%@”).", @"Replaced one occurrence of “%@”.", @"Replaced %2$ld occurrences of “%@”." },
		{ @"Nothing replaced (no matches for “%@”).",    @"Replaced one match of “%@”.",      @"Replaced %2$ld matches of “%@”."     }
	};
	NSString* format = formatStrings[(findOptions & find::regular_expression) ? 1 : 0][aNumber > 2 ? 2 : aNumber];
	OakShowToolTip([NSString stringWithFormat:format, aFindString, aNumber], [textView positionForWindowUnderCaret]);
}
@end

@implementation OakTextView
@synthesize antiAlias;
@synthesize initiateDragTimer, dragScrollTimer, optionDownDate, showColumnSelectionCursor, showDragCursor, choiceMenu;
@synthesize markedRanges;
@synthesize document;
@synthesize refreshNestCount;
@synthesize liveSearchViewController, liveSearchString, liveSearchRanges;

- (NSImage*)imageForRanges:(ng::ranges_t const&)ranges imageRect:(NSRect*)outRect
{
	NSRect srcRect = NSZeroRect, visibleRect = [self visibleRect];
	citerate(range, ranges)
		srcRect = NSUnionRect(srcRect, NSIntersectionRect(visibleRect, layout->rect_for_range(range->min().index, range->max().index)));

	NSBezierPath* clip = [NSBezierPath bezierPath];
	citerate(rect, layout->rects_for_ranges(ranges))
		[clip appendBezierPath:[NSBezierPath bezierPathWithRect:NSOffsetRect(*rect, -NSMinX(srcRect), -NSMinY(srcRect))]];

	NSImage* image = [[[NSImage alloc] initWithSize:NSMakeSize(std::max<CGFloat>(NSWidth(srcRect), 1), std::max<CGFloat>(NSHeight(srcRect), 1))] autorelease];
	[image setFlipped:[self isFlipped]];
	[image lockFocus];
	[clip addClip];

	CGContextRef context = (CGContextRef)[[NSGraphicsContext currentContext] graphicsPort];
	CGContextTranslateCTM(context, -NSMinX(srcRect), -NSMinY(srcRect));

	NSRectClip(srcRect);
	layout->draw(context, srcRect, [self isFlipped], false, ng::ranges_t(), ng::ranges_t(), false, cf::color_t("#000000"));

	[image unlockFocus];
	[image setFlipped:NO];

	if(outRect)
		*outRect = srcRect;

	return image;
}

- (void)highlightRanges:(ng::ranges_t const&)ranges
{
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

		delete callback;
		callback = NULL;

		editor.reset();
		layout.reset();

		self.choiceMenu = nil;
		choiceVector.clear();
	}

	if(document = aDocument)
	{
		settings_t const& settings = document->settings();

		editor = ng::editor_for_document(document);
		wrapColumn = settings.get(kSettingsWrapColumnKey, wrapColumn);
		layout.reset(new ng::layout_t(document->buffer(), theme, fontName, fontSize, settings.get(kSettingsSoftWrapKey, false), wrapColumn, document->folded()));
		if(settings.get(kSettingsShowWrapColumnKey, false))
			layout->set_draw_wrap_column(true);

		BOOL hasFocus = (self.keyState & (OakViewViewIsFirstResponderMask|OakViewWindowIsKeyMask|OakViewApplicationIsActiveMask)) == (OakViewViewIsFirstResponderMask|OakViewWindowIsKeyMask|OakViewApplicationIsActiveMask);
		layout->set_is_key(hasFocus);

		callback = new buffer_refresh_callback_t(self);

		editor->set_clipboard(get_clipboard(NSGeneralPboard));
		editor->set_find_clipboard(get_clipboard(NSFindPboard));
		editor->set_replace_clipboard(get_clipboard(NSReplacePboard));

		std::string const visibleRect = document->visible_rect();
		if(document->selection() != NULL_STR)
		{
			ng::ranges_t ranges = convert(document->buffer(), document->selection());
			editor->set_selections(ranges);
			iterate(range, ranges)
				layout->remove_enclosing_folds(range->min().index, range->max().index);
		}

		auto bgColor = theme->styles_for_scope(document->buffer().scope(0).left, fontName, fontSize).background();
		[[self enclosingScrollView] setBackgroundColor:[NSColor colorWithCGColor:bgColor]];
		SetLionScrollerKnobStyle([self enclosingScrollView], cf::color_is_dark(bgColor) ? NSScrollerKnobStyleLight : NSScrollerKnobStyleDark);
		[[self window] setOpaque:CGColorGetAlpha(bgColor) == 1];

		[self reflectDocumentSize];
		[self updateSelection];

		if(visibleRect != NULL_STR)
				[self scrollRectToVisible:NSRectFromString([NSString stringWithCxxString:visibleRect])];
		else	[self ensureSelectionIsInVisibleArea:self];

		document->buffer().add_callback(callback);

		[self resetBlinkCaretTimer];
		[self setNeedsDisplay:YES];
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
		showInvisibles = settings.get(kSettingsShowInvisiblesKey, false);
		antiAlias      = ![[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsDisableAntiAliasKey];

		[self registerForDraggedTypes:[[self class] dropTypes]];

		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(documentWillSave:) name:@"OakDocumentNotificationWillSave" object:nil];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(userDefaultsDidChange:) name:NSUserDefaultsDidChangeNotification object:[NSUserDefaults standardUserDefaults]];
	}
	return self;
}

- (void)dealloc
{
	[[NSNotificationCenter defaultCenter] removeObserver:self];
	[liveSearchString release];
	[self setDocument:document::document_ptr()];
	[super dealloc];
}

- (void)documentWillSave:(NSNotification*)aNotification
{
	if(document && layout)
		document->set_folded(layout->folded_as_string());
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
	CGRect r = layout->rect_at_index(editor->ranges().last().last);
	CGRect s  = [self visibleRect];

	CGFloat x = NSMinX(s), w = NSWidth(s);
	CGFloat y = NSMinY(s), h = NSHeight(s);

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

	CGRect b = [self bounds];
	x = oak::cap(NSMinX(b), x, NSMaxX(b) - w);
	y = oak::cap(NSMinY(b), y, NSMaxY(b) - h);

	NSClipView* contentView = [[self enclosingScrollView] contentView];
	if([contentView respondsToSelector:@selector(_extendNextScrollRelativeToCurrentPosition)])
		[contentView performSelector:@selector(_extendNextScrollRelativeToCurrentPosition)];
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
		choiceMenu.choices = (NSArray*)((CFArrayRef)cf::wrap(choiceVector));

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
	[[NSColor clearColor] set];
	NSRectFill(aRect); // Only necessary when theme uses a transparent background.

	CGContextRef context = (CGContextRef)[[NSGraphicsContext currentContext] graphicsPort];
	if(!antiAlias)
		CGContextSetShouldAntialias(context, false);
	layout->draw(context, aRect, [self isFlipped], showInvisibles, merge(editor->ranges(), [self markedRanges]), liveSearchRanges);
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
		size_t from = utf16::advance(base, aRange.location) - base;
		size_t to   = utf16::advance(base, aRange.location + aRange.length) - base;
		sel.push_back(ng::range_t(range->min() + from, range->min() + to));
	}
	editor->set_selections(sel);
}

- (NSRange)selectedRange
{
	std::string const text = editor->as_string();
	ng::range_t const r = editor->ranges().last();
	NSRange res = NSMakeRange(utf16::distance(text.data(), text.data() + r.min().index), utf16::distance(text.data() + r.min().index, text.data() + r.max().index));
	D(DBF_OakTextView_TextInput, bug("%s\n", [NSStringFromRange(res) UTF8String]););
	return res;
}

- (NSRange)markedRange
{
	D(DBF_OakTextView_TextInput, bug("%s\n", to_s(markedRanges).c_str()););
	if(markedRanges.empty())
		return NSMakeRange(NSNotFound, 0);

	ng::range_t const r = markedRanges.last();
	std::string const text = document->buffer().substr(0, r.max().index);
	return NSMakeRange(utf16::distance(text.data(), text.data() + r.min().index), utf16::distance(text.data() + r.min().index, text.data() + r.max().index));
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
	std::string const text = editor->as_string();
	char const* const base = text.data();
	size_t from = utf16::advance(base, theRange.location, base + text.size()) - base;
	size_t to   = utf16::advance(base + from, theRange.length, base + text.size()) - base;

	CFMutableAttributedStringRef res = CFAttributedStringCreateMutable(kCFAllocatorDefault, 0);
	std::map<size_t, scope::scope_t> scopes = document->buffer().scopes(from, to);
	for(auto pair = scopes.begin(); pair != scopes.end(); )
	{
		styles_t const& styles = theme->styles_for_scope(pair->second, fontName, fontSize);

		size_t i = from + pair->first;
		size_t j = ++pair != scopes.end() ? from + pair->first : to;

		CFMutableAttributedStringRef str = CFAttributedStringCreateMutable(kCFAllocatorDefault, 0);
		CFAttributedStringReplaceString(str, CFRangeMake(0, 0), cf::wrap(text.substr(i, j - i)));
		CFAttributedStringSetAttribute(str, CFRangeMake(0, CFAttributedStringGetLength(str)), kCTFontAttributeName, styles.font());
		CFAttributedStringSetAttribute(str, CFRangeMake(0, CFAttributedStringGetLength(str)), kCTForegroundColorAttributeName, styles.foreground());
		if(styles.underlined())
			CFAttributedStringSetAttribute(str, CFRangeMake(0, CFAttributedStringGetLength(str)), kCTUnderlineStyleAttributeName, cf::wrap(0x1|kCTUnderlinePatternSolid));
		CFAttributedStringReplaceAttributedString(res, CFRangeMake(CFAttributedStringGetLength(res), 0), str);
		CFRelease(str);
	}

	return [(NSAttributedString*)res autorelease];
}

- (NSRect)firstRectForCharacterRange:(NSRange)theRange
{
	std::string const text = editor->as_string();

	size_t index = utf16::advance(text.data(), theRange.location) - text.data();
	NSRect rect = [self convertRect:layout->rect_at_index(index) toView:nil];
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

// ================
// = Bundle Items =
// ================

- (void)performBundleItem:(bundles::item_ptr const&)item
{
	// D(DBF_OakTextView_BundleItems, bug("%s\n", anItem->full_name().c_str()););
	AUTO_REFRESH;
	switch(item->kind())
	{
		case bundles::kItemTypeSnippet:
		{
			[self recordSelector:@selector(insertSnippetWithOptions:) withArgument:ns::to_dictionary(item->plist())];
			editor->snippet_dispatch(item->plist(), editor->variables(item->environment()));
		}
		break;

		case bundles::kItemTypeCommand:
		{
			[self recordSelector:@selector(executeCommandWithOptions:) withArgument:ns::to_dictionary(item->plist())];
			document::run(parse_command(item), document->buffer(), editor->ranges(), document);
		}
		break;

		case bundles::kItemTypeMacro:
		{
			[self recordSelector:@selector(playMacroWithOptions:) withArgument:ns::to_dictionary(item->plist())];
			editor->macro_dispatch(item->plist(), editor->variables(item->environment()));
		}
		break;

		case bundles::kItemTypeGrammar:
		{
			document->set_file_type(item->value_for_field(bundles::kFieldGrammarScope));
			file::set_type(document->virtual_path(), item->value_for_field(bundles::kFieldGrammarScope));
		}
		break;

		case bundles::kItemTypeTheme:
		{
			OakDocumentView* documentView = (OakDocumentView*)[[self enclosingScrollView] superview];
			[documentView setThemeWithUUID:[NSString stringWithCxxString:item->uuid()]];
		}
		break;
	}
}

- (void)applicationDidBecomeActiveNotification:(NSNotification*)aNotification
{
	citerate(item, bundles::query(bundles::kFieldSemanticClass, "callback.application.did-activate", editor->scope()))
		[self performBundleItem:*item];
}

- (void)applicationDidResignActiveNotification:(NSNotification*)aNotification
{
	citerate(item, bundles::query(bundles::kFieldSemanticClass, "callback.application.did-deactivate", editor->scope()))
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

		static std::string const localKeyBindings   = oak::application_t::support("KeyBindings.dict");
		static std::string const defaultKeyBindings = oak::application_t::path("Contents/Resources/KeyBindings.dict");
		static std::string const KeyBindingLocations[] =
		{
			path::exists(localKeyBindings) ? localKeyBindings : defaultKeyBindings,
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
	AUTO_REFRESH;
	std::string const eventString = to_s(anEvent);

	if(KeyEventContext != &KeyBindings)
	{
		plist::dictionary_t::const_iterator pair = KeyEventContext->find(eventString);
		if(pair != KeyEventContext->end())
			return [self handleKeyBindingAction:pair->second], YES;
	}

	std::vector<bundles::item_ptr> const& items = bundles::query(bundles::kFieldKeyEquivalent, eventString, editor->scope());
	if(!items.empty())
	{
		if(bundles::item_ptr item = bundles::show_menu_for_items(items, [self positionForWindowUnderCaret]))
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
	static std::string const SpecialKeys[] =
	{
		"^" + kBackwardDelete, "^" + kForwardDelete,
		"^"   + kUpArrow, "^"   + kDownArrow, "^"   + kLeftArrow, "^"   + kRightArrow,
		"^$"  + kUpArrow, "^$"  + kDownArrow, "^$"  + kLeftArrow, "^$"  + kRightArrow,
		"^~"  + kUpArrow, "^~"  + kDownArrow, "^~"  + kLeftArrow, "^~"  + kRightArrow,
		"^~$" + kUpArrow, "^~$" + kDownArrow, "^~$" + kLeftArrow, "^~$" + kRightArrow,
	};

	if(oak::contains(beginof(SpecialKeys), endof(SpecialKeys), eventString))
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
	std::vector<bundles::item_ptr> const& items = bundles::query(bundles::kFieldKeyEquivalent, to_s(anEvent), editor->scope());
	if(bundles::item_ptr item = bundles::show_menu_for_items(items, [self positionForWindowUnderCaret]))
		[self performBundleItem:item];
	else if(items.empty())
		[self interpretKeyEvents:@[ anEvent ]];

	[NSCursor setHiddenUntilMouseMoves:YES];
	[[NSNotificationCenter defaultCenter] postNotificationName:OakCursorDidHideNotification object:nil];
}

- (void)keyDown:(NSEvent*)anEvent
{
	D(DBF_OakTextView_TextInput, bug("%s\n", [[anEvent description] UTF8String]););
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

		bool didEdit   = oldPrefix != newPrefix;
		bool didDelete = didEdit && oldPrefix.find(newPrefix) == 0;

		if(didEdit && !didDelete)
		{
			NSInteger choiceIndex = NSNotFound;
			if(std::find(choiceVector.begin(), choiceVector.end(), oldContent) != choiceVector.end() && oldContent.find(newContent) == 0)
			{
				choiceIndex = std::find(choiceVector.begin(), choiceVector.end(), oldContent) - choiceVector.begin();
			}
			else
			{
	 			for(size_t i = 0; i < choiceVector.size(); ++i)
				{
					if(choiceVector[i].find(newContent) != 0)
						continue;
					choiceIndex = i;
					break;
				}
			}

			choiceMenu.choiceIndex = choiceIndex;
			if(choiceIndex != NSNotFound && newContent != choiceVector[choiceIndex])
				editor->set_placeholder_content(choiceVector[choiceIndex], newPrefix.size());
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
			editor->perform(ng::kInsertTab);
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
	BOOL isSelectingWithMouse = ([NSEvent slPressedMouseButtons] & 1) && editor->has_selection();

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
	editor->insert_with_pairing([aString UTF8String]);
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
	NSPoint p = NSMakePoint(CGRectGetMinX(r), CGRectGetMaxY(r)-1);
	if(NSPointInRect(p, [self visibleRect]))
			{ p = [[self window] convertBaseToScreen:[self convertPoint:p toView:nil]]; }
	else	{ p = [NSEvent mouseLocation]; p.y -= 16; }

	return p;
}

- (NSString*)selectAndReturnMisspelledWordAtIndex:(size_t)currnetIndex
{
	AUTO_REFRESH;
	NSString* word = nil;
	if(!editor->has_selection())
	{
		ng::buffer_t const& buf = document->buffer();

		size_t from = currnetIndex, to = currnetIndex;
		while(0 < from && text::is_word_char(buf[from-1]))
			from -= buf[from-1].size();
		while(to < buf.size() && text::is_word_char(buf[to]))
			to += buf[to].size();

		if(from != to && ns::is_misspelled(buf.substr(from, to), buf.spelling_language(), buf.spelling_tag()))
		{
			editor->set_selections(ng::range_t(from, to));
			word = [NSString stringWithCxxString:buf.substr(from, to)];
		}
	}
	else
	{
		typedef std::map<std::string, std::string> env;
		env const& vars = editor->variables(env());
		env::const_iterator it = vars.find("TM_SELECTED_TEXT");
		if(it != vars.end() && it->second.find_first_of(" \n\t") == std::string::npos && ns::is_misspelled(it->second, document->buffer().spelling_language(), document->buffer().spelling_tag()))
			word = [NSString stringWithCxxString:it->second];
	}
	return word;
}

- (NSMenu*)contextMenuWithMisspelledWord:(NSString*)aWord
{
	NSMenu* menu = [[[NSMenu alloc] initWithTitle:@""] autorelease];
	NSMenuItem* item = nil;

	if(aWord)
	{
		[[NSSpellChecker sharedSpellChecker] updateSpellingPanelWithMisspelledWord:aWord];
		for(NSString* guess in [[NSSpellChecker sharedSpellChecker] guessesForWord:aWord])
		{
			item = [menu addItemWithTitle:guess action:@selector(contextMenuPerformCorrectWord:) keyEquivalent:@""];
			[item setRepresentedObject:guess];
		}

		if([menu numberOfItems] == 0)
			[menu addItemWithTitle:@"No Guesses Found" action:nil keyEquivalent:@""];

		[menu addItem:[NSMenuItem separatorItem]];
		item = [menu addItemWithTitle:@"Ignore Spelling" action:@selector(contextMenuPerformIgnoreSpelling:) keyEquivalent:@""];
		[item setRepresentedObject:aWord];
		item = [menu addItemWithTitle:@"Learn Spelling" action:@selector(contextMenuPerformLearnSpelling:) keyEquivalent:@""];
		[item setRepresentedObject:aWord];
		[menu addItem:[NSMenuItem separatorItem]];
	}

	static struct { NSString* title; SEL action; } const items[] =
	{
		{ @"Cut",                     @selector(cut:)                           },
		{ @"Copy",                    @selector(copy:)                          },
		{ @"Paste",                   @selector(paste:)                         },
		{ nil,                        nil                                       },
		{ @"Fold/Unfold",             @selector(toggleFolding:)                 },
		{ @"Filter Through Command…", @selector(orderFrontExecuteCommandPanel:) },
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
}

- (void)contextMenuPerformLearnSpelling:(id)sender
{
	D(DBF_OakTextView_Spelling, bug("%s\n", [[sender representedObject] UTF8String]););
	[[NSSpellChecker sharedSpellChecker] learnWord:[sender representedObject]];
}

// =========================
// = Find Protocol: Client =
// =========================

- (void)performFindOperation:(id <OakFindServerProtocol>)aFindServer
{
	AUTO_REFRESH;

	// TODO If aFindServer != self then we should record findWithOptions: instead of find{Next,Previous,All,…}:

	bool onlyInSelection = false;
	switch(aFindServer.findOperation)
	{
		case kFindOperationFindInSelection:
		case kFindOperationCountInSelection: onlyInSelection = editor->has_selection();
		case kFindOperationFind:
		case kFindOperationCount:
		{
			bool isCounting = aFindServer.findOperation == kFindOperationCount || aFindServer.findOperation == kFindOperationCountInSelection;

			std::string const findStr = to_s(aFindServer.findString);
			find::options_t options   = aFindServer.findOptions;

			NSArray* documents = [[OakPasteboard pasteboardWithName:NSFindPboard].auxiliaryOptionsForCurrent objectForKey:@"documents"];
			if(documents && [documents count] > 1)
				options &= ~find::wrap_around;

			ng::ranges_t res;
			citerate(pair, ng::find(document->buffer(), editor->ranges(), findStr, options, onlyInSelection ? editor->ranges() : ng::ranges_t()))
				res.push_back(pair->first);

			if(onlyInSelection && res.sorted() == editor->ranges().sorted())
			{
				res = ng::ranges_t();
				citerate(pair, ng::find(document->buffer(), editor->ranges(), findStr, options, ng::ranges_t()))
					res.push_back(pair->first);
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
							document::show(doc, document::kCollectionCurrent, to_s(range));
							return;
						}
					}
				}
			}

			[aFindServer didFind:res.size() occurrencesOf:aFindServer.findString atPosition:res.size() == 1 ? document->buffer().convert(res.last().min().index) : text::pos_t::undefined];
			if(isCounting)
				break;

			[self recordSelector:(options & find::all_matches) ? (aFindServer.findOperation == kFindOperationFind ? @selector(findAll:) : @selector(findAllInSelection:)) : ((options & find::backwards) ? @selector(findPrevious:) : @selector(findNext:)) withArgument:nil];
			if(!res.empty())
			{
				editor->set_selections(res);
				[self highlightRanges:res];
			}
		}
		break;

		case kFindOperationReplace:
		case kFindOperationReplaceInSelection:
		{
			std::string const findStr    = to_s(aFindServer.findString);
			std::string const replaceStr = to_s(aFindServer.replaceString);
			find::options_t options      = aFindServer.findOptions;
			[self recordSelector:(options & find::all_matches) ? (aFindServer.findOperation == kFindOperationReplace ? @selector(replaceAll:) : @selector(replaceAllInSelection:)) : @selector(replace:) withArgument:nil];

			ng::ranges_t const res = editor->replace(findStr, replaceStr, options, aFindServer.findOperation == kFindOperationReplaceInSelection);
			[aFindServer didReplace:res.size() occurrencesOf:aFindServer.findString with:aFindServer.replaceString];
		}
		break;
	}
}

- (void)setShowLiveSearch:(BOOL)flag
{
	OakDocumentView* documentView = (OakDocumentView*)[[self enclosingScrollView] superview];
	if(flag)
	{
		liveSearchAnchor = editor->ranges();

		if(!liveSearchViewController)
		{
			self.liveSearchViewController = [[NSViewController alloc] initWithNibName:@"SearchField" bundle:[NSBundle bundleForClass:[self class]]];
			[liveSearchViewController.view setAutoresizingMask:NSViewWidthSizable|NSViewMaxYMargin];
			[documentView addAuxiliaryView:liveSearchViewController.view atEdge:NSMinYEdge];
		}

		NSTextField* textField = [[liveSearchViewController.view subviews] lastObject];
		[textField setDelegate:self];
		[textField setStringValue:@""];

		liveSearchAnchor = editor->ranges();
		self.liveSearchString = nil;

		[[self window] makeFirstResponder:[[liveSearchViewController.view subviews] lastObject]];
		[[[self window] firstResponder] setNextResponder:self];
	}
	else if(liveSearchViewController)
	{
		[documentView removeAuxiliaryView:liveSearchViewController.view];
		[[self window] makeFirstResponder:self];
		self.liveSearchViewController = nil;
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
	[self setShowLiveSearch:YES];
}

- (IBAction)showBundlesMenu:(id)sender
{
	OakDocumentView* documentView = (OakDocumentView*)[[self enclosingScrollView] superview];
	[documentView performSelector:@selector(showBundleItemSelector:) withObject:nil];
}

- (IBAction)findNext:(id)sender
{
	if(liveSearchViewController)
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
		[self performFindOperation:[OakTextViewFindServer findServerWithTextView:self operation:kFindOperationFind options:[[OakPasteboard pasteboardWithName:NSFindPboard] current].findOptions]];
	}
}

- (IBAction)findPrevious:(id)sender
{
	if(liveSearchViewController)
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
		[self performFindOperation:[OakTextViewFindServer findServerWithTextView:self operation:kFindOperationFind options:[[OakPasteboard pasteboardWithName:NSFindPboard] current].findOptions | find::backwards]];
	}
}

- (IBAction)findAll:(id)sender               { [self performFindOperation:[OakTextViewFindServer findServerWithTextView:self operation:kFindOperationFind            options:[[OakPasteboard pasteboardWithName:NSFindPboard] current].findOptions | find::all_matches]]; }
- (IBAction)findAllInSelection:(id)sender    { [self performFindOperation:[OakTextViewFindServer findServerWithTextView:self operation:kFindOperationFindInSelection options:[[OakPasteboard pasteboardWithName:NSFindPboard] current].findOptions | find::all_matches]]; }

- (IBAction)replace:(id)sender               { [self performFindOperation:[OakTextViewFindServer findServerWithTextView:self operation:kFindOperationReplace            options:[[OakPasteboard pasteboardWithName:NSFindPboard] current].findOptions]]; }
- (IBAction)replaceAll:(id)sender            { [self performFindOperation:[OakTextViewFindServer findServerWithTextView:self operation:kFindOperationReplace            options:[[OakPasteboard pasteboardWithName:NSFindPboard] current].findOptions | find::all_matches]]; }
- (IBAction)replaceAllInSelection:(id)sender { [self performFindOperation:[OakTextViewFindServer findServerWithTextView:self operation:kFindOperationReplaceInSelection options:[[OakPasteboard pasteboardWithName:NSFindPboard] current].findOptions | find::all_matches]]; }
- (IBAction)replaceAndFind:(id)sender        { /* TODO replaceAndFind: */ }

- (void)findWithOptions:(NSDictionary*)someOptions
{
	AUTO_REFRESH;
	[self recordSelector:_cmd withArgument:someOptions];
	editor->find_dispatch(plist::convert(someOptions));
}

- (void)insertSnippetWithOptions:(NSDictionary*)someOptions // For Dialog popup
{
	AUTO_REFRESH;
	[self recordSelector:_cmd withArgument:someOptions];
	editor->snippet_dispatch(plist::convert(someOptions), editor->variables(std::map<std::string, std::string>()));
}

- (void)undo:(id)anArgument // MACRO?
{
	AUTO_REFRESH;
	if(!document->undo_manager().can_undo())
		return;
	editor->set_selections(document->undo_manager().undo());
}

- (void)redo:(id)anArgument // MACRO?
{
	AUTO_REFRESH;
	if(!document->undo_manager().can_redo())
		return;
	editor->set_selections(document->undo_manager().redo());
}

- (BOOL)expandTabTrigger:(id)sender
{
	if(editor->disallow_tab_expansion())
		return NO;

	AUTO_REFRESH;
	ng::range_t range;
	std::vector<bundles::item_ptr> const& items = items_for_tab_expansion(document->buffer(), editor->ranges(), document->path_attributes(), &range);
	if(bundles::item_ptr item = bundles::show_menu_for_items(items, [self positionForWindowUnderCaret]))
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
		editor->perform(ng::kInsertTab);
	}
}

- (BOOL)validateMenuItem:(NSMenuItem*)aMenuItem
{
	static SEL const RequiresSelection[] = { @selector(cut:), @selector(copy:), @selector(delete:), @selector(copySelectionToFindPboard:) };
	if(oak::contains(beginof(RequiresSelection), endof(RequiresSelection), [aMenuItem action]))
		return [self hasSelection];
	else if([aMenuItem action] == @selector(toggleShowInvisibles:))
		[aMenuItem setState:[self showInvisibles] ? NSOnState : NSOffState];
	else if([aMenuItem action] == @selector(toggleSoftWrap:))
		[aMenuItem setState:[self softWrap] ? NSOnState : NSOffState];
	else if([aMenuItem action] == @selector(toggleShowWrapColumn:))
		[aMenuItem setState:(layout && layout->draw_wrap_column()) ? NSOnState : NSOffState];
	else if([aMenuItem action] == @selector(takeWrapColumnFrom:))
	{
		static NSInteger const Presets[] = { NSWrapColumnWindowWidth, 40, 80 };
		[aMenuItem setState:wrapColumn == [aMenuItem tag] ? NSOnState : NSOffState];
		if([aMenuItem tag] == NSWrapColumnAskUser)
		{
			bool custom = !oak::contains(beginof(Presets), endof(Presets), wrapColumn);
			[aMenuItem setTitle:custom ? [NSString stringWithFormat:@"Other (%d)…", wrapColumn] : @"Other…"];
			[aMenuItem setState:custom ? NSOnState : NSOffState];
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
	NSTimer* oldBlinkCaretTimer = blinkCaretTimer;
	blinkCaretTimer = [aValue retain];
	[oldBlinkCaretTimer invalidate];
	[oldBlinkCaretTimer release];
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
	AUTO_REFRESH;
	layout->set_draw_caret(hideCaret);
	hideCaret = !hideCaret;
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
- (size_t)tabSize             { return document ? document->buffer().indent().tab_size() : 2; }
- (BOOL)softTabs              { return document ? document->buffer().indent().soft_tabs() : NO; }
- (BOOL)showInvisibles        { return showInvisibles; }
- (BOOL)softWrap              { return layout && layout->wrapping(); }

- (void)setTheme:(theme_ptr const&)newTheme
{
	theme = newTheme;

	if(document)
	{
		auto bgColor = theme->styles_for_scope(document->buffer().scope(0).left, fontName, fontSize).background();
		[[self enclosingScrollView] setBackgroundColor:[NSColor colorWithCGColor:bgColor]];
		SetLionScrollerKnobStyle([self enclosingScrollView], cf::color_is_dark(bgColor) ? NSScrollerKnobStyleLight : NSScrollerKnobStyleDark);
		[[self window] setOpaque:CGColorGetAlpha(bgColor) == 1];
	}

	if(layout)
	{
		AUTO_REFRESH;
		layout->set_theme(newTheme);
	}

	[[self window] invalidateCursorRectsForView:self];
}

- (void)setFont:(NSFont*)newFont
{
	fontName = to_s([newFont fontName]);
	fontSize = [newFont pointSize];

	if(layout)
	{
		AUTO_REFRESH;
		layout->set_font(fontName, fontSize);
	}
}

- (void)setTabSize:(size_t)newTabSize
{
	AUTO_REFRESH;
	if(document)
		document->buffer().indent().set_tab_size(newTabSize);
}

- (void)setShowInvisibles:(BOOL)flag
{
	showInvisibles = flag;
	[self setNeedsDisplay:YES];
}

- (void)setSoftWrap:(BOOL)flag
{
	if(layout)
	{
		AUTO_REFRESH;
		layout->set_wrapping(flag, wrapColumn);
	}
}

- (void)setSoftTabs:(BOOL)flag
{
	if(flag != self.softTabs)
		document->buffer().indent().set_soft_tabs(flag);
}

- (void)takeWrapColumnFrom:(id)sender
{
	ASSERT([sender respondsToSelector:@selector(tag)]);
	if(wrapColumn == [sender tag])
		return;

	// TODO Soft wrap
	// if(wrapColumn == NSWrapColumnAskUser)
	// 	;

	wrapColumn = [sender tag];
	settings_t::set(kSettingsWrapColumnKey, wrapColumn);
	if(layout)
	{
		AUTO_REFRESH;
		layout->set_wrapping(self.softWrap, wrapColumn);
	}
}

- (BOOL)hasMultiLineSelection { return multiline(document->buffer(), editor->ranges()); }

- (BOOL)freehandedEditing     { return NO; }
- (BOOL)overwriteMode         { return NO; }

- (IBAction)toggleShowInvisibles:(id)sender
{
	self.showInvisibles = !self.showInvisibles;
	settings_t::set(kSettingsShowInvisiblesKey, (bool)self.showInvisibles, document->file_type());
}

- (IBAction)toggleSoftWrap:(id)sender
{
	self.softWrap = !self.softWrap;
	settings_t::set(kSettingsSoftWrapKey, (bool)self.softWrap, document->file_type());
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

- (scope::context_t const&)scope
{
	static scope::context_t res;
	return res = editor->scope();
}

- (void)setSelectionString:(NSString*)aSelectionString
{
	if([aSelectionString isEqualToString:selectionString])
		return;

	[selectionString release];
	selectionString = [aSelectionString copy];
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
	text::selection_t ranges;
	citerate(range, editor->ranges())
	{
		text::pos_t from = document->buffer().convert(range->first.index);
		text::pos_t to   = document->buffer().convert(range->last.index);
		from.offset = range->first.carry;
		to.offset   = range->last.carry;
		ranges.push_back(text::range_t(from, to, range->columnar));
	}
	document->set_selection(ranges);

	isUpdatingSelection = YES;
	[self setSelectionString:[NSString stringWithCxxString:ranges]];
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

- (GVLineRecord const&)lineRecordForPosition:(CGFloat)yPos
{
	static GVLineRecord res;
	if(!layout)
		return res = GVLineRecord();
	auto record = layout->line_record_for(yPos);
	return res = GVLineRecord(record.line, record.softline, record.top, record.bottom, record.baseline);
}

- (GVLineRecord const&)lineFragmentForLine:(NSUInteger)aLine column:(NSUInteger)aColumn
{
	static GVLineRecord res;
	if(!layout)
		return res = GVLineRecord();
	auto record = layout->line_record_for(text::pos_t(aLine, aColumn));
	return res = GVLineRecord(record.line, record.softline, record.top, record.bottom, record.baseline);
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
		D(DBF_OakTextView_Macros, bug("%s\n", to_s(plist::convert(macroRecordingArray)).c_str()););
		[[NSUserDefaults standardUserDefaults] setObject:[[macroRecordingArray copy] autorelease] forKey:@"OakMacroManagerScratchMacro"];
		[macroRecordingArray release];
		macroRecordingArray = nil;
	}
	else
	{
		macroRecordingArray = [NSMutableArray new];
	}
}

- (IBAction)playScratchMacro:(id)anArgument
{
	D(DBF_OakTextView_Macros, bug("%s\n", to_s(plist::convert([[NSUserDefaults standardUserDefaults] arrayForKey:@"OakMacroManagerScratchMacro"])).c_str()););
	AUTO_REFRESH;
	if(NSArray* scratchMacro = [[NSUserDefaults standardUserDefaults] arrayForKey:@"OakMacroManagerScratchMacro"])
			editor->macro_dispatch(plist::convert(@{ @"commands" : scratchMacro }), editor->variables(std::map<std::string, std::string>()));
	else	NSBeep();
}

- (void)recordSelector:(SEL)aSelector withArgument:(id)anArgument
{
	if(!macroRecordingArray)
		return;

	D(DBF_OakTextView_Macros, bug("%s, %s\n", (char*)aSelector, [[anArgument description] UTF8String]););
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

	scope::context_t scope = editor->scope();
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
	else if(bundles::item_ptr handler = bundles::show_menu_for_items(std::vector<bundles::item_ptr>(allHandlers.begin(), allHandlers.end()), [self positionForWindowUnderCaret]))
	{
		struct callback_t : document::run_callback_t
		{
			callback_t (std::vector<std::string> const& paths, std::string const& modifierFlags) : _paths(paths), _modifier_flags(modifierFlags) { }

			void update_environment (std::map<std::string, std::string>& env)
			{
				env["PWD"] = format_string::expand("${TM_DIRECTORY:-${TM_PROJECT_DIRECTORY:-$TMPDIR}}", env);
				env["TM_MODIFIER_FLAGS"] = _modifier_flags;

				std::vector<std::string> files;
				iterate(path, _paths)
					files.push_back(path::relative_to(*path, env["PWD"]));

				env["TM_DROPPED_FILE"]     = files.front();
				env["TM_DROPPED_FILEPATH"] = _paths.front();

				if(files.size() > 1)
				{
					env["TM_DROPPED_FILES"]     = shell_quote(files);
					env["TM_DROPPED_FILEPATHS"] = shell_quote(_paths);
				}
			}

		private:
			std::vector<std::string> _paths;
			std::string _modifier_flags;
		};

		D(DBF_OakTextView_DragNDrop, bug("execute %s\n", handler->full_name().c_str()););

		static struct { NSUInteger qual; std::string name; } const qualNames[] =
		{
			{ NSShiftKeyMask,     "SHIFT"    },
			{ NSControlKeyMask,   "CONTROL"  },
			{ NSAlternateKeyMask, "OPTION"   },
			{ NSCommandKeyMask,   "COMMAND"  }
		};

		NSUInteger state = [NSEvent slModifierFlags];
		std::vector<std::string> flagNames;
		for(size_t i = 0; i != sizeofA(qualNames); ++i)
		{
			if(state & qualNames[i].qual)
				flagNames.push_back(qualNames[i].name);
		}

		document::run(parse_drag_command(handler), document->buffer(), editor->ranges(), document, std::map<std::string, std::string>(), document::run_callback_ptr((document::run_callback_t*)new callback_t(handlerToFiles[handler->uuid()], text::join(flagNames, "|"))));
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

- (NSCursor*)IBeamCursor
{
	NSCursor* ibeamRegular = [NSCursor IBeamCursor];
	
	if(cf::color_is_dark(theme->styles_for_scope(document->buffer().scope(0).left, fontName, fontSize).background()))
	{
		static NSCursor* ibeamCursor = nil;
		if(!ibeamCursor)
		{
			NSImage* ibeamWhite = [NSImage imageNamed:@"IBeam white" inSameBundleAsClass:[self class]];
			[ibeamWhite setSize:[[ibeamRegular image] size]];
			ibeamCursor = [[NSCursor alloc] initWithImage:ibeamWhite hotSpot:NSMakePoint(4, 9)];
		}
		return ibeamCursor;
	}
	return ibeamRegular;
}

- (void)resetCursorRects
{
	D(DBF_OakTextView_MouseEvents, bug("drag: %s, column selection: %s\n", BSTR(showDragCursor), BSTR(showColumnSelectionCursor)););
	[self addCursorRect:[self visibleRect] cursor:showDragCursor ? [NSCursor arrowCursor] : (showColumnSelectionCursor ? [NSCursor crosshairCursor] : [self IBeamCursor])];
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
	self.antiAlias = ![[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsDisableAntiAliasKey];
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
		if(s.size() > 1 && std::find(beginof(s), endof(s), range.last()) != endof(s))
		{
			ng::ranges_t newSel;
			citerate(cur, s)
			{
				if(*cur != range.last())
					newSel.push_back(*cur);
			}
			s = newSel;
		}
		else
		{
			s.push_back(range.last());
		}
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
}

- (void)startDragForEvent:(NSEvent*)anEvent
{
	ASSERT(layout);

	NSRect srcRect;
	ng::ranges_t const ranges = ng::dissect_columnar(document->buffer(), editor->ranges());
	NSImage* srcImage = [self imageForRanges:ranges imageRect:&srcRect];

	NSImage* image = [[[NSImage alloc] initWithSize:srcImage.size] autorelease];
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
	static struct { NSUInteger modifier; std::string scope; } const map[] =
	{
		{ NSShiftKeyMask,      "dyn.modifier.shift"   },
		{ NSControlKeyMask,    "dyn.modifier.control" },
		{ NSAlternateKeyMask,  "dyn.modifier.option"  },
		{ NSCommandKeyMask,    "dyn.modifier.command" }
	};

	for(size_t i = 0; i < sizeofA(map); ++i)
	{
		if(modifiers & map[i].modifier)
		{
			scope.left  = scope.left.append(map[i].scope);
			scope.right = scope.right.append(map[i].scope);
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

	std::vector<bundles::item_ptr> const& items = bundles::query(bundles::kFieldSemanticClass, "callback.mouse-click", add_modifiers_to_scope(ng::scope(document->buffer(), layout->index_at_point([self convertPoint:[anEvent locationInWindow] fromView:nil]), document->path_attributes()), [anEvent modifierFlags]));
	if(!items.empty())
	{
		if(bundles::item_ptr item = bundles::show_menu_for_items(items, [self positionForWindowUnderCaret]))
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
	NSUInteger oldState = self.keyState;
	[super setKeyState:newState];

	BOOL doesHaveFocus = (self.keyState & (OakViewViewIsFirstResponderMask|OakViewWindowIsKeyMask|OakViewApplicationIsActiveMask)) == (OakViewViewIsFirstResponderMask|OakViewWindowIsKeyMask|OakViewApplicationIsActiveMask);
	BOOL didHaveFocus = (oldState & (OakViewViewIsFirstResponderMask|OakViewWindowIsKeyMask|OakViewApplicationIsActiveMask)) == (OakViewViewIsFirstResponderMask|OakViewWindowIsKeyMask|OakViewApplicationIsActiveMask);

	if(!didHaveFocus && doesHaveFocus)
	{
		[[NSFontManager sharedFontManager] setSelectedFont:self.font isMultiple:NO];
		self.blinkCaretTimer = [NSTimer scheduledTimerWithTimeInterval:[NSEvent caretBlinkInterval] target:self selector:@selector(toggleCaretVisibility:) userInfo:nil repeats:YES];
		[self setShowLiveSearch:NO];
	}
	else if(didHaveFocus && !doesHaveFocus)
	{
		self.blinkCaretTimer = nil;
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
}

// ===========
// = Actions =
// ===========

- (void)handleAction:(ng::action_t)anAction forSelector:(SEL)aSelector
{
	AUTO_REFRESH;
	[self recordSelector:aSelector withArgument:nil];
	editor->perform(anAction, layout.get());
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
- ACTION(deleteToBeginningOfLine);
- ACTION(deleteToBeginningOfParagraph);
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
