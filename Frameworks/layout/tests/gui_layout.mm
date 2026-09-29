#import <layout/src/layout.h>
#import <buffer/src/buffer.h>
#import <selection/src/selection.h>
#import <bundles/src/bundles.h>
#import <io/src/path.h>
#import <ns/src/ns.h>
#import <text/src/utf8.h>
#import <test/cocoa.h>
#import <oak/duration.h>

// Surrogate: 𠻵
// Diacritics: c̄̌
// Thai: ไปกินปูดำที่บ้านกตัญญู
// Right-to-left: ومصادر اخرى ايضاً تُشير بأن اسم الآيفون سيكون
// Double width: 南野 繁弘
// Long line: Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.
// No spaces: Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.

typedef std::shared_ptr<ng::layout_t> layout_ptr;

@interface MyView : NSView
{
	ng::buffer_t buffer;
	layout_ptr layout;
}
@property (nonatomic) NSDate* optionDownDate;
@property (nonatomic) ng::ranges_t selection;
- (void)updateFrameSize;
@end

static void random_insert (ng::buffer_t& dst, std::string const& src)
{
	std::vector<size_t> lengths;
	for(size_t i = 0; i < src.size(); i += lengths.back())
	{
		size_t len = std::min<size_t>(arc4random_uniform(15) + 15, src.size() - i);
		while(i + len < src.size() && src[i + len - 1] != ' ')
			++len;
		lengths.push_back(len);
	}

	std::vector<size_t> ordering(lengths.size());
	std::iota(ordering.begin(), ordering.end(), 0);
	oak::random_shuffle(ordering.begin(), ordering.end());

	std::vector<size_t> srcOffsets(lengths.size(), 0);
	struct fragment_t { size_t dst, src, len; };
	std::vector<fragment_t> insertRanges;
	for(size_t index : ordering)
	{
		size_t offset = std::accumulate(lengths.begin(), lengths.begin() + index, 0);
		insertRanges.push_back((fragment_t){ srcOffsets[index], offset, lengths[index] });
		for(size_t i = index + 1; i < srcOffsets.size(); ++i)
			srcOffsets[i] += lengths[index];
	}

	oak::duration_t timer;
	for(auto const& range : insertRanges)
		dst.replace(range.dst, range.dst, src.substr(range.src, range.len));
	fprintf(stderr, "%.1f seconds to insert and layout %zu substrings (%s)\n", timer.duration(), insertRanges.size(), text::format_size(src.size()).c_str());
	dst.bump_revision();
}

struct refresh_t
{
	refresh_t (MyView* self, ng::layout_t& layout) : _self(self), _layout(layout)
	{
		_layout.begin_refresh_cycle([_self selection]);
	}

	~refresh_t ()
	{
		auto damagedRects = _layout.end_refresh_cycle([_self selection], [_self visibleRect]);
		NSCAssert(_layout.structural_integrity(), @"");
		[_self updateFrameSize];
		for(auto const& rect : damagedRects)
			[_self setNeedsDisplayInRect:rect];

		if(!NSContainsRect([_self visibleRect], _layout.rect_at_index([_self selection].last().last)))
			[_self scrollRectToVisible:NSInsetRect(_layout.rect_at_index([_self selection].last().last), -15, -15)];
	}

private:
	MyView* _self;
	ng::layout_t& _layout;
};

#define AUTO_REFRESH refresh_t dummy(self, *layout)

@implementation MyView
- (BOOL)isFlipped { return YES; }

- (void)updateFrameSize
{
	NSSize s = [[self enclosingScrollView] documentVisibleRect].size;
	[self setFrameSize:NSMakeSize(std::max(s.width, layout->width()), std::max(s.height, layout->height()))];
}

- (void)didParseFrom:(size_t)from to:(size_t)to
{
	AUTO_REFRESH;
	layout->did_update_scopes(from, to);
}

- (id)initWithFrame:(NSRect)aRect
{
	if((self = [super initWithFrame:aRect]))
	{
		struct buffer_refresh_callback_t : ng::callback_t
		{
			buffer_refresh_callback_t (MyView* self) : _self(self) { }
			void did_parse (size_t from, size_t to)
			{
				[_self didParseFrom:from to:to];
			}
		private:
			MyView* _self;
		};

		self.autoresizingMask = 0;

		_selection = ng::ranges_t(0);

		static buffer_refresh_callback_t cb(self);
		buffer.add_callback(&cb);

		for(auto const& item : bundles::query(bundles::kFieldGrammarScope, "source.c++"))
			buffer.set_grammar(item);

		theme_ptr theme = parse_theme(bundles::lookup(kMacClassicThemeUUID));
		layout.reset(new ng::layout_t(buffer, theme, "GillSans", 14, true));
		layout->set_viewport_size([[self enclosingScrollView] documentVisibleRect].size);
	}
	return self;
}

- (BOOL)acceptsFirstResponder
{
	return YES;
}

- (BOOL)becomeFirstResponder
{
	AUTO_REFRESH;
	layout->set_is_key(true);
	layout->set_draw_caret(true);
	return YES;
}

- (BOOL)resignFirstResponder
{
	AUTO_REFRESH;
	layout->set_is_key(false);
	layout->set_draw_caret(false);
	return YES;
}

- (void)viewFrameDidChange:(NSNotification*)aNotification
{
	AUTO_REFRESH;
	layout->set_viewport_size([[self enclosingScrollView] documentVisibleRect].size);
}

- (void)viewDidMoveToSuperview
{
	[self setAutoresizingMask:0];
}

- (void)viewDidMoveToWindow
{
	if(![self window])
		return;

	layout->begin_refresh_cycle(_selection);
	random_insert(buffer, path::content(__FILE__));
	layout->end_refresh_cycle(_selection, [self visibleRect]);

	[self updateFrameSize];
	[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(viewFrameDidChange:) name:NSViewFrameDidChangeNotification object:[self enclosingScrollView]];
}

- (void)drawRect:(NSRect)aRect
{
	layout->draw((CGContextRef)[[NSGraphicsContext currentContext] graphicsPort], aRect, [self isFlipped], _selection);
}

- (ng::ranges_t const&)replaceSelection:(ng::ranges_t const&)someRanges withString:(std::string const&)aString
{
	static ng::ranges_t res;
	res = ng::ranges_t();
	ssize_t offset = 0;
	for(auto const& range : dissect_columnar(buffer, someRanges).sorted())
	{
		ng::index_t from = range.min(), to = range.max();
		if(range.freehanded && from.carry)
		{
			buffer.replace(from.index + offset, from.index + offset, std::string(from.carry, ' '));
			offset += from.carry;
		}

		res.push_back(buffer.replace(from.index + offset, to.index + offset, aString));
		offset += aString.size() - (range.max().index - range.min().index);
	}
	return res;
}

- (void)insertText:(id)someText
{
	_selection = [self replaceSelection:_selection withString:[someText UTF8String]];
}

- (void)flagsChanged:(NSEvent*)anEvent
{
	// this checks if the ‘flags changed’ is caused by left/right option — the virtual key codes aren’t documented anywhere and in theory they could correspond to other keys, but worst case user lose the ability to toggle column selection by single-clicking option
	if([anEvent keyCode] != 58 && [anEvent keyCode] != 61)
		return;

	NSInteger modifiers   = [anEvent modifierFlags] & (NSEventModifierFlagOption | NSEventModifierFlagControl | NSEventModifierFlagCommand);
	BOOL didPressOption   = modifiers == NSEventModifierFlagOption;
	BOOL didReleaseOption = modifiers == 0 && _optionDownDate && [[NSDate date] timeIntervalSinceDate:_optionDownDate] < 0.18;

	self.optionDownDate = didPressOption ? [NSDate date] : nil;

	if(didReleaseOption)
	{
		AUTO_REFRESH;
		_selection = toggle_columnar(_selection);
	}
}

- (void)keyDown:(NSEvent*)anEvent
{
	static std::string const kLeftArrowKeyCode      = utf8::to_s(NSLeftArrowFunctionKey);
	static std::string const kRightArrowKeyCode     = utf8::to_s(NSRightArrowFunctionKey);
	static std::string const kUpArrowKeyCode        = utf8::to_s(NSUpArrowFunctionKey);
	static std::string const kDownArrowKeyCode      = utf8::to_s(NSDownArrowFunctionKey);
	static std::string const kDeleteForwardKeyCode  = utf8::to_s(NSDeleteCharacter);
	static std::string const kDeleteBackwardKeyCode = utf8::to_s(NSDeleteFunctionKey);

	AUTO_REFRESH;
	std::string const key = to_s(anEvent);

	if(key == kLeftArrowKeyCode)
		_selection = ng::move(buffer, _selection, kSelectionMoveLeft, layout.get());
	else if(key == kRightArrowKeyCode)
		_selection = ng::move(buffer, _selection, kSelectionMoveRight, layout.get());
	else if(key == kUpArrowKeyCode)
		_selection = ng::move(buffer, _selection, kSelectionMoveUp, layout.get());
	else if(key == kDownArrowKeyCode)
		_selection = ng::move(buffer, _selection, kSelectionMoveDown, layout.get());

	else if(key == "@" + kLeftArrowKeyCode || key == "^a")
		_selection = ng::move(buffer, _selection, kSelectionMoveToBeginOfSoftLine, layout.get());
	else if(key == "@" + kRightArrowKeyCode || key == "^e")
		_selection = ng::move(buffer, _selection, kSelectionMoveToEndOfSoftLine, layout.get());
	else if(key == "@" + kUpArrowKeyCode)
		_selection = ng::move(buffer, _selection, kSelectionMoveToBeginOfDocument, layout.get());
	else if(key == "@" + kDownArrowKeyCode)
		_selection = ng::move(buffer, _selection, kSelectionMoveToEndOfDocument, layout.get());

	else if(key == "~" + kLeftArrowKeyCode)
		_selection = ng::move(buffer, _selection, kSelectionMoveToBeginOfWord, layout.get());
	else if(key == "~" + kRightArrowKeyCode)
		_selection = ng::move(buffer, _selection, kSelectionMoveToEndOfWord, layout.get());
	else if(key == "~" + kUpArrowKeyCode)
		_selection = ng::move(buffer, _selection, kSelectionMoveToBeginOfColumn, layout.get());
	else if(key == "~" + kDownArrowKeyCode)
		_selection = ng::move(buffer, _selection, kSelectionMoveToEndOfColumn, layout.get());

	else if(key == "^" + kLeftArrowKeyCode)
		_selection = ng::move(buffer, _selection, kSelectionMoveToBeginOfSubWord, layout.get());
	else if(key == "^" + kRightArrowKeyCode)
		_selection = ng::move(buffer, _selection, kSelectionMoveToEndOfSubWord, layout.get());
	else if(key == "^" + kUpArrowKeyCode)
		_selection = ng::move(buffer, _selection, kSelectionMoveToBeginOfParagraph, layout.get());
	else if(key == "^" + kDownArrowKeyCode)
		_selection = ng::move(buffer, _selection, kSelectionMoveToEndOfParagraph, layout.get());

	else if(key == "$" + kLeftArrowKeyCode)
		_selection = ng::extend(buffer, _selection, kSelectionExtendLeft, layout.get());
	else if(key == "$" + kRightArrowKeyCode)
		_selection = ng::extend(buffer, _selection, kSelectionExtendRight, layout.get());
	else if(key == "$" + kUpArrowKeyCode)
		_selection = ng::extend(buffer, _selection, kSelectionExtendUp, layout.get());
	else if(key == "$" + kDownArrowKeyCode)
		_selection = ng::extend(buffer, _selection, kSelectionExtendDown, layout.get());

	else if(key == "~$" + kLeftArrowKeyCode)
		_selection = ng::extend(buffer, _selection, kSelectionExtendToBeginOfWord, layout.get());
	else if(key == "~$" + kRightArrowKeyCode)
		_selection = ng::extend(buffer, _selection, kSelectionExtendToEndOfWord, layout.get());
	else if(key == "~$" + kUpArrowKeyCode)
		_selection = ng::extend(buffer, _selection, kSelectionExtendToBeginOfColumn, layout.get());
	else if(key == "~$" + kDownArrowKeyCode)
		_selection = ng::extend(buffer, _selection, kSelectionExtendToEndOfColumn, layout.get());

	else if(key == "^$" + kLeftArrowKeyCode)
		_selection = ng::extend(buffer, _selection, kSelectionExtendToBeginOfSubWord, layout.get());
	else if(key == "^$" + kRightArrowKeyCode)
		_selection = ng::extend(buffer, _selection, kSelectionExtendToEndOfSubWord, layout.get());
	else if(key == "^$" + kUpArrowKeyCode)
		_selection = ng::extend(buffer, _selection, kSelectionExtendToBeginOfParagraph, layout.get());
	else if(key == "^$" + kDownArrowKeyCode)
		_selection = ng::extend(buffer, _selection, kSelectionExtendToEndOfParagraph, layout.get());

	else if(key == "$@" + kLeftArrowKeyCode)
		_selection = ng::extend(buffer, _selection, kSelectionExtendToBeginOfSoftLine, layout.get());
	else if(key == "$@" + kRightArrowKeyCode)
		_selection = ng::extend(buffer, _selection, kSelectionExtendToEndOfSoftLine, layout.get());
	else if(key == "$@" + kUpArrowKeyCode)
		_selection = ng::extend(buffer, _selection, kSelectionExtendToBeginOfDocument, layout.get());
	else if(key == "$@" + kDownArrowKeyCode)
		_selection = ng::extend(buffer, _selection, kSelectionExtendToEndOfDocument, layout.get());

	else if(key == "^w")
		_selection = ng::extend(buffer, _selection, kSelectionExtendToWord, layout.get());
	else if(key == "^~b")
		_selection = ng::extend(buffer, _selection, kSelectionExtendToScope, layout.get());
	else if(key == "^l")
		_selection = ng::extend(buffer, _selection, kSelectionExtendToSoftLine, layout.get());
	else if(key == "@L")
		_selection = ng::extend(buffer, _selection, kSelectionExtendToLine, layout.get());
	else if(key == "@a")
		_selection = ng::extend(buffer, _selection, kSelectionExtendToAll, layout.get());

	else if(key == kDeleteForwardKeyCode)
		_selection = [self replaceSelection:ng::extend_if_empty(buffer, _selection, kSelectionExtendLeft, layout.get()) withString:""];
	else if((key == kDeleteBackwardKeyCode || key == "^d"))
		_selection = [self replaceSelection:ng::extend_if_empty(buffer, _selection, kSelectionExtendRight, layout.get()) withString:""];

	else if(key == "~" + kDeleteForwardKeyCode)
		_selection = [self replaceSelection:ng::extend_if_empty(buffer, _selection, kSelectionExtendToBeginOfWord, layout.get()) withString:""];
	else if(key == "~" + kDeleteBackwardKeyCode)
		_selection = [self replaceSelection:ng::extend_if_empty(buffer, _selection, kSelectionExtendToEndOfWord, layout.get()) withString:""];
	else if(key == "^" + kDeleteForwardKeyCode)
		_selection = [self replaceSelection:ng::extend_if_empty(buffer, _selection, kSelectionExtendToBeginOfSubWord, layout.get()) withString:""];
	else if(key == "^" + kDeleteBackwardKeyCode)
		_selection = [self replaceSelection:ng::extend_if_empty(buffer, _selection, kSelectionExtendToEndOfSubWord, layout.get()) withString:""];
	else if(key == "@" + kDeleteForwardKeyCode)
		_selection = [self replaceSelection:ng::extend_if_empty(buffer, _selection, kSelectionExtendToBeginOfSoftLine, layout.get()) withString:""];
	else if(key == "@" + kDeleteBackwardKeyCode)
		_selection = [self replaceSelection:ng::extend_if_empty(buffer, _selection, kSelectionExtendToEndOfSoftLine, layout.get()) withString:""];

	else if(key == "\r")
		_selection = [self replaceSelection:_selection withString:"\n"];
	else if(key == "\t")
		_selection = [self replaceSelection:_selection withString:"\t"];

	else if(key == utf8::to_s(NSF1FunctionKey))
		layout->toggle_fold_at_line(buffer.convert(_selection.last().last.index).line, false);
	else if(key == utf8::to_s(NSF2FunctionKey))
		_selection = ng::select_scope(buffer, _selection, "string");
	else if(key == "^p")
		fprintf(stderr, "%s\n", to_s(ng::scope(buffer, _selection)).c_str());
	else if(key == "^s")
		fprintf(stderr, "%s\n", to_s(_selection).c_str());
	else
		[self interpretKeyEvents:@[ anEvent ]];
}

- (void)mouseDown:(NSEvent*)anEvent
{
	AUTO_REFRESH;

	CGPoint pos = [self convertPoint:[anEvent locationInWindow] fromView:nil];
	NSUInteger modifierFlags = [anEvent modifierFlags] & (NSEventModifierFlagShift|NSEventModifierFlagControl|NSEventModifierFlagOption|NSEventModifierFlagCommand);
	bool optionDown  = modifierFlags & NSEventModifierFlagOption;
	bool shiftDown   = modifierFlags & NSEventModifierFlagShift;
	bool commandDown = modifierFlags & NSEventModifierFlagCommand;

	ng::index_t index = layout->index_at_point(pos);
	if(!optionDown)
		index.carry = 0;

	if(shiftDown)
		_selection.last() = ng::range_t(_selection.last().first, index, optionDown, false);
	else if(commandDown)
		_selection.push_back(ng::range_t(index, index, false, optionDown));
	else
		_selection = ng::range_t(index, index, false, optionDown);
}

- (void)rightMouseDown:(NSEvent*)anEvent
{
	NSLog(@"%s", sel_getName(_cmd));
}
@end

static NSScrollView* OakCreateTextView (NSRect aRect = NSMakeRect(0, 0, 600, 800))
{
	NSScrollView* scrollView = [[NSScrollView alloc] initWithFrame:aRect];
	NSSize textViewSize = scrollView.contentSize;

	MyView* textView = [[MyView alloc] initWithFrame:NSMakeRect(0, 0, textViewSize.width, textViewSize.height)];
	textView.autoresizingMask = NSViewWidthSizable;

	scrollView.hasVerticalScroller   = YES;
	scrollView.hasHorizontalScroller = YES;
	scrollView.autohidesScrollers    = YES;
	scrollView.borderType            = NSNoBorder;
	scrollView.autoresizingMask      = NSViewWidthSizable|NSViewHeightSizable;
	scrollView.documentView          = textView;

	return scrollView;
}

#if 0
static class LayoutFixture : public CxxTest::GlobalFixture
{
public:
	bool setUpWorld()
	{
		// [BundlesManager sharedInstance]; // load bundles
		return true;
	}

} fixture;

class LayoutTests : public CxxTest::TestSuite
{
public:
	void test_layout ()
	{
		OakSetupApplicationWithView(OakCreateTextView());
	}
};
#endif
