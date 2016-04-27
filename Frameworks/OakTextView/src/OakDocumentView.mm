#import "OakDocumentView.h"
#import "GutterView.h"
#import "OTVStatusBar.h"
#import <document/document.h>
#import <file/type.h>
#import <text/ctype.h>
#import <text/parse.h>
#import <ns/ns.h>
#import <oak/debug.h>
#import <bundles/bundles.h>
#import <OakFilterList/SymbolChooser.h>
#import <OakFoundation/NSString Additions.h>
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/NSColor Additions.h>
#import <OakAppKit/NSImage Additions.h>
#import <OakAppKit/OakToolTip.h>
#import <OakAppKit/OakPasteboard.h>
#import <OakAppKit/OakPasteboardChooser.h>
#import <OakAppKit/OakUIConstructionFunctions.h>
#import <OakAppKit/NSMenuItem Additions.h>
#import <BundleMenu/BundleMenu.h>

OAK_DEBUG_VAR(OakDocumentView);

static NSString* const kUserDefaultsLineNumberScaleFactorKey = @"lineNumberScaleFactor";

static NSString* const kBookmarksColumnIdentifier = @"bookmarks";
static NSString* const kFoldingsColumnIdentifier  = @"foldings";

@interface OakDisableAccessibilityScrollView : NSScrollView
@end

@implementation OakDisableAccessibilityScrollView
- (BOOL)accessibilityIsIgnored
{
	return YES;
}
@end

@interface OakDocumentView () <GutterViewDelegate, GutterViewColumnDataSource, GutterViewColumnDelegate, OTVStatusBarDelegate>
{
	OBJC_WATCH_LEAKS(OakDocumentView);

	NSScrollView* gutterScrollView;
	GutterView* gutterView;
	NSColor* gutterDividerColor;
	NSMutableDictionary* gutterImages;

	OakBackgroundFillView* gutterDividerView;
	OakBackgroundFillView* statusDividerView;

	NSScrollView* textScrollView;
	OakTextView* textView;
	OTVStatusBar* statusBar;
	document::document_ptr document;
	document::document_t::callback_t* callback;

	NSMutableArray* topAuxiliaryViews;
	NSMutableArray* bottomAuxiliaryViews;

	IBOutlet NSPanel* tabSizeSelectorPanel;
}
@property (nonatomic, readonly) OTVStatusBar* statusBar;
@property (nonatomic) SymbolChooser* symbolChooser;
@property (nonatomic) NSArray* observedKeys;
- (void)updateStyle;
@end

struct document_view_callback_t : document::document_t::callback_t
{
	WATCH_LEAKS(document_view_callback_t);
	document_view_callback_t (OakDocumentView* self) : self(self) { }
	void handle_document_event (document::document_ptr document, event_t event)
	{
		if(event == did_change_marks)
		{
			[[NSNotificationCenter defaultCenter] postNotificationName:GVColumnDataSourceDidChange object:self];
		}
		else if(event == did_change_file_type)
		{
			for(auto const& item : bundles::query(bundles::kFieldGrammarScope, document->file_type()))
				self.statusBar.grammarName = [NSString stringWithCxxString:item->name()];
		}
		else if(event == did_change_indent_settings)
		{
			self.statusBar.tabSize  = document->buffer().indent().tab_size();
			self.statusBar.softTabs = document->buffer().indent().soft_tabs();
		}

		if(document->recent_tracking() && path::exists(document->path()))
		{
			if(event == did_save || event == did_change_path || (event == did_change_open_status && document->is_open()))
				[[NSDocumentController sharedDocumentController] noteNewRecentDocumentURL:[NSURL fileURLWithPath:[NSString stringWithCxxString:document->path()]]];
		}
	}
private:
	__weak OakDocumentView* self;
};

@implementation OakDocumentView
@synthesize textView, statusBar;

- (id)initWithFrame:(NSRect)aRect
{
	D(DBF_OakDocumentView, bug("%s\n", [NSStringFromRect(aRect) UTF8String]););
	if(self = [super initWithFrame:aRect])
	{
		callback = new document_view_callback_t(self);

		textView = [[OakTextView alloc] initWithFrame:NSZeroRect];
		textView.autoresizingMask = NSViewWidthSizable|NSViewHeightSizable;

		textScrollView = [[NSScrollView alloc] initWithFrame:NSZeroRect];
		textScrollView.hasVerticalScroller   = YES;
		textScrollView.hasHorizontalScroller = YES;
		textScrollView.autohidesScrollers    = YES;
		textScrollView.borderType            = NSNoBorder;
		textScrollView.documentView          = textView;

		gutterView = [[GutterView alloc] initWithFrame:NSZeroRect];
		gutterView.partnerView = textView;
		gutterView.delegate    = self;
		[gutterView insertColumnWithIdentifier:kBookmarksColumnIdentifier atPosition:0 dataSource:self delegate:self];
		[gutterView insertColumnWithIdentifier:kFoldingsColumnIdentifier atPosition:2 dataSource:self delegate:self];
		if([[NSUserDefaults standardUserDefaults] boolForKey:@"DocumentView Disable Line Numbers"])
			[gutterView setVisibility:NO forColumnWithIdentifier:GVLineNumbersColumnIdentifier];
		[gutterView setTranslatesAutoresizingMaskIntoConstraints:NO];

		gutterScrollView = [[OakDisableAccessibilityScrollView alloc] initWithFrame:NSZeroRect];
		gutterScrollView.borderType   = NSNoBorder;
		gutterScrollView.documentView = gutterView;

		gutterDividerView = OakCreateVerticalLine(nil);
		statusDividerView = OakCreateHorizontalLine([NSColor colorWithCalibratedWhite:0.500 alpha:1], [NSColor colorWithCalibratedWhite:0.750 alpha:1]);

		statusBar = [[OTVStatusBar alloc] initWithFrame:NSZeroRect];
		statusBar.delegate = self;
		statusBar.target = self;

		OakAddAutoLayoutViewsToSuperview(@[ gutterScrollView, gutterDividerView, textScrollView, statusDividerView, statusBar ], self);
		OakSetupKeyViewLoop(@[ self, textView, statusBar ], NO);

		document::document_ptr doc = document::from_content("", "text.plain"); // file type is only to avoid potential “no grammar” warnings in console
		doc->set_custom_name("null document"); // without a name it grabs an ‘untitled’ token
		[self setDocument:doc];

		self.observedKeys = @[ @"selectionString", @"tabSize", @"softTabs", @"isMacroRecording"];
		for(NSString* keyPath in self.observedKeys)
			[textView addObserver:self forKeyPath:keyPath options:NSKeyValueObservingOptionInitial context:NULL];
	}
	return self;
}

+ (BOOL)requiresConstraintBasedLayout
{
	return YES;
}

- (void)updateConstraints
{
	[self removeConstraints:[self constraints]];
	[super updateConstraints];

	NSMutableArray* stackedViews = [NSMutableArray array];
	[stackedViews addObjectsFromArray:topAuxiliaryViews];
	[stackedViews addObject:gutterScrollView];
	[stackedViews addObjectsFromArray:bottomAuxiliaryViews];

	if(statusBar)
	{
		[stackedViews addObjectsFromArray:@[ statusDividerView, statusBar ]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[statusBar(==statusDividerView)]|" options:NSLayoutFormatAlignAllLeft|NSLayoutFormatAlignAllRight metrics:nil views:NSDictionaryOfVariableBindings(statusDividerView, statusBar)]];
	}

	[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[gutterScrollView(==gutterView)][gutterDividerView][textScrollView(>=100)]|" options:NSLayoutFormatAlignAllTop|NSLayoutFormatAlignAllBottom metrics:nil views:NSDictionaryOfVariableBindings(gutterScrollView, gutterView, gutterDividerView, textScrollView)]];
	[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[topView]" options:0 metrics:nil views:@{ @"topView" : stackedViews[0] }]];
	[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:[bottomView]|" options:0 metrics:nil views:@{ @"bottomView" : [stackedViews lastObject] }]];

	for(size_t i = 0; i < [stackedViews count]-1; ++i)
		[self addConstraint:[NSLayoutConstraint constraintWithItem:stackedViews[i] attribute:NSLayoutAttributeBottom relatedBy:NSLayoutRelationEqual toItem:stackedViews[i+1] attribute:NSLayoutAttributeTop multiplier:1 constant:0]];

	NSArray* array[] = { topAuxiliaryViews, bottomAuxiliaryViews };
	for(NSArray* views : array)
	{
		for(NSView* view in views)
			[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[view]|" options:0 metrics:nil views:NSDictionaryOfVariableBindings(view)]];
	}
}

- (void)setHideStatusBar:(BOOL)flag
{
	if(_hideStatusBar == flag)
		return;

	_hideStatusBar = flag;
	if(_hideStatusBar)
	{
		[statusDividerView removeFromSuperview];
		statusDividerView = nil;

		[statusBar removeFromSuperview];
		statusBar.delegate = nil;
		statusBar.target = nil;
		statusBar = nil;
	}
	else
	{
		statusDividerView = OakCreateHorizontalLine([NSColor colorWithCalibratedWhite:0.500 alpha:1], [NSColor colorWithCalibratedWhite:0.750 alpha:1]);

		statusBar = [[OTVStatusBar alloc] initWithFrame:NSZeroRect];
		statusBar.delegate = self;
		statusBar.target = self;

		OakAddAutoLayoutViewsToSuperview(@[ statusDividerView, statusBar ], self);
	}
	[self setNeedsUpdateConstraints:YES];
}

- (CGFloat)lineHeight
{
	return round(std::min(1.5 * [textView.font capHeight], [textView.font ascender] - [textView.font descender] + [textView.font leading]));
}

- (NSImage*)gutterImage:(NSString*)aName
{
	id res = gutterImages[aName];
	if(!res)
	{
		gutterImages = gutterImages ?: [NSMutableDictionary new];

		if(NSImage* image = [aName hasPrefix:@"/"] ? [[NSImage alloc] initWithContentsOfFile:aName] : [NSImage imageNamed:aName inSameBundleAsClass:[self class]])
		{
			CGFloat imageWidth  = image.size.width;
			CGFloat imageHeight = image.size.height;

			CGFloat viewWidth   = [self widthForColumnWithIdentifier:nil];
			CGFloat viewHeight  = self.lineHeight;

			res = image = [image copy];

			if(imageWidth / imageHeight < viewWidth / viewHeight)
					image.size = NSMakeSize(round(viewHeight * imageWidth / imageHeight), viewHeight);
			else	image.size = NSMakeSize(viewWidth, round(viewWidth * imageHeight / imageWidth));
		}
		else
		{
			res = [NSNull null];
			NSLog(@"%s no image named ‘%@’", sel_getName(_cmd), aName);
		}

		gutterImages[aName] = res;
	}
	return res == [NSNull null] ? nil : res;
}

- (void)updateGutterViewFont:(id)sender
{
	CGFloat const scaleFactor = [[NSUserDefaults standardUserDefaults] floatForKey:kUserDefaultsLineNumberScaleFactorKey] ?: 0.8;

	gutterImages = nil; // force image sizes to be recalculated
	gutterView.lineNumberFont = [NSFont fontWithName:[textView.font fontName] size:round(scaleFactor * [textView.font pointSize] * textView.fontScaleFactor / 100)];
	[gutterView reloadData:self];
}

- (IBAction)makeTextLarger:(id)sender
{
	textView.fontScaleFactor += 10;
	[self updateGutterViewFont:self];
}

- (IBAction)makeTextSmaller:(id)sender
{
	if(textView.fontScaleFactor > 10)
	{
		textView.fontScaleFactor -= 10;
		[self updateGutterViewFont:self];
	}
}

- (IBAction)makeTextStandardSize:(id)sender
{
	textView.fontScaleFactor = 100;
	[self updateGutterViewFont:self];
}

- (void)changeFont:(id)sender
{
	if(NSFont* newFont = [sender convertFont:textView.font ?: [NSFont userFixedPitchFontOfSize:0]])
	{
		settings_t::set(kSettingsFontNameKey, to_s([newFont fontName]));
		settings_t::set(kSettingsFontSizeKey, [newFont pointSize]);
		textView.font = newFont;
		[self updateGutterViewFont:self];
	}
}

- (void)observeValueForKeyPath:(NSString*)aKeyPath ofObject:(id)observableController change:(NSDictionary*)changeDictionary context:(void*)userData
{
	if(observableController != textView || ![self.observedKeys containsObject:aKeyPath])
		return;

	if([aKeyPath isEqualToString:@"selectionString"])
	{
		NSString* str = [textView valueForKey:@"selectionString"];
		[gutterView setHighlightedRange:to_s(str ?: @"1")];
		[statusBar setSelectionString:str];
		_symbolChooser.selectionString = str;

		ng::buffer_t const& buf = document->buffer();
		text::selection_t sel(to_s(str));
		size_t i = buf.convert(sel.last().max());
		statusBar.symbolName = [NSString stringWithCxxString:buf.symbol_at(i)];
	}
	else if([aKeyPath isEqualToString:@"tabSize"])
	{
		statusBar.tabSize = textView.tabSize;
	}
	else if([aKeyPath isEqualToString:@"softTabs"])
	{
		statusBar.softTabs = textView.softTabs;
	}
	else
	{
		[statusBar setValue:[textView valueForKey:aKeyPath] forKey:aKeyPath];
	}
}

- (void)dealloc
{
	gutterView.partnerView = nil;
	gutterView.delegate    = nil;
	statusBar.delegate     = nil;

	for(NSString* keyPath in self.observedKeys)
		[textView removeObserver:self forKeyPath:keyPath];
	[[NSNotificationCenter defaultCenter] removeObserver:self];

	[self setDocument:document::document_ptr()];
	delete callback;

	self.symbolChooser = nil;
}

- (document::document_ptr const&)document
{
	return document;
}

- (void)setDocument:(document::document_ptr const&)aDocument
{
	document::document_ptr oldDocument = document;
	if(oldDocument)
		oldDocument->remove_callback(callback);

	if(aDocument)
		aDocument->sync_open();

	if(document = aDocument)
	{
		document->add_callback(callback);
		document->show();

		for(auto const& item : bundles::query(bundles::kFieldGrammarScope, document->file_type()))
			statusBar.grammarName = [NSString stringWithCxxString:item->name()];
		statusBar.tabSize  = document->buffer().indent().tab_size();
		statusBar.softTabs = document->buffer().indent().soft_tabs();
	}

	[textView setDocument:document];
	[gutterView reloadData:self];
	[self updateStyle];

	if(_symbolChooser)
	{
		_symbolChooser.document        = document;
		_symbolChooser.selectionString = textView.selectionString;
	}

	if(oldDocument)
	{
		oldDocument->hide();
		oldDocument->close();
	}
}

- (void)updateStyle
{
	if(document && [textView theme])
	{
		auto theme = [textView theme];

		[[self window] setOpaque:!theme->is_transparent() && !theme->gutter_styles().is_transparent()];
		[textScrollView setBackgroundColor:[NSColor tmColorWithCGColor:theme->background(document->file_type())]];

		if(theme->is_dark())
		{
			NSImage* whiteIBeamImage = [NSImage imageNamed:@"IBeam white" inSameBundleAsClass:[self class]];
			[whiteIBeamImage setSize:[[[NSCursor IBeamCursor] image] size]];
			[textView setIbeamCursor:[[NSCursor alloc] initWithImage:whiteIBeamImage hotSpot:NSMakePoint(4, 9)]];
			[textScrollView setScrollerKnobStyle:NSScrollerKnobStyleLight];
		}
		else
		{
			[textView setIbeamCursor:[NSCursor IBeamCursor]];
			[textScrollView setScrollerKnobStyle:NSScrollerKnobStyleDark];
		}

		[self updateGutterViewFont:self]; // trigger update of gutter view’s line number font
		auto const& styles = theme->gutter_styles();

		gutterView.foregroundColor           = [NSColor tmColorWithCGColor:styles.foreground];
		gutterView.backgroundColor           = [NSColor tmColorWithCGColor:styles.background];
		gutterView.iconColor                 = [NSColor tmColorWithCGColor:styles.icons];
		gutterView.iconHoverColor            = [NSColor tmColorWithCGColor:styles.iconsHover];
		gutterView.iconPressedColor          = [NSColor tmColorWithCGColor:styles.iconsPressed];
		gutterView.selectionForegroundColor  = [NSColor tmColorWithCGColor:styles.selectionForeground];
		gutterView.selectionBackgroundColor  = [NSColor tmColorWithCGColor:styles.selectionBackground];
		gutterView.selectionIconColor        = [NSColor tmColorWithCGColor:styles.selectionIcons];
		gutterView.selectionIconHoverColor   = [NSColor tmColorWithCGColor:styles.selectionIconsHover];
		gutterView.selectionIconPressedColor = [NSColor tmColorWithCGColor:styles.selectionIconsPressed];
		gutterView.selectionBorderColor      = [NSColor tmColorWithCGColor:styles.selectionBorder];
		gutterScrollView.backgroundColor     = gutterView.backgroundColor;
		gutterDividerView.activeBackgroundColor = [NSColor tmColorWithCGColor:styles.divider];

		[gutterView setNeedsDisplay:YES];
	}
}

- (IBAction)toggleLineNumbers:(id)sender
{
	D(DBF_OakDocumentView, bug("show line numbers %s\n", BSTR([gutterView visibilityForColumnWithIdentifier:GVLineNumbersColumnIdentifier])););
	BOOL isVisibleFlag = ![gutterView visibilityForColumnWithIdentifier:GVLineNumbersColumnIdentifier];
	[gutterView setVisibility:isVisibleFlag forColumnWithIdentifier:GVLineNumbersColumnIdentifier];
	if(isVisibleFlag)
			[[NSUserDefaults standardUserDefaults] removeObjectForKey:@"DocumentView Disable Line Numbers"];
	else	[[NSUserDefaults standardUserDefaults] setObject:@YES forKey:@"DocumentView Disable Line Numbers"];
}

- (BOOL)validateMenuItem:(NSMenuItem*)aMenuItem
{
	if([aMenuItem action] == @selector(toggleLineNumbers:))
		[aMenuItem setTitle:[gutterView visibilityForColumnWithIdentifier:GVLineNumbersColumnIdentifier] ? @"Hide Line Numbers" : @"Show Line Numbers"];
	else if([aMenuItem action] == @selector(takeThemeUUIDFrom:))
		[aMenuItem setState:[textView theme]->uuid() == [[aMenuItem representedObject] UTF8String] ? NSOnState : NSOffState];
	else if([aMenuItem action] == @selector(takeTabSizeFrom:))
		[aMenuItem setState:textView.tabSize == [aMenuItem tag] ? NSOnState : NSOffState];
	else if([aMenuItem action] == @selector(showTabSizeSelectorPanel:))
	{
		static NSInteger const predefined[] = { 2, 3, 4, 8 };
		if(oak::contains(std::begin(predefined), std::end(predefined), textView.tabSize))
		{
			[aMenuItem setTitle:@"Other…"];
			[aMenuItem setState:NSOffState];
		}
		else
		{
			[aMenuItem setTitle:[NSString stringWithFormat:@"Other (%zd)…", textView.tabSize]];
			[aMenuItem setState:NSOnState];
		}
	}
	else if([aMenuItem action] == @selector(setIndentWithTabs:))
		[aMenuItem setState:textView.softTabs ? NSOffState : NSOnState];
	else if([aMenuItem action] == @selector(setIndentWithSpaces:))
		[aMenuItem setState:textView.softTabs ? NSOnState : NSOffState];
	else if([aMenuItem action] == @selector(takeGrammarUUIDFrom:))
	{
		NSString* uuidString = [aMenuItem representedObject];
		if(bundles::item_ptr bundleItem = bundles::lookup(to_s(uuidString)))
		{
			bool selectedGrammar = document && document->file_type() == bundleItem->value_for_field(bundles::kFieldGrammarScope);
			[aMenuItem setState:selectedGrammar ? NSOnState : NSOffState];
		}
	}
	else if([aMenuItem action] == @selector(toggleCurrentBookmark:))
	{
		text::selection_t sel(to_s(textView.selectionString));
		size_t lineNumber = sel.last().max().line;

		ng::buffer_t const& buf = document->buffer();
		[aMenuItem setTitle:buf.get_marks(buf.begin(lineNumber), buf.eol(lineNumber), document::kBookmarkIdentifier).empty() ? @"Set Bookmark" : @"Remove Bookmark"];
	}
	else if([aMenuItem action] == @selector(goToNextBookmark:) || [aMenuItem action] == @selector(goToPreviousBookmark:))
	{
		auto const& buf = document->buffer();
		return buf.get_marks(0, buf.size(), document::kBookmarkIdentifier).empty() ? NO : YES;
	}
	else if([aMenuItem action] == @selector(jumpToNextMark:) || [aMenuItem action] == @selector(jumpToPreviousMark:))
	{
		auto const& buf = document->buffer();
		return buf.get_marks(0, buf.size()).empty() ? NO : YES;
	}
	return YES;
}

// ===================
// = Auxiliary Views =
// ===================

- (void)addAuxiliaryView:(NSView*)aView atEdge:(NSRectEdge)anEdge
{
	topAuxiliaryViews    = topAuxiliaryViews    ?: [NSMutableArray new];
	bottomAuxiliaryViews = bottomAuxiliaryViews ?: [NSMutableArray new];
	if(anEdge == NSMinYEdge)
			[bottomAuxiliaryViews addObject:aView];
	else	[topAuxiliaryViews addObject:aView];
	OakAddAutoLayoutViewsToSuperview(@[ aView ], self);
	[self setNeedsUpdateConstraints:YES];
}

- (void)removeAuxiliaryView:(NSView*)aView
{
	if([topAuxiliaryViews containsObject:aView])
		[topAuxiliaryViews removeObject:aView];
	else if([bottomAuxiliaryViews containsObject:aView])
		[bottomAuxiliaryViews removeObject:aView];
	else
		return;
	[aView removeFromSuperview];
	[self setNeedsUpdateConstraints:YES];
}

// ======================
// = Pasteboard History =
// ======================

- (void)showClipboardHistory:(id)sender
{
	OakPasteboardChooser* chooser = [OakPasteboardChooser sharedChooserForName:NSGeneralPboard];
	chooser.action = @selector(paste:);
	[chooser showWindowRelativeToFrame:[self.window convertRectToScreen:[textView convertRect:[textView visibleRect] toView:nil]]];
}

- (void)showFindHistory:(id)sender
{
	OakPasteboardChooser* chooser = [OakPasteboardChooser sharedChooserForName:NSFindPboard];
	chooser.action = @selector(findNext:);
	[chooser showWindowRelativeToFrame:[self.window convertRectToScreen:[textView convertRect:[textView visibleRect] toView:nil]]];
}

// ==================
// = Symbol Chooser =
// ==================

- (void)selectAndCenter:(NSString*)aSelectionString
{
	textView.selectionString = aSelectionString;
	[textView centerSelectionInVisibleArea:self];
}

- (void)setSymbolChooser:(SymbolChooser*)aSymbolChooser
{
	if(_symbolChooser == aSymbolChooser)
		return;

	if(_symbolChooser)
	{
		[[NSNotificationCenter defaultCenter] removeObserver:self name:NSWindowWillCloseNotification object:_symbolChooser.window];

		_symbolChooser.target   = nil;
		_symbolChooser.document = document::document_ptr();
	}

	if(_symbolChooser = aSymbolChooser)
	{
		_symbolChooser.target          = self;
		_symbolChooser.action          = @selector(symbolChooserDidSelectItems:);
		_symbolChooser.filterString    = @"";
		_symbolChooser.document        = document;
		_symbolChooser.selectionString = textView.selectionString;

		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(symbolChooserWillClose:) name:NSWindowWillCloseNotification object:_symbolChooser.window];
	}
}

- (void)symbolChooserWillClose:(NSNotification*)aNotification
{
	self.symbolChooser = nil;
}

- (IBAction)showSymbolChooser:(id)sender
{
	self.symbolChooser = [SymbolChooser sharedInstance];
	[self.symbolChooser showWindowRelativeToFrame:[self.window convertRectToScreen:[textView convertRect:[textView visibleRect] toView:nil]]];
}

- (void)symbolChooserDidSelectItems:(id)sender
{
	for(id item in [sender selectedItems])
		[self selectAndCenter:[item selectionString]];
}

// =======================
// = Status bar delegate =
// =======================

- (void)takeGrammarUUIDFrom:(id)sender
{
	if(bundles::item_ptr item = bundles::lookup(to_s([sender representedObject])))
		[textView performBundleItem:item];
}

- (void)goToSymbol:(id)sender
{
	[self selectAndCenter:[sender representedObject]];
}

- (void)showSymbolSelector:(NSPopUpButton*)symbolPopUp
{
	NSMenu* symbolMenu = symbolPopUp.menu;
	[symbolMenu removeAllItems];

	ng::buffer_t const& buf = document->buffer();
	text::selection_t sel(to_s(textView.selectionString));
	size_t i = buf.convert(sel.last().max());

	NSInteger index = 0;
	for(auto pair : buf.symbols())
	{
		if(pair.second == "-")
		{
			[symbolMenu addItem:[NSMenuItem separatorItem]];
		}
		else
		{
			std::string const emSpace = " ";

			std::string::size_type offset = 0;
			while(pair.second.find(emSpace, offset) == offset)
				offset += emSpace.size();

			NSMenuItem* item = [symbolMenu addItemWithTitle:[NSString stringWithCxxString:pair.second.substr(offset)] action:@selector(goToSymbol:) keyEquivalent:@""];
			[item setIndentationLevel:offset / emSpace.size()];
			[item setTarget:self];
			[item setRepresentedObject:[NSString stringWithCxxString:buf.convert(pair.first)]];
		}

		if(pair.first <= i)
			++index;
	}

	if(symbolMenu.numberOfItems == 0)
		[symbolMenu addItemWithTitle:@"No symbols to show for current document." action:@selector(nop:) keyEquivalent:@""];

	[symbolPopUp selectItemAtIndex:(index ? index-1 : 0)];
}

- (void)showBundlesMenu:(id)sender
{
	if(!self.statusBar)
		return NSBeep();

	[NSApp sendAction:_cmd to:self.statusBar from:self];
}

- (void)showBundleItemSelector:(NSPopUpButton*)bundleItemsPopUp
{
	NSMenu* bundleItemsMenu = bundleItemsPopUp.menu;
	[bundleItemsMenu removeAllItems];

	std::multimap<std::string, bundles::item_ptr, text::less_t> ordered;
	for(auto item : bundles::query(bundles::kFieldAny, NULL_STR, scope::wildcard, bundles::kItemTypeBundle))
		ordered.emplace(item->name(), item);

	NSMenuItem* selectedItem = nil;
	for(auto pair : ordered)
	{
		bool selectedGrammar = false;
		for(auto item : bundles::query(bundles::kFieldGrammarScope, document->file_type(), scope::wildcard, bundles::kItemTypeGrammar, pair.second->uuid(), true, true))
			selectedGrammar = true;
		if(!selectedGrammar && pair.second->hidden_from_user() || pair.second->menu().empty())
			continue;

		NSMenuItem* menuItem = [bundleItemsMenu addItemWithTitle:[NSString stringWithCxxString:pair.first] action:NULL keyEquivalent:@""];
		menuItem.submenu = [[NSMenu alloc] initWithTitle:[NSString stringWithCxxString:pair.second->uuid()]];
		menuItem.submenu.delegate = [BundleMenuDelegate sharedInstance];

		if(selectedGrammar)
		{
			[menuItem setState:NSOnState];
			selectedItem = menuItem;
		}
	}

	if(ordered.empty())
		[bundleItemsMenu addItemWithTitle:@"No Bundles Loaded" action:@selector(nop:) keyEquivalent:@""];

	if(selectedItem)
		[bundleItemsPopUp selectItem:selectedItem];
}

- (NSUInteger)tabSize
{
	return textView.tabSize;
}

- (void)setTabSize:(NSUInteger)newTabSize
{
	textView.tabSize = newTabSize;
	settings_t::set(kSettingsTabSizeKey, (size_t)newTabSize, document->file_type());
}

- (IBAction)takeTabSizeFrom:(id)sender
{
	D(DBF_OakDocumentView, bug("\n"););
	ASSERT([sender respondsToSelector:@selector(tag)]);
	if([sender tag] > 0)
		self.tabSize = [sender tag];
}

- (IBAction)setIndentWithSpaces:(id)sender
{
	D(DBF_OakDocumentView, bug("\n"););
	textView.softTabs = YES;
	settings_t::set(kSettingsSoftTabsKey, true, document->file_type());
}

- (IBAction)setIndentWithTabs:(id)sender
{
	D(DBF_OakDocumentView, bug("\n"););
	textView.softTabs = NO;
	settings_t::set(kSettingsSoftTabsKey, false, document->file_type());
}

- (IBAction)showTabSizeSelectorPanel:(id)sender
{
	if(!tabSizeSelectorPanel)
		[NSBundle loadNibNamed:@"TabSizeSetting" owner:self];
	[tabSizeSelectorPanel makeKeyAndOrderFront:self];
}

- (void)toggleMacroRecording:(id)sender    { [textView toggleMacroRecording:sender]; }

- (IBAction)takeThemeUUIDFrom:(id)sender
{
	[self setThemeWithUUID:[sender representedObject]];
}

- (void)setThemeWithUUID:(NSString*)themeUUID
{
	if(bundles::item_ptr const& themeItem = bundles::lookup(to_s(themeUUID)))
	{
		[textView setTheme:parse_theme(themeItem)];
		settings_t::set(kSettingsThemeKey, to_s(themeUUID));
		[self updateStyle];
	}
}

// =============================
// = GutterView Delegate Proxy =
// =============================

- (GVLineRecord)lineRecordForPosition:(CGFloat)yPos                              { return [textView lineRecordForPosition:yPos];               }
- (GVLineRecord)lineFragmentForLine:(NSUInteger)aLine column:(NSUInteger)aColumn { return [textView lineFragmentForLine:aLine column:aColumn]; }

// =========================
// = GutterView DataSource =
// =========================

- (CGFloat)widthForColumnWithIdentifier:(id)columnIdentifier
{
	return floor((self.lineHeight-1) / 2) * 2 + 1;
}

- (NSImage*)imageForLine:(NSUInteger)lineNumber inColumnWithIdentifier:(id)columnIdentifier state:(GutterViewRowState)rowState
{
	if([columnIdentifier isEqualToString:kBookmarksColumnIdentifier])
	{
		std::map<size_t, std::string> gutterImageName;

		ng::buffer_t const& buf = document->buffer();
		for(auto const& pair : buf.get_marks(buf.begin(lineNumber), buf.eol(lineNumber)))
		{
			if(!pair.second.second.empty())
				gutterImageName.emplace(0, pair.second.first);
			else if(pair.second.first == document::kBookmarkIdentifier)
				gutterImageName.emplace(1, rowState != GutterViewRowStateRegular ? "Bookmark Hover Remove" : "Bookmark");
			else if(rowState == GutterViewRowStateRegular)
				gutterImageName.emplace(2, pair.second.first);
		}

		if(rowState != GutterViewRowStateRegular)
			gutterImageName.emplace(3, "Bookmark Hover Add");

		if(!gutterImageName.empty())
			return [self gutterImage:[NSString stringWithCxxString:gutterImageName.begin()->second]];
	}
	else if([columnIdentifier isEqualToString:kFoldingsColumnIdentifier])
	{
		switch([textView foldingStateForLine:lineNumber])
		{
			case kFoldingTop:       return [self gutterImage:rowState == GutterViewRowStateRegular ? @"Folding Top"       : @"Folding Top Hover"];
			case kFoldingCollapsed: return [self gutterImage:rowState == GutterViewRowStateRegular ? @"Folding Collapsed" : @"Folding Collapsed Hover"];
			case kFoldingBottom:    return [self gutterImage:rowState == GutterViewRowStateRegular ? @"Folding Bottom"    : @"Folding Bottom Hover"];
		}
	}
	return nil;
}

// =============================
// = Bookmark Submenu Delegate =
// =============================

- (void)takeBookmarkFrom:(id)sender
{
	if([sender respondsToSelector:@selector(representedObject)])
		[self selectAndCenter:[sender representedObject]];
}

- (void)updateBookmarksMenu:(NSMenu*)aMenu
{
	ng::buffer_t& buf = document->buffer();
	std::map<size_t, std::string> const& marks = buf.get_marks(0, buf.size(), document::kBookmarkIdentifier);
	for(auto const& pair : marks)
	{
		size_t n = buf.convert(pair.first).line;
		NSMenuItem* item = [aMenu addItemWithTitle:[NSString stringWithCxxString:text::pad(n+1, 4) + ": " + buf.substr(buf.begin(n), buf.eol(n))] action:@selector(takeBookmarkFrom:) keyEquivalent:@""];
		[item setRepresentedObject:[NSString stringWithCxxString:buf.convert(pair.first)]];
	}

	if(!marks.empty())
		[aMenu addItem:[NSMenuItem separatorItem]];
	[aMenu addItemWithTitle:@"Clear Bookmarks" action:marks.empty() ? NULL : @selector(clearAllBookmarks:) keyEquivalent:@""];
}

// =======================
// = GutterView Delegate =
// =======================

- (void)userDidClickColumnWithIdentifier:(id)columnIdentifier atLine:(NSUInteger)lineNumber
{
	if([columnIdentifier isEqualToString:kBookmarksColumnIdentifier])
	{
		ng::buffer_t& buf = document->buffer();

		std::vector<std::string> info;
		for(auto const& pair : buf.get_marks(buf.begin(lineNumber), buf.eol(lineNumber)))
		{
			if(!pair.second.second.empty())
				info.push_back(pair.second.second);
		}

		if(info.empty())
		{
			for(auto const& pair : buf.get_marks(buf.begin(lineNumber), buf.eol(lineNumber), document::kBookmarkIdentifier))
				return buf.remove_mark(pair.first, document::kBookmarkIdentifier);
			buf.set_mark(buf.begin(lineNumber), document::kBookmarkIdentifier);
		}
		else
		{
			NSView* popoverContainerView = [[NSView alloc] initWithFrame:NSZeroRect];

			NSTextField* textField = OakCreateLabel([NSString stringWithCxxString:text::join(info, "\n")]);
			OakAddAutoLayoutViewsToSuperview(@[ textField ], popoverContainerView);

			NSDictionary* views = NSDictionaryOfVariableBindings(textField);
			[popoverContainerView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(5)-[textField]-(5)-|" options:0 metrics:0 views:views]];
			[popoverContainerView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-(10)-[textField]-(10)-|" options:0 metrics:0 views:views]];

			NSViewController* viewController = [NSViewController new];
			viewController.view = popoverContainerView;

			NSPopover* popover = [NSPopover new];
			popover.behavior = NSPopoverBehaviorTransient;
			popover.contentViewController = viewController;

			GVLineRecord record = [self lineFragmentForLine:lineNumber column:0];
			NSRect rect = NSMakeRect(0, record.firstY, [self widthForColumnWithIdentifier:columnIdentifier], record.lastY - record.firstY);
			[popover showRelativeToRect:rect ofView:gutterView preferredEdge:NSMaxXEdge];
		}
	}
	else if([columnIdentifier isEqualToString:kFoldingsColumnIdentifier])
	{
		[textView toggleFoldingAtLine:lineNumber recursive:OakIsAlternateKeyOrMouseEvent()];
		[[NSNotificationCenter defaultCenter] postNotificationName:GVColumnDataSourceDidChange object:self];
	}
}

// ====================
// = Bookmark Actions =
// ====================

- (void)goToNextMarkOfType:(NSString*)markType
{
	text::selection_t sel(to_s(textView.selectionString));

	ng::buffer_t const& buf = document->buffer();
	std::pair<size_t, std::string> const& pair = buf.next_mark(buf.convert(sel.last().max()), to_s(markType));
	if(pair.second != NULL_STR)
		textView.selectionString = [NSString stringWithCxxString:buf.convert(pair.first)];
}

- (IBAction)goToPreviousMarkOfType:(NSString*)markType
{
	text::selection_t sel(to_s(textView.selectionString));

	ng::buffer_t const& buf = document->buffer();
	std::pair<size_t, std::string> const& pair = buf.prev_mark(buf.convert(sel.last().max()), to_s(markType));
	if(pair.second != NULL_STR)
		textView.selectionString = [NSString stringWithCxxString:buf.convert(pair.first)];
}

- (IBAction)toggleCurrentBookmark:(id)sender
{
	ng::buffer_t& buf = document->buffer();

	text::selection_t sel(to_s(textView.selectionString));
	size_t lineNumber = sel.last().max().line;

	std::vector<size_t> toRemove;
	for(auto const& pair : buf.get_marks(buf.begin(lineNumber), buf.eol(lineNumber), document::kBookmarkIdentifier))
		toRemove.push_back(pair.first);

	if(toRemove.empty())
	{
		buf.set_mark(buf.convert(sel.last().max()), document::kBookmarkIdentifier);
	}
	else
	{
		for(auto const& index : toRemove)
			buf.remove_mark(index, document::kBookmarkIdentifier);
	}
	[[NSNotificationCenter defaultCenter] postNotificationName:GVColumnDataSourceDidChange object:self];
}

- (IBAction)goToNextBookmark:(id)sender
{
	[self goToNextMarkOfType:[NSString stringWithCxxString:document::kBookmarkIdentifier]];
}

- (IBAction)goToPreviousBookmark:(id)sender
{
	[self goToPreviousMarkOfType:[NSString stringWithCxxString:document::kBookmarkIdentifier]];
}

- (void)clearAllBookmarks:(id)sender
{
	document->buffer().remove_all_marks(document::kBookmarkIdentifier);
	[[NSNotificationCenter defaultCenter] postNotificationName:GVColumnDataSourceDidChange object:self];
}

// ========================
// = Jump To Mark Actions =
// ========================

- (IBAction)jumpToNextMark:(id)sender
{
	[self goToNextMarkOfType:nil];
}

- (IBAction)jumpToPreviousMark:(id)sender
{
	[self goToPreviousMarkOfType:nil];
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
		NSAccessibilityDescriptionAttribute,
	]];
	return set;
}

- (NSArray*)accessibilityAttributeNames
{
	static NSArray* attributes = [[[self myAccessibilityAttributeNames] setByAddingObjectsFromArray:[super accessibilityAttributeNames]] allObjects];
	return attributes;
}

- (BOOL)accessibilityIsAttributeSettable:(NSString*)attribute
{
	if([[self myAccessibilityAttributeNames] containsObject:attribute])
		return NO;
	return [super accessibilityIsAttributeSettable:attribute];
}

- (id)accessibilityAttributeValue:(NSString*)attribute
{
	if([attribute isEqualToString:NSAccessibilityRoleAttribute])
		return NSAccessibilityGroupRole;
	else if([attribute isEqualToString:NSAccessibilityDescriptionAttribute])
		return @"Editor";
	else
		return [super accessibilityAttributeValue:attribute];
}
@end

// ============
// = Printing =
// ============

@interface OakPrintDocumentView : NSView
{
	document::document_ptr document;
	NSString* fontName;

	std::shared_ptr<ng::layout_t> layout;
	std::vector<CGRect> pageRects;

	BOOL _needsLayout;
}
@property (nonatomic) CGFloat pageWidth;
@property (nonatomic) CGFloat pageHeight;
@property (nonatomic) CGFloat fontScale;
@property (nonatomic) NSString* themeUUID;
@property (nonatomic) CGFloat fontSize;
@end

@implementation OakPrintDocumentView
- (id)initWithDocument:(document::document_ptr const&)aDocument fontName:(NSString*)aFontName
{
	if(self = [self initWithFrame:NSZeroRect])
	{
		document = aDocument;
		fontName = aFontName;
	}
	return self;
}

- (BOOL)isFlipped
{
	return YES;
}

- (NSString*)printJobTitle
{
	return [NSString stringWithCxxString:document->display_name()];
}

- (BOOL)knowsPageRange:(NSRangePointer)range
{
	NSPrintInfo* info = [[NSPrintOperation currentOperation] printInfo];

	NSRect display = NSIntersectionRect(info.imageablePageBounds, (NSRect){ NSZeroPoint, info.paperSize });
	info.leftMargin   = NSMinX(display);
	info.rightMargin  = info.paperSize.width - NSMaxX(display);
	info.topMargin    = info.paperSize.height - NSMaxY(display);
	info.bottomMargin = NSMinY(display);

	self.pageWidth  = floor(info.paperSize.width - info.leftMargin - info.rightMargin);
	self.pageHeight = floor(info.paperSize.height - info.topMargin - info.bottomMargin);
	self.fontScale  = [[[info dictionary] objectForKey:NSPrintScalingFactor] floatValue];
	self.themeUUID  = [[info dictionary] objectForKey:@"OakPrintThemeUUID"];
	self.fontSize = [info.dictionary[@"OakPrintFontSize"] floatValue];

	[self updateLayout];
	[self setFrame:NSMakeRect(0, 0, self.pageWidth, layout->height())];

	range->location = 1;
	range->length   = pageRects.size();

	return YES;
}

- (NSRect)rectForPage:(NSInteger)pageNumber
{
	NSParameterAssert(0 < pageNumber && pageNumber <= pageRects.size());
	return pageRects[pageNumber-1];
}

- (void)drawRect:(NSRect)aRect
{
	NSEraseRect(aRect);
	if(![NSGraphicsContext currentContextDrawingToScreen] && layout)
		layout->draw((CGContextRef)[[NSGraphicsContext currentContext] graphicsPort], aRect, [self isFlipped], /* selection: */ ng::ranges_t(), /* highlight: */ ng::ranges_t(), /* draw background: */ false);
}

- (void)updateLayout
{
	if(!_needsLayout)
		return;

	pageRects.clear();
	
	theme_ptr theme = parse_theme(bundles::lookup(to_s(self.themeUUID)));
	theme = theme->copy_with_font_name_and_size(to_s(fontName), _fontSize * self.fontScale);
	layout = std::make_shared<ng::layout_t>(document->buffer(), theme, /* softWrap: */ true);
	layout->set_viewport_size(CGSizeMake(self.pageWidth, self.pageHeight));
	layout->update_metrics(CGRectMake(0, 0, CGFLOAT_MAX, CGFLOAT_MAX));

	CGRect pageRect = CGRectMake(0, 0, self.pageWidth, self.pageHeight);
	while(true)
	{
		CGRect lineRect = layout->rect_at_index(layout->index_at_point(CGPointMake(NSMinX(pageRect), NSMaxY(pageRect))).index);
		if(NSMaxY(lineRect) <= NSMinY(pageRect))
			break;
		else if(CGRectContainsRect(pageRect, lineRect))
			pageRect.size.height = NSMaxY(lineRect) - NSMinY(pageRect);
		else
			pageRect.size.height = NSMinY(lineRect) - NSMinY(pageRect);
		pageRects.push_back(pageRect);

		pageRect.origin.y = NSMaxY(pageRect);
		pageRect.size.height = self.pageHeight;
	}

	_needsLayout = NO;
}

- (void)setPageWidth:(CGFloat)newPageWidth    { if(_pageWidth  != newPageWidth)  { _needsLayout = YES; _pageWidth  = newPageWidth;  } }
- (void)setPageHeight:(CGFloat)newPageHeight  { if(_pageHeight != newPageHeight) { _needsLayout = YES; _pageHeight = newPageHeight; } }
- (void)setFontScale:(CGFloat)newFontScale    { if(_fontScale  != newFontScale)  { _needsLayout = YES; _fontScale  = newFontScale;  } }
- (void)setThemeUUID:(NSString*)newThemeUUID  { if(![_themeUUID isEqualToString:newThemeUUID]) { _needsLayout = YES; _themeUUID  = newThemeUUID; } }
- (void)setFontSize:(CGFloat)newFontSize      { if(_fontSize  != newFontSize)  { _needsLayout = YES; _fontSize  = newFontSize;  } }
@end

@interface OakTextViewPrintOptionsViewController : NSViewController <NSPrintPanelAccessorizing>
{
	std::vector<oak::uuid_t> themeUUIDs;
}
@end

#ifndef CONSTRAINT
#define CONSTRAINT(str, align) [constraints addObjectsFromArray:[NSLayoutConstraint constraintsWithVisualFormat:str options:align metrics:nil views:views]]
#endif

@implementation OakTextViewPrintOptionsViewController
- (id)init
{
	if((self = [super init]))
	{
		NSView* contentView = [[NSView alloc] initWithFrame:NSZeroRect];

		NSTextField* themesLabel 		= OakCreateLabel(@"Theme:");
		NSPopUpButton* themes    		= OakCreatePopUpButton();
		NSTextField* fontSizesLabel 	= OakCreateLabel(@"Font size:");
		NSPopUpButton* fontSizes 		= OakCreatePopUpButton();
		NSButton* printHeaders   		= OakCreateCheckBox(@"Print header and footer");

		NSMenu* themesMenu = themes.menu;
		[themesMenu removeAllItems];

		std::multimap<std::string, bundles::item_ptr, text::less_t> ordered;
		for(auto item : bundles::query(bundles::kFieldAny, NULL_STR, scope::wildcard, bundles::kItemTypeTheme))
			ordered.emplace(item->name(), item);

		for(auto pair : ordered)
		{
			[themesMenu addItemWithTitle:[NSString stringWithCxxString:pair.first] action:NULL keyEquivalent:@""];
			themeUUIDs.push_back(pair.second->uuid());
		}

		if(ordered.empty())
			[themesMenu addItemWithTitle:@"No Themes Loaded" action:@selector(nop:) keyEquivalent:@""];
		
		NSMenu *fontSizesMenu = fontSizes.menu;
		[fontSizesMenu removeAllItems];
		for (int size=4; size<23; size++) {
			[fontSizesMenu addItemWithTitle:@(size).stringValue action:NULL keyEquivalent:@""];
		}
		
		[themes bind:NSSelectedIndexBinding toObject:self withKeyPath:@"themeIndex" options:nil];
		[fontSizes bind:NSSelectedValueBinding toObject:self withKeyPath:@"printFontSize" options:nil];
		[printHeaders bind:NSValueBinding toObject:self withKeyPath:@"printHeaderAndFooter" options:nil];

		NSDictionary* views = @{
			@"themesLabel"    : themesLabel,
			@"themes"         : themes,
			@"fontSizesLabel" : fontSizesLabel,
			@"fontSizes"      : fontSizes,
			@"printHeaders"	: printHeaders
		};

		OakAddAutoLayoutViewsToSuperview([views allValues], contentView);

		NSMutableArray* constraints = [NSMutableArray array];
		CONSTRAINT(@"H:|-[themesLabel]-[themes]-|",  				NSLayoutFormatAlignAllBaseline);
		CONSTRAINT(@"H:|-[fontSizesLabel]-[fontSizes]-|",  		NSLayoutFormatAlignAllBaseline);
		CONSTRAINT(@"H:[printHeaders]-|",            				0);
		CONSTRAINT(@"V:|-[themes]-[fontSizes]-[printHeaders]-|",	NSLayoutFormatAlignAllLeft);
		[contentView addConstraints:constraints];

		contentView.frame = (NSRect){ NSZeroPoint, [contentView fittingSize] };
		self.view = contentView;
	}
	return self;
}

- (void)setRepresentedObject:(NSPrintInfo*)printInfo
{
	[super setRepresentedObject:printInfo];
	[self setThemeIndex:[self themeIndex]];
	[self setPrintHeaderAndFooter:[self printHeaderAndFooter]];
	[self setPrintFontSize:[self printFontSize]];
}

- (void)setThemeIndex:(NSInteger)anIndex
{
	if(anIndex < themeUUIDs.size())
	{
		NSPrintInfo* info = [self representedObject];
		[[info dictionary] setObject:[NSString stringWithCxxString:themeUUIDs[anIndex]] forKey:@"OakPrintThemeUUID"];
		[[NSUserDefaults standardUserDefaults] setObject:[NSString stringWithCxxString:themeUUIDs[anIndex]] forKey:@"OakPrintThemeUUID"];
	}
}

- (NSInteger)themeIndex
{
	NSPrintInfo* info = [self representedObject];
	if(NSString* themeUUID = [[info dictionary] objectForKey:@"OakPrintThemeUUID"])
	{
		for(size_t i = 0; i < themeUUIDs.size(); ++i)
		{
			if(themeUUIDs[i] == to_s(themeUUID))
				return i;
		}
	}
	return 0;
}

- (void)setPrintHeaderAndFooter:(BOOL)flag
{
	NSPrintInfo* info = [self representedObject];
	[[info dictionary] setObject:@(flag) forKey:NSPrintHeaderAndFooter];
	[[NSUserDefaults standardUserDefaults] setObject:@(flag) forKey:@"OakPrintHeaderAndFooter"];
}

- (BOOL)printHeaderAndFooter
{
	return [[[[self representedObject] dictionary] objectForKey:NSPrintHeaderAndFooter] boolValue];
}

- (void)setPrintFontSize:(NSNumber*)size
{
	NSPrintInfo* info = [self representedObject];
	[[info dictionary] setObject:size forKey:@"OakPrintFontSize"];
	[[NSUserDefaults standardUserDefaults] setObject:size forKey:@"OakPrintFontSize"];	
}

- (NSNumber*)printFontSize
{
	return [[[self representedObject] dictionary] objectForKey:@"OakPrintFontSize"];
}

- (NSSet*)keyPathsForValuesAffectingPreview
{
	return [NSSet setWithObjects:@"themeIndex", @"printFontSize", @"printHeaderAndFooter", nil];
}

- (NSArray*)localizedSummaryItems
{
	return @[ ]; // TODO
}

- (NSString*)title
{
	return @"TextMate";
}
@end

@implementation OakDocumentView (Printing)
+ (void)initialize
{
	[[NSUserDefaults standardUserDefaults] registerDefaults:@{
		@"OakPrintThemeUUID"       : @"71D40D9D-AE48-11D9-920A-000D93589AF6",
		@"OakPrintHeaderAndFooter" : @NO,
		@"OakPrintFontSize"			: @(11),
	}];
}

- (void)printDocument:(id)sender
{
	NSPrintOperation* printer = [NSPrintOperation printOperationWithView:[[OakPrintDocumentView alloc] initWithDocument:document fontName:textView.font.fontName]];

	NSMutableDictionary* info = [[printer printInfo] dictionary];
	info[@"OakPrintThemeUUID"] = [[NSUserDefaults standardUserDefaults] objectForKey:@"OakPrintThemeUUID"];
	info[NSPrintHeaderAndFooter] = [[NSUserDefaults standardUserDefaults] objectForKey:@"OakPrintHeaderAndFooter"];
	info[@"OakPrintFontSize"] = [[NSUserDefaults standardUserDefaults] objectForKey:@"OakPrintFontSize"];

	[[printer printInfo] setVerticallyCentered:NO];
	[[printer printPanel] setOptions:[[printer printPanel] options] | NSPrintPanelShowsPaperSize | NSPrintPanelShowsOrientation | NSPrintPanelShowsScaling];
	[[printer printPanel] addAccessoryController:[OakTextViewPrintOptionsViewController new]];

	[printer runOperationModalForWindow:[self window] delegate:nil didRunSelector:NULL contextInfo:nil];
}
@end
