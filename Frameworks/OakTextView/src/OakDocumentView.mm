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
#import <settings/settings.h>
#import <OakFilterList/SymbolChooser.h>
#import <OakFoundation/NSString Additions.h>
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/NSImage Additions.h>
#import <OakAppKit/OakToolTip.h>
#import <OakAppKit/OakPasteboard.h>
#import <OakAppKit/OakPasteboardChooser.h>
#import <OakAppKit/OakUIConstructionFunctions.h>
#import <OakAppKit/NSMenuItem Additions.h>
#import <BundleMenu/BundleMenu.h>

OAK_DEBUG_VAR(OakDocumentView);

static NSString* const kUserDefaultsLineNumberScaleFactorKey = @"lineNumberScaleFactor";
static NSString* const kUserDefaultsLineNumberFontNameKey    = @"lineNumberFontName";

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
			self.statusBar.fileType = [NSString stringWithCxxString:document->file_type()];
		}
		else if(event == did_change_indent_settings)
		{
			self.statusBar.tabSize  = document->indent().tab_size();
			self.statusBar.softTabs = document->indent().soft_tabs();
		}
	}
private:
	__weak OakDocumentView* self;
};

@implementation OakDocumentView
- (id)initWithFrame:(NSRect)aRect
{
	D(DBF_OakDocumentView, bug("%s\n", [NSStringFromRect(aRect) UTF8String]););
	if(self = [super initWithFrame:aRect])
	{
		callback = new document_view_callback_t(self);

		_textView = [[OakTextView alloc] initWithFrame:NSZeroRect];
		_textView.autoresizingMask = NSViewWidthSizable|NSViewHeightSizable;

		textScrollView = [[NSScrollView alloc] initWithFrame:NSZeroRect];
		textScrollView.hasVerticalScroller   = YES;
		textScrollView.hasHorizontalScroller = YES;
		textScrollView.autohidesScrollers    = YES;
		textScrollView.borderType            = NSNoBorder;
		textScrollView.documentView          = _textView;

		gutterView = [[GutterView alloc] initWithFrame:NSZeroRect];
		gutterView.partnerView = _textView;
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

		_statusBar = [[OTVStatusBar alloc] initWithFrame:NSZeroRect];
		_statusBar.delegate = self;
		_statusBar.target = self;

		OakAddAutoLayoutViewsToSuperview(@[ gutterScrollView, gutterDividerView, textScrollView, statusDividerView, _statusBar ], self);
		OakSetupKeyViewLoop(@[ self, _textView, _statusBar ], NO);

		document::document_ptr doc = document::from_content("", "text.plain"); // file type is only to avoid potential “no grammar” warnings in console
		doc->set_custom_name("null document"); // without a name it grabs an ‘untitled’ token
		[self setDocument:doc];

		self.observedKeys = @[ @"selectionString", @"tabSize", @"softTabs", @"isMacroRecording"];
		for(NSString* keyPath in self.observedKeys)
			[_textView addObserver:self forKeyPath:keyPath options:NSKeyValueObservingOptionInitial context:NULL];
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

	if(_statusBar)
	{
		[stackedViews addObjectsFromArray:@[ statusDividerView, _statusBar ]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[_statusBar(==statusDividerView)]|" options:NSLayoutFormatAlignAllLeft|NSLayoutFormatAlignAllRight metrics:nil views:NSDictionaryOfVariableBindings(statusDividerView, _statusBar)]];
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

		[_statusBar removeFromSuperview];
		_statusBar.delegate = nil;
		_statusBar.target = nil;
		_statusBar = nil;
	}
	else
	{
		statusDividerView = OakCreateHorizontalLine([NSColor colorWithCalibratedWhite:0.500 alpha:1], [NSColor colorWithCalibratedWhite:0.750 alpha:1]);

		_statusBar = [[OTVStatusBar alloc] initWithFrame:NSZeroRect];
		_statusBar.delegate = self;
		_statusBar.target = self;

		OakAddAutoLayoutViewsToSuperview(@[ statusDividerView, _statusBar ], self);
	}
	[self setNeedsUpdateConstraints:YES];
}

- (CGFloat)lineHeight
{
	return round(std::min(1.5 * [_textView.font capHeight], [_textView.font ascender] - [_textView.font descender] + [_textView.font leading]));
}

- (NSImage*)gutterImage:(NSString*)aName
{
	id res = gutterImages[aName];
	if(!res)
	{
		gutterImages = gutterImages ?: [NSMutableDictionary new];

		NSImage* image = [aName hasPrefix:@"/"] ? [[NSImage alloc] initWithContentsOfFile:aName] : [NSImage imageNamed:aName inSameBundleAsClass:[self class]];
		if(!image && ![aName hasPrefix:@"/"] && ![aName hasSuffix:@" Template"])
			image = [NSImage imageNamed:[aName stringByAppendingString:@" Template"] inSameBundleAsClass:[self class]];

		if([aName hasPrefix:@"/"] && [[aName stringByDeletingPathExtension] hasSuffix:@" Template"])
			[image setTemplate:YES];

		if(image)
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
	NSString* lineNumberFontName = [[NSUserDefaults standardUserDefaults] stringForKey:kUserDefaultsLineNumberFontNameKey] ?: [_textView.font fontName];

	gutterImages = nil; // force image sizes to be recalculated
	gutterView.lineNumberFont = [NSFont fontWithName:lineNumberFontName size:round(scaleFactor * [_textView.font pointSize] * _textView.fontScaleFactor / 100)];
	[gutterView reloadData:self];
}

- (IBAction)makeTextLarger:(id)sender
{
	_textView.fontScaleFactor += 10;
	[self updateGutterViewFont:self];
}

- (IBAction)makeTextSmaller:(id)sender
{
	if(_textView.fontScaleFactor > 10)
	{
		_textView.fontScaleFactor -= 10;
		[self updateGutterViewFont:self];
	}
}

- (IBAction)makeTextStandardSize:(id)sender
{
	_textView.fontScaleFactor = 100;
	[self updateGutterViewFont:self];
}

- (void)changeFont:(id)sender
{
	NSFont* defaultFont = [NSFont userFixedPitchFontOfSize:0];
	if(NSFont* newFont = [sender convertFont:_textView.font ?: defaultFont])
	{
		std::string fontName = [newFont.fontName isEqualToString:defaultFont.fontName] ? NULL_STR : to_s(newFont.fontName);
		settings_t::set(kSettingsFontNameKey, fontName);
		settings_t::set(kSettingsFontSizeKey, [newFont pointSize]);
		_textView.font = newFont;
		[self updateGutterViewFont:self];
	}
}

- (void)observeValueForKeyPath:(NSString*)aKeyPath ofObject:(id)observableController change:(NSDictionary*)changeDictionary context:(void*)userData
{
	if(observableController != _textView || ![self.observedKeys containsObject:aKeyPath])
		return;

	if([aKeyPath isEqualToString:@"selectionString"])
	{
		NSString* str = [_textView valueForKey:@"selectionString"];
		[gutterView setHighlightedRange:to_s(str ?: @"1")];
		[_statusBar setSelectionString:str];
		_symbolChooser.selectionString = str;

		ng::buffer_t const& buf = document->buffer();
		text::selection_t sel(to_s(str));
		size_t i = buf.convert(sel.last().max());
		_statusBar.symbolName = [NSString stringWithCxxString:buf.symbol_at(i)];
	}
	else if([aKeyPath isEqualToString:@"tabSize"])
	{
		_statusBar.tabSize = _textView.tabSize;
	}
	else if([aKeyPath isEqualToString:@"softTabs"])
	{
		_statusBar.softTabs = _textView.softTabs;
	}
	else
	{
		[_statusBar setValue:[_textView valueForKey:aKeyPath] forKey:aKeyPath];
	}
}

- (void)dealloc
{
	gutterView.partnerView = nil;
	gutterView.delegate    = nil;
	_statusBar.delegate    = nil;

	for(NSString* keyPath in self.observedKeys)
		[_textView removeObserver:self forKeyPath:keyPath];
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

		_statusBar.fileType = [NSString stringWithCxxString:document->file_type()];
		_statusBar.tabSize  = document->indent().tab_size();
		_statusBar.softTabs = document->indent().soft_tabs();
	}

	[_textView setDocument:document];
	[gutterView reloadData:self];
	[self updateStyle];

	if(_symbolChooser)
	{
		_symbolChooser.document        = document->document();
		_symbolChooser.selectionString = _textView.selectionString;
	}

	if(oldDocument)
	{
		oldDocument->hide();
		oldDocument->close();
	}
}

- (void)updateStyle
{
	theme_ptr theme = _textView.theme;
	if(theme && document)
	{
		[[self window] setOpaque:!theme->is_transparent() && !theme->gutter_styles().is_transparent()];
		[textScrollView setBackgroundColor:[NSColor colorWithCGColor:theme->background(document->file_type())]];

		if(theme->is_dark())
		{
			NSImage* whiteIBeamImage = [NSImage imageNamed:@"IBeam white" inSameBundleAsClass:[self class]];
			[whiteIBeamImage setSize:[[[NSCursor IBeamCursor] image] size]];
			[_textView setIbeamCursor:[[NSCursor alloc] initWithImage:whiteIBeamImage hotSpot:NSMakePoint(4, 9)]];
			[textScrollView setScrollerKnobStyle:NSScrollerKnobStyleLight];
		}
		else
		{
			[_textView setIbeamCursor:[NSCursor IBeamCursor]];
			[textScrollView setScrollerKnobStyle:NSScrollerKnobStyleDark];
		}

		[self updateGutterViewFont:self]; // trigger update of gutter view’s line number font
		auto const& styles = theme->gutter_styles();

		gutterView.foregroundColor           = [NSColor colorWithCGColor:styles.foreground];
		gutterView.backgroundColor           = [NSColor colorWithCGColor:styles.background];
		gutterView.iconColor                 = [NSColor colorWithCGColor:styles.icons];
		gutterView.iconHoverColor            = [NSColor colorWithCGColor:styles.iconsHover];
		gutterView.iconPressedColor          = [NSColor colorWithCGColor:styles.iconsPressed];
		gutterView.selectionForegroundColor  = [NSColor colorWithCGColor:styles.selectionForeground];
		gutterView.selectionBackgroundColor  = [NSColor colorWithCGColor:styles.selectionBackground];
		gutterView.selectionIconColor        = [NSColor colorWithCGColor:styles.selectionIcons];
		gutterView.selectionIconHoverColor   = [NSColor colorWithCGColor:styles.selectionIconsHover];
		gutterView.selectionIconPressedColor = [NSColor colorWithCGColor:styles.selectionIconsPressed];
		gutterView.selectionBorderColor      = [NSColor colorWithCGColor:styles.selectionBorder];
		gutterScrollView.backgroundColor     = gutterView.backgroundColor;
		gutterDividerView.activeBackgroundColor = [NSColor colorWithCGColor:styles.divider];

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
		[aMenuItem setState:_textView.theme && _textView.theme->uuid() == [[aMenuItem representedObject] UTF8String] ? NSOnState : NSOffState];
	else if([aMenuItem action] == @selector(takeTabSizeFrom:))
		[aMenuItem setState:_textView.tabSize == [aMenuItem tag] ? NSOnState : NSOffState];
	else if([aMenuItem action] == @selector(showTabSizeSelectorPanel:))
	{
		static NSInteger const predefined[] = { 2, 3, 4, 8 };
		if(oak::contains(std::begin(predefined), std::end(predefined), _textView.tabSize))
		{
			[aMenuItem setTitle:@"Other…"];
			[aMenuItem setState:NSOffState];
		}
		else
		{
			[aMenuItem setTitle:[NSString stringWithFormat:@"Other (%zd)…", _textView.tabSize]];
			[aMenuItem setState:NSOnState];
		}
	}
	else if([aMenuItem action] == @selector(setIndentWithTabs:))
		[aMenuItem setState:_textView.softTabs ? NSOffState : NSOnState];
	else if([aMenuItem action] == @selector(setIndentWithSpaces:))
		[aMenuItem setState:_textView.softTabs ? NSOnState : NSOffState];
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
		text::selection_t sel(to_s(_textView.selectionString));
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
	[chooser showWindowRelativeToFrame:[self.window convertRectToScreen:[_textView convertRect:[_textView visibleRect] toView:nil]]];
}

- (void)showFindHistory:(id)sender
{
	OakPasteboardChooser* chooser = [OakPasteboardChooser sharedChooserForName:NSFindPboard];
	chooser.action = @selector(findNext:);
	[chooser showWindowRelativeToFrame:[self.window convertRectToScreen:[_textView convertRect:[_textView visibleRect] toView:nil]]];
}

// ==================
// = Symbol Chooser =
// ==================

- (void)selectAndCenter:(NSString*)aSelectionString
{
	_textView.selectionString = aSelectionString;
	[_textView centerSelectionInVisibleArea:self];
}

- (void)setSymbolChooser:(SymbolChooser*)aSymbolChooser
{
	if(_symbolChooser == aSymbolChooser)
		return;

	if(_symbolChooser)
	{
		[[NSNotificationCenter defaultCenter] removeObserver:self name:NSWindowWillCloseNotification object:_symbolChooser.window];

		_symbolChooser.target   = nil;
		_symbolChooser.document = nil;
	}

	if(_symbolChooser = aSymbolChooser)
	{
		_symbolChooser.target          = self;
		_symbolChooser.action          = @selector(symbolChooserDidSelectItems:);
		_symbolChooser.filterString    = @"";
		_symbolChooser.document        = document->document();
		_symbolChooser.selectionString = _textView.selectionString;

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
	[self.symbolChooser showWindowRelativeToFrame:[self.window convertRectToScreen:[_textView convertRect:[_textView visibleRect] toView:nil]]];
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
		[_textView performBundleItem:item];
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
	text::selection_t sel(to_s(_textView.selectionString));
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
	return _textView.tabSize;
}

- (void)setTabSize:(NSUInteger)newTabSize
{
	_textView.tabSize = newTabSize;
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
	_textView.softTabs = YES;
	settings_t::set(kSettingsSoftTabsKey, true, document->file_type());
}

- (IBAction)setIndentWithTabs:(id)sender
{
	D(DBF_OakDocumentView, bug("\n"););
	_textView.softTabs = NO;
	settings_t::set(kSettingsSoftTabsKey, false, document->file_type());
}

- (IBAction)showTabSizeSelectorPanel:(id)sender
{
	if(!tabSizeSelectorPanel)
		[NSBundle loadNibNamed:@"TabSizeSetting" owner:self];
	[tabSizeSelectorPanel makeKeyAndOrderFront:self];
}

- (void)toggleMacroRecording:(id)sender    { [_textView toggleMacroRecording:sender]; }

- (IBAction)takeThemeUUIDFrom:(id)sender
{
	[self setThemeWithUUID:[sender representedObject]];
}

- (void)setThemeWithUUID:(NSString*)themeUUID
{
	if(bundles::item_ptr const& themeItem = bundles::lookup(to_s(themeUUID)))
	{
		_textView.theme = parse_theme(themeItem);
		settings_t::set(kSettingsThemeKey, to_s(themeUUID));
		[self updateStyle];
	}
}

// =============================
// = GutterView Delegate Proxy =
// =============================

- (GVLineRecord)lineRecordForPosition:(CGFloat)yPos                              { return [_textView lineRecordForPosition:yPos];               }
- (GVLineRecord)lineFragmentForLine:(NSUInteger)aLine column:(NSUInteger)aColumn { return [_textView lineFragmentForLine:aLine column:aColumn]; }

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
				gutterImageName.emplace(1, rowState != GutterViewRowStateRegular ? "Bookmark Hover Remove Template" : "Bookmark Template");
			else if(rowState == GutterViewRowStateRegular)
				gutterImageName.emplace(2, pair.second.first);
		}

		if(rowState != GutterViewRowStateRegular)
			gutterImageName.emplace(3, "Bookmark Hover Add Template");

		if(!gutterImageName.empty())
			return [self gutterImage:[NSString stringWithCxxString:gutterImageName.begin()->second]];
	}
	else if([columnIdentifier isEqualToString:kFoldingsColumnIdentifier])
	{
		switch([_textView foldingStateForLine:lineNumber])
		{
			case kFoldingTop:       return [self gutterImage:rowState == GutterViewRowStateRegular ? @"Folding Top Template"       : @"Folding Top Hover Template"];
			case kFoldingCollapsed: return [self gutterImage:rowState == GutterViewRowStateRegular ? @"Folding Collapsed Template" : @"Folding Collapsed Hover Template"];
			case kFoldingBottom:    return [self gutterImage:rowState == GutterViewRowStateRegular ? @"Folding Bottom Template"    : @"Folding Bottom Hover Template"];
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
		[_textView toggleFoldingAtLine:lineNumber recursive:OakIsAlternateKeyOrMouseEvent()];
		[[NSNotificationCenter defaultCenter] postNotificationName:GVColumnDataSourceDidChange object:self];
	}
}

// ====================
// = Bookmark Actions =
// ====================

- (void)goToNextMarkOfType:(NSString*)markType
{
	text::selection_t sel(to_s(_textView.selectionString));

	ng::buffer_t const& buf = document->buffer();
	std::pair<size_t, std::string> const& pair = buf.next_mark(buf.convert(sel.last().max()), to_s(markType));
	if(pair.second != NULL_STR)
		_textView.selectionString = [NSString stringWithCxxString:buf.convert(pair.first)];
}

- (IBAction)goToPreviousMarkOfType:(NSString*)markType
{
	text::selection_t sel(to_s(_textView.selectionString));

	ng::buffer_t const& buf = document->buffer();
	std::pair<size_t, std::string> const& pair = buf.prev_mark(buf.convert(sel.last().max()), to_s(markType));
	if(pair.second != NULL_STR)
		_textView.selectionString = [NSString stringWithCxxString:buf.convert(pair.first)];
}

- (IBAction)toggleCurrentBookmark:(id)sender
{
	ng::buffer_t& buf = document->buffer();

	text::selection_t sel(to_s(_textView.selectionString));
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
	document->remove_all_marks(document::kBookmarkIdentifier);
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

// ============
// = Printing =
// ============

- (void)printDocument:(id)sender
{
	[document->document() runPrintOperationModalForWindow:self.window fontName:_textView.font.fontName];
}
@end
