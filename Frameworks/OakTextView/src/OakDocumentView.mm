#import "OakDocumentView.h"
#import "GutterView.h"
#import "OTVStatusBar.h"
#import <document/OakDocument.h>
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

	NSMutableArray* topAuxiliaryViews;
	NSMutableArray* bottomAuxiliaryViews;

	IBOutlet NSPanel* tabSizeSelectorPanel;
}
@property (nonatomic, readonly) OTVStatusBar* statusBar;
@property (nonatomic) SymbolChooser* symbolChooser;
@property (nonatomic) NSArray* observedKeys;
- (void)updateStyle;
@end

@implementation OakDocumentView
- (id)initWithFrame:(NSRect)aRect
{
	D(DBF_OakDocumentView, bug("%s\n", [NSStringFromRect(aRect) UTF8String]););
	if(self = [super initWithFrame:aRect])
	{
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

		[gutterScrollView.contentView addConstraint:[NSLayoutConstraint constraintWithItem:gutterView attribute:NSLayoutAttributeLeft relatedBy:NSLayoutRelationEqual toItem:gutterScrollView.contentView attribute:NSLayoutAttributeLeft multiplier:1.0 constant:0.0]];
		[gutterScrollView.contentView addConstraint:[NSLayoutConstraint constraintWithItem:gutterView attribute:NSLayoutAttributeTop relatedBy:NSLayoutRelationEqual toItem:gutterScrollView.contentView attribute:NSLayoutAttributeTop multiplier:1.0 constant:0.0]];
		[gutterScrollView.contentView addConstraint:[NSLayoutConstraint constraintWithItem:gutterView attribute:NSLayoutAttributeRight relatedBy:NSLayoutRelationEqual toItem:gutterScrollView.contentView attribute:NSLayoutAttributeRight multiplier:1.0 constant:0.0]];

		gutterDividerView = OakCreateVerticalLine(nil);
		statusDividerView = OakCreateHorizontalLine([NSColor colorWithCalibratedWhite:0.500 alpha:1], [NSColor colorWithCalibratedWhite:0.750 alpha:1]);

		_statusBar = [[OTVStatusBar alloc] initWithFrame:NSZeroRect];
		_statusBar.delegate = self;
		_statusBar.target = self;

		OakAddAutoLayoutViewsToSuperview(@[ gutterScrollView, gutterDividerView, textScrollView, statusDividerView, _statusBar ], self);
		OakSetupKeyViewLoop(@[ self, _textView, _statusBar ], NO);

		self.document = [OakDocument documentWithString:@"" fileType:@"text.plain" customName:@"placeholder"];

		self.observedKeys = @[ @"selectionString", @"symbol", @"recordingMacro"];
		for(NSString* keyPath in self.observedKeys)
			[_textView addObserver:self forKeyPath:keyPath options:NSKeyValueObservingOptionInitial context:NULL];
	}
	return self;
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
		[self addConstraint:[NSLayoutConstraint constraintWithItem:statusDividerView attribute:NSLayoutAttributeLeft relatedBy:NSLayoutRelationEqual toItem:self attribute:NSLayoutAttributeLeft multiplier:1.0 constant:0.0]];
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
	gutterView.lineNumberFont = [NSFont fontWithName:lineNumberFontName size:round(scaleFactor * [_textView.font pointSize] * _textView.fontScaleFactor)];
	[gutterView reloadData:self];
}

- (IBAction)makeTextLarger:(id)sender
{
	_textView.fontScaleFactor += 0.1;
	[self updateGutterViewFont:self];
}

- (IBAction)makeTextSmaller:(id)sender
{
	if(_textView.fontScaleFactor > 0.1)
	{
		_textView.fontScaleFactor -= 0.1;
		[self updateGutterViewFont:self];
	}
}

- (IBAction)makeTextStandardSize:(id)sender
{
	_textView.fontScaleFactor = 1;
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
	if([aKeyPath isEqualToString:@"selectionString"])
	{
		NSString* str = [_textView valueForKey:@"selectionString"];
		[gutterView setHighlightedRange:to_s(str ?: @"1")];
		[_statusBar setSelectionString:str];
		_symbolChooser.selectionString = str;
	}
	else if([aKeyPath isEqualToString:@"symbol"])
	{
		_statusBar.symbolName = _textView.symbol;
	}
	else if([aKeyPath isEqualToString:@"recordingMacro"])
	{
		_statusBar.recordingMacro = _textView.isRecordingMacro;
	}
	else if([aKeyPath isEqualToString:@"fileType"])
	{
		_statusBar.fileType = self.document.fileType;
	}
	else if([aKeyPath isEqualToString:@"tabSize"])
	{
		_statusBar.tabSize = self.document.tabSize;
	}
	else if([aKeyPath isEqualToString:@"softTabs"])
	{
		_statusBar.softTabs = self.document.softTabs;
	}
}

- (void)dealloc
{
	for(NSString* keyPath in self.observedKeys)
		[_textView removeObserver:self forKeyPath:keyPath];
	[[NSNotificationCenter defaultCenter] removeObserver:self];

	self.document = nil;
	self.symbolChooser = nil;
}

- (void)setDocument:(OakDocument*)aDocument
{
	NSArray* const documentKeys = @[ @"fileType", @"tabSize", @"softTabs" ];

	OakDocument* oldDocument = self.document;
	if(oldDocument)
	{
		for(NSString* key in documentKeys)
			[oldDocument removeObserver:self forKeyPath:key];
		[[NSNotificationCenter defaultCenter] removeObserver:self name:OakDocumentMarksDidChangeNotification object:oldDocument];
	}

	if(aDocument)
		[aDocument loadModalForWindow:self.window completionHandler:nullptr];

	if(_document = aDocument)
	{
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(documentMarksDidChange:) name:OakDocumentMarksDidChangeNotification object:self.document];
		for(NSString* key in documentKeys)
			[self.document addObserver:self forKeyPath:key options:NSKeyValueObservingOptionInitial context:nullptr];
	}

	[_textView setDocument:self.document];
	[gutterView reloadData:self];
	[self updateStyle];

	if(_symbolChooser)
	{
		_symbolChooser.document        = self.document;
		_symbolChooser.selectionString = _textView.selectionString;
	}

	if(oldDocument)
		[oldDocument close];
}

- (void)updateStyle
{
	if(theme_ptr theme = _textView.theme)
	{
		[[self window] setOpaque:!theme->is_transparent() && !theme->gutter_styles().is_transparent()];
		[textScrollView setBackgroundColor:[NSColor colorWithCGColor:theme->background(to_s(self.document.fileType))]];

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
			[aMenuItem setDynamicTitle:[NSString stringWithFormat:@"Other (%zd)…", _textView.tabSize]];
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
			bool selectedGrammar = to_s(self.document.fileType) == bundleItem->value_for_field(bundles::kFieldGrammarScope);
			[aMenuItem setState:selectedGrammar ? NSOnState : NSOffState];
		}
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
		_symbolChooser.document        = self.document;
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

	text::selection_t sel(to_s(_textView.selectionString));
	text::pos_t caret = sel.last().max();

	__block NSInteger index = 0;
	[self.document enumerateSymbolsUsingBlock:^(text::pos_t const& pos, NSString* symbol){
		if([symbol isEqualToString:@"-"])
		{
			[symbolMenu addItem:[NSMenuItem separatorItem]];
		}
		else
		{
			NSUInteger indent = 0;
			while(indent < symbol.length && [symbol characterAtIndex:indent] == 0x2003) // Em-space
				++indent;

			NSMenuItem* item = [symbolMenu addItemWithTitle:[symbol substringFromIndex:indent] action:@selector(goToSymbol:) keyEquivalent:@""];
			[item setIndentationLevel:indent];
			[item setTarget:self];
			[item setRepresentedObject:to_ns(pos)];
		}

		if(pos <= caret)
			++index;
	}];

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
		for(auto item : bundles::query(bundles::kFieldGrammarScope, to_s(self.document.fileType), scope::wildcard, bundles::kItemTypeGrammar, pair.second->uuid(), true, true))
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
	settings_t::set(kSettingsTabSizeKey, (size_t)newTabSize, to_s(self.document.fileType));
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
	settings_t::set(kSettingsSoftTabsKey, true, to_s(self.document.fileType));
}

- (IBAction)setIndentWithTabs:(id)sender
{
	D(DBF_OakDocumentView, bug("\n"););
	_textView.softTabs = NO;
	settings_t::set(kSettingsSoftTabsKey, false, to_s(self.document.fileType));
}

- (IBAction)showTabSizeSelectorPanel:(id)sender
{
	if(!tabSizeSelectorPanel)
		[[NSBundle bundleForClass:[self class]] loadNibNamed:@"TabSizeSetting" owner:self topLevelObjects:NULL];
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
		__block std::map<size_t, NSString*> gutterImageName;

		[self.document enumerateBookmarksAtLine:lineNumber block:^(text::pos_t const& pos, NSString* type, NSString* payload){
			if(payload.length != 0)
				gutterImageName.emplace(0, type);
			else if([type isEqualToString:OakDocumentBookmarkIdentifier])
				gutterImageName.emplace(1, rowState != GutterViewRowStateRegular ? @"Bookmark Hover Remove Template" : @"Bookmark Template");
			else if(rowState == GutterViewRowStateRegular)
				gutterImageName.emplace(2, type);
		}];

		if(rowState != GutterViewRowStateRegular)
			gutterImageName.emplace(3, @"Bookmark Hover Add Template");

		if(!gutterImageName.empty())
			return [self gutterImage:gutterImageName.begin()->second];
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
	[self.document enumerateBookmarksUsingBlock:^(text::pos_t const& pos, NSString* excerpt){
		NSString* prefix = to_ns(text::pad(pos.line+1, 4) + ": ");
		NSMenuItem* item = [aMenu addItemWithTitle:[prefix stringByAppendingString:excerpt] action:@selector(takeBookmarkFrom:) keyEquivalent:@""];
		[item setRepresentedObject:to_ns(pos)];
	}];

	BOOL hasBookmarks = aMenu.numberOfItems;
	if(hasBookmarks)
		[aMenu addItem:[NSMenuItem separatorItem]];
	[aMenu addItemWithTitle:@"Clear Bookmarks" action:hasBookmarks ? @selector(clearAllBookmarks:) : @selector(nop:) keyEquivalent:@""];
}

// =======================
// = GutterView Delegate =
// =======================

- (void)userDidClickColumnWithIdentifier:(id)columnIdentifier atLine:(NSUInteger)lineNumber
{
	if([columnIdentifier isEqualToString:kBookmarksColumnIdentifier])
	{
		__block std::vector<text::pos_t> bookmarks;
		__block NSMutableArray* content = [NSMutableArray array];

		[self.document enumerateBookmarksAtLine:lineNumber block:^(text::pos_t const& pos, NSString* type, NSString* payload){
			if(payload.length != 0)
				[content addObject:payload];
			else if([type isEqualToString:OakDocumentBookmarkIdentifier])
				bookmarks.push_back(pos);
		}];

		if(content.count == 0)
		{
			if(bookmarks.empty())
					[self.document setMarkOfType:OakDocumentBookmarkIdentifier atPosition:text::pos_t(lineNumber, 0) content:nil];
			else	[self.document removeMarkOfType:OakDocumentBookmarkIdentifier atPosition:bookmarks.front()];
		}
		else
		{
			NSView* popoverContainerView = [[NSView alloc] initWithFrame:NSZeroRect];

			NSTextField* textField = OakCreateLabel([content componentsJoinedByString:@"\n"]);
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

- (void)clearAllBookmarks:(id)sender
{
	[self.document removeAllMarksOfType:OakDocumentBookmarkIdentifier];
}

- (void)documentMarksDidChange:(NSNotification*)aNotification
{
	[[NSNotificationCenter defaultCenter] postNotificationName:GVColumnDataSourceDidChange object:self];
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
	[self.document runPrintOperationModalForWindow:self.window fontName:_textView.font.fontName];
}
@end
