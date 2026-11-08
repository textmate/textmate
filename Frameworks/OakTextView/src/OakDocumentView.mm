#import "OakDocumentView.h"
#import "OakTextView_Private.h"
#import "GutterView.h"
#import "OakSwiftUI-Swift.h"
#import <lsp/LSPClient.h>
#import <lsp/LSPManager.h>
#import <lsp/CopilotManager.h>
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
#import <OakAppKit/OakPasteboardChooser.h>
#import <OakAppKit/OakPasteboard.h>
#import <OakAppKit/OakUIConstructionFunctions.h>
#import <OakAppKit/NSMenuItem Additions.h>
#import <BundleMenu/BundleMenu.h>
#import <Preferences/Keys.h>

static NSString* const kBookmarksColumnIdentifier = @"bookmarks";
static NSString* const kFoldingsColumnIdentifier  = @"foldings";

@interface OakDocumentView () <NSAccessibilityGroup, GutterViewDelegate, GutterViewColumnDataSource, GutterViewColumnDelegate, OTVStatusBarDelegate>
{
	NSScrollView* gutterScrollView;
	GutterView* gutterView;
	NSMutableDictionary* gutterImages;

	OakBackgroundFillView* gutterDividerView;

	NSScrollView* textScrollView;

	NSMutableArray* topAuxiliaryViews;
	NSMutableArray* bottomAuxiliaryViews;

	IBOutlet NSPanel* tabSizeSelectorPanel;

	NSUInteger _cursorLine;
	BOOL _cursorLineHasActions;
	NSUInteger _probeGeneration;
}
@property (nonatomic, readonly) OTVStatusBar* statusBar;
@property (nonatomic) SymbolChooser* symbolChooser;
@property (nonatomic) NSArray* observedKeys;
- (void)updateStyle;
@end

@implementation OakDocumentView
- (id)initWithFrame:(NSRect)aRect
{
	if(self = [super initWithFrame:aRect])
	{
		self.accessibilityRole  = NSAccessibilityGroupRole;
		self.accessibilityLabel = @"Editor";

		_cursorLine = NSNotFound;

		_textView = [[OakTextView alloc] initWithFrame:NSZeroRect];
		_textView.autoresizingMask = NSViewWidthSizable|NSViewHeightSizable;

		textScrollView = [[NSScrollView alloc] initWithFrame:NSZeroRect];
		textScrollView.hasVerticalScroller      = YES;
		textScrollView.verticalScrollElasticity = NSScrollElasticityAllowed;
		textScrollView.hasHorizontalScroller    = YES;
		textScrollView.autohidesScrollers       = YES;
		textScrollView.borderType               = NSNoBorder;
		textScrollView.documentView             = _textView;

		gutterView = [[GutterView alloc] initWithFrame:NSZeroRect];
		gutterView.partnerView = _textView;
		gutterView.delegate    = self;
		[gutterView insertColumnWithIdentifier:kBookmarksColumnIdentifier atPosition:0 dataSource:self delegate:self];
		[gutterView insertColumnWithIdentifier:kFoldingsColumnIdentifier atPosition:2 dataSource:self delegate:self];
		if([NSUserDefaults.standardUserDefaults boolForKey:@"DocumentView Disable Line Numbers"])
			[gutterView setVisibility:NO forColumnWithIdentifier:GVLineNumbersColumnIdentifier];
		[gutterView setTranslatesAutoresizingMaskIntoConstraints:NO];

		gutterScrollView = [[NSScrollView alloc] initWithFrame:NSZeroRect];
		gutterScrollView.accessibilityElement = NO;
		gutterScrollView.borderType   = NSNoBorder;
		gutterScrollView.documentView = gutterView;

		[gutterScrollView.contentView addConstraint:[NSLayoutConstraint constraintWithItem:gutterView attribute:NSLayoutAttributeLeft relatedBy:NSLayoutRelationEqual toItem:gutterScrollView.contentView attribute:NSLayoutAttributeLeft multiplier:1.0 constant:0.0]];
		[gutterScrollView.contentView addConstraint:[NSLayoutConstraint constraintWithItem:gutterView attribute:NSLayoutAttributeTop relatedBy:NSLayoutRelationEqual toItem:gutterScrollView.contentView attribute:NSLayoutAttributeTop multiplier:1.0 constant:0.0]];
		[gutterScrollView.contentView addConstraint:[NSLayoutConstraint constraintWithItem:gutterView attribute:NSLayoutAttributeRight relatedBy:NSLayoutRelationEqual toItem:gutterScrollView.contentView attribute:NSLayoutAttributeRight multiplier:1.0 constant:0.0]];

		gutterDividerView = OakCreateVerticalLine(OakBackgroundFillViewStyleNone);

		_statusBar = [[OTVStatusBar alloc] initWithFrame:NSZeroRect];
		_statusBar.delegate = self;
		_statusBar.target = self;

		OakAddAutoLayoutViewsToSuperview(@[ gutterScrollView, gutterDividerView, textScrollView, _statusBar ], self);
		OakSetupKeyViewLoop(@[ self, _textView, _statusBar ]);

		self.document = [OakDocument documentWithString:@"" fileType:@"text.plain" customName:@"placeholder"];

		self.observedKeys = @[ @"selectionString", @"symbol", @"recordingMacro" ];
		for(NSString* keyPath in self.observedKeys)
			[_textView addObserver:self forKeyPath:keyPath options:NSKeyValueObservingOptionInitial context:NULL];

		[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(themeDidChange:) name:OakTextViewThemeDidChangeNotification object:_textView];
		[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(lspDiagnosticsDidChange:) name:LSPDiagnosticsDidChangeNotification object:nil];
		[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(lspServerStatusDidChange:) name:LSPServerStatusDidChangeNotification object:nil];
		[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(lspShowMessage:) name:LSPShowMessageNotification object:nil];
		[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(copilotStatusDidChange:) name:CopilotStatusDidChangeNotification object:nil];
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
		[stackedViews addObject:_statusBar];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[_statusBar]|" options:0 metrics:nil views:NSDictionaryOfVariableBindings(_statusBar)]];
	}

	[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[gutterScrollView(==gutterView)][gutterDividerView][textScrollView(>=100)]|" options:NSLayoutFormatAlignAllTop|NSLayoutFormatAlignAllBottom metrics:nil views:NSDictionaryOfVariableBindings(gutterScrollView, gutterView, gutterDividerView, textScrollView)]];
	[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[topView]" options:0 metrics:nil views:@{ @"topView": stackedViews[0] }]];
	[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:[bottomView]|" options:0 metrics:nil views:@{ @"bottomView": [stackedViews lastObject] }]];

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
		[_statusBar removeFromSuperview];
		_statusBar.delegate = nil;
		_statusBar.target = nil;
		_statusBar = nil;
	}
	else
	{
		_statusBar = [[OTVStatusBar alloc] initWithFrame:NSZeroRect];
		_statusBar.delegate = self;
		_statusBar.target = self;

		OakAddAutoLayoutViewsToSuperview(@[ _statusBar ], self);
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
	CGFloat const scaleFactor = [NSUserDefaults.standardUserDefaults floatForKey:kUserDefaultsLineNumberScaleFactorKey] ?: 0.8;
	NSString* lineNumberFontName = [NSUserDefaults.standardUserDefaults stringForKey:kUserDefaultsLineNumberFontNameKey] ?: [_textView.font fontName];

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
	[NSNotificationCenter.defaultCenter removeObserver:self];

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
		[NSNotificationCenter.defaultCenter removeObserver:self name:OakDocumentMarksDidChangeNotification object:oldDocument];
		[NSNotificationCenter.defaultCenter removeObserver:self name:OakDocumentContentDidChangeNotification object:oldDocument];
		[NSNotificationCenter.defaultCenter removeObserver:self name:OakDocumentDidSaveNotification object:oldDocument];
		[NSNotificationCenter.defaultCenter removeObserver:self name:OakDocumentWillCloseNotification object:oldDocument];
	}

	if(aDocument)
		[aDocument loadModalForWindow:self.window completionHandler:nullptr];

	if(_document = aDocument)
	{
		[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(documentMarksDidChange:) name:OakDocumentMarksDidChangeNotification object:self.document];
		[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(documentContentDidChange:) name:OakDocumentContentDidChangeNotification object:self.document];
		[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(documentDidSave:) name:OakDocumentDidSaveNotification object:self.document];
		[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(documentWillClose:) name:OakDocumentWillCloseNotification object:self.document];
		for(NSString* key in documentKeys)
			[self.document addObserver:self forKeyPath:key options:NSKeyValueObservingOptionInitial context:nullptr];
	}

	[_textView setDocument:self.document];
	[LSPManager.sharedManager documentDidOpen:aDocument];
	[[CopilotManager sharedManager] documentDidOpen:aDocument];
	[[CopilotManager sharedManager] documentDidFocus:aDocument];
	[gutterView reloadData:self];
	[self updateStyle];

	if(_symbolChooser)
	{
		_symbolChooser.TMDocument      = self.document;
		_symbolChooser.selectionString = _textView.selectionString;
	}

	if(oldDocument)
		[oldDocument close];

	if(aDocument)
	{
		__weak OakDocumentView* weakSelf = self;
		dispatch_async(dispatch_get_main_queue(), ^{
			[weakSelf updateLSPStatusBar];
		});
	}
}

- (void)themeDidChange:(NSNotification*)notification
{
	[self updateStyle];
}

- (void)updateStyle
{
	if(theme_ptr theme = _textView.theme)
	{
		[textScrollView setBackgroundColor:[NSColor colorWithCGColor:theme->background(to_s(self.document.fileType))]];
		[textScrollView setScrollerKnobStyle:theme->is_dark() ? NSScrollerKnobStyleLight : NSScrollerKnobStyleDark];

		[_textView setIbeamCursor:NSCursor.IBeamCursor];

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

		gutterDividerView.activeBackgroundColor = [NSColor separatorColor];

		[gutterView setNeedsDisplay:YES];
	}
}

- (BOOL)showsLineNumbers
{
	return [gutterView visibilityForColumnWithIdentifier:GVLineNumbersColumnIdentifier];
}

- (void)setShowsLineNumbers:(BOOL)flag
{
	[gutterView setVisibility:flag forColumnWithIdentifier:GVLineNumbersColumnIdentifier];
}

- (IBAction)toggleLineNumbers:(id)sender
{
	BOOL isVisibleFlag = !self.showsLineNumbers;
	self.showsLineNumbers = isVisibleFlag;
	if(isVisibleFlag)
			[NSUserDefaults.standardUserDefaults removeObjectForKey:@"DocumentView Disable Line Numbers"];
	else	[NSUserDefaults.standardUserDefaults setObject:@YES forKey:@"DocumentView Disable Line Numbers"];
}

- (BOOL)validateMenuItem:(NSMenuItem*)aMenuItem
{
	if([aMenuItem action] == @selector(toggleLineNumbers:))
		[aMenuItem setTitle:[gutterView visibilityForColumnWithIdentifier:GVLineNumbersColumnIdentifier] ? @"Hide Line Numbers" : @"Show Line Numbers"];
	else if([aMenuItem action] == @selector(takeTabSizeFrom:))
		[aMenuItem setState:_textView.tabSize == [aMenuItem tag] ? NSControlStateValueOn : NSControlStateValueOff];
	else if([aMenuItem action] == @selector(showTabSizeSelectorPanel:))
	{
		static NSInteger const predefined[] = { 2, 3, 4, 8 };
		if(oak::contains(std::begin(predefined), std::end(predefined), _textView.tabSize))
		{
			[aMenuItem setTitle:@"Other…"];
			[aMenuItem setState:NSControlStateValueOff];
		}
		else
		{
			[aMenuItem setDynamicTitle:[NSString stringWithFormat:@"Other (%zd)…", _textView.tabSize]];
			[aMenuItem setState:NSControlStateValueOn];
		}
	}
	else if([aMenuItem action] == @selector(setIndentWithTabs:))
		[aMenuItem setState:_textView.softTabs ? NSControlStateValueOff : NSControlStateValueOn];
	else if([aMenuItem action] == @selector(setIndentWithSpaces:))
		[aMenuItem setState:_textView.softTabs ? NSControlStateValueOn : NSControlStateValueOff];
	else if([aMenuItem action] == @selector(takeGrammarUUIDFrom:))
	{
		NSString* uuidString = [aMenuItem representedObject];
		if(bundles::item_ptr bundleItem = bundles::lookup(to_s(uuidString)))
		{
			bool selectedGrammar = to_s(self.document.fileType) == bundleItem->value_for_field(bundles::kFieldGrammarScope);
			[aMenuItem setState:selectedGrammar ? NSControlStateValueOn : NSControlStateValueOff];
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
	OakPasteboardChooser* chooser = [OakPasteboardChooser sharedChooserForPasteboard:OakPasteboard.generalPasteboard];
	chooser.action = @selector(paste:);
	[chooser showWindowRelativeToFrame:[self.window convertRectToScreen:[_textView convertRect:[_textView visibleRect] toView:nil]]];
}

- (void)showFindHistory:(id)sender
{
	OakPasteboardChooser* chooser = [OakPasteboardChooser sharedChooserForPasteboard:OakPasteboard.findPasteboard];
	chooser.action          = @selector(findNext:);
	chooser.alternateAction = @selector(orderFrontFindPanelForProject:);
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
		[NSNotificationCenter.defaultCenter removeObserver:self name:NSWindowWillCloseNotification object:_symbolChooser.window];

		_symbolChooser.target     = nil;
		_symbolChooser.TMDocument = nil;
	}

	if(_symbolChooser = aSymbolChooser)
	{
		_symbolChooser.target          = self;
		_symbolChooser.action          = @selector(symbolChooserDidSelectItems:);
		_symbolChooser.filterString    = @"";
		_symbolChooser.TMDocument      = self.document;
		_symbolChooser.selectionString = _textView.selectionString;

		[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(symbolChooserWillClose:) name:NSWindowWillCloseNotification object:_symbolChooser.window];
	}
}

- (void)symbolChooserWillClose:(NSNotification*)aNotification
{
	self.symbolChooser = nil;
}

- (IBAction)showSymbolChooser:(id)sender
{
	self.symbolChooser = SymbolChooser.sharedInstance;
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
		menuItem.submenu.delegate = BundleMenuDelegate.sharedInstance;

		if(selectedGrammar)
		{
			[menuItem setState:NSControlStateValueOn];
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
	ASSERT([sender respondsToSelector:@selector(tag)]);
	if([sender tag] > 0)
		self.tabSize = [sender tag];
}

- (IBAction)setIndentWithSpaces:(id)sender
{
	_textView.softTabs = YES;
	settings_t::set(kSettingsSoftTabsKey, true, to_s(self.document.fileType));
}

- (IBAction)setIndentWithTabs:(id)sender
{
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

// =============================
// = GutterView Delegate Proxy =
// =============================

- (GVLineRecord)lineRecordForPosition:(CGFloat)yPos                              { return [_textView lineRecordForPosition:yPos];               }
- (GVLineRecord)lineFragmentForLine:(NSUInteger)aLine column:(NSUInteger)aColumn { return [_textView lineFragmentForLine:aLine column:aColumn]; }

// =========================
// = Cursor Line Tracking  =
// =========================

- (void)invalidateCodeActionProbe
{
	_cursorLineHasActions = NO;
	++_probeGeneration;
	[NSNotificationCenter.defaultCenter postNotificationName:GVColumnDataSourceDidChange object:self];
}

- (void)updateCursorLine:(NSUInteger)line
{
	if(_cursorLine == line)
		return;

	NSUInteger oldLine = _cursorLine;
	_cursorLine = line;
	_cursorLineHasActions = NO;
	++_probeGeneration;

	// Redraw gutter for old line (remove lightbulb) and new line
	if(oldLine != NSNotFound || line != NSNotFound)
		[NSNotificationCenter.defaultCenter postNotificationName:GVColumnDataSourceDidChange object:self];

	// Check if new cursor line has diagnostics — if so, probe server for actions
	if(line == NSNotFound)
		return;

	OakDocument* doc = self.document;
	if(!doc)
		return;

	auto const settings = settings_for_path(doc.virtualPath ? to_s(doc.virtualPath) : to_s(doc.path), to_s(doc.fileType), to_s(doc.directory ?: @""));
	if(!settings.get("lspCodeActions", true))
		return;

	LSPManager* lsp = [LSPManager sharedManager];
	if(![lsp serverSupportsCodeActionsForDocument:doc])
		return;

	__block BOOL hasDiagnostic = NO;
	[doc enumerateBookmarksAtLine:line block:^(text::pos_t const& pos, NSString* type, NSString* payload){
		if([type isEqualToString:@"error"] || [type isEqualToString:@"warning"] || [type isEqualToString:@"note"])
			hasDiagnostic = YES;
	}];

	if(!hasDiagnostic)
		return;

	// Probe server for available actions on this line
	NSUInteger generation = _probeGeneration;
	__weak OakDocumentView* weakSelf = self;
	[lsp requestCodeActionsForDocument:doc
		line:line character:0
		endLine:line endCharacter:0
		completion:^(NSArray<NSDictionary*>* actions) {
			dispatch_async(dispatch_get_main_queue(), ^{
				OakDocumentView* strongSelf = weakSelf;
				if(!strongSelf || strongSelf->_probeGeneration != generation)
					return;
				strongSelf->_cursorLineHasActions = actions.count > 0;
				if(strongSelf->_cursorLineHasActions)
					[NSNotificationCenter.defaultCenter postNotificationName:GVColumnDataSourceDidChange object:strongSelf];
			});
		}];
}

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
		// Show lightbulb only when server confirmed actions are available for this line
		if(lineNumber == _cursorLine && _cursorLineHasActions)
		{
			NSImage* img = [NSImage imageWithSystemSymbolName:@"lightbulb.fill" accessibilityDescription:@"Code Actions"];
			if(img)
			{
				CGFloat size = floor((self.lineHeight - 1) / 2) * 2 + 1;
				NSImageSymbolConfiguration* config = [NSImageSymbolConfiguration configurationWithPointSize:size * 0.5 weight:NSFontWeightRegular];
				return [img imageWithSymbolConfiguration:config];
			}
		}

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
		// Lightbulb click triggers code actions
		if(lineNumber == _cursorLine && _cursorLineHasActions)
		{
			[_textView lspCodeActions:self];
			return;
		}

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
		[NSNotificationCenter.defaultCenter postNotificationName:GVColumnDataSourceDidChange object:self];
	}
}

- (void)clearAllBookmarks:(id)sender
{
	[self.document removeAllMarksOfType:OakDocumentBookmarkIdentifier];
}

- (void)documentMarksDidChange:(NSNotification*)aNotification
{
	[NSNotificationCenter.defaultCenter postNotificationName:GVColumnDataSourceDidChange object:self];
}

- (void)documentContentDidChange:(NSNotification*)notification
{
	[LSPManager.sharedManager documentDidChange:notification.object];
	[[CopilotManager sharedManager] documentDidChange:notification.object];
}

- (void)documentDidSave:(NSNotification*)notification
{
	[LSPManager.sharedManager documentDidSave:notification.object];
	[[CopilotManager sharedManager] documentDidSave:notification.object];
}

- (void)documentWillClose:(NSNotification*)notification
{
	[LSPManager.sharedManager documentWillClose:notification.object];
	[[CopilotManager sharedManager] documentWillClose:notification.object];
}

// =======================
// = LSP Status Bar      =
// =======================

- (void)updateLSPStatusBar
{
	if(!_statusBar)
		return;

	LSPManager* lsp = [LSPManager sharedManager];
	OakDocument* doc = self.document;

	NSString* status = [lsp serverStatusForDocument:doc];
	NSDictionary<NSString*, NSNumber*>* counts = [lsp diagnosticCountsForDocument:doc];

	[_statusBar setLspStatus:status
	                  errors:[counts[@"errors"] unsignedIntegerValue]
	                warnings:[counts[@"warnings"] unsignedIntegerValue]
	                    info:[counts[@"info"] unsignedIntegerValue]];

	[_statusBar setCopilotStatus:[CopilotManager sharedManager].status];
}

- (void)lspDiagnosticsDidChange:(NSNotification*)notification
{
	if(!_statusBar || !self.document)
		return;

	NSString* uri = notification.userInfo[@"uri"];
	if(uri)
	{
		NSURL* fileURL = self.document.path ? [NSURL fileURLWithPath:self.document.path] : nil;
		if(fileURL && ![uri isEqualToString:fileURL.absoluteString])
			return;
	}

	[self updateLSPStatusBar];
}

- (void)lspServerStatusDidChange:(NSNotification*)notification
{
	[self updateLSPStatusBar];
}

- (void)lspShowMessage:(NSNotification*)notification
{
	NSNumber* type = notification.userInfo[@"type"];
	if(type && type.intValue == 1 && _statusBar)
		[_statusBar flashLspError];
}

- (void)copilotStatusDidChange:(NSNotification*)notification
{
	CopilotManager* copilot = [CopilotManager sharedManager];
	[_statusBar setCopilotStatus:copilot.status];

	switch(copilot.status)
	{
		case CopilotStatusReady:
			[OakNotificationManager.shared showWithMessage:
				[NSString stringWithFormat:@"Copilot: Connected as %@", copilot.username ?: @"unknown"] type:4];
			break;
		case CopilotStatusAuthRequired:
			[OakNotificationManager.shared showWithMessage:@"Copilot: Authentication required" type:2];
			break;
		case CopilotStatusError:
			[OakNotificationManager.shared showWithMessage:@"Copilot: Server error" type:1];
			break;
		default:
			break;
	}
}

- (void)showCopilotStatusMenu:(NSPopUpButton*)popUpButton
{
	CopilotManager* copilot = [CopilotManager sharedManager];
	NSMenu* menu = popUpButton.menu;
	[menu removeAllItems];

	// Status header
	NSString* headerText;
	switch(copilot.status)
	{
		case CopilotStatusReady:
			headerText = [NSString stringWithFormat:@"Copilot — %@", copilot.username ?: @"ready"];
			break;
		case CopilotStatusConnecting:
			headerText = @"Copilot — connecting…";
			break;
		case CopilotStatusAuthRequired:
			headerText = @"Copilot — sign-in required";
			break;
		case CopilotStatusError:
			headerText = @"Copilot — error";
			break;
		default:
			headerText = @"Copilot — disabled";
			break;
	}
	NSMenuItem* header = [[NSMenuItem alloc] initWithTitle:headerText action:nil keyEquivalent:@""];
	if(copilot.status == CopilotStatusReady)
		header.state = NSControlStateValueOn;
	header.enabled = NO;
	[menu addItem:header];

	[menu addItem:[NSMenuItem separatorItem]];

	if(copilot.status == CopilotStatusAuthRequired)
	{
		NSMenuItem* signIn = [[NSMenuItem alloc] initWithTitle:@"Sign In" action:@selector(copilotSignIn:) keyEquivalent:@""];
		signIn.target = self;
		[menu addItem:signIn];
	}

	if(copilot.status == CopilotStatusReady || copilot.status == CopilotStatusError)
	{
		NSMenuItem* restart = [[NSMenuItem alloc] initWithTitle:@"Restart Server" action:@selector(copilotRestart:) keyEquivalent:@""];
		restart.target = self;
		[menu addItem:restart];
	}

	if(copilot.status == CopilotStatusReady)
	{
		[menu addItem:[NSMenuItem separatorItem]];

		BOOL popupSuppressed = [[NSUserDefaults standardUserDefaults] boolForKey:@"CopilotSuppressAutoPopup"];
		NSMenuItem* suppressItem = [[NSMenuItem alloc] initWithTitle:@"Ghost Text Only (No Popup)"
		                                                      action:@selector(copilotToggleSuppressPopup:)
		                                               keyEquivalent:@""];
		suppressItem.target = self;
		suppressItem.state = popupSuppressed ? NSControlStateValueOn : NSControlStateValueOff;
		[menu addItem:suppressItem];
	}

	[menu addItem:[NSMenuItem separatorItem]];

	NSMenuItem* disable = [[NSMenuItem alloc] initWithTitle:
		copilot.status == CopilotStatusDisabled ? @"Enable Copilot" : @"Disable Copilot"
		action:@selector(copilotToggle:) keyEquivalent:@""];
	disable.target = self;
	[menu addItem:disable];
}

- (void)copilotSignIn:(id)sender
{
	[[CopilotManager sharedManager] signIn];
}

- (void)copilotRestart:(id)sender
{
	CopilotManager* copilot = [CopilotManager sharedManager];
	[copilot shutdown];
	if(self.document)
		[copilot documentDidOpen:self.document];
}

- (void)copilotToggle:(id)sender
{
	CopilotManager* copilot = [CopilotManager sharedManager];
	if(copilot.status == CopilotStatusDisabled)
	{
		settings_t::set("copilotEnabled", true);
		if(self.document)
			[copilot documentDidOpen:self.document];
	}
	else
	{
		settings_t::set("copilotEnabled", false);
		[copilot shutdown];
	}
}

- (void)copilotToggleSuppressPopup:(id)sender
{
	NSUserDefaults* defaults = [NSUserDefaults standardUserDefaults];
	BOOL current = [defaults boolForKey:@"CopilotSuppressAutoPopup"];
	[defaults setBool:!current forKey:@"CopilotSuppressAutoPopup"];
}

- (void)showLSPStatusMenu:(NSPopUpButton*)popUpButton
{
	LSPManager* lsp = [LSPManager sharedManager];
	OakDocument* doc = self.document;
	NSMenu* menu = popUpButton.menu;
	[menu removeAllItems];

	NSString* serverName = [lsp serverNameForDocument:doc];
	NSString* status = [lsp serverStatusForDocument:doc];

	NSString* headerText;
	if(serverName && status)
		headerText = [NSString stringWithFormat:@"%@ — %@", serverName, status];
	else
		headerText = @"No LSP Server";

	NSMenuItem* header = [[NSMenuItem alloc] initWithTitle:headerText action:nil keyEquivalent:@""];
	header.enabled = NO;
	[menu addItem:header];

	if(status)
	{
		[menu addItem:[NSMenuItem separatorItem]];
		NSMenuItem* restart = [[NSMenuItem alloc] initWithTitle:@"Restart Server" action:@selector(lspRestartServer:) keyEquivalent:@""];
		restart.target = self;
		[menu addItem:restart];

		NSMenuItem* reindex = [[NSMenuItem alloc] initWithTitle:@"Re-index Workspace" action:@selector(lspReindexWorkspace:) keyEquivalent:@""];
		reindex.target = self;
		[menu addItem:reindex];
	}

	NSDictionary* counts = [lsp diagnosticCountsForDocument:doc];
	NSUInteger errors   = [counts[@"errors"] unsignedIntegerValue];
	NSUInteger warnings = [counts[@"warnings"] unsignedIntegerValue];
	NSUInteger info     = [counts[@"info"] unsignedIntegerValue];

	if(errors + warnings + info > 0)
	{
		[menu addItem:[NSMenuItem separatorItem]];

		NSMenuItem* next = [[NSMenuItem alloc] initWithTitle:@"Next Diagnostic" action:@selector(lspNextDiagnostic:) keyEquivalent:@""];
		next.target = self;
		[menu addItem:next];

		NSMenuItem* prev = [[NSMenuItem alloc] initWithTitle:@"Previous Diagnostic" action:@selector(lspPrevDiagnostic:) keyEquivalent:@""];
		prev.target = self;
		[menu addItem:prev];
	}
}

- (void)lspRestartServer:(id)sender
{
	[[LSPManager sharedManager] restartServerForDocument:self.document];
}

- (void)lspReindexWorkspace:(id)sender
{
	[[LSPManager sharedManager] reindexWorkspaceForDocument:self.document];
}

- (void)lspNextDiagnostic:(id)sender
{
	NSArray<NSString*>* types = @[ @"error", @"warning", @"note" ];
	text::selection_t sel(to_s(_textView.selectionString));
	text::pos_t caret = sel.last().max();
	text::pos_t next = [self.document nextMarkOfTypes:types fromPosition:caret];
	if(next != text::pos_t::undefined)
	{
		_textView.selectionString = to_ns(next);
		[_textView centerSelectionInVisibleArea:self];
	}
}

- (void)lspPrevDiagnostic:(id)sender
{
	NSArray<NSString*>* types = @[ @"error", @"warning", @"note" ];
	text::selection_t sel(to_s(_textView.selectionString));
	text::pos_t caret = sel.last().max();
	text::pos_t prev = [self.document prevMarkOfTypes:types fromPosition:caret];
	if(prev != text::pos_t::undefined)
	{
		_textView.selectionString = to_ns(prev);
		[_textView centerSelectionInVisibleArea:self];
	}
}

// ============
// = Printing =
// ============

- (void)printDocument:(id)sender
{
	[self.document runPrintOperationModalForWindow:self.window fontName:_textView.font.fontName];
}
@end
