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
#import <OakAppKit/OakUIConstructionFunctions.h>
#import <OakAppKit/NSMenuItem Additions.h>
#import <BundleMenu/BundleMenu.h>

OAK_DEBUG_VAR(OakDocumentView);

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
	NSDictionary* gutterImages;
	NSDictionary* gutterHoverImages;
	NSDictionary* gutterPressedImages;

	NSBox* gutterDividerView;
	NSBox* statusDividerView;

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
@property (nonatomic, retain) NSDictionary* gutterImages;
@property (nonatomic, retain) NSDictionary* gutterHoverImages;
@property (nonatomic, retain) NSDictionary* gutterPressedImages;
@property (nonatomic) OakFilterWindowController* filterWindowController;
@property (nonatomic) SymbolChooser*             symbolChooser;
- (void)updateStyle;
@end

static NSString* const ObservedTextViewKeyPaths[] = { @"selectionString", @"tabSize", @"softTabs", @"isMacroRecording"};

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
			citerate(item, bundles::query(bundles::kFieldGrammarScope, document->file_type()))
				self.statusBar.grammarName = [NSString stringWithCxxString:(*item)->name()];
		}
		else if(event == did_change_indent_settings)
		{
			self.statusBar.tabSize  = document->buffer().indent().tab_size();
			self.statusBar.softTabs = document->buffer().indent().soft_tabs();
		}

		if(document->recent_tracking() && document->path() != NULL_STR)
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
@synthesize gutterImages, gutterHoverImages, gutterPressedImages;

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
		[self addSubview:textScrollView];

		gutterView = [[GutterView alloc] initWithFrame:NSZeroRect];
		gutterView.partnerView = textView;
		gutterView.delegate    = self;
		[gutterView insertColumnWithIdentifier:kBookmarksColumnIdentifier atPosition:0 dataSource:self delegate:self];
		[gutterView insertColumnWithIdentifier:kFoldingsColumnIdentifier atPosition:2 dataSource:self delegate:self];

		gutterScrollView = [[OakDisableAccessibilityScrollView alloc] initWithFrame:NSZeroRect];
		gutterScrollView.borderType   = NSNoBorder;
		gutterScrollView.documentView = gutterView;
		[self addSubview:gutterScrollView];

		if([[NSUserDefaults standardUserDefaults] boolForKey:@"DocumentView Disable Line Numbers"])
			[gutterView setVisibility:NO forColumnWithIdentifier:GVLineNumbersColumnIdentifier];

		gutterDividerView = OakCreateViewWithColor();
		[self addSubview:gutterDividerView];

		statusDividerView = OakCreateHorizontalLine([NSColor colorWithCalibratedWhite:0.500 alpha:1], [NSColor colorWithCalibratedWhite:0.750 alpha:1]);
		[self addSubview:statusDividerView];

		statusBar = [[OTVStatusBar alloc] initWithFrame:NSZeroRect];
		statusBar.delegate = self;
		[self addSubview:statusBar];

		for(NSView* view in @[ gutterScrollView, gutterView, gutterDividerView, textScrollView, statusDividerView, statusBar ])
			[view setTranslatesAutoresizingMaskIntoConstraints:NO];

		document::document_ptr doc = document::from_content("", "text.plain"); // file type is only to avoid potential “no grammar” warnings in console
		doc->set_custom_name("null document"); // without a name it grabs an ‘untitled’ token
		[self setDocument:doc];

		iterate(keyPath, ObservedTextViewKeyPaths)
			[textView addObserver:self forKeyPath:*keyPath options:NSKeyValueObservingOptionInitial context:NULL];
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

	NSDictionary* views = NSDictionaryOfVariableBindings(gutterScrollView, gutterView, gutterDividerView, textScrollView, statusDividerView, statusBar);
	[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[gutterScrollView(==gutterView)][gutterDividerView(==1)][textScrollView(>=100)]|" options:NSLayoutFormatAlignAllTop|NSLayoutFormatAlignAllBottom metrics:nil views:views]];
	[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[statusBar]|"                                                                     options:0 metrics:nil views:views]];
	[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:[statusDividerView][statusBar]|"                                                   options:NSLayoutFormatAlignAllLeft|NSLayoutFormatAlignAllRight metrics:nil views:views]];

	NSMutableArray* stackedViews = [NSMutableArray array];
	[stackedViews addObjectsFromArray:topAuxiliaryViews];
	[stackedViews addObject:gutterScrollView];
	[stackedViews addObjectsFromArray:bottomAuxiliaryViews];
	[stackedViews addObject:statusDividerView];

	[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[topView]" options:0 metrics:nil views:@{ @"topView" : stackedViews[0] }]];
	for(size_t i = 0; i < [stackedViews count]-1; ++i)
		[self addConstraint:[NSLayoutConstraint constraintWithItem:stackedViews[i] attribute:NSLayoutAttributeBottom relatedBy:NSLayoutRelationEqual toItem:stackedViews[i+1] attribute:NSLayoutAttributeTop multiplier:1 constant:0]];

	for(NSArray* views in @[ topAuxiliaryViews, bottomAuxiliaryViews ])
	{
		for(NSView* view in views)
			[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[view]|" options:0 metrics:nil views:NSDictionaryOfVariableBindings(view)]];
	}
}

- (NSImage*)gutterImage:(NSString*)aName
{
	if(NSImage* res = [[NSImage imageNamed:aName inSameBundleAsClass:[self class]] copy])
	{
		// We use capHeight instead of x-height since most fonts have the numbers
		// extend to this height, so centering around the x-height would look off
		CGFloat height = [gutterView.lineNumberFont capHeight];
		CGFloat width = [res size].width * height / [res size].height;

		CGFloat scaleFactor = 1.0;

		// Since all images are vector based and don’t contain any spacing to
		// align it, we need to set the individual scaleFactor per image.
		if([aName hasPrefix:@"Bookmark"]) scaleFactor = 1.0;
		if([aName hasPrefix:@"Folding"])  scaleFactor = 1.5;
		if([aName hasPrefix:@"Search"])   scaleFactor = 1.2;

		[res setSize:NSMakeSize(round(width * scaleFactor), round(height * scaleFactor))];

		return res;
	}
	NSLog(@"%s no image named ‘%@’", sel_getName(_cmd), aName);
	return nil;
}

- (void)setFont:(NSFont*)newFont
{
	textView.font = newFont;
	gutterView.lineNumberFont = [NSFont fontWithName:[newFont fontName] size:round(0.8 * [newFont pointSize])];

	self.gutterImages = @{
		kBookmarksColumnIdentifier : @[ [NSNull null], [self gutterImage:@"Bookmark"], [self gutterImage:@"Search Mark"] ],
		kFoldingsColumnIdentifier  : @[ [NSNull null], [self gutterImage:@"Folding Top"], [self gutterImage:@"Folding Collapsed"], [self gutterImage:@"Folding Bottom"] ],
	};

	self.gutterHoverImages = @{
		kBookmarksColumnIdentifier : @[ [self gutterImage:@"Bookmark Hover Add"], [self gutterImage:@"Bookmark Hover Remove"], [self gutterImage:@"Bookmark Hover Add"] ],
		kFoldingsColumnIdentifier  : @[ [NSNull null], [self gutterImage:@"Folding Top Hover"], [self gutterImage:@"Folding Collapsed Hover"], [self gutterImage:@"Folding Bottom Hover"] ],
	};

	self.gutterPressedImages = @{
		kBookmarksColumnIdentifier : @[ [self gutterImage:@"Bookmark"], [self gutterImage:@"Bookmark"], [self gutterImage:@"Bookmark"] ],
		kFoldingsColumnIdentifier  : @[ [NSNull null], [self gutterImage:@"Folding Top Hover"], [self gutterImage:@"Folding Collapsed Hover"], [self gutterImage:@"Folding Bottom Hover"] ],
	};

	[gutterView reloadData:self];
}

- (IBAction)makeTextLarger:(id)sender       { [self setFont:[NSFont fontWithName:[textView.font fontName] size:[textView.font pointSize] + 1]]; }
- (IBAction)makeTextSmaller:(id)sender      { [self setFont:[NSFont fontWithName:[textView.font fontName] size:std::max<CGFloat>([textView.font pointSize] - 1, 5)]]; }

- (void)changeFont:(id)sender
{
	if(NSFont* newFont = [sender convertFont:textView.font])
	{
		settings_t::set(kSettingsFontNameKey, to_s([newFont fontName]));
		settings_t::set(kSettingsFontSizeKey, (size_t)[newFont pointSize]);
		[self setFont:newFont];
	}
}

- (void)observeValueForKeyPath:(NSString*)aKeyPath ofObject:(id)observableController change:(NSDictionary*)changeDictionary context:(void*)userData
{
	if(observableController != textView || ![[NSArray arrayWithObjects:&ObservedTextViewKeyPaths[0] count:sizeofA(ObservedTextViewKeyPaths)] containsObject:aKeyPath])
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

	iterate(keyPath, ObservedTextViewKeyPaths)
		[textView removeObserver:self forKeyPath:*keyPath];
	[[NSNotificationCenter defaultCenter] removeObserver:self];

	[self setDocument:document::document_ptr()];
	delete callback;

	self.filterWindowController = nil;
	self.symbolChooser          = nil;
}

- (document::document_ptr const&)document
{
	return document;
}

- (void)setDocument:(document::document_ptr const&)aDocument
{
	document::document_ptr oldDocument = document;
	if(oldDocument)
	{
		oldDocument->remove_callback(callback);
		oldDocument->set_visible_rect(to_s(NSStringFromRect([self.textView visibleRect])));
	}

	if(aDocument)
		aDocument->open();

	if(document = aDocument)
	{
		document->add_callback(callback);
		document->show();

		citerate(item, bundles::query(bundles::kFieldGrammarScope, document->file_type()))
			statusBar.grammarName = [NSString stringWithCxxString:(*item)->name()];
		statusBar.tabSize  = document->buffer().indent().tab_size();
		statusBar.softTabs = document->buffer().indent().soft_tabs();
	}

	[textView setDocument:document];
	[gutterView reloadData:self];
	[self updateStyle];

	if(_symbolChooser)
		_symbolChooser.document = document;

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

		[[self window] setOpaque:!theme->is_transparent()];
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

		[self setFont:textView.font]; // trigger update of gutter view’s line number font
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
		gutterDividerView.borderColor        = [NSColor tmColorWithCGColor:styles.divider];

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
		[aMenuItem setState:[gutterView visibilityForColumnWithIdentifier:GVLineNumbersColumnIdentifier] ? NSOffState : NSOnState];
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
	return YES;
}

// ===================
// = Auxiliary Views =
// ===================

- (void)addAuxiliaryView:(NSView*)aView atEdge:(NSRectEdge)anEdge
{
	[aView setTranslatesAutoresizingMaskIntoConstraints:NO];

	topAuxiliaryViews    = topAuxiliaryViews    ?: [NSMutableArray new];
	bottomAuxiliaryViews = bottomAuxiliaryViews ?: [NSMutableArray new];
	if(anEdge == NSMinYEdge)
			[bottomAuxiliaryViews addObject:aView];
	else	[topAuxiliaryViews addObject:aView];
	[self addSubview:aView];
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
	[[OakPasteboard pasteboardWithName:NSGeneralPboard] selectItemAtPosition:[textView positionForWindowUnderCaret] andCall:@selector(paste:)];
}

- (void)showFindHistory:(id)sender
{
	[[OakPasteboard pasteboardWithName:NSFindPboard] selectItemAtPosition:[textView positionForWindowUnderCaret] andCall:@selector(findNext:)];
}

// ==================
// = Symbol Chooser =
// ==================

- (void)setFilterWindowController:(OakFilterWindowController*)controller
{
	if(_filterWindowController == controller)
		return;

	if(_filterWindowController)
	{
		[[NSNotificationCenter defaultCenter] removeObserver:self name:NSWindowWillCloseNotification object:_filterWindowController.window];
		_filterWindowController.target = nil;
		[_filterWindowController close];
	}

	if(_filterWindowController = controller)
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(filterWindowWillClose:) name:NSWindowWillCloseNotification object:_filterWindowController.window];
}

- (void)filterWindowWillClose:(NSNotification*)notification
{
	self.filterWindowController = nil;
	self.symbolChooser = nil;
}

- (IBAction)showSymbolChooser:(id)sender
{
	if(self.filterWindowController)
	{
		[self.filterWindowController.window makeKeyAndOrderFront:self];
		return;
	}

	self.symbolChooser = [SymbolChooser symbolChooserForDocument:document];
	self.symbolChooser.selectionString = textView.selectionString;

	self.filterWindowController                         = [OakFilterWindowController new];
	self.filterWindowController.dataSource              = self.symbolChooser;
	self.filterWindowController.action                  = @selector(symbolChooserDidSelectItems:);
	self.filterWindowController.sendActionOnSingleClick = YES;
	[self.filterWindowController showWindowRelativeToWindow:self.window];
}

- (void)symbolChooserDidSelectItems:(id)sender
{
	for(id item in [sender selectedItems])
		[textView setSelectionString:[item selectionString]];
}

// =======================
// = Status bar delegate =
// =======================

- (void)takeGrammarUUIDFrom:(id)sender
{
	if(bundles::item_ptr item = bundles::lookup(to_s((NSString*)[sender representedObject])))
		[textView performBundleItem:item];
}

- (void)goToSymbol:(id)sender
{
	[textView setSelectionString:[sender representedObject]];
}

- (void)showSymbolSelector:(NSPopUpButton*)symbolPopUp
{
	NSMenu* symbolMenu = symbolPopUp.menu;
	[symbolMenu removeAllItems];

	ng::buffer_t const& buf = document->buffer();
	text::selection_t sel([textView.selectionString UTF8String]);
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
	[NSApp sendAction:_cmd to:self.statusBar from:self];
}

- (void)showBundleItemSelector:(NSPopUpButton*)bundleItemsPopUp
{
	NSMenu* bundleItemsMenu = bundleItemsPopUp.menu;
	[bundleItemsMenu removeAllItems];

	std::multimap<std::string, bundles::item_ptr, text::less_t> ordered;
	for(auto item : bundles::query(bundles::kFieldAny, NULL_STR, scope::wildcard, bundles::kItemTypeBundle))
		ordered.insert(std::make_pair(item->name(), item));

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

- (IBAction)takeTabSizeFrom:(id)sender
{
	D(DBF_OakDocumentView, bug("\n"););
	ASSERT([sender respondsToSelector:@selector(tag)]);
	if([sender tag] > 0)
	{
		textView.tabSize = [sender tag];
		settings_t::set(kSettingsTabSizeKey, (size_t)[sender tag], document->file_type());
	}
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

enum bookmark_state_t { kBookmarkNoMark, kBookmarkRegularMark, kBookmarkSearchMark };

static std::string const kBookmarkType = "bookmark";

- (NSUInteger)stateForColumnWithIdentifier:(id)columnIdentifier atLine:(NSUInteger)lineNumber
{
	if([columnIdentifier isEqualToString:kBookmarksColumnIdentifier])
	{
		ng::buffer_t const& buf = document->buffer();
		std::map<size_t, std::string> const& marks = buf.get_marks(buf.begin(lineNumber), buf.eol(lineNumber));
		iterate(pair, marks)
		{
			if(pair->second == kBookmarkType)
				return kBookmarkRegularMark;
		}

		iterate(pair, marks)
		{
			if(pair->second == "search")
				return kBookmarkSearchMark;
		}
		return kBookmarkNoMark;
	}
	else if([columnIdentifier isEqualToString:kFoldingsColumnIdentifier])
	{
		return [textView foldingStateForLine:lineNumber];
	}
	return 0;
}

- (NSImage*)imageForState:(NSUInteger)state forColumnWithIdentifier:(id)identifier
{
	NSArray* array = gutterImages[identifier];
	return array && state < [array count] && array[state] != [NSNull null] ? array[state] : nil;
}

- (NSImage*)hoverImageForState:(NSUInteger)state forColumnWithIdentifier:(id)identifier
{
	NSArray* array = gutterHoverImages[identifier];
	return array && state < [array count] && array[state] != [NSNull null] ? array[state] : nil;
}

- (NSImage*)pressedImageForState:(NSUInteger)state forColumnWithIdentifier:(id)identifier
{
	NSArray* array = gutterPressedImages[identifier];
	return array && state < [array count] && array[state] != [NSNull null] ? array[state] : nil;
}

// =============================
// = Bookmark Submenu Delegate =
// =============================

- (void)takeBookmarkFrom:(id)sender
{
	if([sender respondsToSelector:@selector(representedObject)])
		[textView setSelectionString:[sender representedObject]];
}

- (void)updateBookmarksMenu:(NSMenu*)aMenu
{
	ng::buffer_t& buf = document->buffer();
	std::map<size_t, std::string> const& marks = buf.get_marks(0, buf.size(), kBookmarkType);
	iterate(pair, marks)
	{
		size_t n = buf.convert(pair->first).line;
		NSMenuItem* item = [aMenu addItemWithTitle:[NSString stringWithCxxString:text::pad(n+1, 4) + ": " + buf.substr(buf.begin(n), buf.eol(n))] action:@selector(takeBookmarkFrom:) keyEquivalent:@""];
		[item setRepresentedObject:[NSString stringWithCxxString:buf.convert(pair->first)]];
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
		std::map<size_t, std::string> const& marks = buf.get_marks(buf.begin(lineNumber), buf.eol(lineNumber), kBookmarkType);
		iterate(pair, marks)
		{
			if(pair->second == kBookmarkType)
				return buf.remove_mark(buf.begin(lineNumber) + pair->first, pair->second);
		}
		buf.set_mark(buf.begin(lineNumber), kBookmarkType);
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

- (IBAction)toggleCurrentBookmark:(id)sender
{
	ng::buffer_t& buf = document->buffer();

	text::selection_t sel([textView.selectionString UTF8String]);
	size_t lineNumber = sel.last().max().line;

	std::vector<size_t> toRemove;
	std::map<size_t, std::string> const& marks = buf.get_marks(buf.begin(lineNumber), buf.eol(lineNumber), kBookmarkType);
	iterate(pair, marks)
	{
		if(pair->second == kBookmarkType)
			toRemove.push_back(buf.begin(lineNumber) + pair->first);
	}

	if(toRemove.empty())
	{
		buf.set_mark(buf.convert(sel.last().max()), kBookmarkType);
	}
	else
	{
		iterate(index, toRemove)
			buf.remove_mark(*index, kBookmarkType);
	}
	[[NSNotificationCenter defaultCenter] postNotificationName:GVColumnDataSourceDidChange object:self];
}

- (IBAction)goToNextBookmark:(id)sender
{
	text::selection_t sel([textView.selectionString UTF8String]);

	ng::buffer_t const& buf = document->buffer();
	std::pair<size_t, std::string> const& pair = buf.next_mark(buf.convert(sel.last().max()), kBookmarkType);
	if(pair.second != NULL_STR)
		textView.selectionString = [NSString stringWithCxxString:buf.convert(pair.first)];
}

- (IBAction)goToPreviousBookmark:(id)sender
{
	text::selection_t sel([textView.selectionString UTF8String]);

	ng::buffer_t const& buf = document->buffer();
	std::pair<size_t, std::string> const& pair = buf.prev_mark(buf.convert(sel.last().max()), kBookmarkType);
	if(pair.second != NULL_STR)
		textView.selectionString = [NSString stringWithCxxString:buf.convert(pair.first)];
}

- (void)clearAllBookmarks:(id)sender
{
	document->buffer().remove_all_marks(kBookmarkType);
	[[NSNotificationCenter defaultCenter] postNotificationName:GVColumnDataSourceDidChange object:self];
}

// =================
// = Accessibility =
// =================

- (BOOL)accessibilityIsIgnored
{
	return NO;
}

- (NSArray*)accessibilityAttributeNames
{
	static NSArray* attributes = nil;
	if(!attributes)
	{
		NSSet* set = [NSSet setWithArray:[super accessibilityAttributeNames]];
		set = [set setByAddingObjectsFromArray:@[
			NSAccessibilityRoleAttribute,
			NSAccessibilityDescriptionAttribute,
		]];
		attributes = [set allObjects];
	}
	return attributes;
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
