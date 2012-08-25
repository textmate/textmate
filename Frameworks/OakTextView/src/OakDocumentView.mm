#import "OakDocumentView.h"
#import "OTVStatusBar.h"
#import <document/document.h>
#import <file/type.h>
#import <text/ctype.h>
#import <text/parse.h>
#import <ns/ns.h>
#import <oak/debug.h>
#import <bundles/bundles.h>
#import <OakFoundation/OakFoundation.h>
#import <OakFoundation/NSString Additions.h>
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/NSColor Additions.h>
#import <OakAppKit/NSImage Additions.h>
#import <OakAppKit/OakToolTip.h>
#import <OakAppKit/OakPasteboard.h>
#import <OakAppKit/NSMenuItem Additions.h>
#import <BundleMenu/BundleMenuDelegate.h>

OAK_DEBUG_VAR(OakDocumentView);

static NSString* const kBookmarksColumnIdentifier = @"bookmarks";
static NSString* const kFoldingsColumnIdentifier  = @"foldings";

@interface OakDocumentView ()
@property (nonatomic, readonly) OTVStatusBar* statusBar;
@property (nonatomic, retain) NSColor* gutterDividerColor;
- (void)updateStyle;
@end

// ===========================================
// = OakTextView GutterView Delegate Methods =
// ===========================================

struct retained_image_t
{
	retained_image_t (char const* name) : image(name ? [[NSImage imageNamed:@(name) inSameBundleAsClass:[OakTextView class]] retain] : nil) { }
	operator NSImage* () const { return image; }
	NSImage* image;
};

// =============================

static NSString* const ObservedTextViewKeyPaths[] = { @"selectionString", @"tabSize", @"softTabs", @"freehandedEditing", @"overwriteMode", @"isMacroRecording"};

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
	OakDocumentView* self;
};

@implementation OakDocumentView
@synthesize textView, statusBar, gutterDividerColor;

- (BOOL)showResizeThumb               { return statusBar.showResizeThumb; }
- (void)setShowResizeThumb:(BOOL)flag { statusBar.showResizeThumb = flag; }

- (id)initWithFrame:(NSRect)aRect
{
	D(DBF_OakDocumentView, bug("%s\n", [NSStringFromRect(aRect) UTF8String]););
	if(self = [super initWithFrame:aRect])
	{
		callback = new document_view_callback_t(self);

		CGFloat gutterViewWidth = 40;

		NSRect textScrollViewFrame = NSMakeRect(gutterViewWidth, OakStatusBarHeight, NSWidth(aRect)-gutterViewWidth, NSHeight(aRect)-OakStatusBarHeight);
		NSSize textViewSize = [NSScrollView contentSizeForFrameSize:textScrollViewFrame.size hasHorizontalScroller:YES hasVerticalScroller:YES borderType:NSNoBorder];

		textScrollView = [[NSScrollView alloc] initWithFrame:textScrollViewFrame];
		textView = [[OakTextView alloc] initWithFrame:NSMakeRect(0, 0, textViewSize.width, textViewSize.height)];
		textView.autoresizingMask        = NSViewWidthSizable|NSViewHeightSizable;

		textScrollView.hasVerticalScroller   = YES;
		textScrollView.hasHorizontalScroller = YES;
		textScrollView.borderType            = NSNoBorder;
		textScrollView.autoresizingMask      = NSViewWidthSizable|NSViewHeightSizable;
		textScrollView.documentView          = textView;

		[self addSubview:textScrollView];

		NSRect gutterScrollViewFrame = NSMakeRect(0, OakStatusBarHeight, gutterViewWidth - 1, NSHeight(aRect)-OakStatusBarHeight);
		NSSize gutterViewSize = [NSScrollView contentSizeForFrameSize:gutterScrollViewFrame.size hasHorizontalScroller:NO hasVerticalScroller:NO borderType:NSNoBorder];

		gutterView = [[GutterView alloc] initWithFrame:NSMakeRect(0, 0, gutterViewSize.width, gutterViewSize.height)];
		gutterView.partnerView      = textView;
		gutterView.delegate         = self;
		[gutterView insertColumnWithIdentifier:kBookmarksColumnIdentifier atPosition:0 dataSource:self delegate:self];
		[gutterView insertColumnWithIdentifier:kFoldingsColumnIdentifier atPosition:2 dataSource:self delegate:self];

		gutterScrollView = [[NSScrollView alloc] initWithFrame:gutterScrollViewFrame];
		gutterScrollView.borderType       = NSNoBorder;
		gutterScrollView.autoresizingMask = NSViewHeightSizable;
		gutterScrollView.documentView     = gutterView;

		[self addSubview:gutterScrollView];

		if([[NSUserDefaults standardUserDefaults] boolForKey:@"DocumentView Disable Line Numbers"])
			[[gutterScrollView documentView] setVisibility:NO forColumnWithIdentifier:GVLineNumbersColumnIdentifier];
		
		NSRect statusBarFrame = NSMakeRect(0, 0, NSWidth(aRect), OakStatusBarHeight);
		statusBar = [[OTVStatusBar alloc] initWithFrame:statusBarFrame];
		statusBar.delegate = self;
		statusBar.autoresizingMask = NSViewWidthSizable;
		[self addSubview:statusBar];

		[self setDocument:document::from_content("", "text.plain")]; // file type is only to avoid potential “no grammar” warnings in console

		iterate(keyPath, ObservedTextViewKeyPaths)
			[textView addObserver:self forKeyPath:*keyPath options:NSKeyValueObservingOptionInitial context:NULL];
	}
	return self;
}

- (void)setFont:(NSFont*)newFont
{
	textView.font = newFont;
	gutterView.lineNumberFont = [NSFont fontWithName:[newFont fontName] size:round(0.8 * [newFont pointSize])];
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

	// if([aKeyPath isEqualToString:@"document.grammarUUID"])
	// {
	// 	NSString* uuidString = document.grammarUUID;
	// 	statusBar.grammarName = uuidString ? [NSString stringWithCxxString:bundles::lookup([uuidString UTF8String])->name()] : nil;
	// }
	/*else*/ if([aKeyPath isEqualToString:@"selectionString"])
	{
		char const* str = [[textView valueForKey:@"selectionString"] UTF8String] ?: "1";
		[gutterView setHighlightedRange:str];
		[statusBar setCaretPosition:str];

		ng::buffer_t const& buf = document->buffer();
		text::selection_t sel([textView.selectionString UTF8String]);
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
	iterate(keyPath, ObservedTextViewKeyPaths)
		[textView removeObserver:self forKeyPath:*keyPath];
	[[NSNotificationCenter defaultCenter] removeObserver:self];

	[self setDocument:document::document_ptr()];
	delete callback;

	[gutterScrollView release];
	[gutterView release];
	[gutterDividerColor release];
	[textScrollView release];
	[textView release];
	[statusBar release];
	[topAuxiliaryViews release];
	[bottomAuxiliaryViews release];
	[super dealloc];
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
	[self updateStyle];

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
		[self setFont:textView.font]; // trigger update of gutter view’s line number font	
		auto styles = [textView theme]->styles_for_scope(document->buffer().scope(0).left, NULL_STR, 0);
		self.gutterDividerColor = [NSColor colorWithCGColor:styles.gutterDivider()] ?: [NSColor grayColor];

		gutterView.foregroundColor = [NSColor colorWithCGColor:styles.gutterForeground()];
		gutterView.backgroundColor = [NSColor colorWithCGColor:styles.gutterBackground()];
		gutterScrollView.backgroundColor = gutterView.backgroundColor;
		gutterView.selectionForegroundColor = [NSColor colorWithCGColor:styles.gutterSelectionForeground()];
		gutterView.selectionBackgroundColor = [NSColor colorWithCGColor:styles.gutterSelectionBackground()];
		gutterView.selectionDividerColor = [NSColor colorWithCGColor:styles.gutterSelectionDivider()];

		[self setNeedsDisplay:YES];
		[textView setNeedsDisplay:YES];
		[gutterView setNeedsDisplay:YES];
	}
}

- (BOOL)isOpaque
{
	return YES; // this is not entirely true as this view only draws the separator between gutter and text view, but we know all the other areas are completely drawn by subviews, so we return ‘YES’ to avoid our parent view (window) having to fill the entire area each time the separator needs to be redrawn
}

- (IBAction)toggleLineNumbers:(id)sender
{
	D(DBF_OakDocumentView, bug("show line numbers %s\n", BSTR([[gutterScrollView documentView] visibilityForColumnWithIdentifier:GVLineNumbersColumnIdentifier])););
	BOOL isVisibleFlag = ![[gutterScrollView documentView] visibilityForColumnWithIdentifier:GVLineNumbersColumnIdentifier];
	[[gutterScrollView documentView] setVisibility:isVisibleFlag forColumnWithIdentifier:GVLineNumbersColumnIdentifier];

	if(isVisibleFlag)
			[[NSUserDefaults standardUserDefaults] removeObjectForKey:@"DocumentView Disable Line Numbers"];
	else	[[NSUserDefaults standardUserDefaults] setObject:YES_obj forKey:@"DocumentView Disable Line Numbers"];
}

- (void)toggleContinuousSpellChecking:(id)sender
{
	bool flag = !document->buffer().live_spelling();
	document->buffer().set_live_spelling(flag);
	settings_t::set(kSettingsSpellCheckingKey, flag, document->file_type(), document->path());
}

- (void)takeSpellingLanguageFrom:(id)sender
{
	[[NSSpellChecker sharedSpellChecker] setLanguage:[sender representedObject]];
	document->buffer().set_spelling_language(to_s((NSString*)[sender representedObject]));
}

- (BOOL)validateMenuItem:(NSMenuItem*)aMenuItem
{
	if([aMenuItem action] == @selector(toggleLineNumbers:))
		[aMenuItem setState:[[gutterScrollView documentView] visibilityForColumnWithIdentifier:GVLineNumbersColumnIdentifier] ? NSOffState : NSOnState];
	else if([aMenuItem action] == @selector(takeThemeUUIDFrom:))
		[aMenuItem setState:[textView theme]->uuid() == [[aMenuItem representedObject] UTF8String] ? NSOnState : NSOffState];
	else if([aMenuItem action] == @selector(takeTabSizeFrom:))
		[aMenuItem setState:textView.tabSize == [aMenuItem tag] ? NSOnState : NSOffState];
	else if([aMenuItem action] == @selector(toggleContinuousSpellChecking:))
		[aMenuItem setState:document->buffer().live_spelling() ? NSOnState : NSOffState];
	else if([aMenuItem action] == @selector(takeSpellingLanguageFrom:))
		[aMenuItem setState:[[NSString stringWithCxxString:document->buffer().spelling_language()] isEqualToString:[aMenuItem representedObject]] ? NSOnState : NSOffState];
	return YES;
}

// ===================
// = Auxiliary Views =
// ===================

- (void)layoutAuxiliaryViews
{
	CGFloat topHeight = 0, bottomHeight = 0;
	for(NSView* view in topAuxiliaryViews)
		topHeight += NSHeight(view.frame);
	for(NSView* view in bottomAuxiliaryViews)
		bottomHeight += NSHeight(view.frame);

	CGFloat totalHeight = NSHeight(self.frame);
	CGFloat docHeight   = totalHeight - NSHeight(statusBar.frame) - topHeight - bottomHeight;
	CGFloat gutterWidth = NSWidth(gutterScrollView.frame) + 1;

	CGFloat y = NSHeight(statusBar.frame);
	for(NSView* view in bottomAuxiliaryViews)
	{
		[view setFrame:NSMakeRect(0, y, NSWidth(self.frame), NSHeight(view.frame))];
		y += NSHeight(view.frame);
	}

	[gutterScrollView setFrame:NSMakeRect(0, y, gutterWidth - 1, docHeight)];
	[textScrollView setFrame:NSMakeRect(gutterWidth, y, NSWidth(textScrollView.frame), docHeight)];

	y += docHeight;
	for(NSView* view in topAuxiliaryViews)
	{
		[view setFrame:NSMakeRect(0, y, NSWidth(self.frame), NSHeight(view.frame))];
		y += NSHeight(view.frame);
	}
}

- (void)addAuxiliaryView:(NSView*)aView atEdge:(NSRectEdge)anEdge
{
	topAuxiliaryViews    = topAuxiliaryViews    ?: [NSMutableArray new];
	bottomAuxiliaryViews = bottomAuxiliaryViews ?: [NSMutableArray new];
	if(anEdge == NSMinYEdge)
			[bottomAuxiliaryViews addObject:aView];
	else	[topAuxiliaryViews addObject:aView];
	[self addSubview:aView];
	[self layoutAuxiliaryViews];
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
	[self layoutAuxiliaryViews];
}

- (void)drawRect:(NSRect)aRect
{
	if([bottomAuxiliaryViews count])
	{
		CGFloat y = NSHeight(statusBar.frame), height = 0;
		for(NSView* view in bottomAuxiliaryViews)
			height += NSHeight(view.frame);

		[[NSColor lightGrayColor] set];
		NSRectFill(NSIntersectionRect(NSMakeRect(NSMinX(aRect), y, NSWidth(aRect), height - 1), aRect));
		[[NSColor grayColor] set];
		NSRectFill(NSIntersectionRect(NSMakeRect(NSMinX(aRect), y + height - 1, NSWidth(aRect), 1), aRect));
	}

	if([topAuxiliaryViews count])
	{
		CGFloat height = 0;
		for(NSView* view in topAuxiliaryViews)
			height += NSHeight(view.frame);

		[[NSColor lightGrayColor] set];
		NSRectFill(NSIntersectionRect(NSMakeRect(NSMinX(aRect), NSHeight(self.frame) - height + 1, NSWidth(aRect), height - 1), aRect));
		[[NSColor grayColor] set];
		NSRectFill(NSIntersectionRect(NSMakeRect(NSMinX(aRect), NSHeight(self.frame) - height, NSWidth(aRect), 1), aRect));
	}

	// Draw the border between gutter and text views
	[gutterDividerColor set];
	NSRect gutterFrame = gutterScrollView.frame;
	NSRectFill(NSMakeRect(NSMaxX(gutterFrame), NSMinY(gutterFrame), 1, NSHeight(gutterFrame)));
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

// =======================
// = Status bar delegate =
// =======================

- (void)takeGrammarUUIDFrom:(id)sender
{
	if(bundles::item_ptr item = bundles::lookup(to_s((NSString*)[sender representedObject])))
	{
		document->set_file_type(item->value_for_field(bundles::kFieldGrammarScope));
		file::set_type(document->virtual_path(), item->value_for_field(bundles::kFieldGrammarScope));
	}
}

- (IBAction)showLanguageSelector:(id)sender
{
	std::multimap<std::string, bundles::item_ptr, text::less_t> grammars;
	citerate(item, bundles::query(bundles::kFieldAny, NULL_STR, scope::wildcard, bundles::kItemTypeGrammar))
		grammars.insert(std::make_pair((*item)->name(), *item));

	NSMenu* menu = [[NSMenu new] autorelease];
	iterate(pair, grammars)
	{
		bool selectedGrammar = document->file_type() == pair->second->value_for_field(bundles::kFieldGrammarScope);
		if(!selectedGrammar && pair->second->hidden_from_user())
			continue;

		NSMenuItem* item = [menu addItemWithTitle:[NSString stringWithCxxString:pair->first] action:@selector(takeGrammarUUIDFrom:) keyEquivalent:@""];
		[item setKeyEquivalentCxxString:pair->second->value_for_field(bundles::kFieldKeyEquivalent)];
		[item setTarget:self];
		[item setRepresentedObject:[NSString stringWithCxxString:pair->second->uuid()]];

		if(selectedGrammar)
			[item setState:NSOnState];
	}

	if(grammars.empty())
		[menu addItemWithTitle:@"No Grammars Loaded" action:@selector(nop:) keyEquivalent:@""];

	[statusBar showMenu:menu withSelectedIndex:-1 forCellWithTag:[sender tag] font:[NSFont controlContentFontOfSize:[NSFont smallSystemFontSize]] popup:YES];
}

- (void)goToSymbol:(id)sender
{
	[textView setSelectionString:[sender representedObject]];
}

- (IBAction)showSymbolSelector:(id)sender
{
	ng::buffer_t const& buf = document->buffer();
	text::selection_t sel([textView.selectionString UTF8String]);
	size_t i = buf.convert(sel.last().max());

	NSInteger index = 0;
	NSMenu* menu = [[NSMenu new] autorelease];
	citerate(pair, buf.symbols())
	{
		if(pair->second == "-")
		{
			[menu addItem:[NSMenuItem separatorItem]];
		}
		else
		{
			NSMenuItem* item = [menu addItemWithTitle:[NSString stringWithCxxString:pair->second] action:@selector(goToSymbol:) keyEquivalent:@""];
			[item setTarget:self];
			[item setRepresentedObject:[NSString stringWithCxxString:buf.convert(pair->first)]];
		}

		if(pair->first <= i)
			++index;
	}
	if(menu.numberOfItems == 0)
		[[menu addItemWithTitle:@"No symbols to show for current document." action:@selector(dummy:) keyEquivalent:@""] setEnabled:NO];
	[statusBar showMenu:menu withSelectedIndex:(index ? index-1 : 0) forCellWithTag:[sender tag] font:[NSFont controlContentFontOfSize:[NSFont smallSystemFontSize]] popup:YES];
}

- (IBAction)showBundleItemSelector:(id)sender
{
	std::multimap<std::string, bundles::item_ptr, text::less_t> ordered;
	citerate(item, bundles::query(bundles::kFieldAny, NULL_STR, scope::wildcard, bundles::kItemTypeBundle))
		ordered.insert(std::make_pair((*item)->name(), *item));
	
	NSMenu* menu = [NSMenu new];
	iterate(pair, ordered)
	{
		bool selectedGrammar = false;
		citerate(item, bundles::query(bundles::kFieldGrammarScope, document->file_type(), scope::wildcard, bundles::kItemTypeGrammar, pair->second->uuid(), true, true))
			selectedGrammar = true;
		if(!selectedGrammar && pair->second->hidden_from_user() || pair->second->menu().empty())
			continue;

		NSMenuItem* menuItem = [menu addItemWithTitle:[NSString stringWithCxxString:pair->first] action:NULL keyEquivalent:@""];
		menuItem.submenu = [[NSMenu new] autorelease];
		menuItem.submenu.delegate = [[[BundleMenuDelegate alloc] initWithBundleItem:pair->second] autorelease];
		menuItem.submenu.autoenablesItems = NO;

		if(selectedGrammar)
			[menuItem setState:NSOnState];
	}
	
	if(ordered.empty())
		[menu addItemWithTitle:@"No Bundles Loaded" action:@selector(nop:) keyEquivalent:@""];

	[statusBar showMenu:menu withSelectedIndex:-1 forCellWithTag:sender ? [sender tag] : 1 font:[NSFont controlContentFontOfSize:[NSFont smallSystemFontSize]] popup:YES];
	[menu release];
}

- (IBAction)showTabSizeSelector:(id)sender
{
	NSInteger index = 0;
	NSInteger sizes[] = { 2, 3, 4, 8 };
	NSMenu* menu = [[NSMenu new] autorelease];
	[[menu addItemWithTitle:@"Indent Size" action:@selector(dummy:) keyEquivalent:@""] setEnabled:NO];
	iterate(size, sizes)
	{
		NSMenuItem* item = [menu addItemWithTitle:[NSString stringWithFormat:@"\u2003%ld", *size] action:@selector(takeTabSizeFrom:) keyEquivalent:@""];
		[item setTarget:self];
		[item setTag:*size];
		if(*size == textView.tabSize)
		{
			[item setState:NSOnState];
			index = menu.numberOfItems - 1;
		}
	}
	[[menu addItemWithTitle:@"\u2003Other…" action:@selector(showTabSizeSelectorPanel:) keyEquivalent:@""] setTarget:self];
	[menu addItem:[NSMenuItem separatorItem]];
	[[menu addItemWithTitle:@"Indent Using" action:@selector(dummy:) keyEquivalent:@""] setEnabled:NO];
	NSMenuItem* item = nil;
	item = [menu addItemWithTitle:@"\u2003Tabs" action:@selector(setIndentWithTabs:) keyEquivalent:@""];
	[item setTarget:self];
	[item setState:(textView.softTabs ? NSOffState : NSOnState)];
	item = [menu addItemWithTitle:@"\u2003Spaces" action:@selector(setIndentWithSpaces:) keyEquivalent:@""];
	[item setTarget:self];
	[item setState:(textView.softTabs ? NSOnState : NSOffState)];
	[statusBar showMenu:menu withSelectedIndex:index forCellWithTag:[sender tag] font:[NSFont controlContentFontOfSize:[NSFont smallSystemFontSize]] popup:YES];
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

- (IBAction)toggleOverwriteMode:(id)sender     { /*[textView toggleOverwriteMode:sender];*/ }
- (IBAction)toggleFreehandedEditing:(id)sender { /*[textView toggleFreehandedEditing:sender];*/ }
- (IBAction)toggleMacroRecording:(id)sender    { [textView toggleMacroRecording:sender]; }

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

- (GVLineRecord const&)lineRecordForPosition:(CGFloat)yPos                              { return [textView lineRecordForPosition:yPos];               }
- (GVLineRecord const&)lineFragmentForLine:(NSUInteger)aLine column:(NSUInteger)aColumn { return [textView lineFragmentForLine:aLine column:aColumn]; }

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
	if([identifier isEqualToString:kBookmarksColumnIdentifier])
	{
		static retained_image_t images[] = { NULL, "Bookmark", "Search Mark" };
		return state < sizeofA(images) ? images[state] : nil;
	}
	else if([identifier isEqualToString:kFoldingsColumnIdentifier])
	{
		static retained_image_t images[] = { NULL, "Folding Top", "Folding Collapsed", "Folding Bottom" };
		return state < sizeofA(images) ? images[state] : nil;
	}
	return nil;
}

- (NSImage*)hoverImageForState:(NSUInteger)state forColumnWithIdentifier:(id)identifier
{
	if([identifier isEqualToString:kBookmarksColumnIdentifier])
	{
		static retained_image_t images[] = { "Bookmark Hover Add", "Bookmark Hover Remove", "Bookmark Hover Add" };
		return state < sizeofA(images) ? images[state] : nil;
	}
	else if([identifier isEqualToString:kFoldingsColumnIdentifier])
	{
		static retained_image_t images[] = { NULL, "Folding Top Hover", "Folding Collapsed Hover", "Folding Bottom Hover" };
		return state < sizeofA(images) ? images[state] : nil;
	}
	return nil;
}

- (NSImage*)pressedImageForState:(NSUInteger)state forColumnWithIdentifier:(id)identifier
{
	if([identifier isEqualToString:kBookmarksColumnIdentifier])
	{
		static retained_image_t images[] = { "Bookmark Pressed", "Bookmark Pressed", "Bookmark Pressed" };
		return state < sizeofA(images) ? images[state] : nil;
	}
	else if([identifier isEqualToString:kFoldingsColumnIdentifier])
	{
		static retained_image_t images[] = { NULL, "Folding Top Pressed", "Folding Collapsed Pressed", "Folding Bottom Pressed" };
		return state < sizeofA(images) ? images[state] : nil;
	}
	return nil;
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
@end
