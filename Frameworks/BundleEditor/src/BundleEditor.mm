#import "BundleEditor.h"
#import "PropertiesViewController.h"
#import "OakRot13Transformer.h"
#import "be_entry.h"
#import <OakFoundation/NSString Additions.h>
#import <OakFoundation/OakStringListTransformer.h>
#import <OakAppKit/NSAlert Additions.h>
#import <OakAppKit/NSImage Additions.h>
#import <OakAppKit/OakSound.h>
#import <OakAppKit/OakFileIconImage.h>
#import <OakTextView/OakDocumentView.h>
#import <document/OakDocument.h>
#import <document/OakDocumentController.h>
#import <BundlesManager/BundlesManager.h>
#import <command/runner.h> // fix_shebang
#import <plist/ascii.h>
#import <plist/delta.h>
#import <regexp/format_string.h>
#import <text/decode.h>
#import <cf/cf.h>
#import <ns/ns.h>
#import <io/environment.h>
#import <settings/settings.h>
#import <oak/debug.h>

OAK_DEBUG_VAR(BundleEditor);

@class OakCommand;

@interface BundleEditor () <OakTextViewDelegate>
{
	IBOutlet NSBrowser* browser;
	IBOutlet OakDocumentView* documentView;
	NSDrawer* drawer;

	be::entry_ptr bundles;
	std::map<bundles::item_ptr, plist::dictionary_t> changes;

	BOOL propertiesChanged;

	bundles::item_ptr bundleItem;
	OakDocument* bundleItemContent;
}
- (void)didChangeBundleItems;
- (void)didChangeModifiedState;
@property (nonatomic) PropertiesViewController* sharedPropertiesViewController;
@property (nonatomic) PropertiesViewController* extraPropertiesViewController;
@property (nonatomic) NSMutableDictionary* bundleItemProperties;
- (bundles::item_ptr const&)bundleItem;
- (void)setBundleItem:(bundles::item_ptr const&)aBundleItem;
@end

namespace
{
	static bundles::kind_t const PlistItemKinds[] = { bundles::kItemTypeSettings, bundles::kItemTypeMacro, bundles::kItemTypeTheme };

	static std::vector<std::string> const& PlistKeySortOrder ()
	{
		static auto const res = new std::vector<std::string>{ "shellVariables", "disabled", "name", "value", "comment", "match", "begin", "while", "end", "applyEndPatternLast", "captures", "beginCaptures", "whileCaptures", "endCaptures", "contentName", "injections", "patterns", "repository", "include", "increaseIndentPattern", "decreaseIndentPattern", "indentNextLinePattern", "unIndentedLinePattern", "disableIndentCorrections", "indentOnPaste", "indentedSoftWrap", "format", "foldingStartMarker", "foldingStopMarker", "foldingIndentedBlockStart", "foldingIndentedBlockIgnore", "characterClass", "smartTypingPairs", "highlightPairs", "showInSymbolList", "symbolTransformation", "disableDefaultCompletion", "completions", "completionCommand", "spellChecking", "softWrap", "fontName", "fontStyle", "fontSize", "foreground", "background", "bold", "caret", "invisibles", "italic", "misspelled", "selection", "underline" };
		return *res;
	}

	static struct item_info_t { bundles::kind_t kind; std::string plist_key; std::string grammar; std::string file_type; std::string kind_string; NSString* scope; NSString* view_controller; NSString* file; } item_infos[] =
	{
		{ bundles::kItemTypeBundle,       "description", "text.html.basic",                "tmBundle",       "bundle",         @"attr.bundle-editor.bundle",         @"BundleProperties",     @"Bundle"       },
		{ bundles::kItemTypeCommand,      "command",     NULL_STR,                         "tmCommand",      "command",        @"attr.bundle-editor.command",        @"CommandProperties",    @"Command"      },
		{ bundles::kItemTypeDragCommand,  "command",     NULL_STR,                         "tmDragCommand",  "dragCommand",    @"attr.bundle-editor.command.drop",   @"FileDropProperties",   @"Drag Command" },
		{ bundles::kItemTypeSnippet,      "content",     "text.tm-snippet",                "tmSnippet",      "snippet",        @"attr.bundle-editor.snippet",        @"SnippetProperties",    @"Snippet"      },
		{ bundles::kItemTypeSettings,     "settings",    "source.plist.textmate.settings", "tmPreferences",  "settings",       @"attr.bundle-editor.settings",       nil,                     @"Settings"     },
		{ bundles::kItemTypeGrammar,      NULL_STR,      "source.plist.textmate.grammar",  "tmLanguage",     "grammar",        @"attr.bundle-editor.grammar",        @"GrammarProperties",    @"Grammar"      },
		{ bundles::kItemTypeProxy,        "content",     "text.plain",                     "tmProxy",        "proxy",          @"attr.bundle-editor.proxy",          nil,                     @"Proxy"        },
		{ bundles::kItemTypeTheme,        NULL_STR,      "source.plist",                   "tmTheme",        "theme",          @"attr.bundle-editor.theme",          @"ThemeProperties",      @"Theme"        },
		{ bundles::kItemTypeMacro,        "commands",    "source.plist",                   "tmMacro",        "macro",          @"attr.bundle-editor.macro",          @"MacroProperties",      @"Macro"        },
	};

	item_info_t const& info_for (bundles::kind_t kind)
	{
		for(auto const& it : item_infos)
		{
			if(it.kind == kind)
				return it;
		}

		static item_info_t dummy;
		return dummy;
	}
}

static NSMutableArray* wrap_array (std::vector<std::string> const& array, NSString* key)
{
	NSMutableArray* res = [NSMutableArray array];
	for(auto const& str : array)
		[res addObject:[NSMutableDictionary dictionaryWithObject:[NSString stringWithCxxString:str] forKey:key]];
	return res;
}

static plist::array_t unwrap_array (NSArray* array, NSString* key)
{
	plist::array_t res;
	for(NSDictionary* dict in array)
		res.push_back(to_s([dict objectForKey:key]));
	return res;
}

namespace
{
	struct expand_visitor_t : boost::static_visitor<void>
	{
		expand_visitor_t (std::map<std::string, std::string> const& variables) : _variables(variables) { }

		void operator() (bool value) const                     { }
		void operator() (int32_t value) const                  { }
		void operator() (uint64_t value) const                 { }
		void operator() (oak::date_t const& value) const       { }
		void operator() (std::vector<char> const& value) const { }
		void operator() (std::string& str) const               { str = format_string::expand(str, _variables); }
		void operator() (plist::array_t& array) const          { for(auto& item : array) boost::apply_visitor(*this, item); }
		void operator() (plist::dictionary_t& dict) const      { for(auto& pair : dict)  boost::apply_visitor(*this, pair.second); }

	private:
		std::map<std::string, std::string> const& _variables;
	};
}

static be::entry_ptr parent_for_column (NSBrowser* aBrowser, NSInteger aColumn, be::entry_ptr entry)
{
	for(size_t col = 0; col < aColumn; ++col)
	{
		NSInteger row = [aBrowser selectedRowInColumn:col];
		if(row == -1)
		{
			fprintf(stderr, "*** abort\n");
			return be::entry_ptr();
		}
		entry = entry->children()[row];
	}
	return entry;
}

@implementation BundleEditor
+ (instancetype)sharedInstance
{
	static BundleEditor* sharedInstance = [self new];
	return sharedInstance;
}

- (id)init
{
	if(self = [super initWithWindowNibName:@"BundleEditor"])
	{
		D(DBF_BundleEditor, bug("\n"););

		struct callback_t : bundles::callback_t
		{
			callback_t (BundleEditor* self) : self(self) { }
			void bundles_did_change ()                   { [self didChangeBundleItems]; }
		private:
			BundleEditor* self;
		};

		static callback_t cb(self);
		bundles::add_callback(&cb);
	}
	return self;
}

- (void)windowDidLoad
{
	static struct { NSString* name; NSArray* array; } const converters[] =
	{
		{ @"OakSaveStringListTransformer",                  @[ @"nop", @"saveActiveFile", @"saveModifiedFiles" ] },
		{ @"OakInputStringListTransformer",                 @[ @"selection", @"document", @"scope", @"line", @"word", @"character", @"none" ] },
		{ @"OakInputFormatStringListTransformer",           @[ @"text", @"xml" ] },
		{ @"OakOutputLocationStringListTransformer",        @[ @"replaceInput", @"replaceDocument", @"atCaret", @"afterInput", @"newWindow", @"toolTip", @"discard", @"replaceSelection" ] },
		{ @"OakOutputFormatStringListTransformer",          @[ @"text", @"snippet", @"html", @"completionList" ] },
		{ @"OakOutputCaretStringListTransformer",           @[ @"afterOutput", @"selectOutput", @"interpolateByChar", @"interpolateByLine", @"heuristic" ] },
	};

	[OakRot13Transformer register];
	for(auto const& converter : converters)
		[OakStringListTransformer createTransformerWithName:converter.name andObjectsArray:converter.array];

	drawer = [[NSDrawer alloc] initWithContentSize:NSZeroSize preferredEdge:NSMaxXEdge];
	[drawer setParentWindow:[self window]];

	bundles = be::bundle_entries();

	[browser setDelegate:self];
	[browser loadColumnZero];
	[browser setAutohidesScroller:YES];

	documentView.textView.delegate = self;

	[[self window] makeFirstResponder:browser];
}

- (NSString*)scopeAttributes
{
	return bundleItem && bundleItemContent ? info_for(bundleItem->kind()).scope : nil;
}

- (void)didChangeBundleItems
{
	std::vector<std::string> selection;
	be::entry_ptr entry = bundles;
	for(NSInteger col = 0; col < [browser lastColumn]+1; ++col)
	{
		NSInteger row = [browser selectedRowInColumn:col];
		if(row == -1 || row >= entry->children().size())
			break;
		entry = entry->children()[row];
		selection.push_back(entry->identifier());
	}

	bundles = be::bundle_entries();
	[browser loadColumnZero];

	entry = bundles;
	for(size_t col = 0; col < selection.size(); ++col)
	{
		for(size_t row = 0; row < entry->children().size(); ++row)
		{
			if(selection[col] == entry->children()[row]->identifier())
			{
				[browser selectRow:row inColumn:col];
				entry = entry->children()[row];
				break;
			}
		}
	}
}

- (void)didChangeModifiedState
{
	[self setDocumentEdited:bundleItem && (changes.find(bundleItem) != changes.end() || propertiesChanged || bundleItemContent.isDocumentEdited)];
}

// ==================
// = Action Methods =
// ==================

- (void)createItemOfType:(bundles::kind_t)aType
{
	NSString* path = [[NSBundle bundleForClass:[self class]] pathForResource:info_for(aType).file ofType:@"plist"];
	if(!path || ![[NSFileManager defaultManager] fileExistsAtPath:path])
		return;

	NSInteger row = [browser selectedRowInColumn:0];
	bundles::item_ptr bundle = row != -1 ? bundles->children()[row]->represented_item() : bundles::item_ptr();
	if(aType == bundles::kItemTypeBundle || bundle)
	{
		std::map<std::string, std::string> environment = variables_for_path(oak::basic_environment());
		ABMutableMultiValue* value = [[[ABAddressBook sharedAddressBook] me] valueForProperty:kABEmailProperty];
		if(NSString* email = [value valueAtIndex:[value indexForIdentifier:[value primaryIdentifier]]])
			environment.emplace("TM_ROT13_EMAIL", decode::rot13(to_s(email)));

		auto item = std::make_shared<bundles::item_t>(oak::uuid_t().generate(), aType == bundles::kItemTypeBundle ? bundles::item_ptr() : bundle, aType);
		plist::dictionary_t plist = plist::load(to_s(path));
		expand_visitor_t visitor(environment);
		visitor(plist);
		plist[bundles::kFieldUUID] = to_s(item->uuid());
		if(plist.find(bundles::kFieldName) == plist.end())
			plist[bundles::kFieldName] = std::string("untitled");
		item->set_plist(plist);
		changes.emplace(item, plist);
		bundles::add_item(item);
		[self revealBundleItem:item];
		[self didChangeModifiedState];
	}
}

- (void)newDocument:(id)sender
{
	// kItemTypeMacro, kItemTypeMenu, kItemTypeMenuItemSeparator

	NSMenu* menu = [[NSMenu alloc] initWithTitle:@"Item Types"];
	for(auto const& it : item_infos)
	{
		static int const types = bundles::kItemTypeBundle|bundles::kItemTypeCommand|bundles::kItemTypeDragCommand|bundles::kItemTypeSnippet|bundles::kItemTypeSettings|bundles::kItemTypeGrammar|bundles::kItemTypeProxy|bundles::kItemTypeTheme;
		if((it.kind & types) == it.kind)
			[[menu addItemWithTitle:it.file action:NULL keyEquivalent:@""] setTag:it.kind];
	}

	NSAlert* alert = [NSAlert tmAlertWithMessageText:@"Create New Item" informativeText:@"Please choose what you want to create:" buttons:@"Create", @"Cancel", nil];
	NSPopUpButton* typeChooser = [[NSPopUpButton alloc] initWithFrame:NSZeroRect pullsDown:NO];
	[typeChooser setMenu:menu];
	[typeChooser sizeToFit];
	[alert setAccessoryView:typeChooser];
	[alert beginSheetModalForWindow:self.window completionHandler:^(NSInteger returnCode){
		if(returnCode == NSAlertFirstButtonReturn)
			[self createItemOfType:(bundles::kind_t)[[(NSPopUpButton*)[alert accessoryView] selectedItem] tag]];
	}];
	[[alert window] recalculateKeyViewLoop];
	[[alert window] makeFirstResponder:typeChooser];
}

- (void)delete:(id)sender
{
	if(bundleItem && bundleItem->move_to_trash())
	{
		OakPlayUISound(OakSoundDidTrashItemUISound);
		bundles::item_ptr trashedItem = bundleItem;

		bundles::item_ptr newSelectedItem;
		bool foundItem = false;
		for(auto const& entry : parent_for_column(browser, [browser selectedColumn], bundles)->children())
		{
			if(bundles::item_ptr item = entry->represented_item())
			{
				if(item->uuid() == bundleItem->uuid())
					foundItem = true;
				else if(item->kind() != bundles::kItemTypeMenu && item->kind() != bundles::kItemTypeMenuItemSeparator)
					newSelectedItem = item;

				if(foundItem && newSelectedItem)
					break;
			}
		}

		if(newSelectedItem)
			[self revealBundleItem:newSelectedItem];

		changes.erase(trashedItem);
		bundles::remove_item(trashedItem);
		[self didChangeModifiedState];

		if(!trashedItem->paths().empty())
		{
			std::string itemFolder = path::parent(trashedItem->paths().front());
			if(trashedItem->kind() == bundles::kItemTypeBundle && trashedItem->paths().size() == 1)
				itemFolder = path::parent(itemFolder);
			[[BundlesManager sharedInstance] reloadPath:[NSString stringWithCxxString:itemFolder]];
		}
	}
}

- (void)revealBundleItem:(bundles::item_ptr const&)anItem
{
	if(!anItem)
		return;

	[self showWindow:self];
	[self setBundleItem:anItem];

	if(anItem->paths().empty())
	{
		changes.emplace(anItem, anItem->plist());
		[self didChangeModifiedState];
	}

	std::vector<be::entry_ptr> const& allBundles = bundles->children();
	iterate(bundle, allBundles)
	{
		if((anItem->bundle() ?: anItem) != (*bundle)->represented_item())
			continue;

		[browser selectRow:(bundle - allBundles.begin()) inColumn:0];
		for(std::vector< std::pair<std::vector<be::entry_ptr>, int> > stack(1, std::make_pair((*bundle)->children(), -1)); !stack.empty(); stack.pop_back())
		{
			for(++stack.back().second; stack.back().second < stack.back().first.size(); ++stack.back().second)
			{
				be::entry_ptr entry = stack.back().first[stack.back().second];
				if(entry->has_children())
				{
					stack.emplace_back(entry->children(), -1);
				}
				else if(entry->represented_item() == anItem)
				{
					for(size_t j = 0; j < stack.size(); ++j)
						[browser selectRow:stack[j].second inColumn:j+1];
					return;
				}
			}
		}
	}
}

- (BOOL)commitEditing
{
	if(!bundleItem || !bundleItemContent)
		return YES;

	[_sharedPropertiesViewController commitEditing];
	[_extraPropertiesViewController commitEditing];

	if(!propertiesChanged && bundleItemContent.isDocumentEdited == NO)
		return YES;

	plist::dictionary_t plist = plist::convert((__bridge CFPropertyListRef)_bundleItemProperties);

	std::string const content = to_s(bundleItemContent.content);
	item_info_t const& info = info_for(bundleItem->kind());

	plist::any_t parsedContent;
	if(info.plist_key == NULL_STR || oak::contains(std::begin(PlistItemKinds), std::end(PlistItemKinds), info.kind))
	{
		bool success = false;
		parsedContent = plist::parse_ascii(content, &success);
		if(!success)
		{
			NSAlert* alert = [NSAlert tmAlertWithMessageText:@"Error Parsing Property List" informativeText:@"The property list is not valid.\n\nUnfortunately I am presently unable to point to where the parser failed." buttons:@"OK", nil];
			[alert beginSheetModalForWindow:self.window completionHandler:^(NSInteger returnCode){ }];
			return NO;
		}
	}

	if(info.plist_key == NULL_STR)
	{
		if(plist::dictionary_t const* plistSubset = boost::get<plist::dictionary_t>(&parsedContent))
		{
			std::vector<std::string> keys;
			if(info.kind == bundles::kItemTypeGrammar)
				keys = { "comment", "patterns", "repository", "injections" };
			else if(info.kind == bundles::kItemTypeTheme)
				keys = { "gutterSettings", "settings", "colorSpaceName" };

			for(auto const& key : keys)
			{
				if(plistSubset->find(key) != plistSubset->end())
						plist[key] = plistSubset->find(key)->second;
				else	plist.erase(key);
			}
		}
	}
	else
	{
		if(oak::contains(std::begin(PlistItemKinds), std::end(PlistItemKinds), info.kind))
				plist[info.plist_key] = parsedContent;
		else	plist[info.plist_key] = content;
	}

	switch(info.kind)
	{
		case bundles::kItemTypeGrammar:
			plist[bundles::kFieldGrammarExtension] = unwrap_array([_bundleItemProperties objectForKey:[NSString stringWithCxxString:bundles::kFieldGrammarExtension]], @"extension");
		break;

		case bundles::kItemTypeDragCommand:
			plist[bundles::kFieldDropExtension] = unwrap_array([_bundleItemProperties objectForKey:[NSString stringWithCxxString:bundles::kFieldDropExtension]], @"extension");
		break;
	}

	if(plist::equal(plist, bundleItem->plist()))
			changes.erase(bundleItem);
	else	changes[bundleItem] = plist;

	propertiesChanged = NO;
	[bundleItemContent markDocumentSaved];

	[self didChangeModifiedState];
	return YES;
}

- (void)saveDocument:(id)sender
{
	[self commitEditing];
	std::map<bundles::item_ptr, plist::dictionary_t> failedToSave;
	for(auto const& pair : changes)
	{
		auto item = pair.first;

		item->set_plist(pair.second);
		if(item->save())
		{
			[[BundlesManager sharedInstance] reloadPath:[NSString stringWithCxxString:item->paths().front()]];
		}
		else
		{
			failedToSave.insert(pair);
		}
	}
	changes.swap(failedToSave);

	if(!changes.empty())
	{
		NSAlert* alert = [NSAlert tmAlertWithMessageText:@"Error Saving Bundle Item" informativeText:@"Sorry, but something went wrong while trying to save your changes. More info may be available via the console." buttons:@"OK", nil];
		[alert beginSheetModalForWindow:self.window completionHandler:^(NSInteger returnCode){
			if(returnCode == NSAlertSecondButtonReturn) // Discard Changes
			{
				changes.clear();
				[self didChangeModifiedState];
			}
		}];
	}

	[self didChangeModifiedState];
}

// =====================
// = NSBrowserDelegate =
// =====================

- (NSInteger)browser:(NSBrowser*)aBrowser numberOfRowsInColumn:(NSInteger)aColumn
{
	be::entry_ptr entry = parent_for_column(aBrowser, aColumn, bundles);
	return entry && entry->has_children() ? entry->children().size() : 0;
}

- (void)browser:(NSBrowser*)aBrowser willDisplayCell:(id)aCell atRow:(NSInteger)aRow column:(NSInteger)aColumn
{
	if(NSBrowserCell* cell = [aCell isKindOfClass:[NSBrowserCell class]] ? aCell : nil)
	{
		static NSMutableParagraphStyle* paragraphStyle = nil;
		if(!paragraphStyle)
		{
			paragraphStyle = [[NSMutableParagraphStyle alloc] init];
			[paragraphStyle setLineBreakMode:NSLineBreakByTruncatingTail];
		}

		be::entry_ptr entry = parent_for_column(aBrowser, aColumn, bundles)->children()[aRow];

		NSDictionary* attrs = @{
			NSForegroundColorAttributeName : entry->disabled() ? [NSColor grayColor] : [NSColor blackColor],
			NSParagraphStyleAttributeName  : paragraphStyle
		};
		[cell setAttributedStringValue:[[NSAttributedString alloc] initWithString:[NSString stringWithCxxString:entry->name()] attributes:attrs]];
		[cell setLeaf:!entry->has_children()];
		[cell setLoaded:YES];

		NSMenu* menu = [NSMenu new];
		if(bundles::item_ptr item = entry->represented_item())
		{
			if(entry->identifier() == "Menu Actions")
			{
				[cell setImage:[NSImage imageNamed:@"MenuItem" inSameBundleAsClass:[self class]]];
				return;
			}
			[cell setImage:[NSImage imageNamed:info_for(item->kind()).file inSameBundleAsClass:[self class]]];

			if(item->kind() == bundles::kItemTypeBundle)
			{
				NSMenuItem* menuItem = [menu addItemWithTitle:@"Export Bundle…" action:@selector(exportBundle:) keyEquivalent:@""];
				menuItem.target = self;
				menuItem.representedObject = [NSString stringWithCxxString:item->uuid()];
			}

			auto paths = item->paths();
			if(paths.size() == 1)
			{
				[menu addItem:[self createMenuItemForCxxPath:paths.front()]];
			}
			else if(paths.size() > 1)
			{
				NSMenu* submenu = [NSMenu new];
				for(std::string const& path : paths)
				{
					NSMenuItem* item = [self createMenuItemForCxxPath:path];
					item.title = [[NSString stringWithCxxString:path] stringByAbbreviatingWithTildeInPath];
					[submenu addItem:item];
				}

				NSMenuItem* submenuItem = [menu addItemWithTitle:@"Show in Finder" action:nil keyEquivalent:@""];
				submenuItem.submenu = submenu;
			}

			NSMenuItem* menuItem = [menu addItemWithTitle:@"Copy UUID" action:@selector(copyUUID:) keyEquivalent:@""];
			menuItem.target = self;
			menuItem.representedObject = [NSString stringWithCxxString:item->uuid()];
		}
		else
		{
			std::string const& path = entry->represented_path();
			if(path != NULL_STR)
			{
				[cell setImage:[OakFileIconImage fileIconImageWithPath:[NSString stringWithCxxString:path] size:NSMakeSize(16, 16)]];
				[menu addItem:[self createMenuItemForCxxPath:path]];
			}
		}
		[cell setMenu:menu];
	}
}

- (NSMenuItem*)createMenuItemForCxxPath:(std::string const&)path
{
	NSMenuItem* item = [[NSMenuItem alloc] initWithTitle:[NSString stringWithFormat:@"Show “%@” in Finder", [NSString stringWithCxxString:path::display_name(path)]] action:@selector(showInFinder:) keyEquivalent:@""];
	item.target = self;
	item.representedObject = [NSString stringWithCxxString:path];
	return item;
}

- (void)copyUUID:(NSMenuItem*)sender
{
	[[NSPasteboard generalPasteboard] declareTypes:@[ NSStringPboardType ] owner:nil];
	[[NSPasteboard generalPasteboard] setString:[sender representedObject] forType:NSStringPboardType];
}

- (void)exportBundle:(id)sender
{
	oak::uuid_t const uuid = to_s((NSString*)[sender representedObject]);
	if(bundles::item_ptr bundle = bundles::lookup(uuid))
	{
		std::string name = bundle->name();
		std::replace(name.begin(), name.end(), '/', ':');
		std::replace(name.begin(), name.end(), '.', '_');

		NSSavePanel* savePanel = [NSSavePanel savePanel];
		[savePanel setNameFieldStringValue:[NSString stringWithCxxString:name + ".tmbundle"]];
		[savePanel beginSheetModalForWindow:self.window completionHandler:^(NSInteger result) {
			if(result == NSFileHandlingPanelOKButton)
			{
				NSString* path = [[savePanel.URL filePathURL] path];
				if([[NSFileManager defaultManager] fileExistsAtPath:path])
				{
					NSError* error;
					if(![[NSFileManager defaultManager] removeItemAtPath:path error:&error])
					{
						[self.window presentError:error];
						return;
					}
				}

				std::string const dest = to_s(path);
				bool res = true;
				for(auto const& item : bundles::query(bundles::kFieldAny, NULL_STR, scope::wildcard, ~(bundles::kItemTypeMenu|bundles::kItemTypeMenuItemSeparator), uuid, false, true, false))
					res = res && item->save_to(dest);

				if(!res)
				{
					NSAlert* alert        = [[NSAlert alloc] init];
					alert.messageText     = @"Failed to Save Bundle";
					alert.informativeText = [NSString stringWithFormat:@"Unknown error while saving bundle as “%@”.", [path stringByAbbreviatingWithTildeInPath]];
					[alert addButtonWithTitle:@"OK"];
					[alert runModal];
				}
			}
		}];
	}
}

- (void)showInFinder:(id)sender
{
	if(![sender respondsToSelector:@selector(representedObject)])
		return;
	if(NSString* path = [sender representedObject])
		[[NSWorkspace sharedWorkspace] activateFileViewerSelectingURLs:@[ [NSURL fileURLWithPath:path] ]];
}

// ====================
// = NSBrowser Target =
// ====================

- (IBAction)browserSelectionDidChange:(id)sender
{
	NSInteger aColumn = [browser selectedColumn];
	NSInteger aRow    = aColumn != -1 ? [browser selectedRowInColumn:aColumn] : -1;
	if(aColumn != -1 && aRow != -1)
	{
		if(bundles::item_ptr item = parent_for_column(browser, aColumn, bundles)->children()[aRow]->represented_item())
		{
			if(item->kind() != bundles::kItemTypeMenu && item->kind() != bundles::kItemTypeMenuItemSeparator)
				[self setBundleItem:item];
		}
	}
}

// =======================
// = Setting Bundle Item =
// =======================

- (void)observeValueForKeyPath:(NSString*)aKeyPath ofObject:(id)anObject change:(NSDictionary*)someChange context:(void*)context
{
	if(![aKeyPath isEqualToString:@"documentEdited"])
		propertiesChanged = YES;
	[self didChangeModifiedState];
}

- (void)setBundleItemProperties:(NSMutableDictionary*)someProperties
{
	static std::string const BindingKeys[] = { bundles::kFieldIsDisabled, bundles::kFieldName, bundles::kFieldKeyEquivalent, bundles::kFieldTabTrigger, bundles::kFieldScopeSelector, bundles::kFieldSemanticClass, bundles::kFieldContentMatch, bundles::kFieldHideFromUser, bundles::kFieldDropExtension, bundles::kFieldGrammarExtension, bundles::kFieldGrammarFirstLineMatch, bundles::kFieldGrammarScope, bundles::kFieldGrammarInjectionSelector, "beforeRunningCommand", "input", "inputFormat", "outputLocation", "outputFormat", "outputCaret", "autoScrollOutput", "contactName", "contactEmailRot13", "description", "disableAutoIndent", "useGlobalClipboard", "author", "comment" };

	NSMutableDictionary* oldProperties = _bundleItemProperties;
	_bundleItemProperties = someProperties;
	for(auto const& str : BindingKeys)
	{
		NSString* key = [NSString stringWithCxxString:str];
		[oldProperties removeObserver:self forKeyPath:key];
		[someProperties addObserver:self forKeyPath:key options:0 context:NULL];
	}
	propertiesChanged = NO;
}

static NSMutableDictionary* DictionaryForBundleItem (bundles::item_ptr const& aBundleItem)
{
	NSMutableDictionary* res = ns::to_mutable_dictionary(aBundleItem->plist());
	switch(info_for(aBundleItem->kind()).kind)
	{
		case bundles::kItemTypeCommand:
		{
			bundle_command_t cmd = parse_command(aBundleItem);

			struct { NSString* key; int index; NSArray* array; } const popups[] =
			{
				{ @"beforeRunningCommand",  cmd.pre_exec,      @[ @"nop", @"saveActiveFile", @"saveModifiedFiles" ] },
				{ @"input",                 cmd.input,         @[ @"selection", @"document", @"scope", @"line", @"word", @"character", @"none" ] },
				{ @"inputFormat",           cmd.input_format,  @[ @"text", @"xml" ] },
				{ @"outputLocation",        cmd.output,        @[ @"replaceInput", @"replaceDocument", @"atCaret", @"afterInput", @"newWindow", @"toolTip", @"discard", @"replaceSelection" ] },
				{ @"outputFormat",          cmd.output_format, @[ @"text", @"snippet", @"html", @"completionList" ] },
				{ @"outputCaret",           cmd.output_caret,  @[ @"afterOutput", @"selectOutput", @"interpolateByChar", @"interpolateByLine", @"heuristic" ] },
			};

			[res removeObjectForKey:@"output"];
			[res removeObjectForKey:@"dontFollowNewOutput"];
			[res setObject:@2 forKey:@"version"];
			if(cmd.auto_scroll_output)
				[res setObject:@YES forKey:@"autoScrollOutput"];
			for(auto const& popup : popups)
				[res setObject:[popup.array objectAtIndex:popup.index] forKey:popup.key];
		}
		break;

		case bundles::kItemTypeGrammar:
		{
			[res setObject:wrap_array(aBundleItem->values_for_field(bundles::kFieldGrammarExtension), @"extension") forKey:[NSString stringWithCxxString:bundles::kFieldGrammarExtension]];
		}
		break;

		case bundles::kItemTypeDragCommand:
		{
			[res setObject:wrap_array(aBundleItem->values_for_field(bundles::kFieldDropExtension), @"extension") forKey:[NSString stringWithCxxString:bundles::kFieldDropExtension]];
		}
		break;
	}
	return res;
}

static NSMutableDictionary* DictionaryForPropertyList (plist::dictionary_t const& plist, bundles::item_ptr const& aBundleItem)
{
	NSMutableDictionary* res = ns::to_mutable_dictionary(plist);
	switch(info_for(aBundleItem->kind()).kind)
	{
		case bundles::kItemTypeGrammar:
			[res setObject:wrap_array(aBundleItem->values_for_field(bundles::kFieldGrammarExtension), @"extension") forKey:[NSString stringWithCxxString:bundles::kFieldGrammarExtension]];
		break;

		case bundles::kItemTypeDragCommand:
			[res setObject:wrap_array(aBundleItem->values_for_field(bundles::kFieldDropExtension), @"extension") forKey:[NSString stringWithCxxString:bundles::kFieldDropExtension]];
		break;
	}
	return res;
}

- (bundles::item_ptr const&)bundleItem
{
	return bundleItem;
}

- (BOOL)window:(NSWindow*)aWindow shouldDragDocumentWithEvent:(NSEvent*)anEvent from:(NSPoint)dragImageLocation withPasteboard:(NSPasteboard*)aPasteboard
{
	return bundleItem && bundleItem->paths().size() == 1;
}

- (BOOL)window:(NSWindow*)aWindow shouldPopUpDocumentPathMenu:(NSMenu*)menu
{
	if(!bundleItem)
		return NO;

	auto const& paths = bundleItem->paths();
	if(paths.size() == 1)
		return YES;

	[menu removeAllItems];
	for(std::string const& path : paths)
	{
		NSMenuItem* item = [self createMenuItemForCxxPath:path];
		item.title = [[NSString stringWithCxxString:path] stringByAbbreviatingWithTildeInPath];
		item.state = NSOffState;
		[menu addItem:item];
	}
	return YES;
}

- (void)setBundleItem:(bundles::item_ptr const&)aBundleItem
{
	if(bundleItem == aBundleItem)
		return;

	[self commitEditing];

	if(bundleItemContent)
		[bundleItemContent removeObserver:self forKeyPath:@"documentEdited"];

	bundleItem        = aBundleItem;
	bundleItemContent = nil;

	std::map<bundles::item_ptr, plist::dictionary_t>::const_iterator it = changes.find(bundleItem);
	self.bundleItemProperties = it != changes.end() ? DictionaryForPropertyList(it->second, bundleItem) : DictionaryForBundleItem(bundleItem);

	item_info_t const& info = info_for(bundleItem->kind());

	[[self window] setTitle:[NSString stringWithCxxString:bundleItem->name_with_bundle()]];
	NSString* bundleItemTitle = [NSString stringWithCxxString:bundleItem->name()];

	auto const& paths = bundleItem->paths();
	if(paths.size() == 1)
	{
		self.window.representedURL = [NSURL fileURLWithPath:[NSString stringWithCxxString:paths.front()]];
	}
	else
	{
		self.window.representedFilename = NSHomeDirectory();
		[self.window standardWindowButton:NSWindowDocumentIconButton].image = [[NSWorkspace sharedWorkspace] iconForFileType:[NSString stringWithCxxString:info.file_type]];
	}

	plist::dictionary_t const& plist = it != changes.end() ? it->second : bundleItem->plist();
	if(info.plist_key == NULL_STR)
	{
		std::vector<std::string> keys;
		if(info.kind == bundles::kItemTypeGrammar)
			keys = { "comment", "patterns", "repository", "injections" };
		else if(info.kind == bundles::kItemTypeTheme)
			keys = { "gutterSettings", "settings", "colorSpaceName" };

		plist::dictionary_t plistSubset;
		for(auto const& key : keys)
		{
			if(plist.find(key) != plist.end())
				plistSubset[key] = plist.find(key)->second;
		}
		bundleItemContent = [OakDocument documentWithString:to_ns(to_s(plistSubset, plist::kPreferSingleQuotedStrings, PlistKeySortOrder())) fileType:to_ns(info.grammar) customName:bundleItemTitle];
	}
	else if(oak::contains(std::begin(PlistItemKinds), std::end(PlistItemKinds), info.kind))
	{
		if(plist.find(info.plist_key) != plist.end())
			bundleItemContent = [OakDocument documentWithString:to_ns(to_s(plist.find(info.plist_key)->second, plist::kPreferSingleQuotedStrings, PlistKeySortOrder())) fileType:to_ns(info.grammar) customName:bundleItemTitle];
	}
	else
	{
		std::string str;
		if(plist::get_key_path(plist, info.plist_key, str))
		{
			if(info.kind == bundles::kItemTypeCommand || info.kind == bundles::kItemTypeDragCommand)
				command::fix_shebang(&str);
			bundleItemContent = [OakDocument documentWithString:to_ns(str) fileType:to_ns(info.grammar) customName:bundleItemTitle];
		}
	}

	bundleItemContent = bundleItemContent ?: [OakDocument documentWithString:@"" fileType:nil customName:bundleItemTitle];
	documentView.document = bundleItemContent;
	[bundleItemContent addObserver:self forKeyPath:@"documentEdited" options:0 context:nullptr];

	_sharedPropertiesViewController = [[PropertiesViewController alloc] initWithName:@"SharedProperties"];
	_extraPropertiesViewController  = nil;
	[_sharedPropertiesViewController setProperties:_bundleItemProperties];

	if(info.kind == bundles::kItemTypeBundle)
		_sharedPropertiesViewController = nil;

	NSView* sharedView = [_sharedPropertiesViewController view];
	if(info.view_controller)
	{
		_extraPropertiesViewController = [[PropertiesViewController alloc] initWithName:info.view_controller];
		[_extraPropertiesViewController setProperties:_bundleItemProperties];
		NSView* extraView = [_extraPropertiesViewController view];

		CGFloat delta = _extraPropertiesViewController.indent - _sharedPropertiesViewController.indent;
		if(delta > 0)
				[sharedView setFrame:NSOffsetRect(sharedView.frame, delta, 0)];
		else	[extraView setFrame:NSOffsetRect(extraView.frame, -delta, 0)];

		NSView* contentView = [[NSView alloc] initWithFrame:NSMakeRect(0, 0, std::max(NSMaxX(sharedView.frame) + 1, NSMaxX(extraView.frame) + 1), NSHeight(sharedView.frame) + NSHeight(extraView.frame))];
		[sharedView setAutoresizingMask:NSViewMinXMargin|NSViewMinYMargin];
		[extraView setAutoresizingMask:NSViewMinXMargin|NSViewMinYMargin];

		[sharedView setFrame:NSOffsetRect(sharedView.frame, 0, NSHeight(extraView.frame))];
		[contentView addSubview:sharedView];
		[contentView addSubview:extraView];

		[drawer setContentView:[[NSView alloc] initWithFrame:NSZeroRect]];
		[drawer setContentSize:contentView.frame.size];
		[drawer setContentView:contentView];
		[drawer open:self];
	}
	else
	{
		[sharedView setAutoresizingMask:NSViewWidthSizable|NSViewHeightSizable];
		[drawer setContentSize:sharedView.frame.size];
		[drawer setContentView:sharedView];
		[drawer open:self];
	}
	[[[drawer contentView] window] recalculateKeyViewLoop];
}

static NSString* DescriptionForChanges (std::map<bundles::item_ptr, plist::dictionary_t> const& changes)
{
	NSString* res = [NSString stringWithCxxString:text::format("Do you want to save the changes made to %zu items?", changes.size())];
	if(changes.size() == 1)
	{
		bundles::item_ptr item = changes.begin()->first;
		NSString* name = [NSString stringWithCxxString:item->name()];
		if(item->kind() == bundles::kItemTypeBundle)
		{
			res = [NSString stringWithFormat:@"Do you want to save the changes made to the bundle named “%@”?", name];
		}
		else
		{
			NSString* bundleName = [NSString stringWithCxxString:item->bundle()->name()];
			NSString* type = [info_for(item->kind()).file lowercaseString];
			res = [NSString stringWithFormat:@"Do you want to save the changes made to the %@ item named “%@” in the “%@” bundle?", type, name, bundleName];
		}
	}
	return res;
}

- (BOOL)windowShouldClose:(id)sender
{
	[self commitEditing];
	if(changes.empty())
		return YES;

	NSAlert* alert = [[NSAlert alloc] init];
	[alert setAlertStyle:NSAlertStyleWarning];
	[alert setMessageText:DescriptionForChanges(changes)];
	[alert setInformativeText:@"Your changes will be lost if you don’t save them."];
	[alert addButtons:@"Save", @"Cancel", @"Don’t Save", nil];
	[alert beginSheetModalForWindow:self.window completionHandler:^(NSInteger returnCode){
		if(returnCode != NSAlertSecondButtonReturn) // Not "Cancel"
		{
			if(returnCode == NSAlertFirstButtonReturn) // "Save"
				[self saveDocument:self];
			else if(returnCode == NSAlertThirdButtonReturn) // "Don’t Save"
				changes.clear();
			[self close];
		}
	}];
	return NO;
}

// ====================
// = Running Commands =
// ====================

- (void)updateEnvironment:(std::map<std::string, std::string>&)res forCommand:(OakCommand*)aCommand
{
	[documentView.textView updateEnvironment:res];
}
@end
