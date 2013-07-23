#import "BundleEditor.h"
#import "PropertiesViewController.h"
#import "OakRot13Transformer.h"
#import "be_entry.h"
#import <OakFoundation/NSString Additions.h>
#import <OakFoundation/OakStringListTransformer.h>
#import <OakAppKit/NSAlert Additions.h>
#import <OakAppKit/NSImage Additions.h>
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/OakSound.h>
#import <OakAppKit/OakFileIconImage.h>
#import <OakTextView/OakDocumentView.h>
#import <BundlesManager/BundlesManager.h>
#import <command/runner.h> // fix_shebang
#import <plist/ascii.h>
#import <plist/delta.h>
#import <text/decode.h>
#import <cf/cf.h>
#import <ns/ns.h>
#import <oak/debug.h>

OAK_DEBUG_VAR(BundleEditor);

static BundleEditor* SharedInstance;

@interface BundleEditor ()
- (void)didChangeBundleItems;
- (void)didChangeModifiedState;
@property (nonatomic, retain) PropertiesViewController* sharedPropertiesViewController;
@property (nonatomic, retain) PropertiesViewController* extraPropertiesViewController;
@property (nonatomic, retain) NSMutableDictionary* bundleItemProperties;
- (bundles::item_ptr const&)bundleItem;
- (void)setBundleItem:(bundles::item_ptr const&)aBundleItem;
@end

@interface NSBrowser (SnowLeopard)
- (void)setAutohidesScroller:(BOOL)flag;
@end

namespace
{
	static bundles::kind_t const PlistItemKinds[] = { bundles::kItemTypeSettings, bundles::kItemTypeMacro, bundles::kItemTypeTheme };

	static std::vector<std::string> const& PlistKeySortOrder ()
	{
		static auto const res = new std::vector<std::string>{ "shellVariables", "disabled", "name", "value", "comment", "match", "begin", "while", "end", "applyEndPatternLast", "captures", "beginCaptures", "whileCaptures", "endCaptures", "contentName", "injections", "patterns", "repository", "include", "increaseIndentPattern", "decreaseIndentPattern", "indentNextLinePattern", "unIndentedLinePattern", "disableIndentCorrections", "indentOnPaste", "indentedSoftWrap", "format", "foldingStartMarker", "foldingStopMarker", "foldingIndentedBlockStart", "foldingIndentedBlockIgnore", "characterClass", "smartTypingPairs", "highlightPairs", "showInSymbolList", "symbolTransformation", "disableDefaultCompletion", "completions", "completionCommand", "spellChecking", "softWrap", "fontName", "fontStyle", "fontSize", "foreground", "background", "bold", "caret", "invisibles", "italic", "misspelled", "selection", "underline" };
		return *res;
	}

	static struct item_info_t { bundles::kind_t kind; std::string plist_key; std::string grammar; std::string file_type; std::string kind_string; NSString* view_controller; NSString* file; } item_infos[] =
	{
		{ bundles::kItemTypeBundle,       "description", "text.html.basic",                "tmBundle",       "bundle",         @"BundleProperties",     @"Bundle"       },
		{ bundles::kItemTypeCommand,      "command",     NULL_STR,                         "tmCommand",      "command",        @"CommandProperties",    @"Command"      },
		{ bundles::kItemTypeDragCommand,  "command",     NULL_STR,                         "tmDragCommand",  "dragCommand",    @"FileDropProperties",   @"Drag Command" },
		{ bundles::kItemTypeSnippet,      "content",     "text.tm-snippet",                "tmSnippet",      "snippet",        @"SnippetProperties",    @"Snippet"      },
		{ bundles::kItemTypeSettings,     "settings",    "source.plist.textmate.settings", "tmPreferences",  "settings",       nil,                     @"Settings"     },
		{ bundles::kItemTypeGrammar,      NULL_STR,      "source.plist.textmate.grammar",  "tmLanguage",     "grammar",        @"GrammarProperties",    @"Grammar"      },
		{ bundles::kItemTypeProxy,        "content",     "text.plain",                     "tmProxy",        "proxy",          nil,                     @"Proxy"        },
		{ bundles::kItemTypeTheme,        NULL_STR,      "source.plist",                   "tmTheme",        "theme",          @"ThemeProperties",      @"Theme"        },
		{ bundles::kItemTypeMacro,        "commands",    "source.plist",                   "tmMacro",        "macro",          @"MacroProperties",      @"Macro"        },
	};

	item_info_t const& info_for (bundles::kind_t kind)
	{
		iterate(it, item_infos)
		{
			if(it->kind == kind)
				return *it;
		}

		static item_info_t dummy;
		return dummy;
	}
}

static NSMutableArray* wrap_array (std::vector<std::string> const& array, NSString* key)
{
	NSMutableArray* res = [NSMutableArray array];
	iterate(str, array)
		[res addObject:[NSMutableDictionary dictionaryWithObject:[NSString stringWithCxxString:*str] forKey:key]];
	return res;
}

static plist::array_t unwrap_array (NSArray* array, NSString* key)
{
	plist::array_t res;
	for(NSDictionary* dict in array)
		res.push_back(to_s((NSString*)[dict objectForKey:key]));
	return res;
}

namespace
{
	struct expand_visitor_t : boost::static_visitor<void>
	{
		expand_visitor_t (std::map<std::string, std::string> const& variables) : _variables(variables) { }

		template <typename T>
		void operator() (T value) const                   { }
		void operator() (std::string& str) const          { str = format_string::expand(str, _variables); }
		void operator() (plist::array_t& array) const     { iterate(it, array) boost::apply_visitor(*this, *it); }
		void operator() (plist::dictionary_t& dict) const { iterate(pair, dict) boost::apply_visitor(*this, pair->second); }

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
@synthesize sharedPropertiesViewController, extraPropertiesViewController, bundleItemProperties;

+ (BundleEditor*)sharedInstance
{
	return SharedInstance ?: [self new];
}

- (id)init
{
	if(SharedInstance)
	{
	}
	else if(self = SharedInstance = [super initWithWindowNibName:@"BundleEditor"])
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

		struct document_callback_t : document::document_t::callback_t
		{
			document_callback_t (BundleEditor* self) : _self(self) { }

			void handle_document_event (document::document_ptr document, event_t event)
			{
				switch(event)
				{
					case did_change_modified_status:
						[_self didChangeModifiedState];
					break;
				}
			}

		private:
			BundleEditor* _self;
		};

		documentCallback = new document_callback_t(self);
	}
	return SharedInstance;
}

- (void)dealloc
{
	if(bundleItemContent)
		bundleItemContent->remove_callback(documentCallback);
	delete documentCallback;
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
	for(size_t i = 0; i != sizeofA(converters); ++i)
		[OakStringListTransformer createTransformerWithName:converters[i].name andObjectsArray:converters[i].array];

	drawer = [[NSDrawer alloc] initWithContentSize:NSZeroSize preferredEdge:NSMaxXEdge];
	[drawer setParentWindow:[self window]];

	bundles = be::bundle_entries();

	[browser setDelegate:self];
	[browser loadColumnZero];
	[browser setAutohidesScroller:YES];

	[[self window] makeFirstResponder:browser];
}

- (void)didChangeBundleItems
{
	std::vector<std::string> selection;
	be::entry_ptr entry = bundles;
	for(NSInteger col = 0; col < [browser lastColumn]+1; ++col)
	{
		NSInteger row = [browser selectedRowInColumn:col];
		if(row == -1)
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
	[self setDocumentEdited:bundleItem && (changes.find(bundleItem) != changes.end() || propertiesChanged || bundleItemContent->is_modified())];
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
			environment.insert(std::make_pair("TM_ROT13_EMAIL", decode::rot13(to_s(email))));

		bundles::item_ptr item(new bundles::item_t(oak::uuid_t().generate(), aType == bundles::kItemTypeBundle ? bundles::item_ptr() : bundle, aType));
		plist::dictionary_t plist = plist::load(to_s(path));
		expand_visitor_t visitor(environment);
		visitor(plist);
		plist[bundles::kFieldUUID] = to_s(item->uuid());
		if(plist.find(bundles::kFieldName) == plist.end())
			plist[bundles::kFieldName] = std::string("untitled");
		item->set_plist(plist);
		changes.insert(std::make_pair(item, plist));
		bundles::add_item(item);
		[self revealBundleItem:item];
		[self didChangeModifiedState];
	}
}

- (void)newDocument:(id)sender
{
	// kItemTypeMacro, kItemTypeMenu, kItemTypeMenuItemSeparator

	NSMenu* menu = [[NSMenu alloc] initWithTitle:@"Item Types"];
	iterate(it, item_infos)
	{
		static int const types = bundles::kItemTypeBundle|bundles::kItemTypeCommand|bundles::kItemTypeDragCommand|bundles::kItemTypeSnippet|bundles::kItemTypeSettings|bundles::kItemTypeGrammar|bundles::kItemTypeProxy|bundles::kItemTypeTheme;
		if((it->kind & types) == it->kind)
			[[menu addItemWithTitle:it->file action:NULL keyEquivalent:@""] setTag:it->kind];
	}

	NSAlert* alert = [NSAlert alertWithMessageText:@"Create New Item" defaultButton:@"Create" alternateButton:@"Cancel" otherButton:nil informativeTextWithFormat:@"Please choose what you want to create:"];
	NSPopUpButton* typeChooser = [[NSPopUpButton alloc] initWithFrame:NSZeroRect pullsDown:NO];
	[typeChooser setMenu:menu];
	[typeChooser sizeToFit];
	[alert setAccessoryView:typeChooser];
	OakShowAlertForWindow(alert, self.window, ^(NSInteger returnCode){
		if(returnCode == NSAlertDefaultReturn)
			[self createItemOfType:(bundles::kind_t)[[(NSPopUpButton*)[alert accessoryView] selectedItem] tag]];
	});
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
		iterate(entry, parent_for_column(browser, [browser selectedColumn], bundles)->children())
		{
			if(bundles::item_ptr item = (*entry)->represented_item())
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

		NSLog(@"%s deleting ‘%@’…", sel_getName(_cmd), [NSString stringWithCxxString:trashedItem->full_name()]);
		if(!trashedItem->paths().empty())
		{
			std::string itemFolder = path::parent(trashedItem->paths().front());
			if(trashedItem->kind() == bundles::kItemTypeBundle && trashedItem->paths().size() == 1)
				itemFolder = path::parent(itemFolder);
			NSLog(@"%s rescan ‘%@’", sel_getName(_cmd), [NSString stringWithCxxString:itemFolder]);
			[[BundlesManager sharedInstance] reloadPath:[NSString stringWithCxxString:itemFolder]];
		}
	}
}

- (void)revealBundleItem:(bundles::item_ptr const&)anItem
{
	[self showWindow:self];
	[self setBundleItem:anItem];

	if(anItem->paths().empty())
	{
		changes.insert(std::make_pair(anItem, anItem->plist()));
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
					stack.push_back(std::make_pair(entry->children(), -1));
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

	[sharedPropertiesViewController commitEditing];
	[extraPropertiesViewController commitEditing];

	if(!propertiesChanged && !bundleItemContent->is_modified())
		return YES;

	plist::dictionary_t plist = plist::convert((__bridge CFPropertyListRef)bundleItemProperties);

	std::string const& content = bundleItemContent->content();
	item_info_t const& info = info_for(bundleItem->kind());

	plist::any_t parsedContent;
	if(info.plist_key == NULL_STR || oak::contains(std::begin(PlistItemKinds), std::end(PlistItemKinds), info.kind))
	{
		bool success = false;
		parsedContent = plist::parse_ascii(content, &success);
		if(!success)
		{
			NSAlert* alert = [NSAlert alertWithMessageText:@"Error Parsing Property List" defaultButton:@"OK" alternateButton:nil otherButton:nil informativeTextWithFormat:@"The property list is not valid.\n\nUnfortunately I am presently unable to point to where the parser failed."];
			OakShowAlertForWindow(alert, self.window, ^(NSInteger returnCode){ });
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

			iterate(key, keys)
			{
				if(plistSubset->find(*key) != plistSubset->end())
						plist[*key] = plistSubset->find(*key)->second;
				else	plist.erase(*key);
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
			plist[bundles::kFieldGrammarExtension] = unwrap_array([bundleItemProperties objectForKey:[NSString stringWithCxxString:bundles::kFieldGrammarExtension]], @"extension");
		break;

		case bundles::kItemTypeDragCommand:
			plist[bundles::kFieldDropExtension] = unwrap_array([bundleItemProperties objectForKey:[NSString stringWithCxxString:bundles::kFieldDropExtension]], @"extension");;
		break;
	}

	if(plist::equal(plist, bundleItem->plist()))
			changes.erase(bundleItem);
	else	changes[bundleItem] = plist;

	propertiesChanged = NO;
	bundleItemContent->set_disk_revision(bundleItemContent->revision());

	[self didChangeModifiedState];
	return YES;
}

- (void)saveDocument:(id)sender
{
	[self commitEditing];
	std::map<bundles::item_ptr, plist::dictionary_t> failedToSave;
	iterate(pair, changes)
	{
		auto item = pair->first;
		bool rescanParentFolder = item->kind() == bundles::kItemTypeBundle && (!item->local() || item->paths().empty());

		item->set_plist(pair->second);
		NSLog(@"%s saving ‘%@’…", sel_getName(_cmd), [NSString stringWithCxxString:item->full_name()]);
		if(item->save())
		{
			std::string itemFolder = path::parent(item->paths().front());
			NSLog(@"%s rescan ‘%@’", sel_getName(_cmd), [NSString stringWithCxxString:itemFolder]);
			[[BundlesManager sharedInstance] reloadPath:[NSString stringWithCxxString:itemFolder]];
			if(rescanParentFolder)
			{
				NSLog(@"%s rescan ‘%@’", sel_getName(_cmd), [NSString stringWithCxxString:path::parent(itemFolder)]);
				[[BundlesManager sharedInstance] reloadPath:[NSString stringWithCxxString:path::parent(itemFolder)]];
			}
		}
		else
		{
			failedToSave.insert(*pair);
		}
	}
	changes.swap(failedToSave);

	if(!changes.empty())
	{
		NSAlert* alert = [NSAlert alertWithMessageText:@"Error Saving Bundle Item" defaultButton:@"OK" alternateButton:nil otherButton:nil informativeTextWithFormat:@"Sorry, but something went wrong while trying to save your changes. More info may be available via the console."];
		OakShowAlertForWindow(alert, self.window, ^(NSInteger returnCode){
			if(returnCode == NSAlertSecondButtonReturn) // Discard Changes
			{
				changes.clear();
				[self didChangeModifiedState];
			}
		});
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

		if(bundles::item_ptr item = entry->represented_item())
		{
			[cell setImage:[NSImage imageNamed:info_for(item->kind()).file inSameBundleAsClass:[self class]]];
		}
		else
		{
			std::string const& path = entry->represented_path();
			if(path != NULL_STR)
				[cell setImage:[OakFileIconImage fileIconImageWithPath:[NSString stringWithCxxString:path] size:NSMakeSize(16, 16)]];
		}
	}
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
	propertiesChanged = YES;
	[self didChangeModifiedState];
}

- (void)setBundleItemProperties:(NSMutableDictionary*)someProperties
{
	static std::string const BindingKeys[] = { bundles::kFieldIsDisabled, bundles::kFieldName, bundles::kFieldKeyEquivalent, bundles::kFieldTabTrigger, bundles::kFieldScopeSelector, bundles::kFieldSemanticClass, bundles::kFieldContentMatch, bundles::kFieldHideFromUser, bundles::kFieldDropExtension, bundles::kFieldGrammarExtension, bundles::kFieldGrammarFirstLineMatch, bundles::kFieldGrammarScope, bundles::kFieldGrammarInjectionSelector, "beforeRunningCommand", "input", "inputFormat", "outputLocation", "outputFormat", "outputCaret", "autoScrollOutput", "contactName", "contactEmailRot13", "description", "disableAutoIndent", "useGlobalClipboard", "author", "comment" };

	NSMutableDictionary* oldProperties = bundleItemProperties;
	bundleItemProperties = someProperties;
	iterate(str, BindingKeys)
	{
		NSString* key = [NSString stringWithCxxString:*str];
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
			for(size_t i = 0; i != sizeofA(popups); ++i)
				[res setObject:[popups[i].array objectAtIndex:popups[i].index] forKey:popups[i].key];
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

- (BOOL)window:(NSWindow*)aWindow shouldDragDocumentWithEvent:(NSEvent*)anEvent from:(NSPoint)dragImageLocation withPasteboard:(NSPasteboard*)aPasteboard { return NO; }
- (BOOL)window:(NSWindow*)aWindow shouldPopUpDocumentPathMenu:(NSMenu*)aMenu                                                                              { return NO; }

- (void)setBundleItem:(bundles::item_ptr const&)aBundleItem
{
	if(bundleItem == aBundleItem)
		return;

	[self commitEditing];

	if(bundleItemContent)
		bundleItemContent->remove_callback(documentCallback);

	bundleItem        = aBundleItem;
	bundleItemContent = document::document_ptr();

	std::map<bundles::item_ptr, plist::dictionary_t>::const_iterator it = changes.find(bundleItem);
	self.bundleItemProperties = it != changes.end() ? DictionaryForPropertyList(it->second, bundleItem) : DictionaryForBundleItem(bundleItem);

	item_info_t const& info = info_for(bundleItem->kind());

	[[self window] setTitle:[NSString stringWithCxxString:bundleItem->full_name()]];
	[[self window] setRepresentedFilename:NSHomeDirectory()];
	[[[self window] standardWindowButton:NSWindowDocumentIconButton] setImage:[[NSWorkspace sharedWorkspace] iconForFileType:[NSString stringWithCxxString:info.file_type]]];

	plist::dictionary_t const& plist = it != changes.end() ? it->second : bundleItem->plist();
	if(info.plist_key == NULL_STR)
	{
		std::vector<std::string> keys;
		if(info.kind == bundles::kItemTypeGrammar)
			keys = { "comment", "patterns", "repository", "injections" };
		else if(info.kind == bundles::kItemTypeTheme)
			keys = { "gutterSettings", "settings", "colorSpaceName" };

		plist::dictionary_t plistSubset;
		iterate(key, keys)
		{
			if(plist.find(*key) != plist.end())
				plistSubset[*key] = plist.find(*key)->second;
		}
		bundleItemContent = document::from_content(to_s(plistSubset, plist::kPreferSingleQuotedStrings, PlistKeySortOrder()), info.grammar);
	}
	else if(oak::contains(std::begin(PlistItemKinds), std::end(PlistItemKinds), info.kind))
	{
		if(plist.find(info.plist_key) != plist.end())
			bundleItemContent = document::from_content(to_s(plist.find(info.plist_key)->second, plist::kPreferSingleQuotedStrings, PlistKeySortOrder()), info.grammar);
	}
	else
	{
		std::string str;
		if(plist::get_key_path(plist, info.plist_key, str))
		{
			if(info.kind == bundles::kItemTypeCommand || info.kind == bundles::kItemTypeDragCommand)
				command::fix_shebang(&str);
			bundleItemContent = document::from_content(str, info.grammar);
		}
	}

	bundleItemContent = bundleItemContent ?: document::from_content("");
	bundleItemContent->set_custom_name("«bundle item»");
	bundleItemContent->add_callback(documentCallback);
	[documentView setDocument:bundleItemContent];

	self.sharedPropertiesViewController = [[PropertiesViewController alloc] initWithName:@"SharedProperties"];
	self.extraPropertiesViewController  = nil;
	[sharedPropertiesViewController setProperties:bundleItemProperties];

	if(info.kind == bundles::kItemTypeBundle)
		self.sharedPropertiesViewController = nil;

	NSView* sharedView = [sharedPropertiesViewController view];
	if(info.view_controller)
	{
		self.extraPropertiesViewController = [[PropertiesViewController alloc] initWithName:info.view_controller];
		[extraPropertiesViewController setProperties:bundleItemProperties];
		NSView* extraView = [extraPropertiesViewController view];

		CGFloat delta = extraPropertiesViewController.indent - sharedPropertiesViewController.indent;
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
	[alert setAlertStyle:NSWarningAlertStyle];
	[alert setMessageText:DescriptionForChanges(changes)];
	[alert setInformativeText:@"Your changes will be lost if you don’t save them."];
	[alert addButtons:@"Save", @"Cancel", @"Don’t Save", nil];
	OakShowAlertForWindow(alert, self.window, ^(NSInteger returnCode){
		if(returnCode != NSAlertSecondButtonReturn) // Not "Cancel"
		{
			if(returnCode == NSAlertFirstButtonReturn) // "Save"
				[self saveDocument:self];
			else if(returnCode == NSAlertThirdButtonReturn) // "Don’t Save"
				changes.clear();
			[self close];
		}
	});
	return NO;
}
@end
