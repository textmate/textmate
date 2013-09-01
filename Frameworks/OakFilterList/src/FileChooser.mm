#import "FileChooser.h"
#import "OakAbbreviations.h"
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/OakFileIconImage.h>
#import <OakAppKit/OakUIConstructionFunctions.h>
#import <OakFoundation/NSString Additions.h>
#import <OakFileBrowser/OFBPathInfoCell.h>
#import <ns/ns.h>
#import <text/format.h>
#import <text/parse.h>
#import <text/ranker.h>
#import <regexp/regexp.h>
#import <oak/algorithm.h>
#import <oak/duration.h>

static NSString* const kUserDefaultsShowOpenFilesInFileChooserKey = @"showOpenFilesInFileChooser";

static NSButton* OakCreateScopeButton (NSString* label, SEL action, NSUInteger tag)
{
	NSButton* res = [NSButton new];
	[[res cell] setBackgroundStyle:NSBackgroundStyleRaised];
	[[res cell] setControlSize:NSSmallControlSize];
	NSString* accessibilityRole = NSAccessibilityRadioButtonRole;
	[[res cell] accessibilitySetOverrideValue:accessibilityRole forAttribute:NSAccessibilityRoleAttribute];
	[[res cell] accessibilitySetOverrideValue:NSAccessibilityRoleDescription(accessibilityRole, nil) forAttribute:NSAccessibilityRoleDescriptionAttribute];
	res.bezelStyle                      = NSRecessedBezelStyle;
	res.buttonType                      = NSPushOnPushOffButton;
	res.title                           = label;
	res.tag                             = tag;
	res.action                          = action;
	res.showsBorderOnlyWhileMouseInside = YES;

	return res;
}

@interface OakScopeBarView : NSView
@end

@implementation OakScopeBarView
- (BOOL)accessibilityIsIgnored { return NO; }
- (id)accessibilityAttributeValue:(NSString *)attribute
{
	if([attribute isEqualToString:NSAccessibilityRoleAttribute])
		return NSAccessibilityRadioGroupRole;
	else
		return [super accessibilityAttributeValue:attribute];
}
@end

@interface OakNonActivatingTableView : NSTableView
@end

@implementation OakNonActivatingTableView
- (NSCell*)preparedCellAtColumn:(NSInteger)column row:(NSInteger)row
{
	OFBPathInfoCell* res = (OFBPathInfoCell*)[super preparedCellAtColumn:column row:row];
	res.disableHighlight = [self.window isKeyWindow];
	return res;
}

- (void)highlightSelectionInClipRect:(NSRect)clipRect
{
	if(![self.window isKeyWindow])
		return [super highlightSelectionInClipRect:clipRect];

	[[NSColor alternateSelectedControlColor] set];
	[[self selectedRowIndexes] enumerateRangesInRange:[self rowsInRect:clipRect] options:0 usingBlock:^(NSRange range, BOOL* stop){
		for(NSUInteger row = range.location; row < NSMaxRange(range); ++row)
		{
			NSRect rect = [self rectOfRow:row];
			rect.size.height -= 1;
			NSRectFill(rect);
		}
	}];
}
@end

static NSMutableAttributedString* CreateAttributedStringWithMarkedUpRanges (NSFont* baseFont, NSColor* textColor, NSColor* matchedTextColor, std::string const& in, std::vector< std::pair<size_t, size_t> > const& ranges, size_t offset = 0)
{
	NSDictionary* baseAttributes      = @{ NSForegroundColorAttributeName : textColor,        };
	NSDictionary* highlightAttributes = @{ NSForegroundColorAttributeName : matchedTextColor, NSUnderlineStyleAttributeName : @1 };

	NSMutableAttributedString* res = [[NSMutableAttributedString alloc] init];

	size_t from = 0;
	for(auto range : ranges)
	{
		[res appendAttributedString:[[NSAttributedString alloc] initWithString:[NSString stringWithCxxString:std::string(in.begin() + from, in.begin() + range.first + offset)] attributes:baseAttributes]];
		[res appendAttributedString:[[NSAttributedString alloc] initWithString:[NSString stringWithCxxString:std::string(in.begin() + range.first + offset, in.begin() + range.second + offset)] attributes:highlightAttributes]];
		from = range.second + offset;
	}
	if(from < in.size())
		[res appendAttributedString:[[NSAttributedString alloc] initWithString:[NSString stringWithCxxString:in.substr(from)] attributes:baseAttributes]];

	return res;
}

// =======================

namespace
{
	struct filter_string_t
	{
		std::string path      = NULL_STR;
		std::string name      = NULL_STR;
		std::string extension = NULL_STR;
		std::string selection = NULL_STR;
		std::string symbol    = NULL_STR;
		std::string raw_path  = NULL_STR;

		filter_string_t (std::string const& str)
		{
			if(str == NULL_STR || str.empty())
				return;

			if(regexp::match_t const& m = regexp::search("(?x)  \\A  (?: (?:/(?=.*/))? (.*) / )?  ([^/]*?)  (\\.[^/]+?)?  (?: :([\\d+:-x\\+]*) | @(.*) )?  \\z", str))
			{
				_initialized = true;

				path      = m[1];
				name      = m.did_match(2) ? m[2] : "";
				extension = m[3];
				selection = m[4];
				symbol    = m[5];

				raw_path = full_path();

				path = oak::normalize_filter(path);
				name = oak::normalize_filter(name);
			}
		}

		std::string full_path () const
		{
			return (path != NULL_STR ? path + "/" : "") + name + (extension != NULL_STR ? extension : "");
		}

		explicit operator bool () const { return _initialized; }

	private:
		bool _initialized = false;
	};

	struct document_record_t
	{
		document_record_t (document::document_ptr const& doc)
		{
			full_path  = doc->path();
			name       = full_path == NULL_STR ? doc->display_name() : path::name(full_path);
			display    = name;
			lru_rank   = -doc->lru().value();

			if(full_path == NULL_STR)
				identifier = doc->identifier();
		}

		oak::uuid_t identifier;
		std::string full_path;
		std::string name;
		std::string display;
		double lru_rank;

		bool matched           = true;
		size_t display_parents = 0;
		bool place_last        = false;
		double rank            = 0;

		std::vector<std::pair<size_t, size_t>> cover;
		NSNumber* tableview_item = nil;
		OakFileIconImage* image = nil;
	};
}

static path::glob_list_t globs_for_path (std::string const& path)
{
	settings_t const settings = settings_for_path(NULL_STR, "", path);
	path::glob_list_t res;

	res.add_exclude_glob(settings.get(kSettingsExcludeDirectoriesInFileChooserKey), path::kPathItemDirectory);
	res.add_exclude_glob(settings.get(kSettingsExcludeDirectoriesKey),              path::kPathItemDirectory);
	res.add_exclude_glob(settings.get(kSettingsExcludeFilesInFileChooserKey),       path::kPathItemFile);
	res.add_exclude_glob(settings.get(kSettingsExcludeFilesKey),                    path::kPathItemFile);
	res.add_exclude_glob(settings.get(kSettingsExcludeInFileChooserKey),            path::kPathItemAny);
	res.add_exclude_glob(settings.get(kSettingsExcludeKey),                         path::kPathItemAny);
	res.add_exclude_glob(settings.get(kSettingsBinaryKey),                          path::kPathItemAny);

	res.add_include_glob(settings.get(kSettingsIncludeDirectoriesKey),              path::kPathItemDirectory);
	res.add_include_glob(settings.get(kSettingsIncludeFilesInFileChooserKey),       path::kPathItemFile);
	res.add_include_glob(settings.get(kSettingsIncludeFilesKey),                    path::kPathItemFile);
	res.add_include_glob(settings.get(kSettingsIncludeInFileChooserKey),            path::kPathItemAny);
	res.add_include_glob(settings.get(kSettingsIncludeKey),                         path::kPathItemAny);

	return res;
}

@interface FileChooser () <NSWindowDelegate, NSTextFieldDelegate, NSTableViewDataSource, NSTableViewDelegate>
{
	scm::info_ptr                                 _scmInfo;
	std::vector<document::document_ptr>           _openDocuments;
	std::map<oak::uuid_t, document::document_ptr> _openDocumentsMap;
	oak::uuid_t                                   _currentDocument;
	std::vector<document_record_t>                _records;
	document::scanner_ptr                         _scanner;
}
@property (nonatomic) NSSearchField*       searchField;
@property (nonatomic) NSButton*            allButton;
@property (nonatomic) NSButton*            openDocumentsButton;
@property (nonatomic) NSButton*            scmChangesButton;
@property (nonatomic) NSTableView*         tableView;
@property (nonatomic) NSTextField*         statusTextField;
@property (nonatomic) NSTextField*         itemCountTextField;
@property (nonatomic) NSProgressIndicator* progressIndicator;

@property (nonatomic) NSUInteger           sourceIndex;
@property (nonatomic) NSArray*             items;

@property (nonatomic) BOOL                 polling;
@property (nonatomic) NSTimer*             pollTimer;
@property (nonatomic) CGFloat              pollInterval;

@property (nonatomic) FileChooser*         retainedSelf;
@end

@implementation FileChooser
+ (FileChooser*)sharedInstance
{
	static FileChooser* sharedInstance = [FileChooser new];
	return sharedInstance;
}

- (id)init
{
	if((self = [super init]))
	{
		_items = @[ ];

		_searchField               = [[NSSearchField alloc] initWithFrame:NSZeroRect];
		_searchField.delegate      = self;
		[_searchField.cell setScrollable:YES];
		if(![NSApp isFullKeyboardAccessEnabled])
		{
			_searchField.focusRingType = NSFocusRingTypeNone;
		}

		_allButton           = OakCreateScopeButton(@"All",                   @selector(takeSourceIndexFrom:), 0);
		_openDocumentsButton = OakCreateScopeButton(@"Open Documents",        @selector(takeSourceIndexFrom:), 1);
		_scmChangesButton    = OakCreateScopeButton(@"Uncommitted Documents", @selector(takeSourceIndexFrom:), 2);
		[_allButton setState:NSOnState];
		[_scmChangesButton setEnabled:NO];
		OakScopeBarView* scopeBar = [OakScopeBarView new];
		NSDictionary* scopeButtons = @{
			@"allButton"           : _allButton,
			@"openDocumentsButton" : _openDocumentsButton,
			@"scmChangesButton"    : _scmChangesButton,
		};
		for(NSView* scopeButton in @[_allButton, _openDocumentsButton, _scmChangesButton])
		{
			[scopeButton setTranslatesAutoresizingMaskIntoConstraints:NO];
			[scopeBar addSubview:scopeButton];
		}
		[scopeBar addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[allButton]-[openDocumentsButton]-[scmChangesButton]|" options:0 metrics:nil views:scopeButtons]];
		[scopeBar addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[allButton]|" options:0 metrics:0 views:scopeButtons]];

		NSCell* cell = [OFBPathInfoCell new];
		cell.lineBreakMode = NSLineBreakByTruncatingMiddle;

		NSTableColumn* tableColumn = [[NSTableColumn alloc] initWithIdentifier:@"name"];
		[tableColumn setDataCell:cell];

		_tableView = [[OakNonActivatingTableView alloc] initWithFrame:NSZeroRect];
		[_tableView addTableColumn:tableColumn];
		_tableView.headerView              = nil;
		_tableView.focusRingType           = NSFocusRingTypeNone;
		_tableView.allowsEmptySelection    = NO;
		_tableView.allowsMultipleSelection = YES;
		_tableView.refusesFirstResponder   = YES;
		_tableView.doubleAction            = @selector(accept:);
		_tableView.target                  = self;
		_tableView.dataSource              = self;
		_tableView.delegate                = self;

		NSScrollView* scrollView         = [[NSScrollView alloc] initWithFrame:NSZeroRect];
		scrollView.hasVerticalScroller   = YES;
		scrollView.hasHorizontalScroller = NO;
		scrollView.autohidesScrollers    = YES;
		scrollView.borderType            = NSNoBorder;
		scrollView.documentView          = _tableView;

		_statusTextField = [[NSTextField alloc] initWithFrame:NSZeroRect];
		_statusTextField.bezeled         = NO;
		_statusTextField.bordered        = NO;
		_statusTextField.drawsBackground = NO;
		_statusTextField.editable        = NO;
		_statusTextField.font            = OakStatusBarFont();
		_statusTextField.selectable      = NO;
		[[_statusTextField cell] setBackgroundStyle:NSBackgroundStyleRaised];

		_itemCountTextField = [[NSTextField alloc] initWithFrame:NSZeroRect];
		_itemCountTextField.bezeled         = NO;
		_itemCountTextField.bordered        = NO;
		_itemCountTextField.drawsBackground = NO;
		_itemCountTextField.editable        = NO;
		_itemCountTextField.font            = OakStatusBarFont();
		_itemCountTextField.selectable      = NO;
		[[_itemCountTextField cell] setBackgroundStyle:NSBackgroundStyleRaised];

		_progressIndicator = [[NSProgressIndicator alloc] initWithFrame:NSZeroRect];
		_progressIndicator.style                = NSProgressIndicatorSpinningStyle;
		_progressIndicator.controlSize          = NSSmallControlSize;
		_progressIndicator.displayedWhenStopped = NO;

		_window = [[NSPanel alloc] initWithContentRect:NSMakeRect(600, 700, 400, 500) styleMask:(NSTitledWindowMask|NSClosableWindowMask|NSResizableWindowMask|NSTexturedBackgroundWindowMask) backing:NSBackingStoreBuffered defer:NO];
		[_window setAutorecalculatesContentBorderThickness:NO forEdge:NSMaxYEdge];
		[_window setAutorecalculatesContentBorderThickness:NO forEdge:NSMinYEdge];
		[_window setContentBorderThickness:57 forEdge: NSMaxYEdge];
		[_window setContentBorderThickness:23 forEdge: NSMinYEdge];
		[[_window standardWindowButton:NSWindowMiniaturizeButton] setHidden:YES];
		[[_window standardWindowButton:NSWindowZoomButton] setHidden:YES];
		_window.autorecalculatesKeyViewLoop = YES;
		_window.delegate                    = self;
		_window.level                       = NSFloatingWindowLevel;
		_window.releasedWhenClosed          = NO;

		NSDictionary* views = @{
			@"searchField"        : _searchField,
			@"aboveScopeBarDark"  : OakCreateHorizontalLine([NSColor grayColor], [NSColor lightGrayColor]),
			@"aboveScopeBarLight" : OakCreateHorizontalLine([NSColor colorWithCalibratedWhite:0.797 alpha:1.000], [NSColor colorWithCalibratedWhite:0.912 alpha:1.000]),
			@"scopeBar"           : scopeBar,
			@"topDivider"         : OakCreateHorizontalLine([NSColor darkGrayColor], [NSColor colorWithCalibratedWhite:0.551 alpha:1.000]),
			@"scrollView"         : scrollView,
			@"bottomDivider"      : OakCreateHorizontalLine([NSColor grayColor], [NSColor lightGrayColor]),
			@"statusTextField"    : _statusTextField,
			@"itemCountTextField" : _itemCountTextField,
			@"progressIndicator"  : _progressIndicator,
		};

		NSView* contentView = _window.contentView;
		for(NSView* view in [views allValues])
		{
			[view setTranslatesAutoresizingMaskIntoConstraints:NO];
			[contentView addSubview:view];
		}

		[_statusTextField setContentCompressionResistancePriority:NSLayoutPriorityDefaultLow forOrientation:NSLayoutConstraintOrientationHorizontal];
		[_statusTextField setContentHuggingPriority:NSLayoutPriorityDefaultLow forOrientation:NSLayoutConstraintOrientationHorizontal];
		[_itemCountTextField setContentHuggingPriority:NSLayoutPriorityDefaultHigh forOrientation:NSLayoutConstraintOrientationHorizontal];

		[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(8)-[searchField(>=50)]-(8)-|"                      options:0 metrics:nil views:views]];
		[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[aboveScopeBarDark(==aboveScopeBarLight)]|"          options:0 metrics:nil views:views]];
		[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(8)-[scopeBar]-(>=8)-|" options:NSLayoutFormatAlignAllBaseline metrics:nil views:views]];
		[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[scrollView(==topDivider,==bottomDivider)]|"         options:0 metrics:nil views:views]];
		[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(24)-[statusTextField]-[itemCountTextField]-(4)-[progressIndicator]-(4)-|" options:NSLayoutFormatAlignAllCenterY metrics:nil views:views]];
		[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-(2)-[searchField]-(8)-[aboveScopeBarDark][aboveScopeBarLight]-(3)-[scopeBar]-(4)-[topDivider][scrollView(>=50)][bottomDivider]-(4)-[statusTextField]-(5)-|" options:0 metrics:nil views:views]];

		if([[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsShowOpenFilesInFileChooserKey])
			self.sourceIndex = 1;

		[self updateWindowTitle];

		_retainedSelf = self;
	}
	return self;
}

- (void)dealloc
{
	_window.delegate      = nil;
	_searchField.delegate = nil;
	_tableView.target     = nil;
	_tableView.dataSource = nil;
	_tableView.delegate   = nil;
}

- (void)showWindow:(id)sender
{
	_retainedSelf = self;

	_tableView.target     = self;
	_tableView.dataSource = self;
	_tableView.delegate   = self;

	[_window makeKeyAndOrderFront:self];
	[_window makeFirstResponder:_searchField];
}

- (void)close
{
	[_window performClose:self];
}

- (void)windowWillClose:(NSNotification*)aNotification
{
	[self shutdownScanner];

	_tableView.target     = nil;
	_tableView.dataSource = nil;
	_tableView.delegate   = nil;

	_scmInfo.reset();
	_openDocuments.clear();
	_openDocumentsMap.clear();
	_records.clear();

	_retainedSelf = nil;
}

- (void)updateWindowTitle
{
	NSString* src = nil;
	switch(self.sourceIndex)
	{
		case 0: src = [self.path stringByAbbreviatingWithTildeInPath]; break;
		case 1: src = @"Open Documents";                               break;
		case 2: src = @"Uncommitted Documents";                        break;
	}
	_window.title = [NSString stringWithFormat:@"Go to File… — %@", src];
}

- (BOOL)allowsMultipleSelection                             { return _tableView.allowsMultipleSelection; }
- (void)setAllowsMultipleSelection:(BOOL)flag               { _tableView.allowsMultipleSelection = flag; }

- (oak::uuid_t const&)currentDocument                       { return _currentDocument; }
- (void)setCurrentDocument:(oak::uuid_t const&)newDocument  { _currentDocument = newDocument; [self reload]; }

- (std::vector<document::document_ptr> const&)openDocuments
{
	return _openDocuments;
}

- (void)setOpenDocuments:(std::vector<document::document_ptr> const&)newDocuments
{
	_openDocuments = newDocuments;
	_openDocumentsMap.clear();
	std::transform(_openDocuments.begin(), _openDocuments.end(), inserter(_openDocumentsMap, _openDocumentsMap.end()), [](document::document_ptr const& doc){ return std::make_pair(doc->identifier(), doc); });
	[self reload];
}

- (void)takeSourceIndexFrom:(id)sender
{
	if([sender respondsToSelector:@selector(tag)])
		self.sourceIndex = [sender tag];
}

- (void)setSourceIndex:(NSUInteger)newIndex
{
	_sourceIndex = newIndex;
	switch(newIndex)
	{
		case 0: self.onlyShowOpenDocuments = NO;  break;
		case 1: self.onlyShowOpenDocuments = YES; break;
		case 2: break;
	}

	for(NSButton* button in @[ _allButton, _openDocumentsButton, _scmChangesButton ])
		[button setState:[button tag] == _sourceIndex ? NSOnState : NSOffState];

	[self updateWindowTitle];
}

// =================
// = Filter String =
// =================

- (void)setFilterString:(NSString*)aString
{
	if(_filterString == aString || [_filterString isEqualToString:aString])
		return;

	_filterString = [aString copy];
	_searchField.stringValue = aString;
	[self updateRecordsFrom:0];
}

- (void)addRecordsForDocuments:(std::vector<document::document_ptr> const&)documents
{
	std::string const path = to_s(_path);
	std::set<std::string> openPaths;
	std::transform(_openDocuments.begin(), _openDocuments.end(), inserter(openPaths, openPaths.end()), [](document::document_ptr const& doc){ return doc->path(); });

	bool insertAll = _records.empty();

	NSUInteger firstDirty = _records.size();
	NSUInteger index      = _records.size();
	for(auto doc : documents)
	{
		if(insertAll || (_openDocumentsMap.find(doc->identifier()) == _openDocumentsMap.end() && openPaths.find(doc->path()) == openPaths.end()))
		{
			document_record_t record(doc);
			record.place_last     = doc->identifier() == _currentDocument;
			record.tableview_item = @(index++);
			_records.push_back(record);
		}
	}

	[self updateRecordsFrom:firstDirty];
}

inline void rank_record (document_record_t& record, filter_string_t const& filter, std::string const& basePath, path::glob_list_t const& glob, std::vector<std::string> const& bindings)
{
	record.matched = false;
	if(glob.exclude(record.full_path))
		return;

	if(filter.extension != NULL_STR)
	{
		// Check if filter string’s extension is a subset and that the
		// subset match is followed by a period or is end of string.
		std::string::size_type ext = record.name.find(filter.extension);
		if(ext == std::string::npos || ext + filter.extension.size() < record.name.size() && record.name[ext + filter.extension.size()] != '.')
			return;
	}

	record.cover.clear();
	record.display         = record.name;
	record.display_parents = 0;

	if(!filter)
	{
		record.matched = true;
		record.rank    = record.place_last ? 1 : 0;
		return;
	}

	double path_rank = 1;
	std::vector<std::pair<size_t, size_t>> path_cover;
	if(filter.path != NULL_STR)
	{
		std::string prefix = (record.full_path == NULL_STR) ? "" : path::relative_to(path::parent(record.full_path), basePath);
		if(double rank = oak::rank(filter.path, prefix, &path_cover))
		{
			path_rank = 1 - rank;
			record.display = (record.full_path == NULL_STR) ? "" : prefix + (prefix.empty() ? "" : "/");
		}
		else
		{
			return;
		}
	}

	if(double rank = oak::rank(filter.name, record.name, &record.cover))
	{
		record.matched = true;

		if(filter.path != NULL_STR)
		{
			for(auto pair : record.cover)
				path_cover.push_back(std::make_pair(pair.first + record.display.size(), pair.second + record.display.size()));
			record.display = record.display + record.name;
			record.cover.swap(path_cover);
		}

		size_t bindingIndex = std::find(bindings.begin(), bindings.end(), record.full_path) - bindings.begin();
		if((filter.selection != NULL_STR || filter.symbol != NULL_STR) && record.place_last && filter.full_path().empty())
			record.rank = 0;
		else if(!filter.raw_path.empty() && path::is_child(filter.raw_path, record.full_path))
			record.rank = 0;
		else if(record.place_last)
			record.rank = 1;
		else if(bindingIndex != bindings.size())
			record.rank = -1.0 * (bindings.size() - bindingIndex);
		else if(filter.name.empty())
			record.rank = path_rank;
		else
			record.rank = path_rank * (1 - rank);
	}
}

- (void)updateRecordsFrom:(NSUInteger)first
{
	filter_string_t filter(to_s(_filterString));
	path::glob_list_t glob;
	std::string const basePath = to_s(_path);

	std::vector<std::string> bindings;
	for(NSString* str in [[OakAbbreviations abbreviationsForName:@"OakFileChooserBindings"] stringsForAbbreviation:[NSString stringWithCxxString:filter.full_path()]])
		bindings.push_back(to_s(str));

	size_t const count  = _records.size() - first;
	size_t const stride = 256;
	dispatch_apply(count / stride, dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^(size_t n){
		for(size_t i = n*stride; i < (n+1)*stride; ++i)
			rank_record(_records[first + i], filter, basePath, glob, bindings);
	});
   for(size_t i = count - (count % stride); i < count; ++i)
		rank_record(_records[first + i], filter, basePath, glob, bindings);

	std::vector<document_record_t const*> include;
	for(auto const& record : _records)
	{
		if(record.matched)
			include.push_back(&record);
	}
	std::sort(include.begin(), include.end(), [](document_record_t const* lhs, document_record_t const* rhs){ return (lhs->rank < rhs->rank) || ((lhs->rank == rhs->rank) && ((lhs->lru_rank < rhs->lru_rank) || (lhs->lru_rank == rhs->lru_rank && lhs->name < rhs->name))); });

	NSMutableArray* array = [NSMutableArray arrayWithCapacity:include.size()];
	for(auto record : include)
		[array addObject:record->tableview_item];
	self.items = array;

	if(!_scanner)
		[self updateParents];
}

- (void)updateParents
{
	if(filter_string_t(to_s(_filterString)).path != NULL_STR)
		return;

	std::vector<std::string> paths;
	for(NSNumber* index in _items)
	{
		document_record_t const& record = _records[index.unsignedIntValue];
		paths.push_back(record.full_path);
	}

	std::vector<size_t> const& visibleParents = path::disambiguate(paths);
	for(NSUInteger i = 0; i < _items.count; ++i)
	{
		NSNumber* index = _items[i];
		_records[index.unsignedIntValue].display_parents = visibleParents[i];
	}

	[_tableView reloadData];
}

- (void)setItems:(NSArray*)anArray
{
	_items = anArray;
	[_tableView reloadData];
	[_tableView scrollRowToVisible:_tableView.selectedRow == -1 ? 0 : _tableView.selectedRow];

	[self updateStatusText:self];

	_itemCountTextField.stringValue = [NSString stringWithFormat:@"%@ item%s", [NSNumberFormatter localizedStringFromNumber:@(_items.count) numberStyle:NSNumberFormatterDecimalStyle], _items.count == 1 ? "" : "s"];
}

- (void)updateSCMStatus
{
	if(!_scmInfo)
		return;

	NSRange visibleRange = [_tableView rowsInRect:[_tableView visibleRect]];
	for(NSUInteger row = visibleRange.location; row < NSMaxRange(visibleRange); ++row)
	{
		NSNumber* index = _items[row];
		document_record_t const& record = _records[index.unsignedIntValue];
		if(record.full_path != NULL_STR)
		{
			scm::status::type scmStatus = _scmInfo->status(record.full_path);
			if(record.image.scmStatus != scmStatus)
			{
				record.image.scmStatus = scmStatus;
				[_tableView setNeedsDisplayInRect:[_tableView rectOfRow:row]];
			}
		}
	}
}

// ========
// = Path =
// ========

- (void)setPath:(NSString*)aString
{
	if(_path == aString || [_path isEqualToString:aString])
		return;
	_path = aString;

	if(_onlyShowOpenDocuments)
		return;

	[self shutdownScanner];

	self.items = @[ ];

	_records.clear();
	[self addRecordsForDocuments:_openDocuments];
	settings_t const settings = settings_for_path(NULL_STR, "", to_s(_path));
	_scanner = std::make_shared<document::scanner_t>(to_s(_path), globs_for_path(to_s(_path)), settings.get(kSettingsFollowSymbolicLinksKey, false), false, false);
	_scmInfo = scm::info(to_s(_path));
	if(_scmInfo)
	{
		_scmInfo->add_callback(^(scm::info_t const& info){
			[self updateSCMStatus];
		});
	}

	_pollInterval = 0.01;
	_pollTimer = [NSTimer scheduledTimerWithTimeInterval:_pollInterval target:self selector:@selector(fetchScannerResults:) userInfo:nil repeats:NO];
	[_progressIndicator startAnimation:self];

	[self updateWindowTitle];
}

- (void)setOnlyShowOpenDocuments:(BOOL)flag
{
	if(_onlyShowOpenDocuments == flag)
		return;

	_onlyShowOpenDocuments = flag;
	[self reload];
	[[NSUserDefaults standardUserDefaults] setObject:@(_onlyShowOpenDocuments) forKey:kUserDefaultsShowOpenFilesInFileChooserKey];
}

- (void)reload
{
	if(_onlyShowOpenDocuments)
	{
		[self shutdownScanner];

		_records.clear();
		[self addRecordsForDocuments:_openDocuments];
	}
	else
	{
		NSString* path = _path;
		_path = nil;
		self.path = path;
	}
}

- (void)fetchScannerResults:(NSTimer*)aTimer
{
	bool isRunning = _scanner->is_running();
	[self addRecordsForDocuments:_scanner->accept_documents()];

	if(isRunning)
	{
		_pollInterval = std::min(_pollInterval * 2, 0.32);
		_pollTimer = [NSTimer scheduledTimerWithTimeInterval:_pollInterval target:self selector:@selector(fetchScannerResults:) userInfo:nil repeats:NO];
	}
	else
	{
		[self shutdownScanner];
		[self updateStatusText:self];
		[self updateParents];
	}
}

- (void)shutdownScanner
{
	[_progressIndicator stopAnimation:self];
	[_pollTimer invalidate];
	_pollTimer = nil;
	_scanner.reset();
}

- (void)updateStatusText:(id)sender
{
	if(_scanner)
	{
		std::string path = path::relative_to(_scanner->get_current_path(), to_s(_path));
		[_statusTextField.cell setLineBreakMode:NSLineBreakByTruncatingMiddle];
		_statusTextField.stringValue = [NSString stringWithFormat:@"Searching “%@”…", [NSString stringWithCxxString:path]];
	}
	else if(_tableView.selectedRow == -1)
	{
		_statusTextField.stringValue = @"";
	}
	else
	{
		NSNumber* index = _items[_tableView.selectedRow];
		document_record_t const& record = _records[index.unsignedIntValue];

		std::string prefix = record.full_path;
		if(prefix != NULL_STR)
		{
			prefix = path::with_tilde(prefix.substr(0, prefix.size() - record.display.size()));
			if(prefix.size() && prefix[prefix.size()-1] != '/')
				prefix += '/';
		}
		else // untitled file
		{
			prefix = "";
		}

		std::string path = prefix + record.display;
		[_statusTextField.cell setLineBreakMode:NSLineBreakByTruncatingHead];
		_statusTextField.stringValue = [NSString stringWithCxxString:path];
	}
}

// =========================
// = NSTableViewDataSource =
// =========================

- (NSInteger)numberOfRowsInTableView:(NSTableView*)aTableView
{
	return _items.count;
}

- (id)tableView:(NSTableView*)aTableView objectValueForTableColumn:(NSTableColumn*)aTableColumn row:(NSInteger)rowIndex
{
	if([aTableColumn.identifier isEqualToString:@"name"])
	{
		NSNumber* index = _items[rowIndex];
		document_record_t const& record = _records[index.unsignedIntValue];

		std::string path = record.display;
		if(record.display_parents)
		{
			auto v = text::split(path::parent(record.full_path), "/");
			v.erase(v.begin(), v.end() - std::min(record.display_parents, v.size()));
			path += " — " + text::join(v, "/");
		}

		NSColor* textColor        = [NSColor controlTextColor];
		NSColor* matchedTextColor = [NSColor controlTextColor];
		if([aTableView isRowSelected:rowIndex] && [self.window isKeyWindow])
		{
			textColor        = [NSColor alternateSelectedControlTextColor];
			matchedTextColor = [NSColor alternateSelectedControlTextColor];
		}

		NSMutableAttributedString* str = CreateAttributedStringWithMarkedUpRanges(OakControlFont(), textColor, matchedTextColor, path, record.cover);
		NSMutableParagraphStyle* pStyle = [[NSParagraphStyle defaultParagraphStyle] mutableCopy];
		[pStyle setLineBreakMode:NSLineBreakByTruncatingMiddle];
		[str addAttribute:NSParagraphStyleAttributeName value:pStyle range:NSMakeRange(0, str.length)];
		return str;
	}
	return nil;
}

- (void)tableViewSelectionDidChange:(NSNotification*)notification
{
	[self updateStatusText:self];
}

- (NSArray*)selectedItems
{
	NSMutableArray* res = [NSMutableArray array];
	NSIndexSet* indexes = [_tableView selectedRowIndexes];
	for(NSUInteger i = [indexes firstIndex]; i != NSNotFound; i = [indexes indexGreaterThanIndex:i])
	{
		NSNumber* index = _items[i];
		document_record_t const& record = _records[index.unsignedIntValue];

		NSMutableDictionary* item = [NSMutableDictionary dictionary];
		item[@"identifier"] = [NSString stringWithCxxString:record.identifier];
		filter_string_t filter(to_s(_filterString));
		if(filter.selection != NULL_STR)
			item[@"selectionString"] = [NSString stringWithCxxString:filter.selection];
		if(record.full_path != NULL_STR)
			item[@"path"] = [NSString stringWithCxxString:record.full_path];
		[res addObject:item];
	}
	return res;
}

- (void)tableView:(NSTableView*)aTableView willDisplayCell:(OFBPathInfoCell*)cell forTableColumn:(NSTableColumn*)aTableColumn row:(NSInteger)rowIndex
{
	if(![aTableColumn.identifier isEqualToString:@"name"])
		return;

	NSNumber* index = _items[rowIndex];
	document_record_t& record = _records[index.unsignedIntValue];

	BOOL isOpen = NO, isModified = NO, isOnDisk = YES;
	for(document::document_ptr const& doc : _openDocuments)
	{
		if(doc->path() != NULL_STR ? doc->path() == record.full_path : doc->identifier() == record.identifier)
		{
			isOpen     = YES;
			isModified = doc->is_modified();
			isOnDisk   = doc->is_on_disk();
		}
	}

	cell.objectValue = [self tableView:aTableView objectValueForTableColumn:aTableColumn row:rowIndex];
	if([cell respondsToSelector:@selector(setImage:)])
	{
		if(!record.image)
			record.image = [[OakFileIconImage alloc] initWithSize:NSMakeSize(16, 16)];

		record.image.path     = [NSString stringWithCxxString:record.full_path];
		record.image.exists   = isOnDisk;
		record.image.modified = isModified;

		if(_scmInfo && record.full_path != NULL_STR)
			record.image.scmStatus = _scmInfo->status(record.full_path);

		[cell setImage:record.image];
	}

	if([cell respondsToSelector:@selector(setIsOpen:)])
		cell.isOpen = isOpen;
}

// =================
// = Action Method =
// =================

- (void)accept:(id)sender
{
	[_window orderOut:self];
	if(_action)
		[NSApp sendAction:_action to:_target from:self];

	if(_filterString)
	{
		filter_string_t filter(to_s(_filterString));

		NSIndexSet* indexes = [_tableView selectedRowIndexes];
		for(NSUInteger i = [indexes firstIndex]; i != NSNotFound; i = [indexes indexGreaterThanIndex:i])
		{
			NSNumber* index = _items[i];
			document_record_t const& record = _records[index.unsignedIntValue];

			if(record.full_path != NULL_STR)
				[[OakAbbreviations abbreviationsForName:@"OakFileChooserBindings"] learnAbbreviation:[NSString stringWithCxxString:filter.full_path()] forString:[NSString stringWithCxxString:record.full_path]];
		}
	}

	[_window close]; // Should be last as it gives up ‘retainedSelf’
}

- (void)cancel:(id)sender
{
	[self close];
}

- (IBAction)goToParentFolder:(id)sender
{
	self.path = [_path stringByDeletingLastPathComponent];
}

- (void)updateGoToMenu:(NSMenu*)aMenu
{
	if(_window.isKeyWindow)
	{
		[[aMenu addItemWithTitle:@"All" action:@selector(takeSourceIndexFrom:) keyEquivalent:@"1"] setTag:0];
		[[aMenu addItemWithTitle:@"Open Documents" action:@selector(takeSourceIndexFrom:) keyEquivalent:@"2"] setTag:1];
	}
	else
	{
		[aMenu addItemWithTitle:@"No Sources" action:@selector(nop:) keyEquivalent:@""];
	}
}

- (BOOL)validateMenuItem:(NSMenuItem*)item
{
	BOOL activate = YES;
	if([item action] == @selector(goToParentFolder:))
		activate = _onlyShowOpenDocuments == NO && to_s(_path) != path::parent(to_s(_path));
	else if([item action] == @selector(takeSourceIndexFrom:))
		[item setState:[item tag] == self.sourceIndex ? NSOnState : NSOffState];
	return activate;
}

// =========================
// = Search Field Delegate =
// =========================

- (void)moveSelectedRowByOffset:(NSInteger)anOffset extendingSelection:(BOOL)extend
{
	if([_tableView numberOfRows])
	{
		if(_tableView.allowsMultipleSelection == NO)
			extend = NO;
		NSInteger row = oak::cap((NSInteger)0, [_tableView selectedRow] + anOffset, [_tableView numberOfRows] - 1);
		[_tableView selectRowIndexes:[NSIndexSet indexSetWithIndex:row] byExtendingSelection:extend];
		[_tableView scrollRowToVisible:row];
	}
}

- (int)visibleRows                                      { return (int)floorf(NSHeight([_tableView visibleRect]) / ([_tableView rowHeight]+[_tableView intercellSpacing].height)) - 1; }

- (void)moveUp:(id)sender                               { [self moveSelectedRowByOffset:-1 extendingSelection:NO]; }
- (void)moveDown:(id)sender                             { [self moveSelectedRowByOffset:+1 extendingSelection:NO]; }
- (void)moveUpAndModifySelection:(id)sender             { [self moveSelectedRowByOffset:-1 extendingSelection:YES];}
- (void)moveDownAndModifySelection:(id)sender           { [self moveSelectedRowByOffset:+1 extendingSelection:YES];}
- (void)movePageUp:(id)sender                           { [self moveSelectedRowByOffset:-[self visibleRows] extendingSelection:NO]; }
- (void)movePageDown:(id)sender                         { [self moveSelectedRowByOffset:+[self visibleRows] extendingSelection:NO]; }
- (void)moveToBeginningOfDocument:(id)sender            { [self moveSelectedRowByOffset:-(INT_MAX >> 1) extendingSelection:NO]; }
- (void)moveToEndOfDocument:(id)sender                  { [self moveSelectedRowByOffset:+(INT_MAX >> 1) extendingSelection:NO]; }

- (void)pageUp:(id)sender                               { [self movePageUp:sender]; }
- (void)pageDown:(id)sender                             { [self movePageDown:sender]; }
- (void)scrollPageUp:(id)sender                         { [self movePageUp:sender]; }
- (void)scrollPageDown:(id)sender                       { [self movePageDown:sender]; }

- (IBAction)insertNewline:(id)sender                    { [self accept:sender]; }
- (IBAction)insertNewlineIgnoringFieldEditor:(id)sender { [self accept:sender]; }
- (IBAction)cancelOperation:(id)sender                  { [self cancel:sender]; }

- (BOOL)control:(NSControl*)aControl textView:(NSTextView*)aTextView doCommandBySelector:(SEL)aCommand
{
	static auto const forward = new std::set<SEL>{ @selector(moveUp:), @selector(moveDown:), @selector(moveUpAndModifySelection:), @selector(moveDownAndModifySelection:), @selector(pageUp:), @selector(pageDown:), @selector(movePageUp:), @selector(movePageDown:), @selector(scrollPageUp:), @selector(scrollPageDown:), @selector(moveToBeginningOfDocument:), @selector(moveToEndOfDocument:), @selector(insertNewline:), @selector(insertNewlineIgnoringFieldEditor:), @selector(cancelOperation:) };
	if(forward->find(aCommand) != forward->end() && [self respondsToSelector:aCommand])
		return [NSApp sendAction:aCommand to:self from:aControl];
	return NO;
}

- (void)controlTextDidChange:(NSNotification*)aNotification
{
	self.filterString = _searchField.stringValue;
}
@end
