#import "OakFileChooser.h"
#import "SymbolList.h"
#import "../OakAbbreviations.h"
#import "../highlight_ranges.h"
#import <OakAppKit/OakSubmenuController.h>
#import <OakFoundation/NSString Additions.h>
#import <OakFoundation/OakHistoryList.h>
#import <text/case.h>
#import <io/path.h>
#import <regexp/glob.h>
#import <text/ranker.h>
#import <settings/settings.h>
#import <oak/duration.h>
#import <OakAppKit/NSMenu Additions.h>
#import <ns/ns.h>

OAK_DEBUG_VAR(FilterList_OakFileChooser);

// ==========
// = Helper =
// ==========

struct file_chooser_t
{
	struct item_t
	{
		item_t () { }
		item_t (document::document_ptr document, std::string const& matchName, double rank, std::vector< std::pair<size_t, size_t> > const& matchRanges) : _document(document), _match_name(matchName), _rank(rank), _match_ranges(matchRanges) { }
		bool operator< (item_t const& rhs) const;

		document::document_ptr _document;
		std::string _match_name;
		double _rank;
		std::vector< std::pair<size_t, size_t> > _match_ranges;
		size_t _parents = 0;
	};

	void setup (std::string const& path, std::string const& globString, std::string const& rankString);
	void set_excluded_document (oak::uuid_t const& uuid);
	void set_path (std::string const& path);
	void set_filtering (std::string const& globString, std::string const& rankString);

	void set_documents (std::vector<document::document_ptr> const& documents);

	bool running () const;
	bool poll_scanner ();
	void wait () const;
	void stop_scanner ();

	std::string const& glob_string () const { return _glob_string; }
	std::string const& rank_string () const { return _rank_string; }

	std::vector<item_t> const& items () const { return _ranked_items; }

private:
	void add_documents (std::vector<document::document_ptr> const& documents);

	std::shared_ptr<document::scanner_t> _scanner;

	std::string _path        = NULL_STR;
	std::string _glob_string = NULL_STR;
	std::string _rank_string = NULL_STR;

	oak::uuid_t _excluded_document;

	std::vector<document::document_ptr> _all_documents;
	std::vector<document::document_ptr> _filtered_documents;
	std::vector<item_t> _ranked_items;
};

bool file_chooser_t::item_t::operator< (item_t const& rhs) const
{
	std::tuple<double, double, std::string> lhsValue(_rank, -_document->lru().value(), _match_name), rhsValue(rhs._rank, -rhs._document->lru().value(), rhs._match_name);
	return lhsValue < rhsValue;
}

static std::string parents (std::string const& path, size_t n)
{
	std::string::const_reverse_iterator const& last = path.rend();
	std::string::const_reverse_iterator const& to   = std::find(path.rbegin(), last, '/');
	if(n == 0 || to == last)
		return "";

	std::string::const_reverse_iterator from = to;
	std::string components;
	for(; n > 0 && from != last; --n)
	{
		if(components.size() > 0)
			components = "/" + components;
		components = path::name(std::string(path.begin(), from.base()-1)) + components;
		if(n > 0)
			from = std::find(++from, last, '/');
	}

	return " — " + components;
}

template <typename _OutputIter>
_OutputIter filter (std::vector<document::document_ptr> const& documents, path::glob_t const& glob, _OutputIter out)
{
	iterate(doc, documents)
	{
		if((*doc)->path() == NULL_STR || glob.does_match((*doc)->path()))
			*out++ = *doc;
	}
	return out;
}

template <typename _OutputIter>
_OutputIter lru_rank (std::vector<document::document_ptr> const& documents, oak::uuid_t const& currentDocument, _OutputIter out)
{
	iterate(doc, documents)
	{
		double rank = (*doc)->identifier() == currentDocument ? 1 : 0;
		*out++ = file_chooser_t::item_t(*doc, (*doc)->path() == NULL_STR ? (*doc)->display_name() : path::name((*doc)->path()), rank, std::vector< std::pair<size_t, size_t> >());
	}
	return out;
}

namespace
{
	struct filter_string_t
	{
		std::string raw_filter = NULL_STR;
		std::string filter     = NULL_STR;
		std::string suffix     = NULL_STR;
		std::string selection  = NULL_STR;
		std::string symbol     = NULL_STR;
		bool path_match        = false;

		bool empty () const { return (filter == "" || filter == NULL_STR) && suffix == NULL_STR && symbol == NULL_STR; }
	};

	filter_string_t parse_filter_string (std::string const& filter)
	{
		filter_string_t res;
		auto first = filter.begin(), last = filter.end();

		auto selectionBegin = std::find(first, last, ':');
		auto symbolBegin    = std::find(first, last, '@');
		if(selectionBegin != last)
		{
			res.selection = std::string(selectionBegin+1, last);
			last = selectionBegin;
		}
		else if(symbolBegin != last)
		{
			res.symbol = std::string(symbolBegin+1, last);
			last = symbolBegin;
		}

		res.raw_filter = std::string(first, last);

		auto suffixBegin = std::find(first, last, '.');
		if(suffixBegin != last)
		{
			if(suffixBegin+1 != last)
				res.suffix = std::string(suffixBegin, last);
			last = suffixBegin;
		}

		res.filter = oak::normalize_filter(std::string(first, last));
		res.path_match = res.filter.find('/') != std::string::npos;

		return res;
	}
}

template <typename _OutputIter>
_OutputIter rank (std::vector<document::document_ptr> const& documents, std::string const& rankString, std::string const& rootPath, oak::uuid_t const& currentDocument, _OutputIter out)
{
	auto info = parse_filter_string(rankString);
	std::string const& filter = info.filter;

	NSArray* previousBindings = [[OakAbbreviations abbreviationsForName:@"OakFileChooserBindings"] stringsForAbbreviation:[NSString stringWithCxxString:filter]];

	iterate(doc, documents)
	{
		std::string const& path = (*doc)->path();
		std::string const& candidate = path == NULL_STR ? (*doc)->display_name() : (info.path_match ? (path.find(rootPath) == 0 ? path::relative_to(path, rootPath) : path) : path::name(path));

		if(info.suffix != NULL_STR && candidate.rfind(info.suffix) != candidate.size() - info.suffix.size())
			continue;

		std::vector< std::pair<size_t, size_t> > ranges;
		if(double rank = oak::rank(filter, candidate, &ranges))
		{
			NSUInteger bindingIndex = [previousBindings indexOfObject:[NSString stringWithCxxString:path]];
			if(info.raw_filter == candidate)
				*out++ = file_chooser_t::item_t(*doc, candidate, -0.1, ranges);
			else if(currentDocument == (*doc)->identifier())
				*out++ = file_chooser_t::item_t(*doc, candidate, 1, ranges);
			else if(bindingIndex != NSNotFound)
				*out++ = file_chooser_t::item_t(*doc, candidate, -1 + (bindingIndex+1), ranges);
			else
				*out++ = file_chooser_t::item_t(*doc, candidate, 1 - rank, ranges);
		}
	}
	return out;
}

void file_chooser_t::setup (std::string const& path, std::string const& globString, std::string const& rankString)
{
	_glob_string = globString;
	_rank_string = rankString;

	set_path(path);
}

void file_chooser_t::set_excluded_document (oak::uuid_t const& uuid)
{
	_excluded_document = uuid;
}

void file_chooser_t::set_path (std::string const& path)
{
	if(path == _path)
		return;

	_path = path;

	_all_documents.clear();
	_filtered_documents.clear();
	_ranked_items.clear();

	auto const settings = settings_for_path(NULL_STR, "", path);
	path::glob_list_t globs;
	globs.add_exclude_glob(settings.get(kSettingsExcludeDirectoriesInFileChooserKey, NULL_STR), path::kPathItemDirectory);
	globs.add_exclude_glob(settings.get(kSettingsExcludeFilesInFileChooserKey,       NULL_STR), path::kPathItemFile);
	for(auto key : { kSettingsExcludeInFileChooserKey, kSettingsExcludeKey, kSettingsBinaryKey })
		globs.add_exclude_glob(settings.get(key, NULL_STR));
	globs.add_include_glob("*", path::kPathItemDirectory);
	globs.add_include_glob("{,.}*", path::kPathItemFile);

	_scanner.reset(new document::scanner_t(_path, globs));
}

void file_chooser_t::add_documents (std::vector<document::document_ptr> const& documents)
{
	std::vector<document::document_ptr> filteredDocuments;
	filter(documents, _glob_string, back_inserter(filteredDocuments));
	_filtered_documents.insert(_filtered_documents.end(), filteredDocuments.begin(), filteredDocuments.end());

	auto info = parse_filter_string(_rank_string);
	if(info.empty())
			lru_rank(filteredDocuments, _excluded_document, back_inserter(_ranked_items));
	else	rank(filteredDocuments, _rank_string, _path, _excluded_document, back_inserter(_ranked_items));
	std::sort(_ranked_items.begin(), _ranked_items.end());

	std::vector<std::string> paths;
	iterate(item, _ranked_items)
		paths.push_back(item->_document->path());
	std::vector<size_t> const& visibleParents = path::disambiguate(paths);
	for(size_t i = 0; i < _ranked_items.size(); ++i)
		_ranked_items[i]._parents = visibleParents[i];
}

void file_chooser_t::set_documents (std::vector<document::document_ptr> const& documents)
{
	_all_documents = documents;
	_filtered_documents.clear();
	_ranked_items.clear();

	_path = NULL_STR;
	_scanner.reset();
	add_documents(documents);
}

void file_chooser_t::set_filtering (std::string const& globString, std::string const& rankString)
{
	if(globString == _glob_string && rankString == _rank_string)
		return;

	_glob_string = globString;
	_rank_string = rankString;

	_filtered_documents.clear();
	_ranked_items.clear();

	add_documents(_all_documents);
}

bool file_chooser_t::running () const
{
	return _scanner.get() != NULL;
}

bool file_chooser_t::poll_scanner ()
{
	if(!running())
		return false;

	bool stillRunning = _scanner->is_running();
	std::vector<document::document_ptr> newDocuments = _scanner->accept_documents();
	_all_documents.insert(_all_documents.end(), newDocuments.begin(), newDocuments.end());
	add_documents(newDocuments);

	if(!stillRunning)
		_scanner.reset();

	return stillRunning;
}

void file_chooser_t::stop_scanner ()
{
	_scanner.reset();
}

void file_chooser_t::wait () const
{
	if(running())
		_scanner->wait();
}

// ==========

@interface OakFileChooser ()
@property (nonatomic, retain) OakTimer* scannerProbeTimer;
@property (nonatomic, readonly) NSString* effectivePath;
@property (nonatomic, retain) NSString* title;
- (void)updateTitle;
@end

@interface FileChooserItem : NSObject <NSCopying>
{
	file_chooser_t::item_t data;
	NSString* selectionString;
}
+ (FileChooserItem*)fileChooserItemWithItem:(file_chooser_t::item_t const&)someItem selection:(NSString*)aSelection;
@property (nonatomic, readonly) NSString* path;
@property (nonatomic, readonly) NSString* selectionString;
@property (nonatomic, readonly) NSString* identifier;
@property (nonatomic, readonly) document::document_ptr const& document;
@end

@implementation FileChooserItem
@synthesize selectionString;

+ (FileChooserItem*)fileChooserItemWithItem:(file_chooser_t::item_t const&)someItem selection:(NSString*)aSelection { return [[FileChooserItem alloc] initWithItem:someItem selection:aSelection]; }
- (id)copyWithZone:(NSZone*)zone              { return [[FileChooserItem alloc] initWithItem:data selection:selectionString]; }
- (BOOL)isEqual:(FileChooserItem*)anotherItem { return [self.identifier isEqualToString:anotherItem.identifier]; }
- (id)objectForKey:(id)key                    { return [self valueForKey:key]; }

- (FileChooserItem*)initWithItem:(file_chooser_t::item_t const&)someItem selection:(NSString*)aSelection
{
	if(self = [super init])
	{
		data = someItem;
		selectionString = aSelection;
	}
	return self;
}

- (NSString*)path                         { return [NSString stringWithCxxString:data._document->path()]; }
- (NSString*)identifier                   { return [NSString stringWithCxxString:data._document->identifier()]; }
- (document::document_ptr const&)document { return data._document; }

- (NSAttributedString*)displayString
{
	NSAttributedString* res = AttributedStringWithMarkedUpRanges(data._match_name, data._match_ranges);
	if(data._parents)
	{
		NSMutableAttributedString* prefix = [res mutableCopy];
		NSAttributedString* suffix = [[NSAttributedString alloc] initWithString:[NSString stringWithCxxString:parents(data._document->path(), data._parents)] attributes:nil];
		[prefix appendAttributedString:suffix];
		res = prefix;
	}
	return res;
}

- (NSAttributedString*)infoString
{
	std::string const& name = data._match_name;
	std::string const& path = data._document->path() == NULL_STR ? name : data._document->path();
	std::string::size_type prefixLen = path.rfind(name);
	if(prefixLen == std::string::npos || prefixLen + name.size() != path.size() || prefixLen != 0 && path[prefixLen-1] != '/')
		prefixLen = 0;
	std::string const prefix = prefixLen ? path::with_tilde(path.substr(0, prefixLen)) : "";
	return AttributedStringWithMarkedUpRanges(prefix.empty() ? path : prefix + name, data._match_ranges, prefix.size());
}
@end

@interface FileChooserViewController : NSViewController <NSComboBoxDelegate, NSSplitViewDelegate>
{
	OBJC_WATCH_LEAKS(FileChooserViewController);
	NSSearchField* searchField;
	NSComboBox* globComboBox;
	OakHistoryList* globHistoryList;
	NSSegmentedControl* sourceSelector;
	OakFileChooser* fileChooser;
}
@end

@implementation FileChooserViewController
- (void)updateTitles
{
	[sourceSelector setLabel:[NSString stringWithCxxString:path::display_name(fileChooser.projectPath.UTF8String)] forSegment:0];
	[sourceSelector setLabel:[NSString stringWithCxxString:path::display_name(fileChooser.path.UTF8String)] forSegment:1];
}

- (id)initWithFileChooser:(OakFileChooser*)chooser
{
	if((self = [super init]))
	{
		fileChooser = chooser;

		const CGFloat viewWidth      = 200;
		const CGFloat splitViewWidth = viewWidth - 20;

		globHistoryList = [[OakHistoryList alloc] initWithName:[NSString stringWithFormat:@"Find in Folder Globs.%@", fileChooser.projectPath] stackSize:10 defaultItems:@"*", @"*.txt", @"*.{c,h}", nil];
		fileChooser.globString = globHistoryList.head;

		globComboBox = [[NSComboBox alloc] initWithFrame:NSMakeRect(0, 0, 35, 26)];
		globComboBox.font             = [NSFont userFixedPitchFontOfSize:12];
		globComboBox.delegate         = self;
		globComboBox.autoresizingMask = NSViewWidthSizable;

		[globComboBox bind:@"value" toObject:globHistoryList withKeyPath:@"head" options:nil];
		[globComboBox bind:@"contentValues" toObject:globHistoryList withKeyPath:@"list" options:nil];

		searchField                  = [[NSSearchField alloc] initWithFrame:NSMakeRect(0, 0, splitViewWidth - NSWidth(globComboBox.frame), 0)];
		searchField.action           = @selector(didChangeFilterString:);
		searchField.target           = self;
		searchField.stringValue      = fileChooser.filterString;
		searchField.autoresizingMask = NSViewWidthSizable;
		[searchField.cell setScrollable:YES];

		NSSplitView* splitView     = [[NSSplitView alloc] initWithFrame:NSMakeRect((viewWidth-splitViewWidth)/2, 8, splitViewWidth, 26)];
		splitView.delegate         = self;
		splitView.autoresizingMask = NSViewWidthSizable;
		[splitView setVertical:YES];
		[splitView addSubview:searchField];
		[splitView addSubview:globComboBox];
		splitView.autosaveName = @"File Chooser Splitter Position";

		sourceSelector                       = [[NSSegmentedControl alloc] initWithFrame:NSMakeRect(-1, NSMaxY(splitView.frame) + 5, viewWidth+2, 23)];
		sourceSelector.refusesFirstResponder = YES;
		sourceSelector.target                = self;
		sourceSelector.action                = @selector(takeSourceIndexFrom:);
		sourceSelector.segmentCount          = 3;
		sourceSelector.segmentStyle          = NSSegmentStyleSmallSquare;
		sourceSelector.autoresizingMask      = NSViewWidthSizable;
		sourceSelector.selectedSegment       = fileChooser.sourceIndex;
		[sourceSelector setLabel:@"Open Files" forSegment:2];

		self.view                  = [[NSView alloc] initWithFrame:NSMakeRect(0, 0, viewWidth, NSMaxY(sourceSelector.frame)-3)];
		self.view.autoresizingMask = NSViewWidthSizable;
		[self.view addSubview:splitView];
		[self.view addSubview:sourceSelector];

		[self updateTitles];

		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(viewFrameDidChange:) name:NSViewFrameDidChangeNotification object:self.view];
	}
	return self;
}

- (void)didChangeFilterString:(NSSearchField*)sender { fileChooser.filterString = sender.stringValue; }
- (void)setSearchFieldDelegate:(id)aDelegate         { searchField.delegate = aDelegate; }

- (void)comboBoxSelectionDidChange:(NSNotification*)aNotification
{
	if(aNotification.object == globComboBox)
		fileChooser.globString = globComboBox.stringValue;
}

- (IBAction)takeSourceIndexFrom:(id)sender
{
	sourceSelector.selectedSegment = [sender respondsToSelector:@selector(selectedSegment)] ? [sender selectedSegment] : [[OakSubmenuController sharedInstance] tagForSender:sender];
	fileChooser.sourceIndex        = sourceSelector.selectedSegment;
	[self updateTitles];
}

- (void)viewFrameDidChange:(NSNotification*)notification
{
	// Distribute source segment widths
	for(NSUInteger index = 0; index < sourceSelector.segmentCount; ++index)
		[sourceSelector setWidth:(sourceSelector.superview.frame.size.width / sourceSelector.segmentCount) forSegment:index];
}

- (void)updateGoToMenu:(NSMenu*)aMenu
{
	if(!self.view.window.isKeyWindow)
	{
		[aMenu addItemWithTitle:@"No Sources" action:@selector(nop:) keyEquivalent:@""];
		return;
	}

	for(NSUInteger i = 0; i < sourceSelector.segmentCount; ++i)
	{
		NSMenuItem* item = [aMenu addItemWithTitle:[sourceSelector labelForSegment:i] action:@selector(takeSourceIndexFrom:) keyEquivalent:i < 10 ? [NSString stringWithFormat:@"%c", '0' + (char)((i+1) % 10)] : @""];
		item.tag         = i;
		if(i == sourceSelector.selectedSegment)
			[item setState:NSOnState];
	}
}

- (IBAction)goToParentFolder:(id)sender
{
	fileChooser.path = [fileChooser.path stringByDeletingLastPathComponent];
	[self updateTitles];
}

- (BOOL)validateMenuItem:(NSMenuItem*)item
{
	if([item action] == @selector(goToParentFolder:))
			return fileChooser.sourceIndex == 1 && path::parent(fileChooser.path.UTF8String) != fileChooser.path.UTF8String;
	else	return YES;
}

- (void)dealloc
{
	searchField.target = nil;

	[globComboBox unbind:@"value"];
	[globComboBox unbind:@"contentValues"];

	[[NSNotificationCenter defaultCenter] removeObserver:self];
}
@end

@implementation OakFileChooser
{
	OBJC_WATCH_LEAKS(OakFileChooser)
	NSString* _path;
	NSString* projectPath;

	NSViewController* viewController;

	file_chooser_t helper;
	document::document_ptr document;

	OakTimer* scannerProbeTimer;
	double pollInterval;
	NSUInteger sourceIndex;

	NSString* title;
}
@synthesize scannerProbeTimer, path = _path, projectPath, sourceIndex, title;

- (NSString*)effectivePath
{
	return sourceIndex == 0 ? self.projectPath : self.path;
}

// ============
// = Scanning =
// ============

- (void)startProbing
{
	pollInterval = 0.01;
	self.scannerProbeTimer = [OakTimer scheduledTimerWithTimeInterval:pollInterval target:self selector:@selector(updateFileItems:) repeats:NO];
}

- (void)stopProbing
{
	self.scannerProbeTimer = nil;
	helper.stop_scanner();
}

- (void)updateFileItems:(OakTimer*)timer
{
	bool stillRunning = helper.poll_scanner();
	[[NSNotificationCenter defaultCenter] postNotificationName:FLDataSourceItemsDidChangeNotification object:self];

	if(stillRunning)
	{
		pollInterval = std::min(pollInterval * 2, 0.32);
		self.scannerProbeTimer = [OakTimer scheduledTimerWithTimeInterval:pollInterval target:self selector:@selector(updateFileItems:) repeats:NO];
	}
	else
	{
		[self stopProbing];
	}
}

// ==================
// = Setup/Teardown =
// ==================

- (id)initWithPath:(NSString*)aPath projectPath:(NSString*)project
{
	if((self = [super init]))
	{
		_path                = aPath;
		projectPath          = project;
		[self updateTitle];

		helper.setup([[self effectivePath] fileSystemRepresentation], "*", "");
		[self startProbing];
	}
	return self;
}

+ (id)fileChooserWithPath:(NSString*)aPath projectPath:(NSString*)project
{
	return [[self alloc] initWithPath:aPath projectPath:project];
}

- (NSViewController*)viewController
{
	if(!viewController)
		viewController = [[FileChooserViewController alloc] initWithFileChooser:self];
	return viewController;
}

- (NSButtonCell*)accessoryButton
{
	NSButtonCell* button = [NSButtonCell new];
	[button setButtonType:NSSwitchButton];
	[button setBezelStyle:NSSmallSquareBezelStyle];
	[button setImagePosition:NSImageOnly];
	[button setBordered:NO];
	[button setImage:[NSImage imageNamed:NSImageNameFollowLinkFreestandingTemplate]];
	[button setAlternateImage:[NSImage imageNamed:NSImageNameFollowLinkFreestandingTemplate]];
	return button;
}

- (void)dealloc
{
	[self stopProbing];
	if(document)
		document->close();
}

// ===========================
// = Filter list data source =
// ===========================

- (BOOL)preservesSelectionWhenFiltering
{
	return YES;
}

- (BOOL)allowsMultipleSelection
{
	return YES;
}

- (void)updateTitle
{
	if(sourceIndex == 2)
			self.title = @"Open Files";
	else	self.title = [NSString stringWithCxxString:"Go to File – " + path::with_tilde(self.effectivePath.UTF8String)];
}

- (void)setSourceIndex:(NSUInteger)index
{
	if(index == sourceIndex)
		return;

	sourceIndex = index;
	if(sourceIndex == 2)
			helper.set_documents(document::scanner_t::open_documents());
	else	helper.set_path([[self effectivePath] fileSystemRepresentation]);
	[self startProbing];
	[self updateTitle];
}

- (void)setPath:(NSString*)newPath
{
	if(newPath != _path)
	{
		_path = newPath;
		if(sourceIndex == 1)
		{
			helper.set_path([[self effectivePath] fileSystemRepresentation]);
			[self startProbing];
		}
		[self updateTitle];
	}
}

- (NSString*)filterString
{
	return [NSString stringWithCxxString:helper.rank_string()];
}

- (void)setFilterString:(NSString*)string
{
	auto before = parse_filter_string(helper.rank_string());
	helper.set_filtering(helper.glob_string(), [string UTF8String]);
	auto after = parse_filter_string(helper.rank_string());

	if(before.symbol == NULL_STR && after.symbol != NULL_STR)
	{
		if(before.raw_filter != after.raw_filter)
			[[NSNotificationCenter defaultCenter] postNotificationName:FLDataSourceItemsDidChangeNotification object:self];
		[[NSNotificationCenter defaultCenter] postNotificationName:FLDataSourceItemsShouldDescendNotification object:self];
	}
	else if(before.symbol != NULL_STR && after.symbol == NULL_STR)
		[[NSNotificationCenter defaultCenter] postNotificationName:FLDataSourceItemsShouldAscendNotification object:self];
	else
		[[NSNotificationCenter defaultCenter] postNotificationName:FLDataSourceItemsDidChangeNotification object:self];
}

- (NSString*)globString
{
	return [NSString stringWithCxxString:helper.glob_string()];
}

- (void)setGlobString:(NSString*)string
{
	helper.set_filtering([string UTF8String], helper.rank_string());
	[[NSNotificationCenter defaultCenter] postNotificationName:FLDataSourceItemsDidChangeNotification object:self];
}

- (void)descendIntoItem:(FileChooserItem*)anItem
{
	if(!anItem)
	{
		if(document)
			document->close();
		document.reset();
	}
	else
	{
		struct callback_t : document::open_callback_t
		{
			void show_error (std::string const& path, document::document_ptr document, std::string const& message, oak::uuid_t const& filter)
			{
				fprintf(stderr, "%s: %s\n", path.c_str(), message.c_str());
			}

			void show_document (std::string const& path, document::document_ptr document)
			{
				// [[NSNotificationCenter defaultCenter] postNotificationName:FLDataSourceItemsDidChangeNotification object:self];
			}
		};

		document = anItem.document;
		if(document->try_open(document::open_callback_ptr((document::open_callback_t*)new callback_t)))
			[[NSNotificationCenter defaultCenter] postNotificationName:FLDataSourceItemsDidChangeNotification object:self];
	}
}

- (NSString*)excludeDocumentWithIdentifier
{
	return @""; // this is provided as excludeDocumentWithIdentifier is a read/write property instead of only a setter
}

- (void)setExcludeDocumentWithIdentifier:(NSString*)anExcludeDocumentWithIdentifier
{
	helper.set_excluded_document([anExcludeDocumentWithIdentifier UTF8String]);
	[[NSNotificationCenter defaultCenter] postNotificationName:FLDataSourceItemsDidChangeNotification object:self];
}

- (NSArray*)items
{
	D(DBF_FilterList_OakFileChooser, bug("filter ‘%s’\n", to_s(self.filterString).c_str()););

	auto info = parse_filter_string(helper.rank_string());
	if(info.symbol != NULL_STR && document)
		return SymbolListForDocument(document, info.symbol);

	NSString* selection = [NSString stringWithCxxString:info.selection];
	NSMutableArray* matchingItems = [NSMutableArray array];
	iterate(item, helper.items())
		[matchingItems addObject:[FileChooserItem fileChooserItemWithItem:*item selection:selection]];
	return matchingItems;
}

- (NSAttributedString*)displayStringForItem:(id)item
{
	return [item displayString];
}

- (NSAttributedString*)infoStringForItem:(id)item
{
	NSMutableAttributedString* str = [[item infoString] mutableCopy];
	[str appendAttributedString:[[NSAttributedString alloc] initWithString:[NSString stringWithFormat:@"\t%zu item%s", helper.items().size(), helper.items().size() != 1 ? "s" : ""] attributes:nil]];
	return str;
}

- (void)makeItemsBestFitForCurrentSearch:(NSArray*)theItems
{
	for(FileChooserItem* item in theItems)
		[[OakAbbreviations abbreviationsForName:@"OakFileChooserBindings"] learnAbbreviation:self.filterString forString:item.path];
}

- (BOOL)moreItemsToCome
{
	return helper.running();
}

- (void)waitForAllItems
{
	helper.wait();
}

- (void)stopLoading
{
	[self stopProbing];
}
@end
