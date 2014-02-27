#import "FileChooser.h"
#import "OakAbbreviations.h"
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/OakFileIconImage.h>
#import <OakAppKit/OakUIConstructionFunctions.h>
#import <OakAppKit/OakScopeBarView.h>
#import <OakFoundation/NSString Additions.h>
#import <OakFileBrowser/OFBPathInfoCell.h>
#import <ns/ns.h>
#import <text/format.h>
#import <text/parse.h>
#import <text/ranker.h>
#import <regexp/regexp.h>
#import <oak/algorithm.h>
#import <oak/duration.h>

static NSString* const kUserDefaultsFileChooserSourceIndexKey = @"fileChooserSourceIndex";

NSUInteger const kFileChooserAllSourceIndex                = 0;
NSUInteger const kFileChooserOpenDocumentsSourceIndex      = 1;
NSUInteger const kFileChooserUncommittedChangesSourceIndex = 2;

namespace
{
	struct filter_string_t
	{
		std::string path      = NULL_STR;
		std::string name      = NULL_STR;
		std::string selection = NULL_STR;
		std::string symbol    = NULL_STR;
		std::string raw_path  = NULL_STR;

		filter_string_t (std::string const& str)
		{
			if(str == NULL_STR || str.empty())
				return;

			if(str.find("*") != std::string::npos)
			{
				_is_glob = true;
				_glob = path::glob_t(str);
			}
			else if(regexp::match_t const& m = regexp::search("(?x)  \\A  (?: (?:/(?=.*/))? (.*) / )?  ([^/]*?)  (?: :([\\d+:-x\\+]*) | @(.*) )?  \\z", str))
			{
				_initialized = true;

				path      = m[1];
				name      = m.did_match(2) ? m[2] : "";
				selection = m[3];
				symbol    = m[4];

				raw_path = full_path();

				path = oak::normalize_filter(path);
				name = oak::normalize_filter(name);
			}
		}

		std::string full_path () const
		{
			return (path != NULL_STR ? path + "/" : "") + name;
		}

		bool is_glob () const
		{
			return _is_glob;
		}

		path::glob_t glob () const
		{
			return _glob;
		}

		explicit operator bool () const { return _initialized; }

	private:
		bool _initialized = false;
		bool _is_glob = false;
		path::glob_t _glob = "*";
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

	inline void rank_record (document_record_t& record, filter_string_t const& filter, std::string const& basePath, path::glob_list_t const& glob, std::vector<std::string> const& bindings)
	{
		record.matched = false;
		if(glob.exclude(record.full_path))
			return;

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

@interface FileChooser ()
{
	scm::info_ptr                                 _scmInfo;
	std::vector<document::document_ptr>           _openDocuments;
	std::map<oak::uuid_t, document::document_ptr> _openDocumentsMap;
	oak::uuid_t                                   _currentDocument;
	std::vector<document_record_t>                _records;
	document::scanner_ptr                         _scanner;
}
@property (nonatomic) NSProgressIndicator* progressIndicator;

@property (nonatomic) BOOL                 polling;
@property (nonatomic) NSTimer*             pollTimer;
@property (nonatomic) CGFloat              pollInterval;
@end

@implementation FileChooser
+ (instancetype)sharedInstance
{
	static id sharedInstance = [self new];
	return sharedInstance;
}

- (id)init
{
	if((self = [super init]))
	{
		[self.window setContentBorderThickness:57 forEdge:NSMaxYEdge];
		self.tableView.allowsMultipleSelection = YES;

		NSCell* cell = [OFBPathInfoCell new];
		cell.lineBreakMode = NSLineBreakByTruncatingMiddle;
		[[self.tableView tableColumnWithIdentifier:@"name"] setDataCell:cell];

		OakScopeBarView* scopeBar = [OakScopeBarView new];
		scopeBar.labels = @[ @"All", @"Open Documents", @"Uncommitted Documents" ];

		_progressIndicator = [[NSProgressIndicator alloc] initWithFrame:NSZeroRect];
		_progressIndicator.style                = NSProgressIndicatorSpinningStyle;
		_progressIndicator.controlSize          = NSSmallControlSize;
		_progressIndicator.displayedWhenStopped = NO;

		NSDictionary* views = @{
			@"searchField"        : self.searchField,
			@"aboveScopeBarDark"  : OakCreateHorizontalLine([NSColor grayColor], [NSColor lightGrayColor]),
			@"aboveScopeBarLight" : OakCreateHorizontalLine([NSColor colorWithCalibratedWhite:0.797 alpha:1.000], [NSColor colorWithCalibratedWhite:0.912 alpha:1.000]),
			@"scopeBar"           : scopeBar,
			@"topDivider"         : OakCreateHorizontalLine([NSColor darkGrayColor], [NSColor colorWithCalibratedWhite:0.551 alpha:1.000]),
			@"scrollView"         : self.scrollView,
			@"bottomDivider"      : OakCreateHorizontalLine([NSColor grayColor], [NSColor lightGrayColor]),
			@"statusTextField"    : self.statusTextField,
			@"itemCountTextField" : self.itemCountTextField,
			@"progressIndicator"  : _progressIndicator,
		};

		NSView* contentView = self.window.contentView;
		for(NSView* view in [views allValues])
		{
			[view setTranslatesAutoresizingMaskIntoConstraints:NO];
			[contentView addSubview:view];
		}

		[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(8)-[searchField(>=50)]-(8)-|"                      options:0 metrics:nil views:views]];
		[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[aboveScopeBarDark(==aboveScopeBarLight)]|"          options:0 metrics:nil views:views]];
		[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(8)-[scopeBar]-(>=8)-|" options:NSLayoutFormatAlignAllBaseline metrics:nil views:views]];
		[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[scrollView(==topDivider,==bottomDivider)]|"         options:0 metrics:nil views:views]];
		[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(24)-[statusTextField]-[itemCountTextField]-(4)-[progressIndicator]-(4)-|" options:NSLayoutFormatAlignAllCenterY metrics:nil views:views]];
		[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-(2)-[searchField]-(8)-[aboveScopeBarDark][aboveScopeBarLight]-(3)-[scopeBar]-(4)-[topDivider][scrollView(>=50)][bottomDivider]-(4)-[statusTextField]-(5)-|" options:0 metrics:nil views:views]];

		self.sourceIndex = [[NSUserDefaults standardUserDefaults] integerForKey:kUserDefaultsFileChooserSourceIndexKey];

		[self updateWindowTitle];

		[scopeBar bind:NSValueBinding toObject:self withKeyPath:@"sourceIndex" options:nil];
	}
	return self;
}

- (void)showWindow:(id)sender
{
	if(_path && !_scmInfo)
		[self obtainSCMInfo];
	[super showWindow:sender];
}

- (void)windowWillClose:(NSNotification*)aNotification
{
	[self shutdownScanner];

	_scmInfo.reset();
	_openDocuments.clear();
	_openDocumentsMap.clear();
	_records.clear();

	self.items = @[ ];
}

- (void)updateWindowTitle
{
	NSString* src = nil;
	switch(self.sourceIndex)
	{
		case kFileChooserAllSourceIndex:                src = [self.path stringByAbbreviatingWithTildeInPath]; break;
		case kFileChooserOpenDocumentsSourceIndex:      src = @"Open Documents";                               break;
		case kFileChooserUncommittedChangesSourceIndex: src = @"Uncommitted Documents";                        break;
	}
	self.window.title = [NSString stringWithFormat:@"Go to File — %@", src];
}

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
	if(_sourceIndex != newIndex)
	{
		_sourceIndex = newIndex;
		[self updateWindowTitle];
		[self reload];

		if(_sourceIndex == 0)
				[[NSUserDefaults standardUserDefaults] removeObjectForKey:kUserDefaultsFileChooserSourceIndexKey];
		else	[[NSUserDefaults standardUserDefaults] setObject:@(_sourceIndex) forKey:kUserDefaultsFileChooserSourceIndexKey];
	}
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

- (void)updateRecordsFrom:(NSUInteger)first
{
	filter_string_t filter(to_s(self.filterString));
	std::string const basePath = to_s(_path);
	std::vector<document_record_t const*> include;

	if(filter.is_glob())
	{
		for(size_t i = first; i < _records.size(); ++i)
		{
			auto& record = _records[i];
			record.cover.clear();
			record.display_parents = 0;

			for(auto const& str : { record.name, path::relative_to(record.full_path, basePath), record.full_path })
			{
				if(record.matched = filter.glob().does_match(str))
				{
					record.display = str;
					break;
				}
			}
		}

		for(auto const& record : _records)
		{
			if(record.matched)
				include.push_back(&record);
		}
		std::sort(include.begin(), include.end(), [](document_record_t const* lhs, document_record_t const* rhs){ return lhs->name < rhs->name; });
	}
	else
	{
		path::glob_list_t glob;

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

		for(auto const& record : _records)
		{
			if(record.matched)
				include.push_back(&record);
		}
		std::sort(include.begin(), include.end(), [](document_record_t const* lhs, document_record_t const* rhs){ return (lhs->rank < rhs->rank) || ((lhs->rank == rhs->rank) && ((lhs->lru_rank < rhs->lru_rank) || (lhs->lru_rank == rhs->lru_rank && lhs->name < rhs->name))); });
	}

	NSMutableArray* array = [NSMutableArray arrayWithCapacity:include.size()];
	for(auto record : include)
		[array addObject:record->tableview_item];
	self.items = array;

	if(!_scanner)
		[self updateParents];
}

- (void)updateParents
{
	if(filter_string_t(to_s(self.filterString)).path != NULL_STR)
		return;

	std::vector<std::string> paths;
	for(NSNumber* index in self.items)
	{
		document_record_t const& record = _records[index.unsignedIntValue];
		paths.push_back(record.full_path);
	}

	std::vector<size_t> const& visibleParents = path::disambiguate(paths);
	for(NSUInteger i = 0; i < self.items.count; ++i)
	{
		NSNumber* index = self.items[i];
		_records[index.unsignedIntValue].display_parents = visibleParents[i];
	}

	[self.tableView reloadData];
}

- (void)updateSCMStatus
{
	if(!_scmInfo)
		return;

	if(_sourceIndex == kFileChooserUncommittedChangesSourceIndex)
	{
		[self reloadSCMStatus];
		return;
	}

	NSRange visibleRange = [self.tableView rowsInRect:[self.tableView visibleRect]];
	for(NSUInteger row = visibleRange.location; row < NSMaxRange(visibleRange); ++row)
	{
		NSNumber* index = self.items[row];
		document_record_t const& record = _records[index.unsignedIntValue];
		if(record.full_path != NULL_STR)
		{
			scm::status::type scmStatus = _scmInfo->status(record.full_path);
			if(record.image.scmStatus != scmStatus)
			{
				record.image.scmStatus = scmStatus;
				[self.tableView setNeedsDisplayInRect:[self.tableView rectOfRow:row]];
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

	_scmInfo.reset();
	[self obtainSCMInfo];

	if(_sourceIndex == kFileChooserUncommittedChangesSourceIndex)
		[self reloadSCMStatus];
	if(_sourceIndex != kFileChooserAllSourceIndex)
		return;

	[self shutdownScanner];

	self.items = @[ ];

	_records.clear();
	[self addRecordsForDocuments:_openDocuments];
	settings_t const settings = settings_for_path(NULL_STR, "", to_s(_path));
	_scanner = std::make_shared<document::scanner_t>(to_s(_path), globs_for_path(to_s(_path)), settings.get(kSettingsFollowSymbolicLinksKey, false), false, false);

	_pollInterval = 0.01;
	_pollTimer = [NSTimer scheduledTimerWithTimeInterval:_pollInterval target:self selector:@selector(fetchScannerResults:) userInfo:nil repeats:NO];
	[_progressIndicator startAnimation:self];

	[self updateWindowTitle];
}

- (scm::info_ptr)obtainSCMInfo
{
	if(!_scmInfo && (_scmInfo = scm::info(to_s(_path))))
	{
		_scmInfo->add_callback(^(scm::info_t const& info){
			[self updateSCMStatus];
		});
	}
	return _scmInfo;
}

- (void)reload
{
	switch(_sourceIndex)
	{
		case kFileChooserAllSourceIndex:
		{
			NSString* path = _path;
			_path = nil;
			self.path = path;
		}
		break;

		case kFileChooserOpenDocumentsSourceIndex:
		{
			[self shutdownScanner];

			_records.clear();
			[self addRecordsForDocuments:_openDocuments];
		}
		break;

		case kFileChooserUncommittedChangesSourceIndex:
		{
			[self shutdownScanner];
			[self reloadSCMStatus];
		}
		break;
	}
}

- (void)reloadSCMStatus
{
	std::vector<document::document_ptr> scmStatus;
	if([self obtainSCMInfo])
	{
		for(auto pair : _scmInfo->status())
		{
			if(pair.second & (scm::status::modified|scm::status::added|scm::status::deleted|scm::status::conflicted))
				scmStatus.push_back(document::create(pair.first));
		}
	}

	_records.clear();
	[self addRecordsForDocuments:scmStatus];
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

- (void)updateItems:(id)sender
{
	[self updateRecordsFrom:0];
}

- (void)updateStatusText:(id)sender
{
	if(_scanner)
	{
		std::string path = path::relative_to(_scanner->get_current_path(), to_s(_path));
		[self.statusTextField.cell setLineBreakMode:NSLineBreakByTruncatingMiddle];
		self.statusTextField.stringValue = [NSString stringWithFormat:@"Searching “%@”…", [NSString stringWithCxxString:path]];
	}
	else if(self.tableView.selectedRow == -1)
	{
		self.statusTextField.stringValue = @"";
	}
	else
	{
		NSNumber* index = self.items[self.tableView.selectedRow];
		document_record_t const& record = _records[index.unsignedIntValue];

		std::string path = record.full_path;
		if(path != NULL_STR)
		{
			if(path.find(to_s(self.path)) == 0)
					path = path::relative_to(path, to_s(self.path));
			else	path = path::with_tilde(path);
		}
		else // untitled file
		{
			path = record.display;
		}

		[self.statusTextField.cell setLineBreakMode:NSLineBreakByTruncatingHead];
		self.statusTextField.stringValue = [NSString stringWithCxxString:path];
	}
}

- (NSArray*)selectedItems
{
	NSMutableArray* res = [NSMutableArray array];
	NSIndexSet* indexes = [self.tableView selectedRowIndexes];
	for(NSUInteger i = [indexes firstIndex]; i != NSNotFound; i = [indexes indexGreaterThanIndex:i])
	{
		NSNumber* index = self.items[i];
		document_record_t const& record = _records[index.unsignedIntValue];

		NSMutableDictionary* item = [NSMutableDictionary dictionary];
		item[@"identifier"] = [NSString stringWithCxxString:record.identifier];
		filter_string_t filter(to_s(self.filterString));
		if(filter.selection != NULL_STR)
			item[@"selectionString"] = [NSString stringWithCxxString:filter.selection];
		if(record.full_path != NULL_STR)
			item[@"path"] = [NSString stringWithCxxString:record.full_path];
		[res addObject:item];
	}
	return res;
}

// =========================
// = NSTableViewDataSource =
// =========================

- (id)tableView:(NSTableView*)aTableView objectValueForTableColumn:(NSTableColumn*)aTableColumn row:(NSInteger)rowIndex
{
	if([aTableColumn.identifier isEqualToString:@"name"])
	{
		NSNumber* index = self.items[rowIndex];
		document_record_t const& record = _records[index.unsignedIntValue];

		std::string path = record.display;
		if(record.display_parents)
		{
			auto v = text::split(path::parent(record.full_path), "/");
			v.erase(v.begin(), v.end() - std::min(record.display_parents, v.size()));
			path += " — " + text::join(v, "/");
		}

		NSMutableAttributedString* str = CreateAttributedStringWithMarkedUpRanges(path, record.cover);
		NSMutableParagraphStyle* pStyle = [[NSParagraphStyle defaultParagraphStyle] mutableCopy];
		[pStyle setLineBreakMode:NSLineBreakByTruncatingMiddle];
		[str addAttribute:NSParagraphStyleAttributeName value:pStyle range:NSMakeRange(0, str.length)];
		return str;
	}
	return nil;
}

- (void)tableView:(NSTableView*)aTableView willDisplayCell:(OFBPathInfoCell*)cell forTableColumn:(NSTableColumn*)aTableColumn row:(NSInteger)rowIndex
{
	if(![aTableColumn.identifier isEqualToString:@"name"])
		return;

	NSNumber* index = self.items[rowIndex];
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
	if(self.filterString)
	{
		filter_string_t filter(to_s(self.filterString));

		NSIndexSet* indexes = [self.tableView selectedRowIndexes];
		for(NSUInteger i = [indexes firstIndex]; i != NSNotFound; i = [indexes indexGreaterThanIndex:i])
		{
			NSNumber* index = self.items[i];
			document_record_t const& record = _records[index.unsignedIntValue];

			if(record.full_path != NULL_STR)
				[[OakAbbreviations abbreviationsForName:@"OakFileChooserBindings"] learnAbbreviation:[NSString stringWithCxxString:filter.full_path()] forString:[NSString stringWithCxxString:record.full_path]];
		}
	}

	[super accept:sender];
}

- (IBAction)goToParentFolder:(id)sender
{
	self.path = [_path stringByDeletingLastPathComponent];
}

- (void)updateGoToMenu:(NSMenu*)aMenu
{
	if(self.window.isKeyWindow)
	{
		[[aMenu addItemWithTitle:@"All" action:@selector(takeSourceIndexFrom:) keyEquivalent:@"1"] setTag:kFileChooserAllSourceIndex];
		[[aMenu addItemWithTitle:@"Open Documents" action:@selector(takeSourceIndexFrom:) keyEquivalent:@"2"] setTag:kFileChooserOpenDocumentsSourceIndex];
		[[aMenu addItemWithTitle:@"Uncommitted Documents" action:@selector(takeSourceIndexFrom:) keyEquivalent:@"3"] setTag:kFileChooserUncommittedChangesSourceIndex];
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
		activate = _sourceIndex == kFileChooserAllSourceIndex && to_s(_path) != path::parent(to_s(_path));
	else if([item action] == @selector(takeSourceIndexFrom:))
		[item setState:[item tag] == self.sourceIndex ? NSOnState : NSOffState];
	return activate;
}
@end
