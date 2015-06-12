#import "FileChooser.h"
#import "OakAbbreviations.h"
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/OakFileIconImage.h>
#import <OakAppKit/OakUIConstructionFunctions.h>
#import <OakAppKit/OakRolloverButton.h>
#import <OakAppKit/OakScopeBarView.h>
#import <OakAppKit/NSImage Additions.h>
#import <OakFoundation/NSString Additions.h>
#import <OakFoundation/OakFoundation.h>
#import <OakFileBrowser/OFBPathInfoCell.h>
#import <ns/ns.h>
#import <text/format.h>
#import <text/parse.h>
#import <text/ctype.h>
#import <text/ranker.h>
#import <oak/algorithm.h>
#import <oak/duration.h>

@interface NSObject (FileBrowserDelegate)
- (void)fileBrowser:(id)aFileBrowser closeURL:(NSURL*)anURL;
@end

static NSString* const kUserDefaultsFileChooserSourceIndexKey = @"fileChooserSourceIndex";

NSUInteger const kFileChooserAllSourceIndex                = 0;
NSUInteger const kFileChooserOpenDocumentsSourceIndex      = 1;
NSUInteger const kFileChooserUncommittedChangesSourceIndex = 2;

namespace
{
	struct document_record_t
	{
		document_record_t (document::document_ptr const& doc, std::string const& base)
		{
			full_path = doc->path();
			prefix    = full_path == NULL_STR ? "" : path::relative_to(path::parent(full_path), base);
			name      = full_path == NULL_STR ? doc->display_name() : path::name(full_path);
			lru_rank  = -doc->lru().value();

			if(prefix.empty() && full_path != NULL_STR)
				prefix = ".";

			if(full_path == NULL_STR)
				identifier = doc->identifier();
		}

		oak::uuid_t identifier;
		std::string full_path;
		std::string prefix;
		std::string name;
		double lru_rank;

		bool matched    = true;
		bool is_current = false;
		double rank     = 0;

		std::vector<std::pair<size_t, size_t>> cover_prefix, cover_name;
		NSNumber* tableview_item = nil;
		OakFileIconImage* image = nil;
	};

	inline void rank_record (document_record_t& record, std::string const& filter, path::glob_list_t const& glob, std::vector<std::string> const& bindings)
	{
		record.matched = false;
		if(glob.exclude(record.full_path))
			return;

		record.cover_prefix.clear();
		record.cover_name.clear();

		double rank = record.is_current ? 0.1 : 3;
		if(!filter.empty() && filter != NULL_STR)
		{
			std::vector<std::pair<size_t, size_t>> cover;
			if(rank = oak::rank(filter, record.name, &record.cover_name))
			{
				rank += 1;

				auto it = std::find(bindings.begin(), bindings.end(), record.full_path);
				if(it != bindings.end())
					rank = 2 + (bindings.end() - it) / (double)bindings.size();
			}
			else if(rank = oak::rank(filter, record.prefix + "/" + record.name, &cover))
			{
				for(auto pair : cover)
				{
					if(pair.first < record.prefix.size())
						record.cover_prefix.emplace_back(pair.first, std::min(pair.second, record.prefix.size()));
					if(record.prefix.size() + 1 < pair.second)
						record.cover_name.emplace_back(std::max(pair.first, record.prefix.size() + 1) - record.prefix.size() - 1, pair.second - record.prefix.size() - 1);
				}
			}
		}

		if(rank)
		{
			record.matched = true;
			record.rank = 3 - rank;
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

	NSString* _globString;
	NSString* _filterString;
	NSString* _selectionString;
	NSString* _symbolString;
}
@property (nonatomic) NSArray* sourceListLabels;
@property (nonatomic) NSProgressIndicator* progressIndicator;

@property (nonatomic) BOOL     polling;
@property (nonatomic) NSTimer* pollTimer;
@property (nonatomic) CGFloat  pollInterval;
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
		_sourceListLabels = @[ @"All", @"Open Documents", @"Uncommitted Documents" ];

		[self.window setContentBorderThickness:57 forEdge:NSMaxYEdge];
		self.tableView.allowsMultipleSelection = YES;
		self.tableView.rowHeight = 38;

		OakScopeBarView* scopeBar = [OakScopeBarView new];
		scopeBar.labels = self.sourceListLabels;

		_progressIndicator = [[NSProgressIndicator alloc] initWithFrame:NSZeroRect];
		_progressIndicator.style                = NSProgressIndicatorSpinningStyle;
		_progressIndicator.controlSize          = NSSmallControlSize;
		_progressIndicator.displayedWhenStopped = NO;

		NSDictionary* views = @{
			@"searchField"        : self.searchField,
			@"aboveScopeBarDark"  : OakCreateHorizontalLine([NSColor grayColor], [NSColor lightGrayColor]),
			@"aboveScopeBarLight" : OakCreateHorizontalLine([NSColor colorWithCalibratedWhite:0.797 alpha:1], [NSColor colorWithCalibratedWhite:0.912 alpha:1]),
			@"scopeBar"           : scopeBar,
			@"topDivider"         : OakCreateHorizontalLine([NSColor darkGrayColor], [NSColor colorWithCalibratedWhite:0.551 alpha:1]),
			@"scrollView"         : self.scrollView,
			@"bottomDivider"      : OakCreateHorizontalLine([NSColor grayColor], [NSColor lightGrayColor]),
			@"statusTextField"    : self.statusTextField,
			@"itemCountTextField" : self.itemCountTextField,
			@"progressIndicator"  : _progressIndicator,
		};

		NSView* contentView = self.window.contentView;
		OakAddAutoLayoutViewsToSuperview([views allValues], contentView);
		OakSetupKeyViewLoop(@[ self.searchField, scopeBar.buttons ]);

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

- (IBAction)selectNextTab:(id)sender     { self.sourceIndex = (self.sourceIndex + 1) % self.sourceListLabels.count; }
- (IBAction)selectPreviousTab:(id)sender { self.sourceIndex = (self.sourceIndex + self.sourceListLabels.count - 1) % self.sourceListLabels.count; }

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
			document_record_t record(doc, path);
			record.is_current     = doc->identifier() == _currentDocument;
			record.tableview_item = @(index++);
			_records.push_back(record);
		}
	}

	[self updateRecordsFrom:firstDirty];
}

- (void)updateRecordsFrom:(NSUInteger)first
{
	std::function<bool(document_record_t const*, document_record_t const*)> sortFunctor = [](document_record_t const* lhs, document_record_t const* rhs){ return (lhs->rank < rhs->rank) || ((lhs->rank == rhs->rank) && ((lhs->lru_rank < rhs->lru_rank) || (lhs->lru_rank == rhs->lru_rank && text::less_t()(lhs->name, rhs->name)))); };

	if(OakNotEmptyString(_globString))
	{
		path::glob_t glob(to_s(_globString), false, false);
		for(size_t i = first; i < _records.size(); ++i)
		{
			auto& record = _records[i];
			record.cover_prefix.clear();
			record.cover_name.clear();
			record.matched = glob.does_match(record.full_path);
		}
		sortFunctor = [](document_record_t const* lhs, document_record_t const* rhs){ return text::less_t()(lhs->name, rhs->name); };
	}
	else
	{
		std::string const filter = to_s(_filterString);
		path::glob_list_t glob;

		std::vector<std::string> bindings;
		for(NSString* str in [[OakAbbreviations abbreviationsForName:@"OakFileChooserBindings"] stringsForAbbreviation:_filterString])
			bindings.push_back(to_s(str));

		size_t const count  = _records.size() - first;
		size_t const stride = 256;
		dispatch_apply(count / stride, dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^(size_t n){
			for(size_t i = n*stride; i < (n+1)*stride; ++i)
				rank_record(_records[first + i], filter, glob, bindings);
		});
	   for(size_t i = count - (count % stride); i < count; ++i)
			rank_record(_records[first + i], filter, glob, bindings);
	}

	std::vector<document_record_t const*> include;
	for(auto const& record : _records)
	{
		if(record.matched)
			include.push_back(&record);
	}
	std::sort(include.begin(), include.end(), sortFunctor);

	NSMutableArray* array = [NSMutableArray arrayWithCapacity:include.size()];
	for(auto record : include)
		[array addObject:record->tableview_item];
	self.items = array;
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
	_scanner = std::make_shared<document::scanner_t>(to_s(_path), globs_for_path(to_s(_path)));
	_scanner->set_follow_directory_links(settings.get(kSettingsFollowSymbolicLinksKey, false));
	_scanner->start();

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
	}
}

- (void)shutdownScanner
{
	[_progressIndicator stopAnimation:self];
	[_pollTimer invalidate];
	_pollTimer = nil;
	_scanner.reset();
}

- (void)updateFilterString:(NSString*)aString
{
	NSString* oldFilter = [(_globString ?: _filterString ?: @"") copy];
	aString = [aString decomposedStringWithCanonicalMapping];

	NSRegularExpression* const ptrn = [NSRegularExpression regularExpressionWithPattern:@"\\A(?:(.*?\\*.*?)|(.*?))(?::([\\d+:-x\\+]*)|@(.*))?\\z" options:NSAnchoredSearch error:nil];
	NSTextCheckingResult* m = aString ? [ptrn firstMatchInString:aString options:NSMatchingAnchored range:NSMakeRange(0, [aString length])] : nil;
	_globString      = m && [m rangeAtIndex:1].location != NSNotFound ? [aString substringWithRange:[m rangeAtIndex:1]] : nil;
	_filterString    = m && [m rangeAtIndex:2].location != NSNotFound ? [NSString stringWithCxxString:oak::normalize_filter(to_s([aString substringWithRange:[m rangeAtIndex:2]]))] : nil;
	_selectionString = m && [m rangeAtIndex:3].location != NSNotFound ? [aString substringWithRange:[m rangeAtIndex:3]] : nil;
	_symbolString    = m && [m rangeAtIndex:4].location != NSNotFound ? [aString substringWithRange:[m rangeAtIndex:4]] : nil;

	if(![oldFilter isEqualToString:_globString ?: _filterString ?: @""])
		[super updateFilterString:aString];
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
			path = record.name;
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
		if(OakNotEmptyString(_selectionString))
			item[@"selectionString"] = _selectionString;
		if(record.full_path != NULL_STR)
			item[@"path"] = [NSString stringWithCxxString:record.full_path];
		[res addObject:item];
	}
	return res;
}

// =========================
// = NSTableViewDataSource =
// =========================

- (NSView*)tableView:(NSTableView*)aTableView viewForTableColumn:(NSTableColumn*)aTableColumn row:(NSInteger)row
{
	NSTableCellView* res = [aTableView makeViewWithIdentifier:aTableColumn.identifier owner:self];
	if(!res)
	{
		OakRolloverButton* closeButton = [[OakRolloverButton alloc] initWithFrame:NSZeroRect];
		OakSetAccessibilityLabel(closeButton, @"Close document");

		Class cl = NSClassFromString(@"OFBPathInfoCell");
		closeButton.regularImage  = [NSImage imageNamed:@"CloseTemplate"         inSameBundleAsClass:cl];
		closeButton.pressedImage  = [NSImage imageNamed:@"ClosePressedTemplate"  inSameBundleAsClass:cl];
		closeButton.rolloverImage = [NSImage imageNamed:@"CloseRolloverTemplate" inSameBundleAsClass:cl];
		closeButton.target        = self;
		closeButton.action        = @selector(takeItemToCloseFrom:);

		res = [[OakFileTableCellView alloc] initWithCloseButton:closeButton];
		res.identifier = aTableColumn.identifier;

		[closeButton bind:NSHiddenBinding toObject:res withKeyPath:@"objectValue.isCloseDisabled" options:nil];
	}

	NSNumber* index = self.items[row];
	document_record_t& record = _records[index.unsignedIntValue];

	// =================
	// = Document Icon =
	// =================

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

	if(!record.image)
		record.image = [[OakFileIconImage alloc] initWithSize:NSMakeSize(32, 32)];

	record.image.path     = [NSString stringWithCxxString:record.full_path];
	record.image.exists   = isOnDisk;
	record.image.modified = isModified;

	if(_scmInfo && record.full_path != NULL_STR)
		record.image.scmStatus = _scmInfo->status(record.full_path);

	res.objectValue = @{
		@"icon"            : record.image,
		@"folder"          : CreateAttributedStringWithMarkedUpRanges(record.prefix, record.cover_prefix, NSLineBreakByTruncatingHead),
		@"name"            : CreateAttributedStringWithMarkedUpRanges(record.name, record.cover_name, NSLineBreakByTruncatingTail),
		@"isCloseDisabled" : @(!isOpen),
	};

	return res;
}

// =================
// = Action Method =
// =================

- (void)accept:(id)sender
{
	if(OakNotEmptyString(_filterString))
	{
		NSIndexSet* indexes = [self.tableView selectedRowIndexes];
		for(NSUInteger i = [indexes firstIndex]; i != NSNotFound; i = [indexes indexGreaterThanIndex:i])
		{
			NSNumber* index = self.items[i];
			document_record_t const& record = _records[index.unsignedIntValue];

			if(record.full_path != NULL_STR && record.cover_prefix.empty())
				[[OakAbbreviations abbreviationsForName:@"OakFileChooserBindings"] learnAbbreviation:_filterString forString:[NSString stringWithCxxString:record.full_path]];
		}
	}

	[super accept:sender];
}

- (void)takeItemToCloseFrom:(NSButton*)sender
{
	NSInteger row = [self.tableView rowForView:sender];
	if(row != -1)
	{
		NSNumber* index = self.items[row];
		document_record_t const& record = _records[index.unsignedIntValue];
		if(record.full_path != NULL_STR)
		{
			// FIXME We need a proper interface to close documents
			if(id target = [NSApp targetForAction:@selector(fileBrowser:closeURL:)])
			{
				[target fileBrowser:nil closeURL:[NSURL fileURLWithPath:[NSString stringWithCxxString:record.full_path]]];

				std::vector<document::document_ptr> newDocuments;
				for(auto const& doc : _openDocuments)
				{
					if(doc->path() != record.full_path)
						newDocuments.push_back(doc);
				}
				self.openDocuments = newDocuments;
			}
		}
	}
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
