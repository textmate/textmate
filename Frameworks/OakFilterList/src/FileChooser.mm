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
#import <document/OakDocument.h>
#import <document/OakDocumentController.h>
#import <scm/scm.h>
#import <ns/ns.h>
#import <regexp/glob.h>
#import <text/format.h>
#import <text/parse.h>
#import <text/ctype.h>
#import <text/ranker.h>
#import <settings/settings.h>
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
		document_record_t (OakDocument* doc, std::string const& base)
		{
			full_path = to_s(doc.path);
			prefix    = full_path == NULL_STR ? "" : path::relative_to(path::parent(full_path), base);
			name      = full_path == NULL_STR ? to_s(doc.displayName) : path::name(full_path);
			lru_rank  = [OakDocumentController.sharedInstance lruRankForDocument:doc];

			if(prefix.empty() && full_path != NULL_STR)
				prefix = ".";

			if(full_path == NULL_STR)
				identifier = doc.identifier;
		}

		NSUUID* identifier;
		std::string full_path;
		std::string prefix;
		std::string name;
		NSInteger lru_rank;

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

static NSDictionary* globs_for_path (std::string const& path)
{
	static std::map<std::string, NSString*> const map = {
		{ kSettingsExcludeDirectoriesInFileChooserKey, kSearchExcludeDirectoryGlobsKey },
		{ kSettingsExcludeDirectoriesKey,              kSearchExcludeDirectoryGlobsKey },
		{ kSettingsExcludeFilesInFileChooserKey,       kSearchExcludeFileGlobsKey      },
		{ kSettingsExcludeFilesKey,                    kSearchExcludeFileGlobsKey      },
		{ kSettingsExcludeInFileChooserKey,            kSearchExcludeGlobsKey          },
		{ kSettingsExcludeKey,                         kSearchExcludeGlobsKey          },
		{ kSettingsBinaryKey,                          kSearchExcludeGlobsKey          },
		{ kSettingsIncludeDirectoriesKey,              kSearchDirectoryGlobsKey        },
		{ kSettingsIncludeFilesInFileChooserKey,       kSearchFileGlobsKey             },
		{ kSettingsIncludeFilesKey,                    kSearchFileGlobsKey             },
		{ kSettingsIncludeInFileChooserKey,            kSearchGlobsKey                 },
		{ kSettingsIncludeKey,                         kSearchGlobsKey                 },
	};

	NSDictionary* res = @{
		kSearchExcludeDirectoryGlobsKey : [NSMutableArray array],
		kSearchExcludeFileGlobsKey      : [NSMutableArray array],
		kSearchExcludeGlobsKey          : [NSMutableArray array],
		kSearchDirectoryGlobsKey        : [NSMutableArray array],
		kSearchFileGlobsKey             : [NSMutableArray array],
		kSearchGlobsKey                 : [NSMutableArray array],
	};

	settings_t const settings = settings_for_path(NULL_STR, "", path);
	for(auto const& pair : map)
	{
		if(NSString* glob = to_ns(settings.get(pair.first)))
			[res[pair.second] addObject:glob];
	}
	return res;
}

@interface FileChooser ()
{
	scm::info_ptr                        _scmInfo;
	NSDictionary<NSUUID*, OakDocument*>* _openDocumentsMap;
	std::vector<document_record_t>       _records;

	NSString* _globString;
	NSString* _filterString;
	NSString* _selectionString;
	NSString* _symbolString;

	BOOL _searching;
	NSString* _searchPath;
	NSUInteger _lastSearchToken;
	NSMutableArray<OakDocument*>* _searchResults;
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
		_searchResults = [NSMutableArray array];

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
		OakSetupKeyViewLoop(@[ self.searchField, scopeBar ]);

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
	[self stopSearch];

	_scmInfo.reset();
	_openDocuments = nil;
	_openDocumentsMap = nil;
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
	self.window.title = src ?: @"Open Quickly";
}

- (void)setCurrentDocument:(NSUUID*)identifier
{
	if(_currentDocument == identifier || [_currentDocument isEqual:identifier])
		return;
	_currentDocument = identifier;
	[self reload];
}

- (void)setOpenDocuments:(NSArray<OakDocument*>*)newDocuments
{
	if(_openDocuments == newDocuments || [_openDocuments isEqualToArray:newDocuments])
		return;

	NSMutableDictionary* dict = [NSMutableDictionary dictionary];
	for(OakDocument* doc in newDocuments)
		dict[doc.identifier] = doc;

	_openDocuments    = newDocuments;
	_openDocumentsMap = dict;
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

- (void)addRecordsForDocuments:(NSArray<OakDocument*>*)documents
{
	std::string const path = to_s(_path);
	NSSet<NSString*>* openPaths = [NSSet setWithArray:[[_openDocuments filteredArrayUsingPredicate:[NSPredicate predicateWithFormat:@"path != NULL"]] valueForKey:@"path"]];

	bool insertAll = _records.empty();

	NSUInteger firstDirty = _records.size();
	NSUInteger index      = _records.size();
	for(OakDocument* doc in documents)
	{
		if(insertAll || (!_openDocumentsMap[doc.identifier] && ![openPaths containsObject:doc.path]))
		{
			document_record_t record(doc, path);
			record.is_current     = [doc.identifier isEqual:_currentDocument];
			record.tableview_item = @(index++);
			_records.push_back(record);
		}
	}

	[self updateRecordsFrom:firstDirty];
}

- (void)updateRecordsFrom:(NSUInteger)first
{
	std::function<bool(document_record_t const*, document_record_t const*)> sortFunctor = [](document_record_t const* lhs, document_record_t const* rhs){ return (lhs->rank < rhs->rank) || ((lhs->rank == rhs->rank) && ((lhs->lru_rank > rhs->lru_rank) || (lhs->lru_rank == rhs->lru_rank && text::less_t()(lhs->name, rhs->name)))); };

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

	if(_sourceIndex == kFileChooserAllSourceIndex)
		[self startSearch:_path];
	else if(_sourceIndex == kFileChooserUncommittedChangesSourceIndex)
		[self reloadSCMStatus];
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
			[self startSearch:_path];
		}
		break;

		case kFileChooserOpenDocumentsSourceIndex:
		{
			[self stopSearch];

			_records.clear();
			[self addRecordsForDocuments:_openDocuments];
		}
		break;

		case kFileChooserUncommittedChangesSourceIndex:
		{
			[self stopSearch];
			[self reloadSCMStatus];
		}
		break;
	}
}

- (void)reloadSCMStatus
{
	NSMutableArray<OakDocument*>* scmStatus = [NSMutableArray array];
	if([self obtainSCMInfo])
	{
		for(auto pair : _scmInfo->status())
		{
			if(pair.second & (scm::status::modified|scm::status::added|scm::status::deleted|scm::status::conflicted))
				[scmStatus addObject:[OakDocument documentWithPath:to_ns(pair.first)]];
		}
	}

	_records.clear();
	[self addRecordsForDocuments:scmStatus];
}

- (void)startSearch:(NSString*)path
{
	if(_searching)
		[self stopSearch];

	self.items = @[ ];
	_records.clear();

	if(!path)
		return;

	[self addRecordsForDocuments:_openDocuments];

	settings_t const settings = settings_for_path(NULL_STR, "", to_s(path));
	NSMutableDictionary* options = [globs_for_path(to_s(path)) mutableCopy];
	options[kSearchFollowDirectoryLinksKey] = @(settings.get(kSettingsFollowSymbolicLinksKey, false));

	size_t searchToken = _lastSearchToken;
	_searching = YES;
	@synchronized(_searchResults) {
		[_searchResults removeAllObjects];
	}

	dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
		[OakDocumentController.sharedInstance enumerateDocumentsAtPath:path options:options usingBlock:^(OakDocument* document, BOOL* stop){
			@synchronized(_searchResults) {
				if(searchToken == _lastSearchToken)
						[_searchResults addObject:document];
				else	*stop = YES;
			}
		}];

		dispatch_async(dispatch_get_main_queue(), ^{
			if(searchToken == _lastSearchToken)
			{
				_searching = NO;
				[self handleSearchResults:nil];
			}
		});
	});

	_pollInterval = 0.01;
	_pollTimer = [NSTimer scheduledTimerWithTimeInterval:_pollInterval target:self selector:@selector(handleSearchResults:) userInfo:nil repeats:NO];
	[_progressIndicator startAnimation:self];
}

- (void)handleSearchResults:(NSTimer*)aTimer
{
	@synchronized(_searchResults) {
		if(_searchResults.count || !_searching)
			_searchPath = _searching ? [_searchResults.lastObject.path stringByDeletingLastPathComponent] : nil;
		[self addRecordsForDocuments:_searchResults];
		[_searchResults removeAllObjects];
	}

	if(_searching)
	{
		_pollInterval = std::min(_pollInterval * 2, 0.32);
		_pollTimer = [NSTimer scheduledTimerWithTimeInterval:_pollInterval target:self selector:@selector(handleSearchResults:) userInfo:nil repeats:NO];
	}
	else
	{
		[self stopSearch];
		[self updateStatusText:self];
	}
}

- (void)stopSearch
{
	if(std::exchange(_searching, NO))
		++_lastSearchToken;

	[_progressIndicator stopAnimation:self];
	[_pollTimer invalidate];
	_pollTimer = nil;
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
	if(_searching)
	{
		std::string path = path::relative_to(to_s(_searchPath), to_s(_path));
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
		item[@"identifier"] = record.identifier.UUIDString;
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

		Class cl = NSClassFromString(@"OakFileBrowser");
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
	for(OakDocument* doc in _openDocuments)
	{
		if(doc.path ? to_s(doc.path) == record.full_path : [doc.identifier isEqual:record.identifier])
		{
			isOpen     = YES;
			isModified = doc.isDocumentEdited;
			isOnDisk   = doc.isOnDisk;
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

				NSMutableArray<OakDocument*>* newDocuments = [NSMutableArray array];
				for(OakDocument* doc in _openDocuments)
				{
					if(to_s(doc.path) != record.full_path)
						[newDocuments addObject:doc];
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

- (void)updateSelectTabMenu:(NSMenu*)aMenu
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
