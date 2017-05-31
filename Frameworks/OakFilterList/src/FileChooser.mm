#import "FileChooser.h"
#import "OakAbbreviations.h"
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/OakUIConstructionFunctions.h>
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

@interface FileChooserItem : NSObject
{
	std::string _path;
	std::string _directory;
	std::string _file;
	NSInteger _lruRank;

	BOOL _isCurrent;
	double _rank;

	std::vector<std::pair<size_t, size_t>> _coverDirectory;
	std::vector<std::pair<size_t, size_t>> _coverFile;

	NSAttributedString* _name;
	NSAttributedString* _folder;
}
@property (nonatomic) OakDocument* document;
@property (nonatomic, readonly) NSImage* icon;
@property (nonatomic, readonly) NSAttributedString* name;
@property (nonatomic, readonly) NSAttributedString* folder;
@property (nonatomic, getter = isCloseDisabled, readonly) BOOL closeDisabled;
@property (nonatomic, getter = isMatched) BOOL matched;
@end

@implementation FileChooserItem
+ (NSSet*)keyPathsForValuesAffectingIcon
{
	return [NSSet setWithObjects:@"document.icon", nil];
}

+ (NSSet*)keyPathsForValuesAffectingCloseDisabled
{
	return [NSSet setWithObjects:@"document.open", @"document.path", nil];
}

- (NSImage*)icon
{
	NSImage* image = [_document.icon copy];
	[image setSize:NSMakeSize(32, 32)];
	return image;
}

- (BOOL)isCloseDisabled
{
	return !_document.open || !_document.path;
}

- (instancetype)initWithDocument:(OakDocument*)aDocument base:(std::string const&)base isCurrent:(BOOL)isCurrent
{
	if(self = [super init])
	{
		_document  = aDocument;
		_path      = to_s(_document.path);
		_directory = _document.path ? path::relative_to(path::parent(_path), base) : "";
		_file      = _document.path ? path::name(_path) : to_s(_document.displayName);
		_lruRank   = [OakDocumentController.sharedInstance lruRankForDocument:_document];
		_isCurrent = isCurrent;

		if(_directory.empty() && _document.path)
			_directory = ".";
	}
	return self;
}

- (void)reset
{
	_matched = NO;
	_name    = nil;
	_folder  = nil;
	_rank    = 0;
	_coverDirectory.clear();
	_coverFile.clear();
}

- (void)updateRankUsingFilter:(std::string const&)filter bindings:(std::vector<std::string> const&)bindings
{
	[self reset];

	double rank = _isCurrent ? 0.1 : 3;
	if(!filter.empty() && filter != NULL_STR)
	{
		std::vector<std::pair<size_t, size_t>> cover;
		if(rank = oak::rank(filter, _file, &_coverFile))
		{
			rank += 1;

			auto it = std::find(bindings.begin(), bindings.end(), _path);
			if(it != bindings.end())
				rank = 2 + (bindings.end() - it) / (double)bindings.size();
		}
		else if(rank = oak::rank(filter, _directory + "/" + _file, &cover))
		{
			for(auto pair : cover)
			{
				if(pair.first < _directory.size())
					_coverDirectory.emplace_back(pair.first, std::min(pair.second, _directory.size()));
				if(_directory.size() + 1 < pair.second)
					_coverFile.emplace_back(std::max(pair.first, _directory.size() + 1) - _directory.size() - 1, pair.second - _directory.size() - 1);
			}
		}
	}

	if(rank)
	{
		_matched = YES;
		_rank = 3 - rank;
	}
}

- (void)updateRankUsingGlob:(path::glob_t const&)glob
{
	[self reset];
	_matched = glob.does_match(_path);
}

- (NSAttributedString*)name
{
	if(!_name)
		_name = CreateAttributedStringWithMarkedUpRanges(_file, _coverFile, NSLineBreakByTruncatingTail);
	return _name;
}

- (NSAttributedString*)folder
{
	if(!_folder)
		_folder = CreateAttributedStringWithMarkedUpRanges(_directory, _coverDirectory, NSLineBreakByTruncatingHead);
	return _folder;
}

- (BOOL)isDirectoryMatched
{
	return !_coverDirectory.empty();
}

- (NSComparisonResult)rankCompare:(FileChooserItem*)otherItem
{
	if(_rank < otherItem->_rank)
		return NSOrderedAscending;
	else if(_rank > otherItem->_rank)
		return NSOrderedDescending;
	else if(_lruRank > otherItem->_lruRank)
		return NSOrderedAscending;
	else if(_lruRank < otherItem->_lruRank)
		return NSOrderedDescending;
	return [self compare:otherItem];
}

- (NSComparisonResult)compare:(FileChooserItem*)otherItem
{
	return [to_ns(_file) localizedCompare:to_ns(otherItem->_file)];
}
@end

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
	scm::info_ptr                     _scmInfo;
	NSMutableArray<FileChooserItem*>* _records;

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

@property (nonatomic) NSTimer* pollTimer;
@property (nonatomic) CGFloat  pollInterval;
@end

@implementation FileChooser
+ (instancetype)sharedInstance
{
	static FileChooser* sharedInstance = [self new];
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

		OakBackgroundFillView* aboveScopeBarDark  = OakCreateHorizontalLine([NSColor grayColor], [NSColor lightGrayColor]);
		OakBackgroundFillView* aboveScopeBarLight = OakCreateHorizontalLine([NSColor colorWithCalibratedWhite:0.797 alpha:1], [NSColor colorWithCalibratedWhite:0.912 alpha:1]);
		OakBackgroundFillView* topDivider         = OakCreateHorizontalLine([NSColor darkGrayColor], [NSColor colorWithCalibratedWhite:0.551 alpha:1]);
		OakBackgroundFillView* bottomDivider      = OakCreateHorizontalLine([NSColor grayColor], [NSColor lightGrayColor]);

		NSDictionary* views = @{
			@"searchField"        : self.searchField,
			@"aboveScopeBarDark"  : aboveScopeBarDark,
			@"aboveScopeBarLight" : aboveScopeBarLight,
			@"scopeBar"           : scopeBar,
			@"topDivider"         : topDivider,
			@"scrollView"         : self.scrollView,
			@"bottomDivider"      : bottomDivider,
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

		[contentView addConstraint:[NSLayoutConstraint constraintWithItem:aboveScopeBarLight attribute:NSLayoutAttributeLeft relatedBy:NSLayoutRelationEqual toItem:contentView attribute:NSLayoutAttributeLeft multiplier:1.0 constant:0.0]];
		[contentView addConstraint:[NSLayoutConstraint constraintWithItem:topDivider         attribute:NSLayoutAttributeLeft relatedBy:NSLayoutRelationEqual toItem:contentView attribute:NSLayoutAttributeLeft multiplier:1.0 constant:0.0]];
		[contentView addConstraint:[NSLayoutConstraint constraintWithItem:bottomDivider      attribute:NSLayoutAttributeLeft relatedBy:NSLayoutRelationEqual toItem:contentView attribute:NSLayoutAttributeLeft multiplier:1.0 constant:0.0]];

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
	[super showWindow:sender];
}

- (void)windowWillClose:(NSNotification*)aNotification
{
	[self stopSearch];
	_scmInfo.reset();
	_records = nil;

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

	NSUInteger firstDirty = _records.count;
	for(OakDocument* doc in documents)
		[_records addObject:[[FileChooserItem alloc] initWithDocument:doc base:path isCurrent:[doc.identifier isEqual:_currentDocument]]];

	[self updateRecordsFrom:firstDirty];
}

- (void)updateRecordsFrom:(NSUInteger)first
{
	SEL compareSelector = @selector(compare:);
	if(OakNotEmptyString(_globString))
	{
		path::glob_t const glob(to_s(_globString), false, false);
		[_records enumerateObjectsAtIndexes:[NSIndexSet indexSetWithIndexesInRange:NSMakeRange(first, _records.count - first)] options:NSEnumerationConcurrent usingBlock:^(FileChooserItem* item, NSUInteger idx, BOOL* stop){
			[item updateRankUsingGlob:glob];
		}];
	}
	else
	{
		compareSelector = @selector(rankCompare:);

		std::string const filter = to_s(_filterString);

		std::vector<std::string> bindings;
		for(NSString* str in [[OakAbbreviations abbreviationsForName:@"OakFileChooserBindings"] stringsForAbbreviation:_filterString])
			bindings.push_back(to_s(str));

		[_records enumerateObjectsAtIndexes:[NSIndexSet indexSetWithIndexesInRange:NSMakeRange(first, _records.count - first)] options:NSEnumerationConcurrent usingBlock:^(FileChooserItem* item, NSUInteger idx, BOOL* stop){
			[item updateRankUsingFilter:filter bindings:bindings];
		}];
	}

	NSArray* array = [_records filteredArrayUsingPredicate:[NSPredicate predicateWithFormat:@"isMatched == YES"]];
	self.items = [array sortedArrayUsingSelector:compareSelector];
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

	if(_sourceIndex == kFileChooserAllSourceIndex)
		[self startSearch:_path];
	else if(_sourceIndex == kFileChooserUncommittedChangesSourceIndex)
		[self reloadSCMStatus];
	[self updateWindowTitle];
}

- (void)reload
{
	[self stopSearch];
	_scmInfo.reset();

	switch(_sourceIndex)
	{
		case kFileChooserAllSourceIndex:
		{
			[self startSearch:_path];
		}
		break;

		case kFileChooserOpenDocumentsSourceIndex:
		{
			_records = [NSMutableArray array];
			[self addRecordsForDocuments:[OakDocumentController.sharedInstance openDocuments]];
		}
		break;

		case kFileChooserUncommittedChangesSourceIndex:
		{
			[self reloadSCMStatus];
		}
		break;
	}
}

- (void)reloadSCMStatus
{
	if(!_scmInfo && (_scmInfo = scm::info(to_s(_path))))
	{
		_scmInfo->push_callback(^(scm::info_t const& info){
			if(_sourceIndex == kFileChooserUncommittedChangesSourceIndex)
				[self reloadSCMStatus];
		});
	}

	_records = [NSMutableArray array];
	if(_scmInfo)
	{
		NSMutableArray<OakDocument*>* scmStatus = [NSMutableArray array];
		for(auto pair : _scmInfo->status())
		{
			if(pair.second & (scm::status::modified|scm::status::added|scm::status::deleted|scm::status::conflicted))
				[scmStatus addObject:[OakDocument documentWithPath:to_ns(pair.first)]];
		}
		[self addRecordsForDocuments:scmStatus];
	}
}

- (void)startSearch:(NSString*)path
{
	if(_searching)
		[self stopSearch];

	self.items = @[ ];
	_records = [NSMutableArray array];

	if(!path)
		return;

	settings_t const settings = settings_for_path(NULL_STR, "", to_s(path));
	NSMutableDictionary* options = [globs_for_path(to_s(path)) mutableCopy];
	options[kSearchFollowDirectoryLinksKey] = @(settings.get(kSettingsFollowSymbolicLinksKey, false));
	options[kSearchIgnoreOrderingKey] = @YES;

	size_t searchToken = _lastSearchToken;
	_searching = YES;
	@synchronized(_searchResults) {
		[_searchResults removeAllObjects];
	}

	dispatch_semaphore_t sem = dispatch_semaphore_create(0);

	dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
		__block BOOL didSignal = NO;

		[OakDocumentController.sharedInstance enumerateDocumentsAtPath:path options:options usingBlock:^(OakDocument* document, BOOL* stop){
			@synchronized(_searchResults) {
				if(document.open == NO)
				{
					dispatch_semaphore_signal(sem);
					didSignal = YES;
				}

				if(searchToken == _lastSearchToken)
						[_searchResults addObject:document];
				else	*stop = YES;
			}
		}];

		if(didSignal == NO)
			dispatch_semaphore_signal(sem);

		dispatch_async(dispatch_get_main_queue(), ^{
			if(searchToken == _lastSearchToken)
			{
				_searching = NO;
				[self handleSearchResults:nil];
			}
		});
	});

	dispatch_semaphore_wait(sem, DISPATCH_TIME_FOREVER);
	[self handleSearchResults:nil];

	_pollInterval = 0.02;
	_pollTimer = [NSTimer scheduledTimerWithTimeInterval:_pollInterval target:self selector:@selector(handleSearchResults:) userInfo:nil repeats:NO];
	[_progressIndicator performSelector:@selector(startAnimation:) withObject:self afterDelay:0.2];
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

	[NSObject cancelPreviousPerformRequestsWithTarget:_progressIndicator selector:@selector(startAnimation:) object:self];
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
		FileChooserItem* record = self.items[self.tableView.selectedRow];

		NSString* path = record.document.path;
		if(path)
		{
			if(self.path && [path hasPrefix:self.path])
					path = to_ns(path::relative_to(to_s(path), to_s(self.path)));
			else	path = [path stringByAbbreviatingWithTildeInPath];
		}
		else // untitled file
		{
			path = record.document.displayName;
		}

		[self.statusTextField.cell setLineBreakMode:NSLineBreakByTruncatingHead];
		self.statusTextField.stringValue = path;
	}
}

- (NSArray*)selectedItems
{
	NSMutableArray* res = [NSMutableArray array];
	for(FileChooserItem* record in [self.items objectsAtIndexes:self.tableView.selectedRowIndexes])
	{
		NSMutableDictionary* item = [NSMutableDictionary dictionary];
		if(OakNotEmptyString(_selectionString))
			item[@"selectionString"] = _selectionString;
		if(record.document.path)
				item[@"path"] = record.document.path;
		else	item[@"identifier"] = record.document.identifier.UUIDString;
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
		NSButton* closeButton = OakCreateCloseButton();
		closeButton.target = self;
		closeButton.action = @selector(takeItemToCloseFrom:);

		res = [[OakFileTableCellView alloc] initWithCloseButton:closeButton];
		res.identifier = aTableColumn.identifier;

		[closeButton bind:NSHiddenBinding toObject:res withKeyPath:@"objectValue.closeDisabled" options:nil];
	}

	res.objectValue = self.items[row];
	return res;
}

// =================
// = Action Method =
// =================

- (void)accept:(id)sender
{
	if(OakNotEmptyString(_filterString))
	{
		for(FileChooserItem* item in [self.items objectsAtIndexes:self.tableView.selectedRowIndexes])
		{
			if(!item.isDirectoryMatched && item.document.path)
				[[OakAbbreviations abbreviationsForName:@"OakFileChooserBindings"] learnAbbreviation:_filterString forString:item.document.path];
		}
	}

	[super accept:sender];
}

- (void)takeItemToCloseFrom:(NSButton*)sender
{
	NSInteger row = [self.tableView rowForView:sender];
	if(row != -1)
	{
		FileChooserItem* item = self.items[row];
		if(item.document.path)
		{
			// FIXME We need a proper interface to close documents
			if(id target = [NSApp targetForAction:@selector(fileBrowser:closeURL:)])
				[target fileBrowser:nil closeURL:[NSURL fileURLWithPath:item.document.path]];
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
