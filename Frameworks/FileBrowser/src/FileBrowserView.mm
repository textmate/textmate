#import "FileBrowserView.h"
#import "FileBrowserNotifications.h"
#import "FileItem.h"
#import "FileItemTableCellView.h"
#import "FileBrowserOutlineView.h"
#import "SCMManager.h"
#import "FSEventsManager.h"
#import "OFB/OFBHeaderView.h"
#import "OFB/OFBActionsView.h"
#import <MenuBuilder/MenuBuilder.h>
#import <OakAppKit/OakUIConstructionFunctions.h>
#import <OakAppKit/NSMenuItem Additions.h>
#import <Preferences/Keys.h>
#import <io/path.h>
#import <io/resource.h>
#import <ns/ns.h>
#import <regexp/glob.h>
#import <settings/settings.h>
#import <text/utf8.h>

static NSMutableIndexSet* MutableLongestCommonSubsequence (NSArray* lhs, NSArray* rhs)
{
	NSInteger width  = lhs.count + 1;
	NSInteger height = rhs.count + 1;

	NSInteger* matrix = new NSInteger[width * height];

	for(NSInteger i = lhs.count; i >= 0; --i)
	{
		for(NSInteger j = rhs.count; j >= 0; --j)
		{
			if(i == lhs.count || j == rhs.count)
				matrix[width*i + j] = 0;
			else if([lhs[i] isEqual:rhs[j]])
				matrix[width*i + j] = matrix[width*(i+1) + j+1] + 1;
			else
				matrix[width*i + j] = MAX(matrix[width*(i+1) + j], matrix[width*i + j+1]);
		}
	}

	NSMutableIndexSet* res = [NSMutableIndexSet indexSet];
	for(NSInteger i = 0, j = 0; i < lhs.count && j < rhs.count; )
	{
		if([lhs[i] isEqual:rhs[j]])
		{
			[res addIndex:i];
			i++;
			j++;
		}
		else if(matrix[width*i + j+1] < matrix[width*(i+1) + j])
		{
			i++;
		}
		else
		{
			j++;
		}
	}

	delete [] matrix;

	return res;
}

@interface FileBrowserView () <NSAccessibilityGroup, NSOutlineViewDataSource, NSOutlineViewDelegate, NSTextFieldDelegate, QLPreviewPanelDataSource>
{
	NSUndoManager* _fileBrowserUndoManager;
	NSArray<FileItem*>* _previewItems;

	NSMutableDictionary<NSURL*, id>* _fileItemObservers;

	NSMutableSet<NSURL*>* _loadingURLs;
	NSArray<void(^)()>* _loadingURLsCompletionHandlers;

	NSMutableSet<NSURL*>* _expandedURLs;
	NSMutableSet<NSURL*>* _selectedURLs;

	NSInteger _expandingChildrenCounter;
	NSInteger _collapsingChildrenCounter;
	NSInteger _nestedCollapsingChildrenCounter;
}
@property (nonatomic, readwrite) FileItem* fileItem;
@property (nonatomic) NSScrollView* scrollView;
@property (nonatomic) BOOL canExpandSymbolicLinks;
@property (nonatomic) BOOL canExpandPackages;
@property (nonatomic) BOOL sortDirectoriesBeforeFiles;
@end

@implementation FileBrowserView
- (instancetype)initWithFrame:(NSRect)aRect
{
	if(self = [super initWithFrame:aRect])
	{
		self.accessibilityRole  = NSAccessibilityGroupRole;
		self.accessibilityLabel = @"File browser";

		_fileItemObservers = [NSMutableDictionary dictionary];
		_loadingURLs       = [NSMutableSet set];

		_canExpandSymbolicLinks     = [NSUserDefaults.standardUserDefaults boolForKey:kUserDefaultsAllowExpandingLinksKey];
		_canExpandPackages          = [NSUserDefaults.standardUserDefaults boolForKey:kUserDefaultsAllowExpandingPackagesKey];
		_sortDirectoriesBeforeFiles = [NSUserDefaults.standardUserDefaults boolForKey:kUserDefaultsFoldersOnTopKey];

		_expandedURLs  = [NSMutableSet set];
		_selectedURLs  = [NSMutableSet set];

		_headerView    = [[OFBHeaderView alloc] initWithFrame:NSZeroRect];
		_actionsView   = [[OFBActionsView alloc] initWithFrame:NSZeroRect];

		_outlineView = [[FileBrowserOutlineView alloc] initWithFrame:NSZeroRect];
		_outlineView.accessibilityLabel       = @"Files";
		_outlineView.allowsMultipleSelection  = YES;
		_outlineView.autoresizesOutlineColumn = NO;
		_outlineView.dataSource               = self;
		_outlineView.delegate                 = self;
		_outlineView.focusRingType            = NSFocusRingTypeNone;
		_outlineView.headerView               = nil;

		[_outlineView setDraggingSourceOperationMask:NSDragOperationLink|NSDragOperationMove|NSDragOperationCopy forLocal:YES];
		[_outlineView setDraggingSourceOperationMask:NSDragOperationEvery forLocal:NO];
		[_outlineView registerForDraggedTypes:@[ NSFilenamesPboardType ]];

		NSTableColumn* tableColumn = [[NSTableColumn alloc] init];
		[_outlineView addTableColumn:tableColumn];
		[_outlineView setOutlineTableColumn:tableColumn];
		[_outlineView sizeLastColumnToFit];

		_scrollView = [[NSScrollView alloc] initWithFrame:NSZeroRect];
		_scrollView.borderType            = NSNoBorder;
		_scrollView.documentView          = _outlineView;
		_scrollView.hasHorizontalScroller = NO;
		_scrollView.hasVerticalScroller   = YES;

		NSView* dividerView = OakCreateHorizontalLine(OakBackgroundFillViewStyleDivider);

		NSDictionary* views = @{
			@"header":  _headerView,
			@"files":   _scrollView,
			@"divider": dividerView,
			@"actions": _actionsView,
		};

		OakAddAutoLayoutViewsToSuperview(views.allValues, self);
		[_headerView removeFromSuperview];
		[self addSubview:_headerView positioned:NSWindowAbove relativeTo:nil];

		OakSetupKeyViewLoop(@[ self, _headerView, _outlineView, _actionsView ], NO);

		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[files(==header,==divider,==actions)]|" options:0 metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[header]-(>=0)-[divider]"               options:NSLayoutFormatAlignAllLeft metrics:nil views:views]];

		if(@available(macos 10.14, *))
		{
			// The OakBackgroundFillViewStyleHeader is only using transparent header material on 10.14 and later
			[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[files][divider][actions]|" options:NSLayoutFormatAlignAllLeft metrics:nil views:views]];

			NSEdgeInsets insets = _scrollView.contentInsets;
			insets.top += _headerView.fittingSize.height;
			_scrollView.automaticallyAdjustsContentInsets = NO;
			_scrollView.contentInsets = insets;

			_outlineView.backgroundColor = NSColor.clearColor;
			_scrollView.drawsBackground  = NO;
		}
		else
		{
			[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[header][files][divider][actions]|" options:NSLayoutFormatAlignAllLeft metrics:nil views:views]];
		}

		[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(userDefaultsDidChange:) name:NSUserDefaultsDidChangeNotification object:NSUserDefaults.standardUserDefaults];
	}
	return self;
}

- (void)dealloc
{
	[NSNotificationCenter.defaultCenter removeObserver:self name:NSUserDefaultsDidChangeNotification object:NSUserDefaults.standardUserDefaults];

	for(id observer in _fileItemObservers.allValues)
		[FileItem removeObserver:observer];
	_fileItemObservers = nil;
}

- (void)userDefaultsDidChange:(id)sender
{
	self.canExpandSymbolicLinks     = [NSUserDefaults.standardUserDefaults boolForKey:kUserDefaultsAllowExpandingLinksKey];
	self.canExpandPackages          = [NSUserDefaults.standardUserDefaults boolForKey:kUserDefaultsAllowExpandingPackagesKey];
	self.sortDirectoriesBeforeFiles = [NSUserDefaults.standardUserDefaults boolForKey:kUserDefaultsFoldersOnTopKey];
}

- (void)setCanExpandSymbolicLinks:(BOOL)flag
{
	if(_canExpandSymbolicLinks == flag)
		return;
	_canExpandSymbolicLinks = flag;

	if(!self.fileItem)
		return;

	NSMutableArray<FileItem*>* stack = [self.fileItem.arrangedChildren mutableCopy];
	while(FileItem* item = stack.firstObject)
	{
		[stack removeObjectAtIndex:0];
		if(item.isLinkToDirectory && (_canExpandPackages || !item.isLinkToPackage))
			[_outlineView reloadItem:item reloadChildren:YES];
		if([_outlineView isExpandable:item] && item.arrangedChildren)
			[stack addObjectsFromArray:item.arrangedChildren];
	}
}

- (void)setCanExpandPackages:(BOOL)flag
{
	if(_canExpandPackages == flag)
		return;
	_canExpandPackages = flag;

	if(!self.fileItem)
		return;

	NSMutableArray<FileItem*>* stack = [self.fileItem.arrangedChildren mutableCopy];
	while(FileItem* item = stack.firstObject)
	{
		[stack removeObjectAtIndex:0];
		if(item.isDirectory && item.isPackage)
			[_outlineView reloadItem:item reloadChildren:YES];
		if([_outlineView isExpandable:item] && item.arrangedChildren)
			[stack addObjectsFromArray:item.arrangedChildren];
	}
}

- (void)setSortDirectoriesBeforeFiles:(BOOL)flag
{
	if(_sortDirectoriesBeforeFiles == flag)
		return;
	_sortDirectoriesBeforeFiles = flag;

	if(!self.fileItem)
		return;

	NSMutableArray<FileItem*>* stack = [NSMutableArray arrayWithObject:self.fileItem];
	while(FileItem* item = stack.firstObject)
	{
		[stack removeObjectAtIndex:0];
		[self rearrangeChildrenInParent:item];
		if(item == self.fileItem || [_outlineView isItemExpanded:item])
			[stack addObjectsFromArray:item.arrangedChildren];
	}
}

- (void)setShowExcludedItems:(BOOL)flag
{
	if(_showExcludedItems == flag)
		return;
	_showExcludedItems = flag;

	if(!self.fileItem)
		return;

	NSMutableArray<FileItem*>* stack = [NSMutableArray arrayWithObject:self.fileItem];
	while(FileItem* item = stack.firstObject)
	{
		[stack removeObjectAtIndex:0];
		[self rearrangeChildrenInParent:item];
		if(item == self.fileItem || [_outlineView isItemExpanded:item])
			[stack addObjectsFromArray:item.arrangedChildren];
	}
}

- (NSComparator)itemComparator
{
	NSArray<NSSortDescriptor*>* sortDescriptors = @[
		[NSSortDescriptor sortDescriptorWithKey:@"localizedName" ascending:YES selector:@selector(localizedCompare:)],
		[NSSortDescriptor sortDescriptorWithKey:@"URL.URLByDeletingLastPathComponent.lastPathComponent" ascending:YES selector:@selector(localizedCompare:)],
	];

	return ^NSComparisonResult(FileItem* lhs, FileItem* rhs){
		if(_sortDirectoriesBeforeFiles)
		{
			if((lhs.isDirectory || lhs.isLinkToDirectory) && !(rhs.isDirectory || rhs.isLinkToDirectory))
				return NSOrderedAscending;
			else if((rhs.isDirectory || rhs.isLinkToDirectory) && !(lhs.isDirectory || lhs.isLinkToDirectory))
				return NSOrderedDescending;
		}

		for(NSSortDescriptor* sortDescriptor in sortDescriptors)
		{
			NSComparisonResult order = [sortDescriptor compareObject:lhs toObject:rhs];
			if(order != NSOrderedSame)
				return order;
		}

		return NSOrderedSame;
	};
}

- (NSPredicate*)itemPredicateForChildrenInParent:(FileItem*)parentOrNil
{
	NSPredicate* predicate = [NSPredicate predicateWithValue:YES];
	if(!_showExcludedItems)
	{
		NSURL* directoryURL = (parentOrNil ?: self.fileItem).URL;
		settings_t const settings = settings_for_path(NULL_STR, "", directoryURL.fileSystemRepresentation);
		bool excludeMissingFiles = [directoryURL.scheme isEqual:@"scm"] ? false : settings.get(kSettingsExcludeSCMDeletedKey, false);

		path::glob_list_t globs;
		globs.add_exclude_glob(settings.get(kSettingsExcludeDirectoriesInBrowserKey), path::kPathItemDirectory);
		globs.add_exclude_glob(settings.get(kSettingsExcludeDirectoriesKey),          path::kPathItemDirectory);
		globs.add_exclude_glob(settings.get(kSettingsExcludeFilesInBrowserKey),       path::kPathItemFile);
		globs.add_exclude_glob(settings.get(kSettingsExcludeFilesKey),                path::kPathItemFile);
		globs.add_exclude_glob(settings.get(kSettingsExcludeInBrowserKey),            path::kPathItemAny);
		globs.add_exclude_glob(settings.get(kSettingsExcludeKey),                     path::kPathItemAny);

		globs.add_include_glob(settings.get(kSettingsIncludeDirectoriesInBrowserKey), path::kPathItemDirectory);
		globs.add_include_glob(settings.get(kSettingsIncludeDirectoriesKey),          path::kPathItemDirectory);
		globs.add_include_glob(settings.get(kSettingsIncludeFilesInBrowserKey),       path::kPathItemFile);
		globs.add_include_glob(settings.get(kSettingsIncludeFilesKey),                path::kPathItemFile);
		globs.add_include_glob(settings.get(kSettingsIncludeInBrowserKey),            path::kPathItemAny);
		globs.add_include_glob(settings.get(kSettingsIncludeKey, "*"),                path::kPathItemAny);

		predicate = [NSPredicate predicateWithBlock:^BOOL(FileItem* item, NSDictionary* bindings){
			if(item.hidden && ![item.URL.lastPathComponent hasPrefix:@"."])
				return NO;

			if(excludeMissingFiles && item.isMissing)
				return NO;

			char const* path = item.URL.fileSystemRepresentation;
			size_t itemType  = item.isDirectory ? path::kPathItemDirectory : path::kPathItemFile;
			return item.hidden ? globs.include(path, itemType) : !globs.exclude(path, itemType);
		}];
	}
	return predicate;
}

- (NSArray<FileItem*>*)arrangeChildren:(NSArray<FileItem*>*)children inParent:(FileItem*)parentOrNil
{
	return [[children filteredArrayUsingPredicate:[self itemPredicateForChildrenInParent:parentOrNil]] sortedArrayUsingComparator:self.itemComparator];
}

- (void)rearrangeChildrenInParent:(FileItem*)item
{
	NSMutableArray<FileItem*>* existingChildren = item.arrangedChildren;
	if(existingChildren && existingChildren.count * item.children.count < 250000)
	{
		NSArray* newArrangedChildren = [self arrangeChildren:item.children inParent:item];

		// ================
		// = Remove Items =
		// ================

		NSMutableIndexSet* indexesToRemove = [NSMutableIndexSet indexSet];
		for(NSUInteger i = 0; i < existingChildren.count; ++i)
		{
			if(![newArrangedChildren containsObject:existingChildren[i]])
				[indexesToRemove addIndex:i];
		}

		if(indexesToRemove.count)
		{
			BOOL wasFirstResponderInOutlineView = [_outlineView.window.firstResponder isKindOfClass:[NSView class]] && [(NSView*)_outlineView.window.firstResponder isDescendantOf:_outlineView];

			[existingChildren removeObjectsAtIndexes:indexesToRemove];
			[_outlineView removeItemsAtIndexes:indexesToRemove inParent:(item != self.fileItem ? item : nil) withAnimation:NSTableViewAnimationEffectFade|NSTableViewAnimationSlideUp];

			if(wasFirstResponderInOutlineView && !([_outlineView.window.firstResponder isKindOfClass:[NSView class]] && [(NSView*)_outlineView.window.firstResponder isDescendantOf:_outlineView]))
				[_outlineView.window makeFirstResponder:_outlineView];
		}

		// =======================
		// = Move Items (rename) =
		// =======================

		NSComparator compare = self.itemComparator;

		BOOL alreadySorted = YES;
		for(NSUInteger i = 1; alreadySorted && i < existingChildren.count; ++i)
			alreadySorted = compare(existingChildren[i-1], existingChildren[i]) != NSOrderedDescending;

		if(!alreadySorted)
		{
			NSMutableIndexSet* lcs = MutableLongestCommonSubsequence(existingChildren, newArrangedChildren);

			std::vector<std::pair<BOOL, FileItem*>> v;
			for(NSUInteger i = 0; i < existingChildren.count; ++i)
				v.emplace_back([lcs containsIndex:i], existingChildren[i]);

			for(NSUInteger i = 0; i < v.size(); )
			{
				if(v[i].first == YES)
				{
					i++;
				}
				else
				{
					FileItem* child = existingChildren[i];

					v.erase(v.begin() + i);
					NSInteger newIndex = 0;
					for(; newIndex < v.size(); ++newIndex)
					{
						if(v[newIndex].first && compare(child, v[newIndex].second) == NSOrderedAscending)
							break;
					}
					v.emplace(v.begin() + newIndex, YES, child);

					[existingChildren removeObjectAtIndex:i];
					[existingChildren insertObject:child atIndex:newIndex];
					[self.outlineView moveItemAtIndex:i inParent:(item != self.fileItem ? item : nil) toIndex:newIndex inParent:(item != self.fileItem ? item : nil)];
				}
			}
		}

		// ================
		// = Insert Items =
		// ================

		NSMutableIndexSet* insertionIndexes = [NSMutableIndexSet indexSet];
		for(NSUInteger i = 0; i < newArrangedChildren.count; ++i)
		{
			FileItem* child = newArrangedChildren[i];
			if(![existingChildren containsObject:child])
				[insertionIndexes addIndex:i];
		}

		if(insertionIndexes.count)
		{
			[existingChildren insertObjects:[newArrangedChildren objectsAtIndexes:insertionIndexes] atIndexes:insertionIndexes];
			[_outlineView insertItemsAtIndexes:insertionIndexes inParent:(item != self.fileItem ? item : nil) withAnimation:NSTableViewAnimationEffectFade|NSTableViewAnimationSlideUp];
		}
	}
	else
	{
		item.arrangedChildren = [[self arrangeChildren:item.children inParent:item] mutableCopy];
		[_outlineView reloadItem:(item != self.fileItem ? item : nil) reloadChildren:YES];

		if(item == self.fileItem)
			[_outlineView setNeedsDisplay:YES];
	}

	[self updateDisambiguationSuffixInParent:item];
}

- (NSString*)disambiguationSuffixForURL:(NSURL*)url numberOfParents:(NSInteger)numberOfParents
{
	NSMutableArray* parentNames = [NSMutableArray array];
	for(NSUInteger i = 0; i < numberOfParents; ++i)
	{
		NSNumber* flag;
		if([url getResourceValue:&flag forKey:NSURLIsVolumeKey error:nil] && [flag boolValue])
			return nil;

		NSURL* parentURL;
		if(![url getResourceValue:&parentURL forKey:NSURLParentDirectoryURLKey error:nil] || [url isEqual:parentURL])
			return nil;

		NSString* parentName;
		if(![parentURL getResourceValue:&parentName forKey:NSURLLocalizedNameKey error:nil])
			return nil;

		[parentNames addObject:parentName];
		url = parentURL;
	}
	return [[parentNames.reverseObjectEnumerator allObjects] componentsJoinedByString:@"/"];
}

- (void)updateDisambiguationSuffixInParent:(FileItem*)item
{
	NSArray* children = [item.arrangedChildren filteredArrayUsingPredicate:[NSPredicate predicateWithFormat:@"URL.isFileURL == YES"]];
	for(FileItem* child in children)
		child.disambiguationSuffix = nil;

	NSInteger showNumberOfParents = 1;
	while(children.count)
	{
		NSCountedSet* countOfConflicts = [[NSCountedSet alloc] initWithArray:[children valueForKeyPath:@"displayName"]];
		NSMutableArray* conflictedChildren = [NSMutableArray array];
		for(FileItem* child in children)
		{
			if([countOfConflicts countForObject:child.displayName] == 1)
				continue;

			if(NSString* newSuffix = [self disambiguationSuffixForURL:child.URL numberOfParents:showNumberOfParents])
			{
				child.disambiguationSuffix = [@" — " stringByAppendingString:newSuffix];
				[conflictedChildren addObject:child];
			}
		}
		children = conflictedChildren;
		++showNumberOfParents;
	}
}

- (void)setModifiedURLs:(NSArray<NSURL*>*)newModifiedURLs
{
	_modifiedURLs = newModifiedURLs;

	if(!self.fileItem)
		return;

	NSMutableArray<FileItem*>* stack = [NSMutableArray arrayWithObject:self.fileItem];
	while(FileItem* item = stack.firstObject)
	{
		[stack removeObjectAtIndex:0];
		item.modified = [_modifiedURLs containsObject:item.URL];
		if(item.children)
			[stack addObjectsFromArray:item.children];
	}
}

- (void)setOpenURLs:(NSArray<NSURL*>*)newOpenURLs
{
	_openURLs = newOpenURLs;

	if(!self.fileItem)
		return;

	NSMutableArray<FileItem*>* stack = [NSMutableArray arrayWithObject:self.fileItem];
	while(FileItem* item = stack.firstObject)
	{
		[stack removeObjectAtIndex:0];
		item.open = [_openURLs containsObject:item.URL];
		if(item.children)
			[stack addObjectsFromArray:item.children];
	}
}

// ===========================
// = Loading/Expanding Items =
// ===========================

- (void)setFileItem:(FileItem*)item
{
	if(_fileItem)
	{
		// Remove visible but non-selected/expanded items from pending selection/expansion
		_expandedURLs = [self.expandedURLs mutableCopy];
		_selectedURLs = [self.selectedURLs mutableCopy];

		for(id observer in _fileItemObservers.allValues)
			[FileItem removeObserver:observer];
		_fileItemObservers = [NSMutableDictionary dictionary];
	}

	_fileItem = item;

	[_outlineView reloadItem:nil reloadChildren:YES];
	[_outlineView deselectAll:self];
	[_outlineView scrollRowToVisible:0];

	[self loadChildrenForItem:item expandChildren:NO];
}

- (void)outlineViewItemDidExpand:(NSNotification*)aNotification
{
	FileItem* item = aNotification.userInfo[@"NSObject"];
	[self loadChildrenForItem:item expandChildren:_expandingChildrenCounter > 0];
}

- (void)loadChildrenForItem:(FileItem*)item expandChildren:(BOOL)flag
{
	if(item.arrangedChildren || item.children)
		return;

	NSURL* url = item.URL;

	if(_fileItemObservers[url])
	{
		NSLog(@"%s *** already has observer for %@", sel_getName(_cmd), item.URL);
		return;
	}

	[_loadingURLs addObject:url];

	__weak FileBrowserView* weakSelf = self;
	_fileItemObservers[url] = [FileItem addObserverToDirectoryAtURL:item.resolvedURL usingBlock:^(NSArray<NSURL*>* urls){
		[weakSelf didReceiveURLs:urls forItemWithURL:url expandChildren:flag];
	}];
}

- (FileItem*)findItemForURL:(NSURL*)url
{
	NSMutableArray<FileItem*>* stack = [NSMutableArray arrayWithObject:self.fileItem];
	while(FileItem* item = stack.firstObject)
	{
		[stack removeObjectAtIndex:0];
		if([item.URL isEqual:url])
			return item;
		if(NSArray<FileItem*>* children = item.arrangedChildren)
			[stack addObjectsFromArray:children];
	}
	return nil;
}

- (void)didReceiveURLs:(NSArray<NSURL*>*)urls forItemWithURL:(NSURL*)url expandChildren:(BOOL)flag
{
	FileItem* item = [self findItemForURL:url];
	if(!item)
	{
		NSLog(@"%s *** unable to find item for %@", sel_getName(_cmd), url);
	}
	else if(item != self.fileItem && ![_outlineView isItemExpanded:item])
	{
		NSLog(@"%s *** item no longer expanded: %@", sel_getName(_cmd), item);

		item.children = nil;
		item.arrangedChildren = nil;
		[_outlineView reloadItem:item reloadChildren:YES];

		[FileItem removeObserver:_fileItemObservers[url]];
		_fileItemObservers[url] = nil;
	}
	else
	{
		NSMutableArray* children = [NSMutableArray array];

		if(item.children)
		{
			NSMutableSet<NSURL*>* newURLs = [NSMutableSet setWithArray:urls];

			for(FileItem* child in item.children)
			{
				if(NSURL* url = child.fileReferenceURL.filePathURL)
					child.URL = url;

				if([newURLs containsObject:child.URL])
				{
					[newURLs removeObject:child.URL];
					[child updateFileProperties];
					[children addObject:child];
				}
			}

			urls = newURLs.allObjects;
		}

		for(NSURL* url in urls)
		{
			FileItem* newItem = [FileItem fileItemWithURL:url];
			newItem.open     = [_openURLs containsObject:url];
			newItem.modified = [_modifiedURLs containsObject:url];
			[children addObject:newItem];
		}

		item.children = [children copy];
		[self rearrangeChildrenInParent:item];

		for(FileItem* child in item.arrangedChildren)
		{
			if((flag && !child.isSymbolicLink || [_expandedURLs containsObject:child.URL] || [child.URL.scheme isEqualToString:@"scm"]) && [_outlineView isExpandable:child])
				[_outlineView expandItem:child expandChildren:flag && !child.isSymbolicLink];

			if([_selectedURLs containsObject:child.URL])
			{
				[_outlineView selectRowIndexes:[NSIndexSet indexSetWithIndex:[_outlineView rowForItem:child]] byExtendingSelection:YES];
				[_selectedURLs removeObject:child.URL];
			}
		}
	}

	[_loadingURLs removeObject:url];
	[self checkLoadCompletionHandlers];
}

- (void)checkLoadCompletionHandlers
{
	if(_loadingURLs.count == 0)
	{
		NSArray<void(^)()>* completionHandlers = _loadingURLsCompletionHandlers;
		_loadingURLsCompletionHandlers = nil;
		for(void(^handler)() in completionHandlers)
			handler();
	}
}

- (void)expandURLs:(NSArray<NSURL*>*)expandURLs selectURLs:(NSArray<NSURL*>*)selectURLs
{
	_loadingURLsCompletionHandlers = [(_loadingURLsCompletionHandlers ?: @[ ]) arrayByAddingObject:^{
		[self performSelector:@selector(centerSelectionInVisibleArea:) withObject:self afterDelay:0];
	}];

	_expandedURLs = expandURLs ? [NSMutableSet setWithArray:expandURLs] : _expandedURLs;
	_selectedURLs = selectURLs ? [NSMutableSet setWithArray:selectURLs] : _selectedURLs;

	NSMutableArray<FileItem*>* stack = [self.fileItem.arrangedChildren mutableCopy];
	while(FileItem* item = stack.firstObject)
	{
		[stack removeObjectAtIndex:0];
		if([_expandedURLs containsObject:item.URL])
		{
			[_outlineView expandItem:item];
			if(NSArray<FileItem*>* arrangedChildren = item.arrangedChildren)
				[stack addObjectsFromArray:arrangedChildren];
		}
	}

	NSMutableIndexSet* indexesToSelect = [NSMutableIndexSet indexSet];
	for(NSUInteger i = 0; i < _outlineView.numberOfRows; ++i)
	{
		FileItem* item = [_outlineView itemAtRow:i];
		if([_selectedURLs containsObject:item.URL])
			[indexesToSelect addIndex:i];
	}
	[_outlineView selectRowIndexes:indexesToSelect byExtendingSelection:NO];

	[self checkLoadCompletionHandlers];
}

- (void)outlineView:(NSOutlineView*)outlineView willExpandItem:(FileItem*)item expandChildren:(BOOL)flag
{
	_expandingChildrenCounter += flag ? 1 : 0;
}

- (void)outlineView:(NSOutlineView*)outlineView didExpandItem:(FileItem*)item expandChildren:(BOOL)flag
{
	_expandingChildrenCounter -= flag ? 1 : 0;
}

- (void)outlineView:(NSOutlineView*)outlineView willCollapseItem:(id)someItem collapseChildren:(BOOL)flag
{
	_collapsingChildrenCounter += flag ? 1 : 0;
}

- (void)outlineView:(NSOutlineView*)outlineView didCollapseItem:(id)someItem collapseChildren:(BOOL)flag
{
	_collapsingChildrenCounter -= flag ? 1 : 0;
}

- (void)outlineViewItemWillCollapse:(NSNotification*)aNotification
{
	FileItem* item = aNotification.userInfo[@"NSObject"];
	if(_nestedCollapsingChildrenCounter == 0 || _collapsingChildrenCounter > 0)
		[_expandedURLs removeObject:item.URL];

	_nestedCollapsingChildrenCounter += 1;
}

- (void)outlineViewItemDidCollapse:(NSNotification*)aNotification
{
	_nestedCollapsingChildrenCounter -= 1;
}

// ================
// = Location URL =
// ================

- (NSURL*)URL
{
	return _fileItem.URL;
}

- (void)setURL:(NSURL*)url
{
	if(FileItem* item = [FileItem fileItemWithURL:url])
		self.fileItem = item;
}

- (void)reload:(id)sender
{
	NSMutableArray<FileItem*>* stack = [NSMutableArray arrayWithObject:self.fileItem];
	while(FileItem* item = stack.firstObject)
	{
		[stack removeObjectAtIndex:0];
		if(!item.arrangedChildren)
			continue;
		[FSEventsManager.sharedInstance reloadDirectoryAtURL:item.resolvedURL];
		[stack addObjectsFromArray:item.arrangedChildren];
	}
}

- (NSArray<FileItem*>*)selectedItems
{
	NSIndexSet* indexSet;

	NSInteger clickedRow = _outlineView.clickedRow;
	if(0 <= clickedRow && clickedRow < _outlineView.numberOfRows && ![_outlineView.selectedRowIndexes containsIndex:clickedRow])
			indexSet = [NSIndexSet indexSetWithIndex:clickedRow];
	else	indexSet = _outlineView.selectedRowIndexes;

	NSMutableArray* res = [NSMutableArray array];
	for(NSUInteger index = indexSet.firstIndex; index != NSNotFound; index = [indexSet indexGreaterThanIndex:index])
		[res addObject:[_outlineView itemAtRow:index]];
	return res;
}

- (NSURL*)directoryURLForNewItems
{
	NSMutableArray<NSURL*>* candidates = [NSMutableArray array];
	for(FileItem* item in self.selectedItems)
	{
		if(item.resolvedURL.isFileURL && [_outlineView isItemExpanded:item])
		{
			[candidates addObject:item.resolvedURL];
		}
		else if(FileItem* parentItem = [_outlineView parentForItem:item])
		{
			if(parentItem.resolvedURL.isFileURL)
				[candidates addObject:parentItem.resolvedURL];
		}
	}
	return candidates.lastObject ?: self.fileItem.URL.filePathURL;
}

- (void)centerSelectionInVisibleArea:(id)sender
{
	if(_outlineView.numberOfSelectedRows == 0)
		return;

	NSInteger row = _outlineView.selectedRowIndexes.firstIndex;

	NSRect rowRect     = [_outlineView rectOfRow:row];
	NSRect visibleRect = _outlineView.visibleRect;
	if(NSMinY(rowRect) < NSMinY(visibleRect) || NSMaxY(rowRect) > NSMaxY(visibleRect))
		[_outlineView scrollPoint:NSMakePoint(NSMinX(rowRect), round(NSMidY(rowRect) - NSHeight(visibleRect)/2))];
}

- (NSSet<NSURL*>*)selectedURLs
{
	NSMutableSet<NSURL*>* res = [_selectedURLs mutableCopy];
	NSIndexSet* selectedIndexes = _outlineView.selectedRowIndexes;
	for(NSUInteger i = 0; i < _outlineView.numberOfRows; ++i)
	{
		FileItem* item = [_outlineView itemAtRow:i];
		if([selectedIndexes containsIndex:i])
				[res addObject:item.URL];
		else	[res removeObject:item.URL];
	}
	return [res copy];
}

- (NSSet<NSURL*>*)expandedURLs
{
	NSMutableSet<NSURL*>* res = [_expandedURLs mutableCopy];
	for(NSUInteger i = 0; i < _outlineView.numberOfRows; ++i)
	{
		FileItem* item = [_outlineView itemAtRow:i];
		if([_outlineView isItemExpanded:item] && ![item.URL.scheme isEqualToString:@"scm"])
				[res addObject:item.URL];
		else	[res removeObject:item.URL];
	}
	return [res copy];
}

// ===========================
// = NSOutlineViewDataSource =
// ===========================

- (NSInteger)outlineView:(NSOutlineView*)outlineView numberOfChildrenOfItem:(FileItem*)item
{
	return (item ?: _fileItem).arrangedChildren.count;
}

- (id)outlineView:(NSOutlineView*)outlineView child:(NSInteger)childIndex ofItem:(FileItem*)item
{
	return (item ?: _fileItem).arrangedChildren[childIndex];
}

- (BOOL)outlineView:(NSOutlineView*)outlineView isItemExpandable:(FileItem*)item
{
	return item.isDirectory && (_canExpandPackages || !item.isPackage) || (_canExpandSymbolicLinks && item.isLinkToDirectory && (_canExpandPackages || !item.isLinkToPackage));
}

- (BOOL)outlineView:(NSOutlineView*)outlineView isGroupItem:(FileItem*)item
{
	return [item.URL.scheme isEqualToString:@"scm"];
}

- (BOOL)outlineView:(NSOutlineView*)outlineView shouldSelectItem:(FileItem*)item
{
	return item.URL.isFileURL;
}

- (id)outlineView:(NSOutlineView*)outlineView objectValueForTableColumn:(NSTableColumn*)tableColumn byItem:(FileItem*)item
{
	return item;
}

- (id <NSPasteboardWriting>)outlineView:(NSOutlineView*)outlineView pasteboardWriterForItem:(FileItem*)item
{
	return item.URL.filePathURL;
}

// ===============================
// = Table cell view constructor =
// ===============================

- (NSView*)outlineView:(NSOutlineView*)outlineView viewForTableColumn:(NSTableColumn*)tableColumn item:(FileItem*)item
{
	FileItemTableCellView* res = [outlineView makeViewWithIdentifier:tableColumn.identifier owner:self];
	if(!res)
	{
		res = [[FileItemTableCellView alloc] init];
		res.identifier  = tableColumn.identifier;
		res.target      = self;
		res.closeAction = @selector(takeItemToCloseFrom:);
		res.openButton.target = self;
		res.openButton.action = @selector(takeItemToOpenFrom:);
		res.textField.delegate = self;
	}
	return res;
}

- (void)takeItemToOpenFrom:(id)sender
{
	NSInteger row = [_outlineView rowForView:sender];
	if(row != -1 && self.openAction)
		[NSApp sendAction:self.openAction to:self.target from:[_outlineView itemAtRow:row]];
}

- (void)takeItemToCloseFrom:(id)sender
{
	NSInteger row = [_outlineView rowForView:sender];
	if(row != -1 && self.closeAction)
		[NSApp sendAction:self.closeAction to:self.target from:[_outlineView itemAtRow:row]];
}

- (BOOL)control:(NSTextField*)textField textShouldEndEditing:(NSText*)fieldEditor
{
	NSInteger row = [_outlineView rowForView:textField];
	if(row == -1)
		return NO;

	FileItem* item = [_outlineView itemAtRow:row];
	NSURL* newURL = [[item.URL URLByDeletingLastPathComponent] URLByAppendingPathComponent:fieldEditor.string isDirectory:item.isDirectory];
	if(![item.URL isEqual:newURL])
	{
		// Because of the animation we need to run this after field editor has been removed
		dispatch_async(dispatch_get_main_queue(), ^{
			[self performOperation:FBOperationRename withURLs:@{ item.URL: newURL } unique:NO select:YES];
		});
	}

	return YES;
}

// =============
// = QuickLook =
// =============

- (NSArray<FileItem*>*)previewableItems
{
	return [self.selectedItems filteredArrayUsingPredicate:[NSPredicate predicateWithFormat:@"previewItemURL != nil"]];
}

- (void)toggleQuickLookPreview:(id)sender
{
	if([QLPreviewPanel sharedPreviewPanelExists] && [[QLPreviewPanel sharedPreviewPanel] isVisible])
			[[QLPreviewPanel sharedPreviewPanel] orderOut:nil];
	else	[[QLPreviewPanel sharedPreviewPanel] makeKeyAndOrderFront:nil];
}

- (BOOL)acceptsPreviewPanelControl:(QLPreviewPanel*)previewPanel
{
	return YES;
}

- (void)beginPreviewPanelControl:(QLPreviewPanel*)previewPanel
{
	_previewItems = self.previewableItems;
	previewPanel.delegate   = self;
	previewPanel.dataSource = self;
}

- (void)endPreviewPanelControl:(QLPreviewPanel*)previewPanel
{
	_previewItems = nil;
}

- (NSInteger)numberOfPreviewItemsInPreviewPanel:(QLPreviewPanel*)previewPanel
{
	return _previewItems.count;
}

- (id <QLPreviewItem>)previewPanel:(QLPreviewPanel*)panel previewItemAtIndex:(NSInteger)index
{
	return _previewItems[index];
}

- (NSRect)previewPanel:(QLPreviewPanel*)previewPanel sourceFrameOnScreenForPreviewItem:(id <QLPreviewItem>)item
{
	return [self imageRectOfItem:item];
}

- (NSRect)imageRectOfItem:(FileItem*)item
{
	NSInteger row = [_outlineView rowForItem:item];
	if(row != -1)
	{
		FileItemTableCellView* view = [_outlineView viewAtColumn:0 row:row makeIfNecessary:YES];
		if([view isKindOfClass:[FileItemTableCellView class]])
		{
			NSButton* imageButton = view.openButton;
			NSRect imageRect = NSIntersectionRect([imageButton convertRect:imageButton.bounds toView:nil], [_outlineView convertRect:_outlineView.visibleRect toView:nil]);
			return NSIsEmptyRect(imageRect) ? NSZeroRect : [self.window convertRectToScreen:imageRect];
		}
	}
	return NSZeroRect;
}

- (BOOL)previewPanel:(QLPreviewPanel*)previewPanel handleEvent:(NSEvent*)event
{
	std::string const eventString = to_s(event);
	if((event.type == NSEventTypeKeyUp || event.type == NSEventTypeKeyDown) && (eventString == utf8::to_s(NSUpArrowFunctionKey) || eventString == utf8::to_s(NSDownArrowFunctionKey)))
	{
		[self.window sendEvent:event];
		_previewItems = self.previewableItems;
		[previewPanel reloadData];
		return YES;
	}
	return NO;
}

// ============
// = Services =
// ============

+ (void)initialize
{
	[NSApplication.sharedApplication registerServicesMenuSendTypes:@[ NSFilenamesPboardType, NSURLPboardType ] returnTypes:@[ ]];
}

- (id)validRequestorForSendType:(NSString*)sendType returnType:(NSString*)returnType
{
	return returnType == nil && sendType != nil && [@[ NSFilenamesPboardType, NSURLPboardType ] containsObject:sendType] ? self : nil;
}

- (BOOL)writeSelectionToPasteboard:(NSPasteboard*)pboard types:(NSArray*)types
{
	NSArray<NSURL*>* urls = [self.previewableItems valueForKeyPath:@"URL"];
	if(urls.count == 0)
		return NO;

	[pboard clearContents];
	[pboard writeObjects:urls];
	return YES;
}

// ===================
// = Accepting Drops =
// ===================

- (NSDragOperation)outlineView:(NSOutlineView*)outlineView validateDrop:(id <NSDraggingInfo>)info proposedItem:(FileItem*)item proposedChildIndex:(NSInteger)childIndex
{
	NSURL* dropURL = (item ?: self.fileItem).resolvedURL.filePathURL;
	if(![_outlineView isExpandable:item] || !dropURL || ![NSFileManager.defaultManager fileExistsAtPath:dropURL.path])
		return NSDragOperationNone;

	NSPasteboard* pboard  = info.draggingPasteboard;
	NSArray* draggedPaths = [pboard propertyListForType:NSFilenamesPboardType];

	dev_t targetDevice   = path::device(dropURL.fileSystemRepresentation);
	BOOL linkOperation   = (NSApp.currentEvent.modifierFlags & NSEventModifierFlagControl) == NSEventModifierFlagControl;
	BOOL toggleOperation = (NSApp.currentEvent.modifierFlags & NSEventModifierFlagOption) == NSEventModifierFlagOption;

	// We accept the drop as long as it is valid for at least one of the items
	for(NSString* draggedPath in draggedPaths)
	{
		BOOL sameSource = (path::device(draggedPath.fileSystemRepresentation) == targetDevice);
		NSDragOperation operation = linkOperation ? NSDragOperationLink : ((sameSource != toggleOperation) ? NSDragOperationMove : NSDragOperationCopy);

		// Can’t move into same location
		NSString* parentPath = draggedPath.stringByDeletingLastPathComponent;
		if(operation == NSDragOperationMove && [parentPath isEqualToString:dropURL.path])
			continue;

		[outlineView setDropItem:item dropChildIndex:NSOutlineViewDropOnItemIndex];
		return operation;
	}
	return NSDragOperationNone;
}

static NSDragOperation filter (NSDragOperation mask)
{
	return (mask & NSDragOperationMove) ? NSDragOperationMove : ((mask & NSDragOperationCopy) ? NSDragOperationCopy : ((mask & NSDragOperationLink) ? NSDragOperationLink : 0));
}

- (NSArray<NSURL*>*)URLsFromPasteboard:(NSPasteboard*)pboard
{
	NSMutableArray<NSURL*>* res = [NSMutableArray array];
	for(NSString* path in [pboard availableTypeFromArray:@[ NSFilenamesPboardType ]] ? [pboard propertyListForType:NSFilenamesPboardType] : @[ ])
		[res addObject:[NSURL fileURLWithPath:path]];
	return res;
}

- (BOOL)outlineView:(NSOutlineView*)outlineView acceptDrop:(id <NSDraggingInfo>)info item:(FileItem*)item childIndex:(NSInteger)childIndex
{
	FileItem* newParent = item ?: self.fileItem;

	NSDragOperation op = filter(info.draggingSourceOperationMask);
	if(op == 0 || ![_outlineView isExpandable:newParent] || !newParent.resolvedURL.isFileURL)
		return NO;

	NSMutableDictionary<NSURL*, NSURL*>* urls = [NSMutableDictionary dictionary];
	for(NSURL* url in [self URLsFromPasteboard:info.draggingPasteboard])
		urls[url] = [newParent.resolvedURL URLByAppendingPathComponent:url.lastPathComponent isDirectory:op != NSDragOperationLink && url.tmHasDirectoryPath];

	switch(op)
	{
		case NSDragOperationLink: [self performOperation:FBOperationLink withURLs:urls unique:NO select:NO]; break;
		case NSDragOperationCopy: [self performOperation:FBOperationCopy withURLs:urls unique:NO select:NO]; break;
		case NSDragOperationMove: [self performOperation:FBOperationMove withURLs:urls unique:NO select:NO]; break;
	}

	return YES;
}

- (void)outlineView:(NSOutlineView*)outlineView didTrashURLs:(NSArray<NSURL*>*)someURLs
{
	[self performOperation:FBOperationTrash sourceURLs:someURLs destinationURLs:nil unique:NO select:NO];
}

// =============
// = Undo/Redo =
// =============

- (NSUndoManager*)undoManager
{
	if(!_fileBrowserUndoManager)
		_fileBrowserUndoManager = [[NSUndoManager alloc] init];
	return _fileBrowserUndoManager;
}

- (NSUndoManager*)activeUndoManager
{
	NSResponder* firstResponder = self.window.firstResponder;
	if([firstResponder isKindOfClass:[NSView class]] && [(NSView*)firstResponder isDescendantOf:self])
			return firstResponder.undoManager;
	else	return self.undoManager;
}

- (void)undo:(id)sender
{
	[self.activeUndoManager undo];
}

- (void)redo:(id)sender
{
	[self.activeUndoManager redo];
}

- (BOOL)validateMenuItem:(NSMenuItem*)menuItem
{
	BOOL res = YES;
	if(menuItem.action == @selector(undo:))
	{
		menuItem.title = self.activeUndoManager.undoMenuItemTitle;
		res = self.activeUndoManager.canUndo;
	}
	else if(menuItem.action == @selector(redo:))
	{
		menuItem.title = self.activeUndoManager.redoMenuItemTitle;
		res = self.activeUndoManager.canRedo;
	}
	else if(menuItem.action == @selector(toggleQuickLookPreview:))
	{
		if([QLPreviewPanel sharedPreviewPanelExists] && [QLPreviewPanel sharedPreviewPanel].isVisible)
			menuItem.title = @"Close Quick Look";
		else if(self.previewableItems.count == 0)
			menuItem.hidden = YES;
		else if(self.previewableItems.count == 1)
			menuItem.title = [NSString stringWithFormat:@"Quick Look “%@”", self.previewableItems.firstObject.localizedName];
		else
			menuItem.title = [NSString stringWithFormat:@"Quick Look %ld Items", self.previewableItems.count];
	}
	return res;
}
@end
