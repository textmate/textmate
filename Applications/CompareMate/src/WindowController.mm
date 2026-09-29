#import "WindowController.h"
#import <OakTextView/src/OakDocumentView.h>
#import <OakTextView/src/GutterView.h>
#import <document/src/OakDocument.h>
#import <FileBrowser/src/FSEventsManager.h>
#import <FileBrowser/src/KEventManager.h>
#import <OakAppKit/src/OakSavePanel.h>
#import <ns/src/ns.h>

@interface OakDocument (CompareMateExternalChanges)
@property (nonatomic) BOOL observeFileSystem;
- (void)importDocumentChanges:(id)sender;
@end

static NSString* const LeftPathRestorationKey = @"CompareMate.leftPath";
static NSString* const RightPathRestorationKey = @"CompareMate.rightPath";
static NSString* const DividerPositionRestorationKey = @"CompareMate.dividerPosition";
static NSString* const FolderFilterRestorationKey = @"CompareMate.folderFilter";
static NSString* const FolderIconLeadingConstraintIdentifier = @"CompareMate.folderIconLeading";
static NSWindowFrameAutosaveName const FileComparisonFrameAutosaveName = @"CompareMate.FileComparisonWindowFrame";
NSString* const CompareMateIgnoredFileNamesDefaultsKey = @"ignoredFileNames";
NSNotificationName const CompareMateIgnoredFileNamesDidChangeNotification = @"CompareMateIgnoredFileNamesDidChangeNotification";

@interface DiffCharacterRange : NSObject
@property (nonatomic) NSUInteger line;
@property (nonatomic) NSRange byteColumns;
@end

typedef NS_ENUM(NSInteger, FolderEntryKind) {
	FolderEntryKindMissing,
	FolderEntryKindFile,
	FolderEntryKindDirectory,
	FolderEntryKindOther,
};

@interface FolderComparisonNode : NSObject
@property (nonatomic) NSString* name;
@property (nonatomic) NSString* relativePath;
@property (nonatomic) NSString* leftPath;
@property (nonatomic) NSString* rightPath;
@property (nonatomic) FolderEntryKind leftKind;
@property (nonatomic) FolderEntryKind rightKind;
@property (nonatomic) NSNumber* leftFileSize;
@property (nonatomic) NSNumber* rightFileSize;
@property (nonatomic) NSDate* leftModificationDate;
@property (nonatomic) NSDate* rightModificationDate;
@property (nonatomic) NSArray<FolderComparisonNode*>* children;
@property (nonatomic) NSString* scanError;
@property (nonatomic, readonly) BOOL isDirectory;
@property (nonatomic, readonly) BOOL representsFile;
@end

@implementation FolderComparisonNode
- (BOOL)isDirectory
{
	return self.leftKind == FolderEntryKindDirectory || self.rightKind == FolderEntryKindDirectory;
}

- (BOOL)representsFile
{
	return self.leftKind == FolderEntryKindFile || self.rightKind == FolderEntryKindFile;
}
@end

static FolderEntryKind FolderEntryKindAtPath (NSString* path)
{
	NSDictionary<NSFileAttributeKey, id>* attributes = [NSFileManager.defaultManager attributesOfItemAtPath:path error:nil];
	NSString* type = attributes[NSFileType];
	if(!type)
		return FolderEntryKindMissing;
	if([type isEqualToString:NSFileTypeDirectory])
		return FolderEntryKindDirectory;
	if([type isEqualToString:NSFileTypeRegular] || [type isEqualToString:NSFileTypeSymbolicLink])
		return FolderEntryKindFile;
	return FolderEntryKindOther;
}

static BOOL FolderFilesAreEqual (NSString* leftPath, NSString* rightPath)
{
	NSDictionary<NSFileAttributeKey, id>* leftAttributes = [NSFileManager.defaultManager attributesOfItemAtPath:leftPath error:nil];
	NSDictionary<NSFileAttributeKey, id>* rightAttributes = [NSFileManager.defaultManager attributesOfItemAtPath:rightPath error:nil];
	if(![leftAttributes[NSFileType] isEqual:rightAttributes[NSFileType]])
		return NO;
	if([leftAttributes[NSFileType] isEqualToString:NSFileTypeSymbolicLink])
	{
		NSString* leftDestination = [NSFileManager.defaultManager destinationOfSymbolicLinkAtPath:leftPath error:nil];
		NSString* rightDestination = [NSFileManager.defaultManager destinationOfSymbolicLinkAtPath:rightPath error:nil];
		return leftDestination && [leftDestination isEqualToString:rightDestination];
	}
	if(![leftAttributes[NSFileSize] isEqual:rightAttributes[NSFileSize]])
		return NO;
	return [NSFileManager.defaultManager contentsEqualAtPath:leftPath andPath:rightPath];
}

static FolderComparisonNode* BuildFolderComparisonNode (NSString* relativePath, NSString* leftRoot, NSString* rightRoot, NSSet<NSString*>* ignoredFileNames)
{
	FolderComparisonNode* node = [[FolderComparisonNode alloc] init];
	node.relativePath = relativePath;
	node.name = relativePath.lastPathComponent;
	node.leftPath = [leftRoot stringByAppendingPathComponent:relativePath];
	node.rightPath = [rightRoot stringByAppendingPathComponent:relativePath];
	node.leftKind = FolderEntryKindAtPath(node.leftPath);
	node.rightKind = FolderEntryKindAtPath(node.rightPath);
	NSDictionary<NSFileAttributeKey, id>* leftAttributes = [NSFileManager.defaultManager attributesOfItemAtPath:node.leftPath error:nil];
	NSDictionary<NSFileAttributeKey, id>* rightAttributes = [NSFileManager.defaultManager attributesOfItemAtPath:node.rightPath error:nil];
	node.leftFileSize = leftAttributes[NSFileSize];
	node.rightFileSize = rightAttributes[NSFileSize];
	node.leftModificationDate = leftAttributes[NSFileModificationDate];
	node.rightModificationDate = rightAttributes[NSFileModificationDate];

	if(node.leftKind == FolderEntryKindDirectory || node.rightKind == FolderEntryKindDirectory)
	{
		NSMutableSet<NSString*>* names = [NSMutableSet set];
		NSError* leftError = nil, *rightError = nil;
		if(node.leftKind == FolderEntryKindDirectory)
		{
			NSArray<NSString*>* leftNames = [NSFileManager.defaultManager contentsOfDirectoryAtPath:node.leftPath error:&leftError];
			if(leftNames)
				[names addObjectsFromArray:leftNames];
		}
		if(node.rightKind == FolderEntryKindDirectory)
		{
			NSArray<NSString*>* rightNames = [NSFileManager.defaultManager contentsOfDirectoryAtPath:node.rightPath error:&rightError];
			if(rightNames)
				[names addObjectsFromArray:rightNames];
		}
		if(leftError || rightError)
			node.scanError = (leftError ?: rightError).localizedDescription;

		NSMutableArray<FolderComparisonNode*>* children = [NSMutableArray array];
		NSArray<NSString*>* sortedNames = [names.allObjects sortedArrayUsingComparator:^NSComparisonResult(NSString* first, NSString* second) {
			return [first localizedStandardCompare:second];
		}];
		for(NSString* name in sortedNames)
		{
			if([ignoredFileNames containsObject:name])
				continue;
			FolderComparisonNode* child = BuildFolderComparisonNode([relativePath stringByAppendingPathComponent:name], leftRoot, rightRoot, ignoredFileNames);
			if(child)
				[children addObject:child];
		}
		node.children = children;

		BOOL const typeMismatchWithFile = (node.leftKind == FolderEntryKindDirectory && node.rightKind != FolderEntryKindDirectory && node.rightKind != FolderEntryKindMissing) || (node.rightKind == FolderEntryKindDirectory && node.leftKind != FolderEntryKindDirectory && node.leftKind != FolderEntryKindMissing);
		return children.count || node.scanError || typeMismatchWithFile ? node : nil;
	}

	node.children = @[];
	if(node.leftKind == FolderEntryKindMissing && node.rightKind == FolderEntryKindMissing)
		return nil;
	if(node.leftKind != node.rightKind)
		return node;
	if(node.leftKind == FolderEntryKindFile && !FolderFilesAreEqual(node.leftPath, node.rightPath))
		return node;
	if(node.leftKind == FolderEntryKindOther)
		return node;
	return nil;
}

static NSArray<FolderComparisonNode*>* BuildFolderComparison (NSString* leftRoot, NSString* rightRoot, NSSet<NSString*>* ignoredFileNames)
{
	NSMutableSet<NSString*>* names = [NSMutableSet set];
	NSError* leftError = nil, *rightError = nil;
	NSArray<NSString*>* leftNames = [NSFileManager.defaultManager contentsOfDirectoryAtPath:leftRoot error:&leftError];
	NSArray<NSString*>* rightNames = [NSFileManager.defaultManager contentsOfDirectoryAtPath:rightRoot error:&rightError];
	if(leftNames)
		[names addObjectsFromArray:leftNames];
	if(rightNames)
		[names addObjectsFromArray:rightNames];

	NSMutableArray<FolderComparisonNode*>* result = [NSMutableArray array];
	NSArray<NSString*>* sortedNames = [names.allObjects sortedArrayUsingComparator:^NSComparisonResult(NSString* first, NSString* second) {
		return [first localizedStandardCompare:second];
	}];
	for(NSString* name in sortedNames)
	{
		if([ignoredFileNames containsObject:name])
			continue;
		FolderComparisonNode* node = BuildFolderComparisonNode(name, leftRoot, rightRoot, ignoredFileNames);
		if(node)
			[result addObject:node];
	}
	if((leftError || rightError) && result.count == 0)
	{
		FolderComparisonNode* errorNode = [[FolderComparisonNode alloc] init];
		errorNode.name = @"Couldn’t read folder";
		errorNode.relativePath = @"";
		errorNode.children = @[];
		errorNode.scanError = (leftError ?: rightError).localizedDescription;
		[result addObject:errorNode];
	}
	return result;
}

static FolderComparisonNode* FilterFolderComparisonNode (FolderComparisonNode* node, BOOL includeSingleFiles)
{
	BOOL const existsOnBothSides = node.leftKind != FolderEntryKindMissing && node.rightKind != FolderEntryKindMissing;
	if(!node.isDirectory)
		return includeSingleFiles || existsOnBothSides ? node : nil;

	NSMutableArray<FolderComparisonNode*>* children = [NSMutableArray array];
	for(FolderComparisonNode* child in node.children)
	{
		if(FolderComparisonNode* filteredChild = FilterFolderComparisonNode(child, includeSingleFiles))
			[children addObject:filteredChild];
	}
	BOOL const kindDiffers = existsOnBothSides && node.leftKind != node.rightKind;
	if(children.count == 0 && !node.scanError.length && !kindDiffers)
		return nil;
	if(children.count == node.children.count)
		return node;

	FolderComparisonNode* filteredNode = [[FolderComparisonNode alloc] init];
	filteredNode.name = node.name;
	filteredNode.relativePath = node.relativePath;
	filteredNode.leftPath = node.leftPath;
	filteredNode.rightPath = node.rightPath;
	filteredNode.leftKind = node.leftKind;
	filteredNode.rightKind = node.rightKind;
	filteredNode.leftFileSize = node.leftFileSize;
	filteredNode.rightFileSize = node.rightFileSize;
	filteredNode.leftModificationDate = node.leftModificationDate;
	filteredNode.rightModificationDate = node.rightModificationDate;
	filteredNode.children = children;
	filteredNode.scanError = node.scanError;
	return filteredNode;
}

static NSArray<FolderComparisonNode*>* FilterFolderComparison (NSArray<FolderComparisonNode*>* nodes, BOOL includeSingleFiles)
{
	if(includeSingleFiles)
		return nodes;
	NSMutableArray<FolderComparisonNode*>* result = [NSMutableArray array];
	for(FolderComparisonNode* node in nodes)
	{
		if(FolderComparisonNode* filteredNode = FilterFolderComparisonNode(node, NO))
			[result addObject:filteredNode];
	}
	return result;
}

@interface FolderWindowController () <NSWindowDelegate, NSOutlineViewDataSource, NSOutlineViewDelegate, NSMenuDelegate>
@property (nonatomic) FolderWindowController* retainedSelf;
@property (nonatomic, copy) NSString* leftPath;
@property (nonatomic, copy) NSString* rightPath;
@property (nonatomic) NSOutlineView* outlineView;
@property (nonatomic) NSScrollView* outlineScrollView;
@property (nonatomic) NSTableColumn* leftNameColumn;
@property (nonatomic) NSTableColumn* rightNameColumn;
@property (nonatomic) NSSegmentedControl* filterControl;
@property (nonatomic) NSTextField* statusLabel;
@property (nonatomic) NSDateFormatter* dateFormatter;
@property (nonatomic, copy) NSArray<FolderComparisonNode*>* allNodes;
@property (nonatomic, copy) NSArray<FolderComparisonNode*>* nodes;
@property (nonatomic) NSUInteger scanGeneration;
@property (nonatomic) BOOL hasCompletedScan;
@property (nonatomic) id leftFileSystemObserver;
@property (nonatomic) id rightFileSystemObserver;
@property (nonatomic) NSTimer* fileSystemRefreshTimer;
@end

@implementation FolderWindowController
- (NSOutlineView*)newOutlineView
{
	NSOutlineView* outlineView = [[NSOutlineView alloc] initWithFrame:NSZeroRect];
	outlineView.dataSource = self;
	outlineView.delegate = self;
	outlineView.rowSizeStyle = NSTableViewRowSizeStyleDefault;
	outlineView.indentationPerLevel = 16;
	outlineView.allowsMultipleSelection = NO;
	outlineView.columnAutoresizingStyle = NSTableViewNoColumnAutoresizing;
	outlineView.target = self;
	outlineView.doubleAction = @selector(openSelectedFile:);

	NSTableColumn* (^addColumn)(NSString*, NSString*, CGFloat, CGFloat) = ^NSTableColumn*(NSString* identifier, NSString* title, CGFloat width, CGFloat minimumWidth) {
		NSTableColumn* column = [[NSTableColumn alloc] initWithIdentifier:identifier];
		column.title = title;
		column.width = width;
		column.minWidth = minimumWidth;
		column.resizingMask = NSTableColumnNoResizing;
		[outlineView addTableColumn:column];
		return column;
	};
	self.leftNameColumn = addColumn(@"leftName", @"Left", 220, 100);
	outlineView.outlineTableColumn = self.leftNameColumn;
	addColumn(@"leftDate", @"Modified", 116, 116);
	addColumn(@"leftSize", @"Size", 60, 60);
	self.rightNameColumn = addColumn(@"rightName", @"Right", 220, 100);
	addColumn(@"rightDate", @"Modified", 116, 116);
	addColumn(@"rightSize", @"Size", 60, 60);
	return outlineView;
}

- (void)resizeFolderColumnsToFit
{
	if(!self.outlineScrollView || !self.leftNameColumn || !self.rightNameColumn)
		return;

	CGFloat fixedWidth = 0;
	for(NSTableColumn* column in self.outlineView.tableColumns)
	{
		if(column != self.leftNameColumn && column != self.rightNameColumn)
			fixedWidth += column.width;
	}
	CGFloat const spacingWidth = self.outlineView.intercellSpacing.width * self.outlineView.tableColumns.count;
	CGFloat const availableWidth = NSWidth(self.outlineScrollView.contentView.bounds);
	CGFloat const nameWidth = MAX(self.leftNameColumn.minWidth, floor((availableWidth - fixedWidth - spacingWidth) / 2));
	self.leftNameColumn.width = nameWidth;
	self.rightNameColumn.width = nameWidth;
}

- (instancetype)initWithLeftPath:(NSString*)leftPath rightPath:(NSString*)rightPath
{
	NSRect const contentRect = NSMakeRect(0, 0, 900, 620);
	NSWindowStyleMask const styleMask = NSWindowStyleMaskTitled|NSWindowStyleMaskResizable|NSWindowStyleMaskClosable|NSWindowStyleMaskMiniaturizable;
	if(self = [self initWithWindow:[[NSWindow alloc] initWithContentRect:contentRect styleMask:styleMask backing:NSBackingStoreBuffered defer:NO]])
	{
		_retainedSelf = self;
		_leftPath = [leftPath copy];
		_rightPath = [rightPath copy];
		_allNodes = @[];
		_nodes = @[];

		NSWindow* window = self.window;
		window.title = [NSString stringWithFormat:@"%@ ↔ %@", leftPath.lastPathComponent, rightPath.lastPathComponent];
		window.delegate = self;
		window.minSize = NSMakeSize(620, 360);
		window.identifier = [NSString stringWithFormat:@"CompareMate.FolderComparison.%@", NSUUID.UUID.UUIDString];
		[window setFrameAutosaveName:window.identifier];
		window.restorationClass = FolderWindowController.class;
		window.restorable = YES;

		NSView* contentView = [[NSView alloc] initWithFrame:contentRect];
		window.contentView = contentView;

		self.filterControl = [NSSegmentedControl segmentedControlWithLabels:@[ @"Changed and Single", @"Changed Only" ] trackingMode:NSSegmentSwitchTrackingSelectOne target:self action:@selector(changeFolderFilter:)];
		self.filterControl.selectedSegment = 0;
		self.filterControl.controlSize = NSControlSizeSmall;
		if(@available(macOS 26, *)) {
			self.filterControl.borderShape = NSControlBorderShapeCapsule;
		}
		[self.filterControl sizeToFit];
		CGFloat const bottomBarHeight = MAX(32, NSHeight(self.filterControl.frame) + 8);
		NSView* bottomBar = [[NSView alloc] initWithFrame:NSMakeRect(0, 0, NSWidth(contentRect), bottomBarHeight)];
		bottomBar.autoresizingMask = NSViewWidthSizable|NSViewMaxYMargin;
		self.filterControl.frame = NSMakeRect(6, floor((bottomBarHeight - NSHeight(self.filterControl.frame)) / 2), NSWidth(self.filterControl.frame), NSHeight(self.filterControl.frame));
		self.filterControl.autoresizingMask = NSViewMaxXMargin;
		[bottomBar addSubview:self.filterControl];
		[contentView addSubview:bottomBar];

		NSPathControl* leftPathControl = [[NSPathControl alloc] initWithFrame:NSZeroRect];
		NSPathControl* rightPathControl = [[NSPathControl alloc] initWithFrame:NSZeroRect];
		for(NSPathControl* pathControl in @[leftPathControl, rightPathControl]) {
			pathControl.pathStyle = NSPathStyleStandard;
			pathControl.editable = NO;
			pathControl.focusRingType = NSFocusRingTypeNone;
			pathControl.translatesAutoresizingMaskIntoConstraints = NO;
			pathControl.target = self;
			pathControl.action = @selector(pathControlClicked:);
		}
		leftPathControl.URL = [NSURL fileURLWithPath:leftPath isDirectory:YES];
		rightPathControl.URL = [NSURL fileURLWithPath:rightPath isDirectory:YES];
		leftPathControl.toolTip = leftPath;
		rightPathControl.toolTip = rightPath;
		NSView* pathHeader = [[NSView alloc] initWithFrame:NSZeroRect];
		pathHeader.frame = NSMakeRect(0, NSHeight(contentRect) - 26, NSWidth(contentRect), 24);
		pathHeader.autoresizingMask = NSViewWidthSizable|NSViewMinYMargin;
		[pathHeader addSubview:leftPathControl];
		[pathHeader addSubview:rightPathControl];
		[NSLayoutConstraint activateConstraints:@[
			[leftPathControl.leadingAnchor constraintEqualToAnchor:pathHeader.leadingAnchor],
			[leftPathControl.topAnchor constraintEqualToAnchor:pathHeader.topAnchor],
			[leftPathControl.bottomAnchor constraintEqualToAnchor:pathHeader.bottomAnchor],
			[leftPathControl.trailingAnchor constraintEqualToAnchor:rightPathControl.leadingAnchor],
			[leftPathControl.widthAnchor constraintEqualToAnchor:rightPathControl.widthAnchor],
			[rightPathControl.topAnchor constraintEqualToAnchor:pathHeader.topAnchor],
			[rightPathControl.bottomAnchor constraintEqualToAnchor:pathHeader.bottomAnchor],
			[rightPathControl.trailingAnchor constraintEqualToAnchor:pathHeader.trailingAnchor],
		]];
		[contentView addSubview:pathHeader];

		self.outlineView = [self newOutlineView];
		CGFloat const outlineTop = NSMinY(pathHeader.frame) - 2;
		NSScrollView* scrollView = [[NSScrollView alloc] initWithFrame:NSMakeRect(0, bottomBarHeight, NSWidth(contentRect), outlineTop - bottomBarHeight)];
		scrollView.autoresizingMask = NSViewWidthSizable|NSViewHeightSizable;
		scrollView.hasVerticalScroller = YES;
		scrollView.hasHorizontalScroller = NO;
		scrollView.borderType = NSBezelBorder;
		self.outlineScrollView = scrollView;
		self.outlineView.frame = scrollView.bounds;
		self.outlineView.autoresizingMask = NSViewWidthSizable;
		scrollView.documentView = self.outlineView;
		[contentView addSubview:scrollView];
		[self resizeFolderColumnsToFit];

		self.dateFormatter = [[NSDateFormatter alloc] init];
		self.dateFormatter.dateStyle = NSDateFormatterShortStyle;
		self.dateFormatter.timeStyle = NSDateFormatterShortStyle;

		self.statusLabel = [NSTextField labelWithString:@"Comparing folders…"];
		CGFloat const statusX = NSMaxX(self.filterControl.frame) + 12;
		self.statusLabel.frame = NSMakeRect(statusX, floor((bottomBarHeight - 17) / 2), NSWidth(contentRect) - statusX - 12, 17);
		self.statusLabel.autoresizingMask = NSViewWidthSizable;
		self.statusLabel.textColor = NSColor.secondaryLabelColor;
		self.statusLabel.lineBreakMode = NSLineBreakByTruncatingMiddle;
		[bottomBar addSubview:self.statusLabel];

		window.initialFirstResponder = self.outlineView;
		[window center];

		__weak FolderWindowController* weakSelf = self;
		void (^folderDidChange)(NSURL*) = ^(NSURL* changedURL) {
			[weakSelf scheduleRefreshFromFileSystemEvent];
		};
		self.leftFileSystemObserver = [FSEventsManager.sharedInstance addObserverToDirectoryAtURL:[NSURL fileURLWithPath:leftPath isDirectory:YES] observeSubdirectories:YES usingBlock:folderDidChange];
		self.rightFileSystemObserver = [FSEventsManager.sharedInstance addObserverToDirectoryAtURL:[NSURL fileURLWithPath:rightPath isDirectory:YES] observeSubdirectories:YES usingBlock:folderDidChange];
		[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(ignoredFileNamesDidChange:) name:CompareMateIgnoredFileNamesDidChangeNotification object:nil];

		[self refreshComparison];
		[self invalidateRestorableState];
	}
	return self;
}

- (NSArray<NSURL*>*)subfolderURLsAtURL:(NSURL*)folderURL
{
	NSArray<NSURLResourceKey>* resourceKeys = @[ NSURLIsDirectoryKey, NSURLIsPackageKey, NSURLLocalizedNameKey ];
	NSArray<NSURL*>* contents = [NSFileManager.defaultManager contentsOfDirectoryAtURL:folderURL includingPropertiesForKeys:resourceKeys options:NSDirectoryEnumerationSkipsHiddenFiles error:nil];
	NSMutableArray<NSURL*>* result = [NSMutableArray array];
	for(NSURL* URL in contents)
	{
		NSNumber* isDirectory = nil;
		NSNumber* isPackage = nil;
		[URL getResourceValue:&isDirectory forKey:NSURLIsDirectoryKey error:nil];
		[URL getResourceValue:&isPackage forKey:NSURLIsPackageKey error:nil];
		if(isDirectory.boolValue && !isPackage.boolValue)
			[result addObject:URL];
	}
	[result sortUsingComparator:^NSComparisonResult(NSURL* leftURL, NSURL* rightURL) {
		NSString* leftName = [NSFileManager.defaultManager displayNameAtPath:leftURL.path];
		NSString* rightName = [NSFileManager.defaultManager displayNameAtPath:rightURL.path];
		return [leftName localizedStandardCompare:rightName];
	}];
	return result;
}

- (NSMenuItem*)addFolderURL:(NSURL*)URL toMenu:(NSMenu*)menu
{
	NSString* title = [NSFileManager.defaultManager displayNameAtPath:URL.path];
	NSMenuItem* item = [menu addItemWithTitle:title action:@selector(openFolderFromPathMenu:) keyEquivalent:@""];
	item.target = self;
	item.representedObject = URL;
	NSImage* icon = [NSWorkspace.sharedWorkspace iconForFile:URL.path].copy;
	icon.size = NSMakeSize(16, 16);
	item.image = icon;
	return item;
}

- (void)populateMenu:(NSMenu*)menu withSubfoldersOfURL:(NSURL*)folderURL includeEnclosingFolders:(BOOL)includeEnclosingFolders
{
	NSArray<NSURL*>* subfolderURLs = [self subfolderURLsAtURL:folderURL];
	for(NSURL* URL in subfolderURLs)
	{
		NSMenuItem* item = [self addFolderURL:URL toMenu:menu];
		if([self subfolderURLsAtURL:URL].count)
		{
			item.submenu = [[NSMenu alloc] initWithTitle:item.title];
			item.submenu.delegate = self;
		}
	}

	if(!includeEnclosingFolders || [folderURL.path isEqualToString:@"/"])
		return;
	if(menu.numberOfItems)
		[menu addItem:NSMenuItem.separatorItem];
	NSMenuItem* heading = [menu addItemWithTitle:@"Enclosing Folders" action:nil keyEquivalent:@""];
	heading.enabled = NO;
	for(NSString* path = folderURL.path.stringByDeletingLastPathComponent; path.length; path = path.stringByDeletingLastPathComponent)
	{
		[self addFolderURL:[NSURL fileURLWithPath:path isDirectory:YES] toMenu:menu];
		if([path isEqualToString:@"/"])
			break;
	}
}

- (void)menuNeedsUpdate:(NSMenu*)menu
{
	if(menu.numberOfItems)
		return;
	NSInteger const parentIndex = [menu.supermenu indexOfItemWithSubmenu:menu];
	if(parentIndex < 0)
		return;
	NSURL* folderURL = [menu.supermenu itemAtIndex:parentIndex].representedObject;
	if(folderURL)
		[self populateMenu:menu withSubfoldersOfURL:folderURL includeEnclosingFolders:NO];
}

- (IBAction)pathControlClicked:(NSPathControl*)sender
{
	NSEvent* event = NSApp.currentEvent;
	if((event.modifierFlags & NSEventModifierFlagCommand) == 0)
		return;
	NSURL* folderURL = sender.clickedPathItem.URL;
	if(!folderURL.isFileURL)
		return;

	NSMenu* menu = [[NSMenu alloc] initWithTitle:@""];
	[self populateMenu:menu withSubfoldersOfURL:folderURL includeEnclosingFolders:YES];
	NSPoint clickLocation = [sender convertPoint:event.locationInWindow fromView:nil];
	NSPathCell* pathCell = (NSPathCell*)sender.cell;
	NSPathComponentCell* pathComponentCell = [pathCell pathComponentCellAtPoint:clickLocation withFrame:sender.bounds inView:sender];
	NSRect pathComponentRect = pathComponentCell ? [pathCell rectOfPathComponentCell:pathComponentCell withFrame:sender.bounds inView:sender] : sender.bounds;
	NSPoint location = NSMakePoint(NSMinX(pathComponentRect) - 10, NSMinY(pathComponentRect));
	[menu popUpMenuPositioningItem:nil atLocation:location inView:sender];
}

- (IBAction)openFolderFromPathMenu:(NSMenuItem*)sender
{
	NSURL* folderURL = sender.representedObject;
	if(folderURL)
		[NSWorkspace.sharedWorkspace openURL:folderURL];
}

+ (void)restoreWindowWithIdentifier:(NSUserInterfaceItemIdentifier)identifier state:(NSCoder*)state completionHandler:(void (^)(NSWindow*, NSError*))completionHandler
{
	NSString* leftPath = [state decodeObjectOfClass:NSString.class forKey:LeftPathRestorationKey];
	NSString* rightPath = [state decodeObjectOfClass:NSString.class forKey:RightPathRestorationKey];
	BOOL leftIsDirectory = NO, rightIsDirectory = NO;
	BOOL leftExists = leftPath.length && [NSFileManager.defaultManager fileExistsAtPath:leftPath isDirectory:&leftIsDirectory];
	BOOL rightExists = rightPath.length && [NSFileManager.defaultManager fileExistsAtPath:rightPath isDirectory:&rightIsDirectory];
	if(!leftExists || !rightExists || !leftIsDirectory || !rightIsDirectory)
	{
		completionHandler(nil, nil);
		return;
	}

	FolderWindowController* windowController = [[FolderWindowController alloc] initWithLeftPath:leftPath rightPath:rightPath];
	windowController.window.identifier = identifier;
	[windowController.window setFrameAutosaveName:identifier];
	completionHandler(windowController.window, nil);
}

- (void)encodeRestorableStateWithCoder:(NSCoder*)coder
{
	[super encodeRestorableStateWithCoder:coder];
	[coder encodeObject:self.leftPath forKey:LeftPathRestorationKey];
	[coder encodeObject:self.rightPath forKey:RightPathRestorationKey];
	[coder encodeInteger:self.filterControl.selectedSegment forKey:FolderFilterRestorationKey];
}

- (void)restoreStateWithCoder:(NSCoder*)coder
{
	[super restoreStateWithCoder:coder];
	if([coder containsValueForKey:FolderFilterRestorationKey])
	{
		NSInteger const selectedSegment = [coder decodeIntegerForKey:FolderFilterRestorationKey];
		if(selectedSegment >= 0 && selectedSegment < self.filterControl.segmentCount)
			self.filterControl.selectedSegment = selectedSegment;
	}
}

- (void)scheduleRefreshFromFileSystemEvent
{
	if(!NSThread.isMainThread)
	{
		__weak FolderWindowController* weakSelf = self;
		dispatch_async(dispatch_get_main_queue(), ^{
			[weakSelf scheduleRefreshFromFileSystemEvent];
		});
		return;
	}

	[self.fileSystemRefreshTimer invalidate];
	self.fileSystemRefreshTimer = [NSTimer scheduledTimerWithTimeInterval:0.2 target:self selector:@selector(fileSystemRefreshTimerDidFire:) userInfo:nil repeats:NO];
}

- (void)fileSystemRefreshTimerDidFire:(NSTimer*)timer
{
	self.fileSystemRefreshTimer = nil;
	[self refreshComparison];
}

- (void)ignoredFileNamesDidChange:(NSNotification*)notification
{
	[self refreshComparison];
}

- (void)refreshComparison
{
	NSUInteger const generation = ++self.scanGeneration;
	NSString* leftPath = self.leftPath;
	NSString* rightPath = self.rightPath;
	NSInteger const selectedRow = self.outlineView.selectedRow;
	NSString* selectedPath = selectedRow >= 0 ? [(FolderComparisonNode*)[self.outlineView itemAtRow:selectedRow] relativePath] : nil;
	NSArray<NSString*>* ignoredNames = [NSUserDefaults.standardUserDefaults stringArrayForKey:CompareMateIgnoredFileNamesDefaultsKey] ?: @[];
	NSSet<NSString*>* ignoredFileNames = [NSSet setWithArray:ignoredNames];
	self.statusLabel.stringValue = @"Comparing folders…";
	__weak FolderWindowController* weakSelf = self;
	dispatch_async(dispatch_get_global_queue(QOS_CLASS_USER_INITIATED, 0), ^{
		NSArray<FolderComparisonNode*>* allNodes = BuildFolderComparison(leftPath, rightPath, ignoredFileNames);
		dispatch_async(dispatch_get_main_queue(), ^{
			FolderWindowController* strongSelf = weakSelf;
			if(!strongSelf || generation != strongSelf.scanGeneration)
				return;
			NSSet<NSString*>* expandedFolderPaths = strongSelf.hasCompletedScan ? [strongSelf expandedFolderPaths] : nil;
			strongSelf.hasCompletedScan = YES;
			strongSelf.allNodes = allNodes;
			[strongSelf reloadOutlineKeepingSelection:selectedPath expandedFolderPaths:expandedFolderPaths];
		});
	});
}

- (NSSet<NSString*>*)expandedFolderPaths
{
	NSMutableSet<NSString*>* result = [NSMutableSet set];
	for(NSInteger row = 0; row < self.outlineView.numberOfRows; ++row)
	{
		FolderComparisonNode* node = [self.outlineView itemAtRow:row];
		if(node.isDirectory && [self.outlineView isItemExpanded:node] && node.relativePath.length)
			[result addObject:node.relativePath];
	}
	return result;
}

- (void)expandFolderNodes:(NSArray<FolderComparisonNode*>*)nodes matchingPaths:(NSSet<NSString*>*)expandedFolderPaths
{
	for(FolderComparisonNode* node in nodes)
	{
		if(node.isDirectory && [expandedFolderPaths containsObject:node.relativePath])
			[self.outlineView expandItem:node];
		[self expandFolderNodes:node.children matchingPaths:expandedFolderPaths];
	}
}

- (void)reloadOutlineKeepingSelection:(NSString*)selectedPath expandedFolderPaths:(NSSet<NSString*>*)expandedFolderPaths
{
	self.nodes = FilterFolderComparison(self.allNodes, self.filterControl.selectedSegment == 0);
	[self.outlineView reloadData];
	if(expandedFolderPaths)
		[self expandFolderNodes:self.nodes matchingPaths:expandedFolderPaths];
	else
		[self.outlineView expandItem:nil expandChildren:YES];

	if(selectedPath.length)
	{
		for(FolderComparisonNode* node in [self fileNodesInDisplayOrder])
		{
			if([node.relativePath isEqualToString:selectedPath])
			{
				NSInteger row = [self.outlineView rowForItem:node];
				if(row >= 0)
					[self.outlineView selectRowIndexes:[NSIndexSet indexSetWithIndex:row] byExtendingSelection:NO];
				break;
			}
		}
	}

	NSUInteger count = [self fileNodesInDisplayOrder].count;
	NSString* summary = count ? [NSString stringWithFormat:@"%lu different %@", count, count == 1 ? @"file" : @"files"] : @"No file differences";
	self.statusLabel.stringValue = [summary stringByAppendingString:@" — ⌘⌥←/→ copy; ⇧⌘⌥←/→ move"];
}

- (IBAction)changeFolderFilter:(id)sender
{
	FolderComparisonNode* selectedNode = [self selectedFileNode];
	NSSet<NSString*>* expandedFolderPaths = [self expandedFolderPaths];
	[self reloadOutlineKeepingSelection:selectedNode.relativePath expandedFolderPaths:expandedFolderPaths];
	[self invalidateRestorableState];
}

- (NSInteger)outlineView:(NSOutlineView*)outlineView numberOfChildrenOfItem:(id)item
{
	return item ? [(FolderComparisonNode*)item children].count : self.nodes.count;
}

- (id)outlineView:(NSOutlineView*)outlineView child:(NSInteger)index ofItem:(id)item
{
	return item ? [(FolderComparisonNode*)item children][index] : self.nodes[index];
}

- (BOOL)outlineView:(NSOutlineView*)outlineView isItemExpandable:(id)item
{
	return [(FolderComparisonNode*)item children].count != 0;
}

- (NSView*)outlineView:(NSOutlineView*)outlineView viewForTableColumn:(NSTableColumn*)tableColumn item:(id)item
{
	FolderComparisonNode* node = item;
	NSString* identifier = tableColumn.identifier;
	BOOL const leftSide = [identifier hasPrefix:@"left"];
	BOOL const nameColumn = [identifier hasSuffix:@"Name"];
	BOOL const dateColumn = [identifier hasSuffix:@"Date"];
	FolderEntryKind const kind = leftSide ? node.leftKind : node.rightKind;
	NSString* path = leftSide ? node.leftPath : node.rightPath;
	NSNumber* fileSize = leftSide ? node.leftFileSize : node.rightFileSize;
	NSDate* modificationDate = leftSide ? node.leftModificationDate : node.rightModificationDate;
	NSTableCellView* cell = [outlineView makeViewWithIdentifier:identifier owner:self];
	if(!cell)
	{
		if(nameColumn)
		{
			cell = [[NSTableCellView alloc] initWithFrame:NSMakeRect(0, 0, tableColumn.width, 22)];
			NSImageView* imageView = [[NSImageView alloc] initWithFrame:NSZeroRect];
			imageView.translatesAutoresizingMaskIntoConstraints = NO;
			imageView.imageAlignment = NSImageAlignCenter;
			imageView.imageScaling = NSImageScaleProportionallyDown;
			cell.imageView = imageView;
			[cell addSubview:imageView];

			NSTextField* textField = [NSTextField labelWithString:@""];
			textField.translatesAutoresizingMaskIntoConstraints = NO;
			textField.lineBreakMode = NSLineBreakByTruncatingMiddle;
			cell.textField = textField;
			[cell addSubview:textField];

			NSLayoutConstraint* imageLeadingConstraint = [imageView.leadingAnchor constraintEqualToAnchor:cell.leadingAnchor constant:2];
			imageLeadingConstraint.identifier = FolderIconLeadingConstraintIdentifier;
			[NSLayoutConstraint activateConstraints:@[
				imageLeadingConstraint,
				[imageView.centerYAnchor constraintEqualToAnchor:cell.centerYAnchor],
				[imageView.widthAnchor constraintEqualToConstant:16],
				[imageView.heightAnchor constraintEqualToConstant:16],
				[textField.leadingAnchor constraintEqualToAnchor:imageView.trailingAnchor constant:6],
				[textField.trailingAnchor constraintEqualToAnchor:cell.trailingAnchor constant:-4],
				[textField.centerYAnchor constraintEqualToAnchor:cell.centerYAnchor],
			]];
		}
		else
		{
			cell = [[NSTableCellView alloc] initWithFrame:NSMakeRect(0, 0, tableColumn.width, 22)];
			NSTextField* textField = [NSTextField labelWithString:@""];
			textField.lineBreakMode = NSLineBreakByTruncatingMiddle;
			textField.autoresizingMask = NSViewWidthSizable;
			cell.textField = textField;
			[cell addSubview:textField];
			textField.frame = NSMakeRect(4, 3, tableColumn.width - 8, 17);
			textField.textColor = NSColor.secondaryLabelColor;
			if([identifier hasSuffix:@"Size"])
				textField.alignment = NSTextAlignmentRight;
		}
		cell.identifier = identifier;
	}

	if(nameColumn)
	{
		cell.textField.stringValue = kind == FolderEntryKindMissing ? @"" : node.name ?: @"";
		cell.imageView.image = kind == FolderEntryKindMissing ? nil : [NSWorkspace.sharedWorkspace iconForFile:path];
		for(NSLayoutConstraint* constraint in cell.constraints)
		{
			if([constraint.identifier isEqualToString:FolderIconLeadingConstraintIdentifier])
			{
				constraint.constant = 2 + (leftSide ? 0 : [outlineView levelForItem:node] * outlineView.indentationPerLevel);
				break;
			}
		}
		cell.toolTip = kind == FolderEntryKindMissing ? nil : path;
	}
	else if(kind == FolderEntryKindMissing)
		cell.textField.stringValue = @"";
	else if(dateColumn)
		cell.textField.stringValue = modificationDate ? [self.dateFormatter stringFromDate:modificationDate] : @"";
	else
		cell.textField.stringValue = kind == FolderEntryKindFile && fileSize ? [NSByteCountFormatter stringFromByteCount:fileSize.longLongValue countStyle:NSByteCountFormatterCountStyleFile] : @"";
	return cell;
}

- (void)addFileNodes:(NSArray<FolderComparisonNode*>*)nodes toArray:(NSMutableArray<FolderComparisonNode*>*)result
{
	for(FolderComparisonNode* node in nodes)
	{
		if(node.representsFile)
			[result addObject:node];
		if(node.isDirectory)
			[self addFileNodes:node.children toArray:result];
	}
}

- (NSArray<FolderComparisonNode*>*)fileNodesInDisplayOrder
{
	NSMutableArray<FolderComparisonNode*>* result = [NSMutableArray array];
	[self addFileNodes:self.nodes toArray:result];
	return result;
}

- (FolderComparisonNode*)selectedFileNode
{
	NSInteger row = self.outlineView.selectedRow;
	FolderComparisonNode* node = row >= 0 ? [self.outlineView itemAtRow:row] : nil;
	return node.representsFile && node.relativePath.length ? node : nil;
}

- (BOOL)canOpenSelectedFile
{
	FolderComparisonNode* node = self.selectedFileNode;
	return node && node.leftKind != FolderEntryKindDirectory && node.rightKind != FolderEntryKindDirectory && node.leftKind != FolderEntryKindOther && node.rightKind != FolderEntryKindOther;
}

- (IBAction)openSelectedFile:(id)sender
{
	NSEventType const eventType = NSApp.currentEvent.type;
	BOOL const openedWithMouse = eventType == NSEventTypeLeftMouseDown || eventType == NSEventTypeLeftMouseUp;
	NSInteger row = openedWithMouse && self.outlineView.clickedRow >= 0 ? self.outlineView.clickedRow : self.outlineView.selectedRow;
	FolderComparisonNode* node = row >= 0 ? [self.outlineView itemAtRow:row] : nil;
	if(!node || node.leftKind == FolderEntryKindDirectory || node.rightKind == FolderEntryKindDirectory || node.leftKind == FolderEntryKindOther || node.rightKind == FolderEntryKindOther)
		return;
	WindowController* controller = [[WindowController alloc] initWithLeftPath:node.leftPath rightPath:node.rightPath];
	[controller.window center];
	[controller showWindow:self];
}

- (void)selectFileWithOffset:(NSInteger)offset
{
	NSArray<FolderComparisonNode*>* files = [self fileNodesInDisplayOrder];
	if(files.count == 0)
	{
		NSBeep();
		return;
	}
	FolderComparisonNode* selected = self.selectedFileNode;
	NSInteger index = selected ? [files indexOfObjectIdenticalTo:selected] : NSNotFound;
	if(index == NSNotFound)
		index = offset > 0 ? 0 : files.count - 1;
	else
		index = (index + offset + files.count) % files.count;
	NSInteger row = [self.outlineView rowForItem:files[index]];
	if(row >= 0)
	{
		[self.outlineView selectRowIndexes:[NSIndexSet indexSetWithIndex:row] byExtendingSelection:NO];
		[self.outlineView scrollRowToVisible:row];
	}
}

- (IBAction)nextChange:(id)sender { [self selectFileWithOffset:1]; }
- (IBAction)previousChange:(id)sender { [self selectFileWithOffset:-1]; }

- (void)showFileOperationError:(NSError*)error
{
	NSAlert* alert = [NSAlert alertWithError:error];
	[alert beginSheetModalForWindow:self.window completionHandler:nil];
}

- (void)performTransferOfNode:(FolderComparisonNode*)node toLeft:(BOOL)toLeft move:(BOOL)move
{
	NSString* sourcePath = toLeft ? node.rightPath : node.leftPath;
	NSString* targetPath = toLeft ? node.leftPath : node.rightPath;
	FolderEntryKind sourceKind = toLeft ? node.rightKind : node.leftKind;
	FolderEntryKind targetKind = toLeft ? node.leftKind : node.rightKind;
	if(sourceKind == FolderEntryKindMissing || sourceKind == FolderEntryKindDirectory || sourceKind == FolderEntryKindOther)
	{
		NSBeep();
		return;
	}

	void (^performTransfer)(void) = ^{
		NSFileManager* fileManager = NSFileManager.defaultManager;
		NSString* targetDirectory = targetPath.stringByDeletingLastPathComponent;
		NSError* error = nil;
		if(![fileManager createDirectoryAtPath:targetDirectory withIntermediateDirectories:YES attributes:nil error:&error])
		{
			[self showFileOperationError:error];
			return;
		}

		NSString* temporaryPath = [targetDirectory stringByAppendingPathComponent:[NSString stringWithFormat:@".CompareMate-%@", NSUUID.UUID.UUIDString]];
		if(![fileManager copyItemAtPath:sourcePath toPath:temporaryPath error:&error])
		{
			[self showFileOperationError:error];
			return;
		}

		BOOL const targetExists = [fileManager fileExistsAtPath:targetPath] || [fileManager attributesOfItemAtPath:targetPath error:nil] != nil;
		BOOL installed = targetExists ? [fileManager replaceItemAtURL:[NSURL fileURLWithPath:targetPath] withItemAtURL:[NSURL fileURLWithPath:temporaryPath] backupItemName:nil options:0 resultingItemURL:nil error:&error] : [fileManager moveItemAtPath:temporaryPath toPath:targetPath error:&error];
		if(!installed)
		{
			[fileManager removeItemAtPath:temporaryPath error:nil];
			[self showFileOperationError:error];
			return;
		}

		if(move && ![fileManager removeItemAtPath:sourcePath error:&error])
		{
			[self showFileOperationError:error];
			[self refreshComparison];
			return;
		}
		[self refreshComparison];
	};

	if(targetKind != FolderEntryKindMissing)
	{
		NSAlert* alert = [[NSAlert alloc] init];
		alert.alertStyle = NSAlertStyleWarning;
		alert.messageText = [NSString stringWithFormat:@"Replace “%@”?", targetPath.lastPathComponent];
		alert.informativeText = move ? @"The destination will be replaced, then the original file will be removed." : @"The destination file will be replaced.";
		[alert addButtonWithTitle:@"Replace"];
		[alert addButtonWithTitle:@"Cancel"];
		[alert beginSheetModalForWindow:self.window completionHandler:^(NSModalResponse response) {
			if(response == NSAlertFirstButtonReturn)
				performTransfer();
		}];
	}
	else
		performTransfer();
}

- (void)transferSelectedFileToLeft:(BOOL)toLeft move:(BOOL)move
{
	FolderComparisonNode* node = self.selectedFileNode;
	if(!node)
	{
		NSBeep();
		return;
	}
	[self performTransferOfNode:node toLeft:toLeft move:move];
}

- (IBAction)copyChangeToLeft:(id)sender { [self transferSelectedFileToLeft:YES move:NO]; }
- (IBAction)copyChangeToRight:(id)sender { [self transferSelectedFileToLeft:NO move:NO]; }
- (IBAction)moveChangeToLeft:(id)sender { [self transferSelectedFileToLeft:YES move:YES]; }
- (IBAction)moveChangeToRight:(id)sender { [self transferSelectedFileToLeft:NO move:YES]; }

- (void)windowWillClose:(NSNotification*)notification
{
	++self.scanGeneration;
	[self.fileSystemRefreshTimer invalidate];
	self.fileSystemRefreshTimer = nil;
	if(self.leftFileSystemObserver)
		[FSEventsManager.sharedInstance removeObserver:self.leftFileSystemObserver];
	if(self.rightFileSystemObserver)
		[FSEventsManager.sharedInstance removeObserver:self.rightFileSystemObserver];
	self.leftFileSystemObserver = nil;
	self.rightFileSystemObserver = nil;
	[NSNotificationCenter.defaultCenter removeObserver:self];
	_retainedSelf = nil;
}

- (void)windowDidBecomeKey:(NSNotification*)notification
{
	[self refreshComparison];
}

- (void)windowDidResize:(NSNotification*)notification
{
	[self resizeFolderColumnsToFit];
}
@end

@implementation DiffCharacterRange
@end

@interface DiffHighlightView : NSObject
@property (nonatomic, weak) OakTextView* textView;
@property (nonatomic, weak) GutterView* gutterView;
@property (nonatomic, copy) NSIndexSet* highlightedLines;
@property (nonatomic, copy) NSIndexSet* markerPositions;
@property (nonatomic, copy) NSIndexSet* activeHighlightedLines;
@property (nonatomic, copy) NSIndexSet* activeMarkerPositions;
@property (nonatomic, copy) NSArray<DiffCharacterRange*>* characterRanges;
@property (nonatomic, copy) NSArray<DiffCharacterRange*>* characterMarkers;
@property (nonatomic) NSUInteger lineCount;
@property (nonatomic) NSColor* highlightColor;
@property (nonatomic) NSColor* markerColor;
@property (nonatomic) NSColor* activeColor;
@property (nonatomic) NSColor* characterColor;
- (instancetype)initWithTextView:(OakTextView*)textView color:(NSColor*)color;
- (void)drawBackgroundInRect:(NSRect)dirtyRect;
- (void)drawForegroundInRect:(NSRect)dirtyRect;
- (void)drawGutterBackgroundInRect:(NSRect)dirtyRect;
@end

@implementation DiffHighlightView
- (instancetype)initWithTextView:(OakTextView*)textView color:(NSColor*)color
{
	if(self = [super init])
	{
		_textView = textView;
		_highlightedLines = NSIndexSet.indexSet;
		_markerPositions = NSIndexSet.indexSet;
		_activeHighlightedLines = NSIndexSet.indexSet;
		_activeMarkerPositions = NSIndexSet.indexSet;
		_characterRanges = @[];
		_characterMarkers = @[];
		_highlightColor = color;
		_markerColor = [color colorWithAlphaComponent:0.85];
		_activeColor = [NSColor.controlAccentColor colorWithAlphaComponent:0.38];
		_characterColor = [color colorWithAlphaComponent:0.52];
	}
	return self;
}

- (void)setHighlightedLines:(NSIndexSet*)highlightedLines
{
	if([_highlightedLines isEqualToIndexSet:highlightedLines])
		return;
	_highlightedLines = [highlightedLines copy];
	self.textView.needsDisplay = YES;
	self.gutterView.needsDisplay = YES;
}

- (void)setMarkerPositions:(NSIndexSet*)markerPositions
{
	if([_markerPositions isEqualToIndexSet:markerPositions])
		return;
	_markerPositions = [markerPositions copy];
	self.textView.needsDisplay = YES;
	self.gutterView.needsDisplay = YES;
}

- (void)setActiveHighlightedLines:(NSIndexSet*)activeHighlightedLines
{
	if([_activeHighlightedLines isEqualToIndexSet:activeHighlightedLines])
		return;
	_activeHighlightedLines = [activeHighlightedLines copy];
	self.textView.needsDisplay = YES;
	self.gutterView.needsDisplay = YES;
}

- (void)setActiveMarkerPositions:(NSIndexSet*)activeMarkerPositions
{
	if([_activeMarkerPositions isEqualToIndexSet:activeMarkerPositions])
		return;
	_activeMarkerPositions = [activeMarkerPositions copy];
	self.textView.needsDisplay = YES;
	self.gutterView.needsDisplay = YES;
}

- (void)setCharacterRanges:(NSArray<DiffCharacterRange*>*)characterRanges
{
	_characterRanges = [characterRanges copy];
	self.textView.needsDisplay = YES;
}

- (void)setCharacterMarkers:(NSArray<DiffCharacterRange*>*)characterMarkers
{
	_characterMarkers = [characterMarkers copy];
	self.textView.needsDisplay = YES;
}

- (void)setLineCount:(NSUInteger)lineCount
{
	if(_lineCount == lineCount)
		return;
	_lineCount = lineCount;
	self.textView.needsDisplay = YES;
	self.gutterView.needsDisplay = YES;
}

- (void)drawBackgroundInRect:(NSRect)dirtyRect
{
	if(!self.textView)
		return;

	[self.highlightColor setFill];
	NSRect const bounds = self.textView.bounds;
	[self.highlightedLines enumerateIndexesUsingBlock:^(NSUInteger line, BOOL* stop) {
		GVLineRecord const firstFragment = [self.textView lineFragmentForLine:line column:0];
		if(firstFragment.lineNumber != line)
			return;

		CGFloat bottom = 0;
		GVLineRecord const nextLine = [self.textView lineFragmentForLine:line + 1 column:0];
		if(nextLine.lineNumber == line + 1)
			bottom = nextLine.firstY;
		else
		{
			GVLineRecord const lastFragment = [self.textView lineFragmentForLine:line column:NSUIntegerMax];
			bottom = lastFragment.lastY;
		}

		NSRect lineRect = NSMakeRect(NSMinX(bounds), firstFragment.firstY, NSWidth(bounds), MAX(1, bottom - firstFragment.firstY));
		lineRect = NSIntersectionRect(lineRect, dirtyRect);
		if(!NSIsEmptyRect(lineRect))
			NSRectFillUsingOperation(lineRect, NSCompositingOperationSourceOver);
	}];

	[self.activeColor setFill];
	[self.activeHighlightedLines enumerateIndexesUsingBlock:^(NSUInteger line, BOOL* stop) {
		GVLineRecord const firstFragment = [self.textView lineFragmentForLine:line column:0];
		if(firstFragment.lineNumber != line)
			return;

		CGFloat bottom = 0;
		GVLineRecord const nextLine = [self.textView lineFragmentForLine:line + 1 column:0];
		if(nextLine.lineNumber == line + 1)
			bottom = nextLine.firstY;
		else
			bottom = [self.textView lineFragmentForLine:line column:NSUIntegerMax].lastY;

		NSRect lineRect = NSIntersectionRect(NSMakeRect(NSMinX(bounds), firstFragment.firstY, NSWidth(bounds), MAX(1, bottom - firstFragment.firstY)), dirtyRect);
		if(!NSIsEmptyRect(lineRect))
			NSRectFillUsingOperation(lineRect, NSCompositingOperationSourceOver);
	}];

	[self.characterColor setFill];
	for(DiffCharacterRange* range in self.characterRanges)
	{
		NSRect const layoutRect = [self.textView rectForLine:range.line byteColumnRange:range.byteColumns];
		NSRect const characterRect = NSIntersectionRect(layoutRect, dirtyRect);
		if(NSIsEmptyRect(characterRect))
			continue;

		NSRectFillUsingOperation(characterRect, NSCompositingOperationSourceOver);
		NSRect const underlineRect = NSIntersectionRect(NSMakeRect(NSMinX(layoutRect), NSMaxY(layoutRect) - 2, NSWidth(layoutRect), 2), dirtyRect);
		if(!NSIsEmptyRect(underlineRect))
			NSRectFillUsingOperation(underlineRect, NSCompositingOperationSourceOver);
	}
}

- (void)drawForegroundInRect:(NSRect)dirtyRect
{
	if(!self.textView)
		return;

	[self.markerColor setFill];
	NSRect const bounds = self.textView.bounds;
	[self.markerPositions enumerateIndexesUsingBlock:^(NSUInteger position, BOOL* stop) {
		CGFloat y = 0;
		if(position < self.lineCount)
		{
			GVLineRecord const line = [self.textView lineFragmentForLine:position column:0];
			if(line.lineNumber != position)
				return;
			y = line.firstY;
		}
		else if(position == self.lineCount && self.lineCount != 0)
		{
			GVLineRecord const lastLine = [self.textView lineFragmentForLine:self.lineCount - 1 column:NSUIntegerMax];
			if(lastLine.lineNumber != self.lineCount - 1)
				return;
			y = lastLine.lastY;
		}
		else
			return;

		NSRect markerRect = NSIntersectionRect(NSMakeRect(NSMinX(bounds), y - 1, NSWidth(bounds), 2), dirtyRect);
		if(!NSIsEmptyRect(markerRect))
			NSRectFillUsingOperation(markerRect, NSCompositingOperationSourceOver);
	}];

	[self.activeColor setFill];
	[self.activeMarkerPositions enumerateIndexesUsingBlock:^(NSUInteger position, BOOL* stop) {
		CGFloat y = 0;
		if(position < self.lineCount)
			y = [self.textView lineFragmentForLine:position column:0].firstY;
		else if(position == self.lineCount && self.lineCount != 0)
			y = [self.textView lineFragmentForLine:self.lineCount - 1 column:NSUIntegerMax].lastY;
		else
			return;

		NSRect markerRect = NSIntersectionRect(NSMakeRect(NSMinX(bounds), y - 2, NSWidth(bounds), 4), dirtyRect);
		if(!NSIsEmptyRect(markerRect))
			NSRectFillUsingOperation(markerRect, NSCompositingOperationSourceOver);
	}];

	[self.markerColor setFill];
	for(DiffCharacterRange* marker in self.characterMarkers)
	{
		NSRect const caretRect = [self.textView caretRectForLine:marker.line byteColumn:marker.byteColumns.location];
		NSRect const markerRect = NSIntersectionRect(NSMakeRect(NSMinX(caretRect) - 1, NSMinY(caretRect) + 1, 2, MAX(2, NSHeight(caretRect) - 2)), dirtyRect);
		if(!NSIsEmptyRect(markerRect))
			NSRectFillUsingOperation(markerRect, NSCompositingOperationSourceOver);
	}
}

- (void)drawGutterBackgroundInRect:(NSRect)dirtyRect
{
	if(!self.textView || !self.gutterView)
		return;

	NSRect const bounds = self.gutterView.bounds;
	void (^drawLines)(NSIndexSet*, NSColor*) = ^(NSIndexSet* lines, NSColor* color) {
		[color setFill];
		[lines enumerateIndexesUsingBlock:^(NSUInteger line, BOOL* stop) {
			GVLineRecord const firstFragment = [self.textView lineFragmentForLine:line column:0];
			if(firstFragment.lineNumber != line)
				return;

			GVLineRecord const nextLine = [self.textView lineFragmentForLine:line + 1 column:0];
			CGFloat const bottom = nextLine.lineNumber == line + 1 ? nextLine.firstY : [self.textView lineFragmentForLine:line column:NSUIntegerMax].lastY;
			NSRect const lineRect = NSIntersectionRect(NSMakeRect(NSMinX(bounds), firstFragment.firstY, NSWidth(bounds), MAX(1, bottom - firstFragment.firstY)), dirtyRect);
			if(!NSIsEmptyRect(lineRect))
				NSRectFillUsingOperation(lineRect, NSCompositingOperationSourceOver);
		}];
	};
	drawLines(self.highlightedLines, self.highlightColor);
	drawLines(self.activeHighlightedLines, self.activeColor);

	void (^drawMarkers)(NSIndexSet*, CGFloat, NSColor*) = ^(NSIndexSet* positions, CGFloat thickness, NSColor* color) {
		[color setFill];
		[positions enumerateIndexesUsingBlock:^(NSUInteger position, BOOL* stop) {
			CGFloat y = 0;
			if(position < self.lineCount)
				y = [self.textView lineFragmentForLine:position column:0].firstY;
			else if(position == self.lineCount && self.lineCount != 0)
				y = [self.textView lineFragmentForLine:self.lineCount - 1 column:NSUIntegerMax].lastY;
			else
				return;

			NSRect const markerRect = NSIntersectionRect(NSMakeRect(NSMinX(bounds), y - thickness / 2, NSWidth(bounds), thickness), dirtyRect);
			if(!NSIsEmptyRect(markerRect))
				NSRectFillUsingOperation(markerRect, NSCompositingOperationSourceOver);
		}];
	};
	drawMarkers(self.markerPositions, 2, self.markerColor);
	drawMarkers(self.activeMarkerPositions, 4, self.activeColor);
}
@end

@interface DiffScrollTransition : NSObject
@property (nonatomic) NSUInteger sourceBoundary;
@property (nonatomic) NSUInteger targetStart;
@property (nonatomic) NSUInteger targetEnd;
@end

@implementation DiffScrollTransition
@end

@interface DiffHunk : NSObject
@property (nonatomic) NSRange leftLines;
@property (nonatomic) NSRange rightLines;
@end

@implementation DiffHunk
@end

@interface DiffCharacterToken : NSObject
@property (nonatomic) NSString* string;
@property (nonatomic) NSRange byteRange;
@end

@implementation DiffCharacterToken
@end

static NSArray<DiffCharacterToken*>* CharacterTokensForLine (NSString* line)
{
	NSMutableArray<DiffCharacterToken*>* tokens = [NSMutableArray array];
	__block NSUInteger byteOffset = 0;
	[line enumerateSubstringsInRange:NSMakeRange(0, line.length) options:NSStringEnumerationByComposedCharacterSequences usingBlock:^(NSString* substring, NSRange substringRange, NSRange enclosingRange, BOOL* stop) {
		NSUInteger const byteLength = [substring lengthOfBytesUsingEncoding:NSUTF8StringEncoding];
		DiffCharacterToken* token = [[DiffCharacterToken alloc] init];
		token.string = substring;
		token.byteRange = NSMakeRange(byteOffset, byteLength);
		[tokens addObject:token];
		byteOffset += byteLength;
	}];
	return tokens;
}

static void AppendCharacterRange (NSArray<DiffCharacterToken*>* tokens, NSRange tokenRange, NSUInteger line, NSMutableArray<DiffCharacterRange*>* ranges)
{
	if(tokenRange.length == 0)
		return;

	DiffCharacterToken* first = tokens[tokenRange.location];
	DiffCharacterToken* last = tokens[NSMaxRange(tokenRange) - 1];
	DiffCharacterRange* range = [[DiffCharacterRange alloc] init];
	range.line = line;
	range.byteColumns = NSMakeRange(first.byteRange.location, NSMaxRange(last.byteRange) - first.byteRange.location);
	[ranges addObject:range];
}

static NSUInteger ByteColumnForTokenBoundary (NSArray<DiffCharacterToken*>* tokens, NSUInteger tokenIndex)
{
	if(tokenIndex < tokens.count)
		return tokens[tokenIndex].byteRange.location;
	return tokens.count ? NSMaxRange(tokens.lastObject.byteRange) : 0;
}

static void AppendCharacterMarker (NSArray<DiffCharacterToken*>* tokens, NSUInteger tokenIndex, NSUInteger line, NSMutableArray<DiffCharacterRange*>* markers)
{
	DiffCharacterRange* marker = [[DiffCharacterRange alloc] init];
	marker.line = line;
	marker.byteColumns = NSMakeRange(ByteColumnForTokenBoundary(tokens, tokenIndex), 0);
	[markers addObject:marker];
}

static void AppendCharacterDifferences (NSString* leftLine, NSString* rightLine, NSUInteger leftLineNumber, NSUInteger rightLineNumber, NSMutableArray<DiffCharacterRange*>* leftRanges, NSMutableArray<DiffCharacterRange*>* rightRanges, NSMutableArray<DiffCharacterRange*>* leftMarkers, NSMutableArray<DiffCharacterRange*>* rightMarkers)
{
	NSArray<DiffCharacterToken*>* leftTokens = CharacterTokensForLine(leftLine);
	NSArray<DiffCharacterToken*>* rightTokens = CharacterTokensForLine(rightLine);
	if(leftTokens.count + rightTokens.count > 8192)
		return;

	NSArray<NSString*>* leftCharacters = [leftTokens valueForKey:@"string"];
	NSArray<NSString*>* rightCharacters = [rightTokens valueForKey:@"string"];
	NSOrderedCollectionDifference<NSString*>* difference = [rightCharacters differenceFromArray:leftCharacters];
	NSMutableIndexSet* removedCharacters = [NSMutableIndexSet indexSet];
	NSMutableIndexSet* insertedCharacters = [NSMutableIndexSet indexSet];
	for(NSOrderedCollectionChange<NSString*>* removal in difference.removals)
		[removedCharacters addIndex:removal.index];
	for(NSOrderedCollectionChange<NSString*>* insertion in difference.insertions)
		[insertedCharacters addIndex:insertion.index];

	NSUInteger leftIndex = 0;
	NSUInteger rightIndex = 0;
	while(leftIndex < leftTokens.count || rightIndex < rightTokens.count)
	{
		BOOL const leftChanged = leftIndex < leftTokens.count && [removedCharacters containsIndex:leftIndex];
		BOOL const rightChanged = rightIndex < rightTokens.count && [insertedCharacters containsIndex:rightIndex];
		if(leftIndex < leftTokens.count && rightIndex < rightTokens.count && !leftChanged && !rightChanged)
		{
			++leftIndex;
			++rightIndex;
			continue;
		}

		NSUInteger const leftStart = leftIndex;
		NSUInteger const rightStart = rightIndex;
		while(leftIndex < leftTokens.count && [removedCharacters containsIndex:leftIndex])
			++leftIndex;
		while(rightIndex < rightTokens.count && [insertedCharacters containsIndex:rightIndex])
			++rightIndex;
		NSUInteger const removedCount = leftIndex - leftStart;
		NSUInteger const insertedCount = rightIndex - rightStart;
		AppendCharacterRange(leftTokens, NSMakeRange(leftStart, removedCount), leftLineNumber, leftRanges);
		AppendCharacterRange(rightTokens, NSMakeRange(rightStart, insertedCount), rightLineNumber, rightRanges);
		if(insertedCount > removedCount)
			AppendCharacterMarker(leftTokens, leftIndex, leftLineNumber, leftMarkers);
		else if(removedCount > insertedCount)
			AppendCharacterMarker(rightTokens, rightIndex, rightLineNumber, rightMarkers);

		if(leftStart == leftIndex && rightStart == rightIndex)
			break;
	}
}

@interface WindowController () <NSWindowDelegate>
@property (nonatomic) NSWindowController* retainedSelf;
@property (nonatomic) OakDocumentView* leftDocumentView;
@property (nonatomic) OakDocumentView* rightDocumentView;
@property (nonatomic) DiffHighlightView* leftDiffHighlightView;
@property (nonatomic) DiffHighlightView* rightDiffHighlightView;
@property (nonatomic) NSSplitViewController* splitViewController;
@property (nonatomic, copy) NSString* leftPath;
@property (nonatomic, copy) NSString* rightPath;
@property (nonatomic) BOOL leftDocumentLoaded;
@property (nonatomic) BOOL rightDocumentLoaded;
@property (nonatomic) NSUInteger diffGeneration;
@property (nonatomic, copy) NSArray<NSNumber*>* leftToRightLineMap;
@property (nonatomic, copy) NSArray<NSNumber*>* rightToLeftLineMap;
@property (nonatomic, copy) NSArray<DiffScrollTransition*>* leftToRightScrollTransitions;
@property (nonatomic, copy) NSArray<DiffScrollTransition*>* rightToLeftScrollTransitions;
@property (nonatomic, copy) NSArray<DiffHunk*>* diffHunks;
@property (nonatomic) NSInteger activeDiffHunkIndex;
@property (nonatomic, weak) NSClipView* leftClipView;
@property (nonatomic, weak) NSClipView* rightClipView;
@property (nonatomic, weak) NSClipView* lastScrolledClipView;
@property (nonatomic) BOOL synchronizingScroll;
@property (nonatomic) BOOL closingWithoutSaving;
@property (nonatomic) id leftFileSystemObserver;
@property (nonatomic) id rightFileSystemObserver;
@property (nonatomic) NSTimer* fileSystemRefreshTimer;
@property (nonatomic) BOOL leftFileSystemNeedsRefresh;
@property (nonatomic) BOOL rightFileSystemNeedsRefresh;
@property (nonatomic) BOOL handlingExternalFileChanges;
@property (nonatomic) NSButton* leftTitlebarFileButton;
@property (nonatomic) NSButton* rightTitlebarFileButton;
@property (nonatomic) NSTextField* leftTitlebarFileName;
@property (nonatomic) NSTextField* rightTitlebarFileName;
- (OakDocumentView*)activeDocumentView;
@end

@implementation WindowController
+ (void)initialize
{
	NSWindow.allowsAutomaticWindowTabbing = NO;
}

- (instancetype)init
{
	return [self initWithLeftPath:nil rightPath:nil];
}

- (instancetype)initWithLeftPath:(NSString*)leftPath rightPath:(NSString*)rightPath
{
	NSRect const contentRect = NSMakeRect(0, 0, 1200, 760);
	NSWindowStyleMask const styleMask = NSWindowStyleMaskTitled|NSWindowStyleMaskResizable|NSWindowStyleMaskClosable|NSWindowStyleMaskMiniaturizable;
	if(self = [self initWithWindow:[[NSWindow alloc] initWithContentRect:contentRect styleMask:styleMask backing:NSBackingStoreBuffered defer:NO]])
	{
		_retainedSelf = self;
		_leftPath = [leftPath copy];
		_rightPath = [rightPath copy];
		_activeDiffHunkIndex = -1;

		self.leftDocumentView = [[OakDocumentView alloc] initWithFrame:NSZeroRect];
		self.leftDocumentView.document = [OakDocument documentWithString:@"" fileType:@"text.plain" customName:@"Left"];
		self.leftDocumentView.textView.softWrap = YES;
		self.leftDocumentView.textView.scrollPastEnd = YES;

		self.rightDocumentView = [[OakDocumentView alloc] initWithFrame:NSZeroRect];
		self.rightDocumentView.document = [OakDocument documentWithString:@"" fileType:@"text.plain" customName:@"Right"];
		self.rightDocumentView.textView.softWrap = YES;
		self.rightDocumentView.textView.scrollPastEnd = YES;

		self.leftDiffHighlightView = [[DiffHighlightView alloc] initWithTextView:self.leftDocumentView.textView color:[NSColor.systemRedColor colorWithAlphaComponent:0.16]];
		self.leftDiffHighlightView.gutterView = self.leftDocumentView.gutterView;
		__weak DiffHighlightView* weakLeftDiffHighlightView = self.leftDiffHighlightView;
		self.leftDocumentView.textView.decorationDrawingBlock = ^(NSRect dirtyRect, OTVDecorationLayer layer) {
			if(layer == OTVDecorationLayerBackground)
				[weakLeftDiffHighlightView drawBackgroundInRect:dirtyRect];
			else
				[weakLeftDiffHighlightView drawForegroundInRect:dirtyRect];
		};
		self.leftDocumentView.gutterView.backgroundDecorationDrawingBlock = ^(NSRect dirtyRect) {
			[weakLeftDiffHighlightView drawGutterBackgroundInRect:dirtyRect];
		};
		self.rightDiffHighlightView = [[DiffHighlightView alloc] initWithTextView:self.rightDocumentView.textView color:[NSColor.systemGreenColor colorWithAlphaComponent:0.16]];
		self.rightDiffHighlightView.gutterView = self.rightDocumentView.gutterView;
		__weak DiffHighlightView* weakRightDiffHighlightView = self.rightDiffHighlightView;
		self.rightDocumentView.textView.decorationDrawingBlock = ^(NSRect dirtyRect, OTVDecorationLayer layer) {
			if(layer == OTVDecorationLayerBackground)
				[weakRightDiffHighlightView drawBackgroundInRect:dirtyRect];
			else
				[weakRightDiffHighlightView drawForegroundInRect:dirtyRect];
		};
		self.rightDocumentView.gutterView.backgroundDecorationDrawingBlock = ^(NSRect dirtyRect) {
			[weakRightDiffHighlightView drawGutterBackgroundInRect:dirtyRect];
		};

		self.leftClipView = self.leftDocumentView.textView.enclosingScrollView.contentView;
		self.rightClipView = self.rightDocumentView.textView.enclosingScrollView.contentView;
		self.leftClipView.postsBoundsChangedNotifications = YES;
		self.rightClipView.postsBoundsChangedNotifications = YES;
		[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(scrollBoundsDidChange:) name:NSViewBoundsDidChangeNotification object:self.leftClipView];
		[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(scrollBoundsDidChange:) name:NSViewBoundsDidChangeNotification object:self.rightClipView];

		NSViewController* leftViewController = [[NSViewController alloc] init];
		leftViewController.view = self.leftDocumentView;

		NSViewController* rightViewController = [[NSViewController alloc] init];
		rightViewController.view = self.rightDocumentView;

		NSSplitViewItem* leftItem = [NSSplitViewItem splitViewItemWithViewController:leftViewController];
		leftItem.minimumThickness = 200;
		leftItem.canCollapse = NO;

		NSSplitViewItem* rightItem = [NSSplitViewItem splitViewItemWithViewController:rightViewController];
		rightItem.minimumThickness = 200;
		rightItem.canCollapse = NO;

		self.splitViewController = [[NSSplitViewController alloc] init];
		self.splitViewController.splitView.vertical = YES;
		self.splitViewController.splitView.dividerStyle = NSSplitViewDividerStyleThin;
		[self.splitViewController addSplitViewItem:leftItem];
		[self.splitViewController addSplitViewItem:rightItem];

		NSWindow* window = self.window;
		window.title = leftPath && rightPath ? [NSString stringWithFormat:@"%@ ↔ %@", leftPath.lastPathComponent, rightPath.lastPathComponent] : @"CompareMate";
		window.delegate = self;
		window.minSize = NSMakeSize(720, 400);
		window.contentViewController = self.splitViewController;
		window.initialFirstResponder = self.leftDocumentView.textView;
		window.identifier = [NSString stringWithFormat:@"CompareMate.Comparison.%@", NSUUID.UUID.UUIDString];
		[window setFrameUsingName:FileComparisonFrameAutosaveName];
		window.restorationClass = WindowController.class;
		window.restorable = YES;
		[self addComparisonTitleToWindow:window];
		[self updateTitlebarFileButtons];

		[window layoutIfNeeded];
		[self.splitViewController.splitView setPosition:NSWidth(self.splitViewController.splitView.bounds) / 2 ofDividerAtIndex:0];
		[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(splitViewDidResizeSubviews:) name:NSSplitViewDidResizeSubviewsNotification object:self.splitViewController.splitView];

		if(leftPath)
			[self loadDocumentAtPath:leftPath intoDocumentView:self.leftDocumentView sideName:@"Left"];
		if(rightPath)
			[self loadDocumentAtPath:rightPath intoDocumentView:self.rightDocumentView sideName:@"Right"];
		[self startObservingComparedFiles];

		[self invalidateRestorableState];
	}
	return self;
}

- (NSButton*)titlebarFileButtonForLeftSide:(BOOL)leftSide
{
	NSButton* button = [[NSButton alloc] initWithFrame:NSZeroRect];
	button.translatesAutoresizingMaskIntoConstraints = NO;
	button.bordered = NO;
	button.imagePosition = NSImageOnly;
	button.imageScaling = NSImageScaleProportionallyDown;
	button.target = self;
	button.action = @selector(titlebarFileButtonClicked:);
	button.tag = leftSide ? 0 : 1;
	button.accessibilityLabel = leftSide ? @"Left comparison file" : @"Right comparison file";
	[NSLayoutConstraint activateConstraints:@[
		[button.widthAnchor constraintEqualToConstant:18],
		[button.heightAnchor constraintEqualToConstant:18],
	]];
	return button;
}

- (NSTextField*)titlebarFileNameLabel
{
	NSTextField* label = [NSTextField labelWithString:@""];
	label.translatesAutoresizingMaskIntoConstraints = NO;
	label.font = [NSFont systemFontOfSize:NSFont.systemFontSize weight:NSFontWeightSemibold];
	label.lineBreakMode = NSLineBreakByTruncatingMiddle;
	label.maximumNumberOfLines = 1;
	[label setContentCompressionResistancePriority:NSLayoutPriorityDefaultLow forOrientation:NSLayoutConstraintOrientationHorizontal];
	return label;
}

- (void)addComparisonTitleToWindow:(NSWindow*)window
{
	self.leftTitlebarFileButton = [self titlebarFileButtonForLeftSide:YES];
	self.rightTitlebarFileButton = [self titlebarFileButtonForLeftSide:NO];
	self.leftTitlebarFileName = [self titlebarFileNameLabel];
	self.rightTitlebarFileName = [self titlebarFileNameLabel];

	NSTextField* separator = [NSTextField labelWithString:@"↔"];
	separator.translatesAutoresizingMaskIntoConstraints = NO;
	separator.textColor = NSColor.secondaryLabelColor;

	NSStackView* titleView = [NSStackView stackViewWithViews:@[
		self.leftTitlebarFileButton,
		self.leftTitlebarFileName,
		separator,
		self.rightTitlebarFileButton,
		self.rightTitlebarFileName,
	]];
	titleView.translatesAutoresizingMaskIntoConstraints = NO;
	titleView.orientation = NSUserInterfaceLayoutOrientationHorizontal;
	titleView.alignment = NSLayoutAttributeCenterY;
	titleView.spacing = 4;
	[titleView setCustomSpacing:10 afterView:self.leftTitlebarFileName];
	[titleView setCustomSpacing:10 afterView:separator];

	NSButton* closeButton = [window standardWindowButton:NSWindowCloseButton];
	NSButton* zoomButton = [window standardWindowButton:NSWindowZoomButton];
	NSView* titlebarView = closeButton.superview;
	[titlebarView addSubview:titleView];
	window.titleVisibility = NSWindowTitleHidden;
	[NSLayoutConstraint activateConstraints:@[
		[titleView.centerXAnchor constraintEqualToAnchor:titlebarView.centerXAnchor],
		[titleView.centerYAnchor constraintEqualToAnchor:closeButton.centerYAnchor],
		[titleView.leadingAnchor constraintGreaterThanOrEqualToAnchor:zoomButton.trailingAnchor constant:12],
		[titleView.trailingAnchor constraintLessThanOrEqualToAnchor:titlebarView.trailingAnchor constant:-12],
	]];
}

- (void)updateTitlebarFileButton:(NSButton*)button nameLabel:(NSTextField*)nameLabel path:(NSString*)path
{
	button.hidden = path.length == 0;
	nameLabel.hidden = path.length == 0;
	button.toolTip = path;
	nameLabel.toolTip = path;
	nameLabel.stringValue = path.lastPathComponent ?: @"";
	if(!path.length)
	{
		button.image = nil;
		return;
	}

	NSImage* image = [NSWorkspace.sharedWorkspace iconForFile:path].copy;
	image.size = NSMakeSize(16, 16);
	button.image = image;
}

- (void)updateTitlebarFileButtons
{
	[self updateTitlebarFileButton:self.leftTitlebarFileButton nameLabel:self.leftTitlebarFileName path:self.leftPath];
	[self updateTitlebarFileButton:self.rightTitlebarFileButton nameLabel:self.rightTitlebarFileName path:self.rightPath];
}

- (IBAction)titlebarFileButtonClicked:(NSButton*)sender
{
	if((NSApp.currentEvent.modifierFlags & NSEventModifierFlagCommand) == 0)
		return;

	NSString* path = sender.tag == 0 ? self.leftPath : self.rightPath;
	if(!path.length)
		return;

	NSMenu* menu = [[NSMenu alloc] initWithTitle:@""];
	for(NSString* itemPath = path; itemPath.length; itemPath = itemPath.stringByDeletingLastPathComponent)
	{
		NSString* title = [NSFileManager.defaultManager displayNameAtPath:itemPath];
		NSMenuItem* item = [menu addItemWithTitle:title.length ? title : itemPath action:@selector(openTitlebarPathItem:) keyEquivalent:@""];
		item.target = self;
		item.representedObject = [NSURL fileURLWithPath:itemPath];
		NSImage* image = [NSWorkspace.sharedWorkspace iconForFile:itemPath].copy;
		image.size = NSMakeSize(16, 16);
		item.image = image;
		if([itemPath isEqualToString:@"/"])
			break;
	}
	NSRect bounds = sender.bounds;
	[menu popUpMenuPositioningItem:nil atLocation:NSMakePoint(NSMinX(bounds) - 14, NSMinY(bounds) - 3) inView:sender];
}

- (IBAction)openTitlebarPathItem:(NSMenuItem*)sender
{
	NSURL* URL = sender.representedObject;
	if(!URL.isFileURL)
		return;

	NSNumber* isDirectory = nil;
	[URL getResourceValue:&isDirectory forKey:NSURLIsDirectoryKey error:nil];
	if(isDirectory.boolValue)
		[NSWorkspace.sharedWorkspace openURL:URL];
	else
		[NSWorkspace.sharedWorkspace activateFileViewerSelectingURLs:@[ URL ]];
}

+ (void)restoreWindowWithIdentifier:(NSUserInterfaceItemIdentifier)identifier state:(NSCoder*)state completionHandler:(void (^)(NSWindow*, NSError*))completionHandler
{
	NSString* leftPath = [state decodeObjectOfClass:NSString.class forKey:LeftPathRestorationKey];
	NSString* rightPath = [state decodeObjectOfClass:NSString.class forKey:RightPathRestorationKey];
	BOOL leftExists = leftPath.length && [NSFileManager.defaultManager fileExistsAtPath:leftPath];
	BOOL rightExists = rightPath.length && [NSFileManager.defaultManager fileExistsAtPath:rightPath];
	if(!leftExists || !rightExists)
	{
		completionHandler(nil, nil);
		return;
	}

	WindowController* windowController = [[WindowController alloc] initWithLeftPath:leftPath rightPath:rightPath];
	windowController.window.identifier = identifier;

	completionHandler(windowController.window, nil);
}

- (void)stopObservingComparedFiles
{
	[self.fileSystemRefreshTimer invalidate];
	self.fileSystemRefreshTimer = nil;
	self.leftFileSystemNeedsRefresh = NO;
	self.rightFileSystemNeedsRefresh = NO;
	if(self.leftFileSystemObserver)
		[KEventManager.sharedInstance removeObserver:self.leftFileSystemObserver];
	if(self.rightFileSystemObserver)
		[KEventManager.sharedInstance removeObserver:self.rightFileSystemObserver];
	self.leftFileSystemObserver = nil;
	self.rightFileSystemObserver = nil;
}

- (void)startObservingComparedFiles
{
	[self stopObservingComparedFiles];
	__weak WindowController* weakSelf = self;
	if(self.leftPath.length)
	{
		self.leftFileSystemObserver = [KEventManager.sharedInstance addObserverToItemAtURL:[NSURL fileURLWithPath:self.leftPath] usingBlock:^(NSURL* URL, NSUInteger mask) {
			[weakSelf scheduleFileSystemRefreshForLeftSide:YES];
		}];
	}
	if(self.rightPath.length)
	{
		self.rightFileSystemObserver = [KEventManager.sharedInstance addObserverToItemAtURL:[NSURL fileURLWithPath:self.rightPath] usingBlock:^(NSURL* URL, NSUInteger mask) {
			[weakSelf scheduleFileSystemRefreshForLeftSide:NO];
		}];
	}
}

- (void)scheduleFileSystemRefreshForLeftSide:(BOOL)leftSide
{
	if(!NSThread.isMainThread)
	{
		__weak WindowController* weakSelf = self;
		dispatch_async(dispatch_get_main_queue(), ^{
			[weakSelf scheduleFileSystemRefreshForLeftSide:leftSide];
		});
		return;
	}

	if(leftSide)
		self.leftFileSystemNeedsRefresh = YES;
	else
		self.rightFileSystemNeedsRefresh = YES;
	[self.fileSystemRefreshTimer invalidate];
	self.fileSystemRefreshTimer = [NSTimer scheduledTimerWithTimeInterval:0.2 target:self selector:@selector(fileSystemRefreshTimerDidFire:) userInfo:nil repeats:NO];
}

- (void)handleExternalChangeForDocumentView:(OakDocumentView*)documentView completionHandler:(void (^)(void))completionHandler
{
	OakDocument* document = documentView.document;
	if(!document.isLoaded || !document.path.length)
	{
		completionHandler();
		return;
	}

	BOOL isDirectory = NO;
	if(![NSFileManager.defaultManager fileExistsAtPath:document.path isDirectory:&isDirectory] || isDirectory)
	{
		completionHandler();
		return;
	}

	void (^reloadDocument)(void) = ^{
		if(documentView.document == document)
		{
			if(document.isDocumentEdited)
				[document markDocumentSaved];
			document.onDisk = YES;
			[document importDocumentChanges:self];
		}
		completionHandler();
	};

	if(!document.isDocumentEdited)
	{
		reloadDocument();
		return;
	}

	NSAlert* alert = [[NSAlert alloc] init];
	alert.alertStyle = NSAlertStyleWarning;
	alert.messageText = [NSString stringWithFormat:@"“%@” Was Changed Outside CompareMate", document.displayName];
	alert.informativeText = @"This file also has unsaved changes in CompareMate. Reload the version on disk and discard the local changes, or ignore the external change?";
	[alert addButtonWithTitle:@"Ignore"];
	[alert addButtonWithTitle:@"Reload"];
	[alert beginSheetModalForWindow:self.window completionHandler:^(NSModalResponse response) {
		if(response == NSAlertSecondButtonReturn)
			reloadDocument();
		else
			completionHandler();
	}];
}

- (void)fileSystemRefreshTimerDidFire:(NSTimer*)timer
{
	self.fileSystemRefreshTimer = nil;
	if(self.handlingExternalFileChanges)
		return;

	BOOL const refreshLeft = self.leftFileSystemNeedsRefresh;
	BOOL const refreshRight = self.rightFileSystemNeedsRefresh;
	self.leftFileSystemNeedsRefresh = NO;
	self.rightFileSystemNeedsRefresh = NO;
	self.handlingExternalFileChanges = YES;

	__weak WindowController* weakSelf = self;
	void (^finish)(void) = ^{
		WindowController* strongSelf = weakSelf;
		if(!strongSelf)
			return;
		strongSelf.handlingExternalFileChanges = NO;
		if(strongSelf.leftFileSystemNeedsRefresh || strongSelf.rightFileSystemNeedsRefresh)
			[strongSelf scheduleFileSystemRefreshForLeftSide:strongSelf.leftFileSystemNeedsRefresh];
	};
	void (^handleRight)(void) = ^{
		WindowController* strongSelf = weakSelf;
		if(!strongSelf)
			return;
		if(refreshRight)
			[strongSelf handleExternalChangeForDocumentView:strongSelf.rightDocumentView completionHandler:finish];
		else
			finish();
	};
	if(refreshLeft)
		[self handleExternalChangeForDocumentView:self.leftDocumentView completionHandler:handleRight];
	else
		handleRight();
}

- (void)encodeRestorableStateWithCoder:(NSCoder*)coder
{
	[super encodeRestorableStateWithCoder:coder];
	[coder encodeObject:self.leftPath forKey:LeftPathRestorationKey];
	[coder encodeObject:self.rightPath forKey:RightPathRestorationKey];
	[coder encodeDouble:NSMaxX(self.leftDocumentView.frame) forKey:DividerPositionRestorationKey];
}

- (void)restoreStateWithCoder:(NSCoder*)coder
{
	[super restoreStateWithCoder:coder];
	if([coder containsValueForKey:DividerPositionRestorationKey])
	{
		CGFloat dividerPosition = [coder decodeDoubleForKey:DividerPositionRestorationKey];
		[self.window layoutIfNeeded];
		[self.splitViewController.splitView setPosition:dividerPosition ofDividerAtIndex:0];
	}
}

- (void)splitViewDidResizeSubviews:(NSNotification*)notification
{
	[self invalidateRestorableState];
}

- (CGFloat)bottomForLine:(NSUInteger)line lineCount:(NSUInteger)lineCount textView:(OakTextView*)textView
{
	if(line + 1 < lineCount)
	{
		GVLineRecord const nextLine = [textView lineFragmentForLine:line + 1 column:0];
		if(nextLine.lineNumber == line + 1)
			return nextLine.firstY;
	}

	GVLineRecord const lastFragment = [textView lineFragmentForLine:line column:NSUIntegerMax];
	return lastFragment.lastY;
}

- (CGFloat)yPositionForLineBoundary:(NSUInteger)position lineCount:(NSUInteger)lineCount textView:(OakTextView*)textView
{
	if(position < lineCount)
	{
		GVLineRecord const line = [textView lineFragmentForLine:position column:0];
		return line.firstY;
	}
	if(position == lineCount && lineCount != 0)
		return [self bottomForLine:lineCount - 1 lineCount:lineCount textView:textView];
	return 0;
}

- (void)scrollBoundsDidChange:(NSNotification*)notification
{
	if(self.synchronizingScroll)
		return;

	NSClipView* sourceClipView = notification.object;
	if(sourceClipView != self.leftClipView && sourceClipView != self.rightClipView)
		return;

	self.lastScrolledClipView = sourceClipView;
	[self synchronizeScrollFromClipView:sourceClipView];
}

- (void)synchronizeScrollFromClipView:(NSClipView*)sourceClipView
{
	if(!sourceClipView || self.synchronizingScroll)
		return;

	BOOL const sourceIsLeft = sourceClipView == self.leftClipView;
	NSClipView* targetClipView = sourceIsLeft ? self.rightClipView : self.leftClipView;
	OakTextView* sourceTextView = sourceIsLeft ? self.leftDocumentView.textView : self.rightDocumentView.textView;
	OakTextView* targetTextView = sourceIsLeft ? self.rightDocumentView.textView : self.leftDocumentView.textView;
	NSArray<NSNumber*>* lineMap = sourceIsLeft ? self.leftToRightLineMap : self.rightToLeftLineMap;
	NSArray<DiffScrollTransition*>* scrollTransitions = sourceIsLeft ? self.leftToRightScrollTransitions : self.rightToLeftScrollTransitions;
	NSUInteger const targetLineCount = (sourceIsLeft ? self.rightToLeftLineMap : self.leftToRightLineMap).count;
	if(!targetClipView || lineMap.count == 0 || targetLineCount == 0)
		return;

	NSRect const sourceBounds = sourceClipView.bounds;
	CGFloat const sourceAnchorY = NSMidY(sourceBounds);
	GVLineRecord const sourceLine = [sourceTextView lineRecordForPosition:sourceAnchorY];
	if(sourceLine.lineNumber >= lineMap.count)
		return;

	NSInteger const encodedTarget = lineMap[sourceLine.lineNumber].integerValue;
	BOOL const mapsToGap = encodedTarget < 0;
	NSUInteger const targetPosition = mapsToGap ? (NSUInteger)(-encodedTarget - 1) : (NSUInteger)encodedTarget;
	CGFloat targetY = [self yPositionForLineBoundary:targetPosition lineCount:targetLineCount textView:targetTextView];
	if(!mapsToGap && targetPosition < targetLineCount)
	{
		CGFloat const sourceTop = [self yPositionForLineBoundary:sourceLine.lineNumber lineCount:lineMap.count textView:sourceTextView];
		CGFloat const sourceBottom = [self bottomForLine:sourceLine.lineNumber lineCount:lineMap.count textView:sourceTextView];
		CGFloat const sourceLineHeight = sourceBottom - sourceTop;
		CGFloat const fraction = sourceLineHeight > 0 ? std::clamp((sourceAnchorY - sourceTop) / sourceLineHeight, (CGFloat)0, (CGFloat)1) : 0;
		CGFloat const targetBottom = [self bottomForLine:targetPosition lineCount:targetLineCount textView:targetTextView];
		targetY += fraction * MAX(0, targetBottom - targetY);
	}

	CGFloat const transitionLength = MAX(NSHeight(sourceBounds), 4 * MAX((CGFloat)1, sourceLine.lastY - sourceLine.firstY));
	for(DiffScrollTransition* transition in scrollTransitions)
	{
		CGFloat const sourceBoundaryY = [self yPositionForLineBoundary:transition.sourceBoundary lineCount:lineMap.count textView:sourceTextView];
		CGFloat const targetStartY = [self yPositionForLineBoundary:transition.targetStart lineCount:targetLineCount textView:targetTextView];
		CGFloat const targetEndY = [self yPositionForLineBoundary:transition.targetEnd lineCount:targetLineCount textView:targetTextView];
		CGFloat const insertedHeight = targetEndY - targetStartY;
		CGFloat progress = std::clamp((sourceAnchorY - (sourceBoundaryY - transitionLength / 2)) / transitionLength, (CGFloat)0, (CGFloat)1);
		progress = progress * progress * (3 - 2 * progress);
		CGFloat const discreteProgress = sourceAnchorY < sourceBoundaryY ? 0 : 1;
		targetY += insertedHeight * (progress - discreteProgress);
	}

	NSRect targetBounds = targetClipView.bounds;
	targetBounds.origin = NSMakePoint(NSMinX(sourceBounds), targetY - NSHeight(targetBounds) / 2);
	targetBounds = [targetClipView constrainBoundsRect:targetBounds];
	if(fabs(NSMinX(targetBounds) - NSMinX(targetClipView.bounds)) < 0.25 && fabs(NSMinY(targetBounds) - NSMinY(targetClipView.bounds)) < 0.25)
		return;

	self.synchronizingScroll = YES;
	[targetClipView scrollToPoint:targetBounds.origin];
	[targetClipView.enclosingScrollView reflectScrolledClipView:targetClipView];
	self.synchronizingScroll = NO;
}

- (void)updateActiveChangeHighlights
{
	if(self.activeDiffHunkIndex < 0 || self.activeDiffHunkIndex >= (NSInteger)self.diffHunks.count)
	{
		self.leftDiffHighlightView.activeHighlightedLines = NSIndexSet.indexSet;
		self.leftDiffHighlightView.activeMarkerPositions = NSIndexSet.indexSet;
		self.rightDiffHighlightView.activeHighlightedLines = NSIndexSet.indexSet;
		self.rightDiffHighlightView.activeMarkerPositions = NSIndexSet.indexSet;
		return;
	}

	DiffHunk* hunk = self.diffHunks[self.activeDiffHunkIndex];
	self.leftDiffHighlightView.activeHighlightedLines = hunk.leftLines.length ? [NSIndexSet indexSetWithIndexesInRange:hunk.leftLines] : NSIndexSet.indexSet;
	self.leftDiffHighlightView.activeMarkerPositions = hunk.leftLines.length ? NSIndexSet.indexSet : [NSIndexSet indexSetWithIndex:hunk.leftLines.location];
	self.rightDiffHighlightView.activeHighlightedLines = hunk.rightLines.length ? [NSIndexSet indexSetWithIndexesInRange:hunk.rightLines] : NSIndexSet.indexSet;
	self.rightDiffHighlightView.activeMarkerPositions = hunk.rightLines.length ? NSIndexSet.indexSet : [NSIndexSet indexSetWithIndex:hunk.rightLines.location];
}

- (CGFloat)centerYForLineRange:(NSRange)lineRange lineCount:(NSUInteger)lineCount textView:(OakTextView*)textView
{
	CGFloat const startY = [self yPositionForLineBoundary:lineRange.location lineCount:lineCount textView:textView];
	if(lineRange.length == 0)
		return startY;
	CGFloat const endY = [self yPositionForLineBoundary:NSMaxRange(lineRange) lineCount:lineCount textView:textView];
	return (startY + endY) / 2;
}

- (void)centerActiveChange
{
	if(self.activeDiffHunkIndex < 0 || self.activeDiffHunkIndex >= (NSInteger)self.diffHunks.count)
		return;

	DiffHunk* hunk = self.diffHunks[self.activeDiffHunkIndex];
	CGFloat const leftY = [self centerYForLineRange:hunk.leftLines lineCount:self.leftToRightLineMap.count textView:self.leftDocumentView.textView];
	CGFloat const rightY = [self centerYForLineRange:hunk.rightLines lineCount:self.rightToLeftLineMap.count textView:self.rightDocumentView.textView];

	self.synchronizingScroll = YES;
	NSArray<NSDictionary*>* scrollRequests = @[
		@{ @"clipView": self.leftClipView, @"y": @(leftY) },
		@{ @"clipView": self.rightClipView, @"y": @(rightY) },
	];
	for(NSDictionary* request in scrollRequests)
	{
		NSClipView* clipView = request[@"clipView"];
		NSRect bounds = clipView.bounds;
		bounds.origin.y = [request[@"y"] doubleValue] - NSHeight(bounds) / 2;
		bounds = [clipView constrainBoundsRect:bounds];
		[clipView scrollToPoint:bounds.origin];
		[clipView.enclosingScrollView reflectScrolledClipView:clipView];
	}
	self.synchronizingScroll = NO;
	self.lastScrolledClipView = self.leftClipView;
}

- (void)selectChangeWithOffset:(NSInteger)offset
{
	if(self.diffHunks.count == 0)
	{
		NSBeep();
		return;
	}

	OakDocumentView* activeDocumentView = self.activeDocumentView;
	BOOL const useLeftRange = activeDocumentView == self.leftDocumentView;
	text::selection_t const selection(to_s(activeDocumentView.textView.selectionString));
	NSUInteger const cursorLine = selection.last().to.line;
	NSInteger targetIndex = -1;
	if(offset > 0)
	{
		for(NSUInteger i = 0; i < self.diffHunks.count; ++i)
		{
			DiffHunk* hunk = self.diffHunks[i];
			NSRange const range = useLeftRange ? hunk.leftLines : hunk.rightLines;
			if(range.location > cursorLine || (range.location == cursorLine && self.activeDiffHunkIndex != (NSInteger)i))
			{
				targetIndex = i;
				break;
			}
		}
		if(targetIndex == -1)
			targetIndex = 0;
	}
	else
	{
		for(NSInteger i = self.diffHunks.count - 1; i >= 0; --i)
		{
			DiffHunk* hunk = self.diffHunks[i];
			NSRange const range = useLeftRange ? hunk.leftLines : hunk.rightLines;
			if(range.location < cursorLine || (range.location == cursorLine && self.activeDiffHunkIndex != i))
			{
				targetIndex = i;
				break;
			}
		}
		if(targetIndex == -1)
			targetIndex = self.diffHunks.count - 1;
	}

	self.activeDiffHunkIndex = targetIndex;
	DiffHunk* targetHunk = self.diffHunks[targetIndex];
	NSRange const targetRange = useLeftRange ? targetHunk.leftLines : targetHunk.rightLines;
	activeDocumentView.textView.selectionString = to_ns(text::pos_t(targetRange.location, 0));
	[self updateActiveChangeHighlights];
	[self centerActiveChange];
}

- (IBAction)nextChange:(id)sender
{
	[self selectChangeWithOffset:1];
}

- (IBAction)previousChange:(id)sender
{
	[self selectChangeWithOffset:-1];
}

- (void)copyActiveChangeToLeft:(BOOL)copyToLeft
{
	if(self.activeDiffHunkIndex < 0 || self.activeDiffHunkIndex >= (NSInteger)self.diffHunks.count)
	{
		NSBeep();
		return;
	}

	DiffHunk* hunk = self.diffHunks[self.activeDiffHunkIndex];
	OakDocument* sourceDocument = copyToLeft ? self.rightDocumentView.document : self.leftDocumentView.document;
	OakDocument* targetDocument = copyToLeft ? self.leftDocumentView.document : self.rightDocumentView.document;
	NSRange const sourceRange = copyToLeft ? hunk.rightLines : hunk.leftLines;
	NSRange const targetRange = copyToLeft ? hunk.leftLines : hunk.rightLines;
	NSArray<NSString*>* sourceLines = [sourceDocument.content ?: @"" componentsSeparatedByString:@"\n"];
	NSMutableArray<NSString*>* targetLines = [[targetDocument.content ?: @"" componentsSeparatedByString:@"\n"] mutableCopy];
	if(NSMaxRange(sourceRange) > sourceLines.count || NSMaxRange(targetRange) > targetLines.count)
	{
		NSBeep();
		return;
	}

	NSArray<NSString*>* replacementLines = [sourceLines subarrayWithRange:sourceRange];
	[targetLines replaceObjectsInRange:targetRange withObjectsFromArray:replacementLines];
	NSString* replacementContent = [targetLines componentsJoinedByString:@"\n"];
	NSString* currentTargetContent = targetDocument.content ?: @"";
	std::string const oldContent(currentTargetContent.UTF8String);
	std::multimap<std::pair<size_t, size_t>, std::string> replacements;
	replacements.emplace(std::make_pair(0, oldContent.size()), std::string(replacementContent.UTF8String));
	if(![targetDocument performReplacements:replacements checksum:0])
		NSBeep();
}

- (IBAction)copyChangeToLeft:(id)sender
{
	[self copyActiveChangeToLeft:YES];
}

- (IBAction)copyChangeToRight:(id)sender
{
	[self copyActiveChangeToLeft:NO];
}

- (OakDocumentView*)activeDocumentView
{
	NSResponder* firstResponder = self.window.firstResponder;
	if([firstResponder isKindOfClass:NSView.class])
	{
		NSView* firstResponderView = (NSView*)firstResponder;
		if(firstResponderView == self.rightDocumentView || [firstResponderView isDescendantOf:self.rightDocumentView])
			return self.rightDocumentView;
		if(firstResponderView == self.leftDocumentView || [firstResponderView isDescendantOf:self.leftDocumentView])
			return self.leftDocumentView;
	}
	return self.leftDocumentView;
}

- (void)updateDocumentState
{
	OakDocument* leftDocument = self.leftDocumentView.document;
	OakDocument* rightDocument = self.rightDocumentView.document;
	self.leftPath = leftDocument.path;
	self.rightPath = rightDocument.path;
	self.window.title = [NSString stringWithFormat:@"%@ ↔ %@", leftDocument.displayName, rightDocument.displayName];
	[self updateTitlebarFileButtons];
	self.window.documentEdited = leftDocument.isDocumentEdited || rightDocument.isDocumentEdited;
	[self invalidateRestorableState];
}

- (void)saveOakDocument:(OakDocument*)document completionHandler:(void(^)(OakDocumentIOResult result))completionHandler
{
	[document saveModalForWindow:self.window completionHandler:^(OakDocumentIOResult result, NSString* errorMessage, oak::uuid_t const& filterUUID) {
		if(result == OakDocumentIOResultSuccess)
		{
			document.observeFileSystem = NO;
			[self updateDocumentState];
			[self startObservingComparedFiles];
		}
		else if(result == OakDocumentIOResultFailure)
		{
			NSAlert* alert = [[NSAlert alloc] init];
			alert.alertStyle = NSAlertStyleCritical;
			alert.messageText = [NSString stringWithFormat:@"Couldn’t save “%@”", document.displayName];
			alert.informativeText = errorMessage.length ? errorMessage : @"Please check the Console for more information.";
			[alert beginSheetModalForWindow:self.window completionHandler:nil];
		}

		if(completionHandler)
			completionHandler(result);
	}];
}

- (NSArray<OakDocument*>*)editedDocuments
{
	NSMutableArray<OakDocument*>* documents = [NSMutableArray array];
	if(self.leftDocumentView.document.isDocumentEdited)
		[documents addObject:self.leftDocumentView.document];
	if(self.rightDocumentView.document.isDocumentEdited)
		[documents addObject:self.rightDocumentView.document];
	return documents;
}

- (void)saveDocuments:(NSArray<OakDocument*>*)documents atIndex:(NSUInteger)index completionHandler:(void(^)(OakDocumentIOResult result))completionHandler
{
	if(index == documents.count)
	{
		if(completionHandler)
			completionHandler(OakDocumentIOResultSuccess);
		return;
	}

	[self saveOakDocument:documents[index] completionHandler:^(OakDocumentIOResult result) {
		if(result == OakDocumentIOResultSuccess)
			[self saveDocuments:documents atIndex:index + 1 completionHandler:completionHandler];
		else if(completionHandler)
			completionHandler(result);
	}];
}

- (IBAction)saveDocument:(id)sender
{
	NSArray<OakDocument*>* documents = self.editedDocuments;
	if(documents.count)
		[self saveDocuments:documents atIndex:0 completionHandler:nil];
}

- (IBAction)saveDocumentAs:(id)sender
{
	OakDocument* document = self.activeDocumentView.document;
	if(!document.isLoaded)
	{
		NSBeep();
		return;
	}

	NSString* suggestedDirectory = document.path.stringByDeletingLastPathComponent;
	NSString* suggestedName = document.path.lastPathComponent ?: [document displayNameWithExtension:YES];
	encoding::type const encoding(to_s(document.diskNewlines), to_s(document.diskEncoding));
	[OakSavePanel showWithPath:suggestedName directory:suggestedDirectory fowWindow:self.window encoding:encoding fileType:document.fileType completionHandler:^(NSString* path, encoding::type const& selectedEncoding) {
		if(!path)
			return;

		document.path = path;
		document.diskNewlines = to_ns(selectedEncoding.newlines());
		document.diskEncoding = to_ns(selectedEncoding.charset());
		[self updateDocumentState];
		[self saveOakDocument:document completionHandler:nil];
	}];
}

- (void)scheduleDiffUpdate
{
	if(!self.leftDocumentLoaded || !self.rightDocumentLoaded)
		return;

	[NSObject cancelPreviousPerformRequestsWithTarget:self selector:@selector(updateDiff) object:nil];
	[self performSelector:@selector(updateDiff) withObject:nil afterDelay:0.12];
}

- (void)updateDiff
{
	NSString* leftContent = self.leftDocumentView.document.content ?: @"";
	NSString* rightContent = self.rightDocumentView.document.content ?: @"";
	NSUInteger const generation = ++self.diffGeneration;
	__weak WindowController* weakSelf = self;

	dispatch_async(dispatch_get_global_queue(QOS_CLASS_USER_INITIATED, 0), ^{
		NSArray<NSString*>* leftLines = [leftContent componentsSeparatedByString:@"\n"];
		NSArray<NSString*>* rightLines = [rightContent componentsSeparatedByString:@"\n"];
		NSOrderedCollectionDifference<NSString*>* difference = [rightLines differenceFromArray:leftLines];
		NSMutableIndexSet* removedLines = [NSMutableIndexSet indexSet];
		NSMutableIndexSet* insertedLines = [NSMutableIndexSet indexSet];
		NSMutableIndexSet* leftMarkerPositions = [NSMutableIndexSet indexSet];
		NSMutableIndexSet* rightMarkerPositions = [NSMutableIndexSet indexSet];
		NSMutableArray<DiffCharacterRange*>* leftCharacterRanges = [NSMutableArray array];
		NSMutableArray<DiffCharacterRange*>* rightCharacterRanges = [NSMutableArray array];
		NSMutableArray<DiffCharacterRange*>* leftCharacterMarkers = [NSMutableArray array];
		NSMutableArray<DiffCharacterRange*>* rightCharacterMarkers = [NSMutableArray array];
		NSMutableArray<NSNumber*>* leftToRightLineMap = [NSMutableArray arrayWithCapacity:leftLines.count];
		NSMutableArray<NSNumber*>* rightToLeftLineMap = [NSMutableArray arrayWithCapacity:rightLines.count];
		NSMutableArray<DiffScrollTransition*>* leftToRightScrollTransitions = [NSMutableArray array];
		NSMutableArray<DiffScrollTransition*>* rightToLeftScrollTransitions = [NSMutableArray array];
		NSMutableArray<DiffHunk*>* diffHunks = [NSMutableArray array];
		for(NSUInteger i = 0; i < leftLines.count; ++i)
			[leftToRightLineMap addObject:@0];
		for(NSUInteger i = 0; i < rightLines.count; ++i)
			[rightToLeftLineMap addObject:@0];

		for(NSOrderedCollectionChange<NSString*>* removal in difference.removals)
			[removedLines addIndex:removal.index];
		for(NSOrderedCollectionChange<NSString*>* insertion in difference.insertions)
			[insertedLines addIndex:insertion.index];

		NSUInteger leftIndex = 0;
		NSUInteger rightIndex = 0;
		while(leftIndex < leftLines.count || rightIndex < rightLines.count)
		{
			BOOL const leftChanged = leftIndex < leftLines.count && [removedLines containsIndex:leftIndex];
			BOOL const rightChanged = rightIndex < rightLines.count && [insertedLines containsIndex:rightIndex];
			if(leftIndex < leftLines.count && rightIndex < rightLines.count && !leftChanged && !rightChanged)
			{
				leftToRightLineMap[leftIndex] = @(rightIndex);
				rightToLeftLineMap[rightIndex] = @(leftIndex);
				++leftIndex;
				++rightIndex;
				continue;
			}

			NSUInteger const leftHunkStart = leftIndex;
			NSUInteger const rightHunkStart = rightIndex;
			while(leftIndex < leftLines.count && [removedLines containsIndex:leftIndex])
				++leftIndex;
			while(rightIndex < rightLines.count && [insertedLines containsIndex:rightIndex])
				++rightIndex;

			NSUInteger const removedCount = leftIndex - leftHunkStart;
			NSUInteger const insertedCount = rightIndex - rightHunkStart;
			DiffHunk* hunk = [[DiffHunk alloc] init];
			hunk.leftLines = NSMakeRange(leftHunkStart, removedCount);
			hunk.rightLines = NSMakeRange(rightHunkStart, insertedCount);
			[diffHunks addObject:hunk];

			NSUInteger const pairedCount = MIN(removedCount, insertedCount);
			for(NSUInteger i = 0; i < pairedCount; ++i)
			{
				leftToRightLineMap[leftHunkStart + i] = @(rightHunkStart + i);
				rightToLeftLineMap[rightHunkStart + i] = @(leftHunkStart + i);
				AppendCharacterDifferences(leftLines[leftHunkStart + i], rightLines[rightHunkStart + i], leftHunkStart + i, rightHunkStart + i, leftCharacterRanges, rightCharacterRanges, leftCharacterMarkers, rightCharacterMarkers);
			}
			for(NSUInteger i = pairedCount; i < removedCount; ++i)
				leftToRightLineMap[leftHunkStart + i] = @(-((NSInteger)rightIndex) - 1);
			for(NSUInteger i = pairedCount; i < insertedCount; ++i)
				rightToLeftLineMap[rightHunkStart + i] = @(-((NSInteger)leftIndex) - 1);

			if(insertedCount > removedCount)
			{
				[leftMarkerPositions addIndex:leftIndex];
				DiffScrollTransition* transition = [[DiffScrollTransition alloc] init];
				transition.sourceBoundary = leftIndex;
				transition.targetStart = rightHunkStart + pairedCount;
				transition.targetEnd = rightIndex;
				[leftToRightScrollTransitions addObject:transition];
			}
			else if(removedCount > insertedCount)
			{
				[rightMarkerPositions addIndex:rightIndex];
				DiffScrollTransition* transition = [[DiffScrollTransition alloc] init];
				transition.sourceBoundary = rightIndex;
				transition.targetStart = leftHunkStart + pairedCount;
				transition.targetEnd = leftIndex;
				[rightToLeftScrollTransitions addObject:transition];
			}

			if(removedCount == 0 && insertedCount == 0)
				break;
		}

		dispatch_async(dispatch_get_main_queue(), ^{
			WindowController* strongSelf = weakSelf;
			if(!strongSelf || strongSelf.diffGeneration != generation)
				return;
			strongSelf.leftDiffHighlightView.highlightedLines = removedLines;
			strongSelf.leftDiffHighlightView.markerPositions = leftMarkerPositions;
			strongSelf.leftDiffHighlightView.characterRanges = leftCharacterRanges;
			strongSelf.leftDiffHighlightView.characterMarkers = leftCharacterMarkers;
			strongSelf.leftDiffHighlightView.lineCount = leftLines.count;
			strongSelf.rightDiffHighlightView.highlightedLines = insertedLines;
			strongSelf.rightDiffHighlightView.markerPositions = rightMarkerPositions;
			strongSelf.rightDiffHighlightView.characterRanges = rightCharacterRanges;
			strongSelf.rightDiffHighlightView.characterMarkers = rightCharacterMarkers;
			strongSelf.rightDiffHighlightView.lineCount = rightLines.count;
			strongSelf.leftToRightLineMap = leftToRightLineMap;
			strongSelf.rightToLeftLineMap = rightToLeftLineMap;
			strongSelf.leftToRightScrollTransitions = leftToRightScrollTransitions;
			strongSelf.rightToLeftScrollTransitions = rightToLeftScrollTransitions;
			strongSelf.diffHunks = diffHunks;
			if(diffHunks.count == 0)
				strongSelf.activeDiffHunkIndex = -1;
			else if(strongSelf.activeDiffHunkIndex >= (NSInteger)diffHunks.count)
				strongSelf.activeDiffHunkIndex = diffHunks.count - 1;
			[strongSelf updateActiveChangeHighlights];
			[strongSelf synchronizeScrollFromClipView:strongSelf.lastScrolledClipView ?: strongSelf.leftClipView];
		});
	});
}

- (void)documentContentDidChange:(NSNotification*)notification
{
	if(notification.object == self.leftDocumentView.document || notification.object == self.rightDocumentView.document)
	{
		[self updateDocumentState];
		++self.diffGeneration;
		self.diffHunks = @[];
		self.activeDiffHunkIndex = -1;
		self.leftDiffHighlightView.characterRanges = @[];
		self.rightDiffHighlightView.characterRanges = @[];
		self.leftDiffHighlightView.characterMarkers = @[];
		self.rightDiffHighlightView.characterMarkers = @[];
		[self updateActiveChangeHighlights];
		[self scheduleDiffUpdate];
	}
}

- (void)loadDocumentAtPath:(NSString*)path intoDocumentView:(OakDocumentView*)documentView sideName:(NSString*)sideName
{
	BOOL isDirectory = NO;
	if(![NSFileManager.defaultManager fileExistsAtPath:path isDirectory:&isDirectory])
	{
		OakDocument* document = [OakDocument documentWithString:@"" fileType:@"text.plain" customName:path.lastPathComponent];
		document.path = path;
		document.onDisk = NO;
		documentView.document = document;
		documentView.textView.softWrap = YES;
		document.observeFileSystem = NO;
		[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(documentContentDidChange:) name:OakDocumentContentDidChangeNotification object:document];
		if(documentView == self.leftDocumentView)
			self.leftDocumentLoaded = YES;
		else
			self.rightDocumentLoaded = YES;
		[self scheduleDiffUpdate];
		[document close];
		return;
	}

	OakDocument* document = [OakDocument documentWithPath:path];
	[document loadModalForWindow:self.window completionHandler:^(OakDocumentIOResult result, NSString* errorMessage, oak::uuid_t const& filterUUID) {
		if(result == OakDocumentIOResultSuccess)
		{
			documentView.document = document;
			documentView.textView.softWrap = YES;
			document.observeFileSystem = NO;
			[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(documentContentDidChange:) name:OakDocumentContentDidChangeNotification object:document];
			if(documentView == self.leftDocumentView)
				self.leftDocumentLoaded = YES;
			else if(documentView == self.rightDocumentView)
				self.rightDocumentLoaded = YES;
			[self scheduleDiffUpdate];
			[document close];
		}
		else
		{
			NSAlert* alert = [[NSAlert alloc] init];
			alert.alertStyle = NSAlertStyleCritical;
			alert.messageText = [NSString stringWithFormat:@"Couldn’t open the %@ file", sideName];
			alert.informativeText = errorMessage.length ? errorMessage : path;
			[alert beginSheetModalForWindow:self.window completionHandler:nil];
		}
	}];
}

- (void)windowDidEndLiveResize:(NSNotification*)notification
{
	[self.window saveFrameUsingName:FileComparisonFrameAutosaveName];
}

- (void)windowWillClose:(NSNotification*)aNotification
{
	[NSObject cancelPreviousPerformRequestsWithTarget:self selector:@selector(updateDiff) object:nil];
	[self stopObservingComparedFiles];
	++self.diffGeneration;
	[NSNotificationCenter.defaultCenter removeObserver:self];
	_retainedSelf = nil;
}

- (BOOL)windowShouldClose:(NSWindow*)sender
{
	if(self.closingWithoutSaving)
		return YES;

	NSArray<OakDocument*>* documents = self.editedDocuments;
	if(documents.count == 0)
		return YES;

	NSAlert* alert = [[NSAlert alloc] init];
	alert.alertStyle = NSAlertStyleWarning;
	alert.messageText = documents.count == 1 ? [NSString stringWithFormat:@"Do you want to save the changes made to “%@”?", documents.firstObject.displayName] : @"Do you want to save the changes made to both files?";
	alert.informativeText = @"Your changes will be lost if you don’t save them.";
	[alert addButtonWithTitle:@"Save"];
	[alert addButtonWithTitle:@"Cancel"];
	[alert addButtonWithTitle:@"Don’t Save"];
	[alert beginSheetModalForWindow:self.window completionHandler:^(NSModalResponse returnCode) {
		if(returnCode == NSAlertFirstButtonReturn)
		{
			[self saveDocuments:documents atIndex:0 completionHandler:^(OakDocumentIOResult result) {
				if(result == OakDocumentIOResultSuccess)
					[self.window close];
			}];
		}
		else if(returnCode == NSAlertThirdButtonReturn)
		{
			self.closingWithoutSaving = YES;
			[self.window close];
		}
	}];
	return NO;
}
@end
