#import "NewComparisonWindowController.h"

@interface FileDropImageView : NSImageView
@property (nonatomic, copy) void (^fileDropHandler)(NSString* path);
@property (nonatomic) BOOL receivingDrag;
- (void)setFilePath:(NSString*)path;
@end

@implementation FileDropImageView
- (void)awakeFromNib
{
	[super awakeFromNib];
	self.wantsLayer = YES;
	self.layer.cornerRadius = 7;
	self.layer.borderWidth = 1;
	self.imageScaling = NSImageScaleProportionallyDown;
	[self registerForDraggedTypes:@[ NSPasteboardTypeFileURL ]];
	[self setFilePath:nil];
	[self updateAppearance];
}

- (void)viewDidChangeEffectiveAppearance
{
	[super viewDidChangeEffectiveAppearance];
	[self updateAppearance];
}

- (void)updateAppearance
{
	self.layer.backgroundColor = (self.receivingDrag ? NSColor.selectedContentBackgroundColor : NSColor.controlBackgroundColor).CGColor;
	self.layer.borderColor = (self.receivingDrag ? NSColor.keyboardFocusIndicatorColor : NSColor.separatorColor).CGColor;
}

- (NSString*)filePathFromDraggingInfo:(id<NSDraggingInfo>)sender
{
	NSArray<NSURL*>* URLs = [sender.draggingPasteboard readObjectsForClasses:@[ NSURL.class ] options:@{ NSPasteboardURLReadingFileURLsOnlyKey: @YES }];
	NSURL* URL = URLs.firstObject;
	NSNumber* isRegularFile = nil, *isDirectory = nil;
	[URL getResourceValue:&isRegularFile forKey:NSURLIsRegularFileKey error:nil];
	[URL getResourceValue:&isDirectory forKey:NSURLIsDirectoryKey error:nil];
	return isRegularFile.boolValue || isDirectory.boolValue ? URL.path : nil;
}

- (NSDragOperation)draggingEntered:(id<NSDraggingInfo>)sender
{
	self.receivingDrag = [self filePathFromDraggingInfo:sender] != nil;
	[self updateAppearance];
	return self.receivingDrag ? NSDragOperationCopy : NSDragOperationNone;
}

- (void)draggingExited:(id<NSDraggingInfo>)sender
{
	self.receivingDrag = NO;
	[self updateAppearance];
}

- (BOOL)performDragOperation:(id<NSDraggingInfo>)sender
{
	NSString* path = [self filePathFromDraggingInfo:sender];
	self.receivingDrag = NO;
	[self updateAppearance];
	if(path && self.fileDropHandler)
		self.fileDropHandler(path);
	return path != nil;
}

- (void)setFilePath:(NSString*)path
{
	BOOL isDirectory = NO;
	NSImage* image = nil;
	if(path.length && [NSFileManager.defaultManager fileExistsAtPath:path isDirectory:&isDirectory])
	{
		image = [NSWorkspace.sharedWorkspace iconForFile:path];
		self.toolTip = path;
		self.accessibilityLabel = [NSString stringWithFormat:@"Selected %@: %@", isDirectory ? @"folder" : @"file", path.lastPathComponent];
	}
	else
	{
		image = [NSImage imageWithSystemSymbolName:@"doc.on.doc" accessibilityDescription:@"File or folder"];
		self.toolTip = @"Drop a file or folder here";
		self.accessibilityLabel = @"File or folder drop target";
	}

	self.image = [image copy];
	self.image.size = NSMakeSize(56, 56);
}
@end

@interface NewComparisonWindowController () <NSComboBoxDelegate>
@property (nonatomic, copy) NewComparisonHandler completionHandler;
@property (nonatomic) IBOutlet NSComboBox* leftPathField;
@property (nonatomic) IBOutlet NSComboBox* rightPathField;
@property (nonatomic) IBOutlet FileDropImageView* leftDropImageView;
@property (nonatomic) IBOutlet FileDropImageView* rightDropImageView;
@property (nonatomic) IBOutlet NSButton* compareButton;
@end

@implementation NewComparisonWindowController
static NSString* const LeftFileHistoryKey = @"CompareMateLeftFileHistory";
static NSString* const RightFileHistoryKey = @"CompareMateRightFileHistory";

- (instancetype)initWithCompletionHandler:(NewComparisonHandler)completionHandler
{
	if(self = [super initWithWindowNibName:@"NewComparisonWindow"])
		_completionHandler = [completionHandler copy];
	return self;
}

- (void)windowDidLoad
{
	[super windowDidLoad];

	[self.leftPathField addItemsWithObjectValues:[NSUserDefaults.standardUserDefaults stringArrayForKey:LeftFileHistoryKey] ?: @[ ]];
	[self.rightPathField addItemsWithObjectValues:[NSUserDefaults.standardUserDefaults stringArrayForKey:RightFileHistoryKey] ?: @[ ]];

	__weak NewComparisonWindowController* weakSelf = self;
	self.leftDropImageView.fileDropHandler = ^(NSString* path) {
		[weakSelf setPath:path forLeftSide:YES];
	};
	self.rightDropImageView.fileDropHandler = ^(NSString* path) {
		[weakSelf setPath:path forLeftSide:NO];
	};

	[self updateState];
	[self.window center];
}

- (NSString*)normalizedPathFromField:(NSComboBox*)field
{
	return field.stringValue.stringByExpandingTildeInPath.stringByStandardizingPath;
}

- (BOOL)pathExists:(NSString*)path isDirectory:(BOOL*)isDirectory
{
	BOOL directory = NO;
	BOOL const exists = path.length && [NSFileManager.defaultManager fileExistsAtPath:path isDirectory:&directory];
	if(isDirectory)
		*isDirectory = directory;
	return exists;
}

- (void)updateState
{
	NSString* leftPath = [self normalizedPathFromField:self.leftPathField];
	NSString* rightPath = [self normalizedPathFromField:self.rightPathField];
	[self.leftDropImageView setFilePath:leftPath];
	[self.rightDropImageView setFilePath:rightPath];
	BOOL leftIsDirectory = NO, rightIsDirectory = NO;
	BOOL const leftExists = [self pathExists:leftPath isDirectory:&leftIsDirectory];
	BOOL const rightExists = [self pathExists:rightPath isDirectory:&rightIsDirectory];
	self.compareButton.enabled = leftExists && rightExists && leftIsDirectory == rightIsDirectory && ![leftPath isEqualToString:rightPath];
}

- (void)setPath:(NSString*)path forLeftSide:(BOOL)leftSide
{
	NSComboBox* field = leftSide ? self.leftPathField : self.rightPathField;
	field.stringValue = path.stringByStandardizingPath;
	[self rememberPath:field.stringValue forLeftSide:leftSide];
	[self updateState];
	[self.window makeFirstResponder:(leftSide ? self.rightPathField : self.leftPathField)];
}

- (void)rememberPath:(NSString*)path forLeftSide:(BOOL)leftSide
{
	NSString* normalizedPath = path.stringByExpandingTildeInPath.stringByStandardizingPath;
	if(![self pathExists:normalizedPath isDirectory:nil])
		return;

	NSString* historyKey = leftSide ? LeftFileHistoryKey : RightFileHistoryKey;
	NSMutableArray<NSString*>* history = [[NSUserDefaults.standardUserDefaults stringArrayForKey:historyKey] mutableCopy] ?: [NSMutableArray array];
	[history removeObject:normalizedPath];
	[history insertObject:normalizedPath atIndex:0];
	if(history.count > 10)
		[history removeObjectsInRange:NSMakeRange(10, history.count - 10)];
	[NSUserDefaults.standardUserDefaults setObject:history forKey:historyKey];

	NSComboBox* comboBox = leftSide ? self.leftPathField : self.rightPathField;
	[comboBox removeAllItems];
	[comboBox addItemsWithObjectValues:history];
}

- (void)chooseFileForLeftSide:(BOOL)leftSide
{
	NSOpenPanel* panel = [NSOpenPanel openPanel];
	panel.canChooseFiles = YES;
	panel.canChooseDirectories = YES;
	panel.allowsMultipleSelection = NO;
	panel.prompt = leftSide ? @"Choose Left" : @"Choose Right";

	NSString* currentPath = [self normalizedPathFromField:(leftSide ? self.leftPathField : self.rightPathField)];
	if(currentPath.length)
	{
		BOOL isDirectory = NO;
		[NSFileManager.defaultManager fileExistsAtPath:currentPath isDirectory:&isDirectory];
		panel.directoryURL = [NSURL fileURLWithPath:(isDirectory ? currentPath : currentPath.stringByDeletingLastPathComponent) isDirectory:YES];
	}

	[panel beginSheetModalForWindow:self.window completionHandler:^(NSModalResponse response) {
		if(response == NSModalResponseOK)
			[self setPath:panel.URL.path forLeftSide:leftSide];
	}];
}

- (IBAction)chooseLeftFile:(id)sender
{
	[self chooseFileForLeftSide:YES];
}

- (IBAction)chooseRightFile:(id)sender
{
	[self chooseFileForLeftSide:NO];
}

- (IBAction)cancel:(id)sender
{
	[self close];
}

- (IBAction)compare:(id)sender
{
	[self updateState];
	if(!self.compareButton.enabled)
	{
		NSBeep();
		return;
	}

	if(self.completionHandler)
	{
		NSString* leftPath = [self normalizedPathFromField:self.leftPathField];
		NSString* rightPath = [self normalizedPathFromField:self.rightPathField];
		[self rememberPath:leftPath forLeftSide:YES];
		[self rememberPath:rightPath forLeftSide:NO];
		self.completionHandler(leftPath, rightPath);
	}
	[self close];
}

- (void)controlTextDidChange:(NSNotification*)notification
{
	[self updateState];
}

- (void)comboBoxSelectionDidChange:(NSNotification*)notification
{
	NSComboBox* comboBox = notification.object;
	if(NSString* selectedPath = comboBox.objectValueOfSelectedItem)
		comboBox.stringValue = selectedPath;
	[self updateState];
}
@end
