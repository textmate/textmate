#import "DocumentWindowController.h"
#import "ProjectLayoutView.h"
#import "SelectGrammarViewController.h"
#import "OakRunCommandWindowController.h"
#import <document/document.h>
#import <document/OakDocument.h>
#import <document/OakDocumentController.h>
#import <OakAppKit/NSAlert Additions.h>
#import <OakAppKit/NSMenuItem Additions.h>
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/OakFileIconImage.h>
#import <OakAppKit/OakFileManager.h>
#import <OakAppKit/OakPasteboard.h>
#import <OakAppKit/OakSavePanel.h>
#import <OakAppKit/OakTabBarView.h>
#import <OakFoundation/NSString Additions.h>
#import <Preferences/Keys.h>
#import <OakTextView/OakDocumentView.h>
#import <OakFileBrowser/OakFileBrowser.h>
#import <OakCommand/OakCommand.h>
#import <HTMLOutputWindow/HTMLOutputWindow.h>
#import <OakFilterList/FileChooser.h>
#import <OakSystem/application.h>
#import <Find/Find.h>
#import <BundlesManager/BundlesManager.h>
#import <BundleEditor/BundleEditor.h>
#import <network/network.h>
#import <file/path_info.h>
#import <io/entries.h>
#import <scm/scm.h>
#import <text/parse.h>
#import <text/tokenize.h>
#import <text/utf8.h>
#import <regexp/glob.h>
#import <settings/settings.h>
#import <ns/ns.h>
#import <kvdb/kvdb.h>

static NSString* const kUserDefaultsAlwaysFindInDocument = @"alwaysFindInDocument";
static NSString* const kUserDefaultsDisableFolderStateRestore = @"disableFolderStateRestore";
static NSString* const kUserDefaultsHideStatusBarKey = @"hideStatusBar";
static NSString* const kUserDefaultsDisableBundleSuggestionsKey = @"disableBundleSuggestions";
static NSString* const kUserDefaultsGrammarsToNeverSuggestKey = @"grammarsToNeverSuggest";

static void show_command_error (std::string const& message, oak::uuid_t const& uuid, NSWindow* window = nil, std::string commandName = NULL_STR)
{
	bundles::item_ptr bundleItem = bundles::lookup(uuid);
	if(commandName == NULL_STR)
		commandName = bundleItem ? bundleItem->name() : "(unknown)";

	NSAlert* alert = [[NSAlert alloc] init];
	[alert setAlertStyle:NSCriticalAlertStyle];
	[alert setMessageText:[NSString stringWithCxxString:text::format("Failure running “%.*s”.", (int)commandName.size(), commandName.data())]];
	[alert setInformativeText:[NSString stringWithCxxString:message] ?: @"No output"];
	[alert addButtonWithTitle:@"OK"];
	if(bundleItem)
		[alert addButtonWithTitle:@"Edit Command"];

	OakShowAlertForWindow(alert, window, ^(NSInteger button){
		if(button == NSAlertSecondButtonReturn)
			[[BundleEditor sharedInstance] revealBundleItem:bundleItem];
	});
}

@interface QuickLookNSURLWrapper : NSObject <QLPreviewItem>
@property (nonatomic) NSURL* url;
@end

@implementation QuickLookNSURLWrapper
- (id)initWithURL:(NSURL*)aURL
{
	if((self = [super init]))
		self.url = aURL;
	return self;
}

- (NSURL*)previewItemURL
{
	return self.url;
}
@end

@interface DocumentWindowController () <NSWindowDelegate, OakTabBarViewDelegate, OakTabBarViewDataSource, OakTextViewDelegate, OakFileBrowserDelegate, QLPreviewPanelDelegate, QLPreviewPanelDataSource>
{
	OBJC_WATCH_LEAKS(DocumentWindowController);

	NSMutableDictionary<NSUUID*, NSNumber*>* _trackedDocuments;
	NSMutableSet<NSUUID*>*                   _stickyDocumentIdentifiers;

	scm::info_ptr                          _projectSCMInfo;
	std::map<std::string, std::string>     _projectSCMVariables;
	std::vector<std::string>               _projectScopeAttributes;  // kSettingsScopeAttributesKey
	std::vector<std::string>               _externalScopeAttributes; // attr.scm.git, attr.project.ninja

	scm::info_ptr                          _documentSCMInfo;
	std::map<std::string, std::string>     _documentSCMVariables;
	std::vector<std::string>               _documentScopeAttributes; // attr.os-version, attr.untitled / attr.rev-path + kSettingsScopeAttributesKey
}
@property (nonatomic) ProjectLayoutView*          layoutView;
@property (nonatomic) OakTabBarView*              tabBarView;
@property (nonatomic) OakDocumentView*            documentView;
@property (nonatomic) OakTextView*                textView;
@property (nonatomic) OakFileBrowser*             fileBrowser;

@property (nonatomic) BOOL                        disableFileBrowserWindowResize;
@property (nonatomic) BOOL                        autoRevealFile;
@property (nonatomic) NSRect                      oldWindowFrame;
@property (nonatomic) NSRect                      newWindowFrame;

@property (nonatomic) HTMLOutputWindowController* htmlOutputWindowController;
@property (nonatomic) OakHTMLOutputView*          htmlOutputView;
@property (nonatomic) BOOL                        htmlOutputInWindow;

@property (nonatomic) NSString*                   projectPath;

@property (nonatomic) NSString*                   documentPath;
@property (nonatomic) NSString*                   documentDisplayName;
@property (nonatomic) BOOL                        documentIsModified;
@property (nonatomic) BOOL                        documentIsOnDisk;
@property (nonatomic) scm::status::type           documentSCMStatus;

@property (nonatomic) NSArray*                    urlArrayForQuickLook;
@property (nonatomic) NSArray<Bundle*>*           bundlesAlreadySuggested;

@property (nonatomic) std::vector<document::document_ptr> cppDocuments;
@property (nonatomic) document::document_ptr              selectedCppDocument;

+ (void)scheduleSessionBackup:(id)sender;

- (void)makeTextViewFirstResponder:(id)sender;
- (void)updateFileBrowserStatus:(id)sender;

- (void)fileBrowser:(OakFileBrowser*)aFileBrowser openURLs:(NSArray*)someURLs;
- (void)fileBrowser:(OakFileBrowser*)aFileBrowser closeURL:(NSURL*)anURL;

- (void)takeNewTabIndexFrom:(id)sender;   // used by newDocumentInTab:
- (void)takeTabsToTearOffFrom:(id)sender; // used by moveDocumentToNewWindow:
@end

namespace
{
	// ==========================================
	// = tracking document controller instances =
	// ==========================================

	static NSMutableDictionary<NSString*, DocumentWindowController*>* AllControllers ()
	{
		static NSMutableDictionary* res = [NSMutableDictionary new];
		return res;
	}

	static NSArray<DocumentWindowController*>* SortedControllers ()
	{
		NSMutableArray* res = [NSMutableArray array];
		for(NSNumber* flag in @[ @NO, @YES ])
		{
			for(NSWindow* window in [NSApp orderedWindows])
			{
				if([window isMiniaturized] == [flag boolValue] && [window.delegate respondsToSelector:@selector(identifier)])
				{
					DocumentWindowController* delegate = (DocumentWindowController*)window.delegate;
					if(id controller = AllControllers()[delegate.identifier])
						[res addObject:controller];
				}
			}
		}
		return res;
	}

	// ======================
	// = document_t helpers =
	// ======================

	static bool is_disposable (document::document_ptr const& doc)
	{
		return doc && !doc->is_modified() && !doc->is_on_disk() && doc->path() == NULL_STR && doc->is_loaded() && doc->buffer().empty();
	}
}

@implementation DocumentWindowController
+ (KVDB*)sharedProjectStateDB
{
	NSString* appSupport = [[NSSearchPathForDirectoriesInDomains(NSApplicationSupportDirectory, NSUserDomainMask, YES) objectAtIndex:0] stringByAppendingPathComponent:@"TextMate"];
	return [KVDB sharedDBUsingFile:@"RecentProjects.db" inDirectory:appSupport];
}

- (id)init
{
	if((self = [super init]))
	{
		_trackedDocuments = [NSMutableDictionary dictionary];
		self.identifier   = [NSString stringWithCxxString:oak::uuid_t().generate()];

		self.tabBarView = [[OakTabBarView alloc] initWithFrame:NSZeroRect];
		self.tabBarView.dataSource = self;
		self.tabBarView.delegate   = self;

		self.documentView = [[OakDocumentView alloc] init];
		self.textView = self.documentView.textView;
		self.textView.delegate = self;

		self.layoutView = [[ProjectLayoutView alloc] initWithFrame:NSZeroRect];
		self.layoutView.tabBarView   = self.tabBarView;
		self.layoutView.documentView = self.documentView;

		NSUInteger windowStyle = (NSTitledWindowMask|NSClosableWindowMask|NSResizableWindowMask|NSMiniaturizableWindowMask|NSTexturedBackgroundWindowMask);
		self.window = [[NSWindow alloc] initWithContentRect:[NSWindow contentRectForFrameRect:[self frameRectForNewWindow] styleMask:windowStyle] styleMask:windowStyle backing:NSBackingStoreBuffered defer:NO];
		self.window.collectionBehavior = NSWindowCollectionBehaviorFullScreenPrimary;
		self.window.delegate           = self;
		self.window.releasedWhenClosed = NO;
		[self.window setContentBorderThickness:0 forEdge:NSMaxYEdge]; // top border
		[self.window setContentBorderThickness:0 forEdge:NSMinYEdge]; // bottom border
		[self.window setAutorecalculatesContentBorderThickness:NO forEdge:NSMaxYEdge];
		[self.window setAutorecalculatesContentBorderThickness:NO forEdge:NSMinYEdge];

		OakAddAutoLayoutViewsToSuperview(@[ self.layoutView ], self.window.contentView);
		OakSetupKeyViewLoop(@[ self.layoutView ], NO);
		self.window.initialFirstResponder = self.textView;

		[self.window.contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[view]|" options:0 metrics:nil views:@{ @"view" : self.layoutView }]];
		[self.window.contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[view]|" options:0 metrics:nil views:@{ @"view" : self.layoutView }]];

		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(userDefaultsDidChange:) name:NSUserDefaultsDidChangeNotification object:[NSUserDefaults standardUserDefaults]];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(applicationDidBecomeActiveNotification:) name:NSApplicationDidBecomeActiveNotification object:NSApp];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(applicationDidResignActiveNotification:) name:NSApplicationDidResignActiveNotification object:NSApp];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(fileManagerWillDeleteItemAtPath:) name:OakFileManagerWillDeleteItemAtPath object:nil];

		[self userDefaultsDidChange:nil];
	}
	return self;
}

- (void)dealloc
{
	[[NSNotificationCenter defaultCenter] removeObserver:self];

	self.window.delegate        = nil;
	self.tabBarView.dataSource  = nil;
	self.tabBarView.delegate    = nil;
	self.textView.delegate      = nil;

	// When option-clicking to close all windows then
	// messages are sent to our window after windowWillClose:
	__autoreleasing __attribute__ ((unused)) NSWindow* delayRelease = self.window;
}

// ======================================
// = Find suitable frame for new window =
// ======================================

- (NSRect)windowFrame
{
	NSRect res = [self.window frame];
	if(self.fileBrowserVisible && !self.disableFileBrowserWindowResize)
		res.size.width -= self.fileBrowserWidth;
	return res;
}

- (NSRect)cascadedWindowFrame
{
	NSRect r = [self windowFrame];
	return { { NSMinX(r) + 21, NSMinY(r) - 23 }, r.size };
}

- (NSRect)frameRectForNewWindow
{
	std::map<CGFloat, NSWindow*> ourWindows;
	for(NSWindow* win in [NSApp windows])
	{
		if([win isVisible] && [win isOnActiveSpace] && ![win isZoomed] && (([win styleMask] & NSFullScreenWindowMask)) != NSFullScreenWindowMask && [[win delegate] isKindOfClass:[self class]])
			ourWindows.emplace(NSMaxY([win frame]), win);
	}

	if(!ourWindows.empty())
	{
		NSRect r = [(DocumentWindowController*)ourWindows.begin()->second.delegate cascadedWindowFrame];

		NSRect scrRect = [[NSScreen mainScreen] visibleFrame];
		if(NSContainsRect(scrRect, r))
			return r;

		r.origin.x = 61;
		r.origin.y = NSMaxY(scrRect) - NSHeight(r);

		BOOL alreadyHasWrappedWindow = NO;
		for(auto pair : ourWindows)
		{
			if(NSEqualPoints([pair.second frame].origin, r.origin))
				alreadyHasWrappedWindow = YES;
		}

		if(alreadyHasWrappedWindow)
		{
			NSWindow* mainWindow = [NSApp mainWindow];
			if([[mainWindow delegate] isKindOfClass:[self class]])
				r = [(DocumentWindowController*)mainWindow.delegate cascadedWindowFrame];
		}

		return r;
	}

	if(NSString* rectStr = [[NSUserDefaults standardUserDefaults] stringForKey:@"DocumentControllerWindowFrame"])
		return NSRectFromString(rectStr);

	NSRect r = [[NSScreen mainScreen] visibleFrame];
	return r = NSIntegralRect(NSInsetRect(r, NSWidth(r) / 3, NSHeight(r) / 5));
}

// =========================

- (void)windowWillClose:(NSNotification*)aNotification
{
	if((([self.window styleMask] & NSFullScreenWindowMask) != NSFullScreenWindowMask) && !self.window.isZoomed)
		[[NSUserDefaults standardUserDefaults] setObject:NSStringFromRect([self windowFrame]) forKey:@"DocumentControllerWindowFrame"];

	self.cppDocuments        = { };
	self.selectedCppDocument = document::document_ptr();
	self.fileBrowserVisible  = NO; // Make window frame small as we no longer respond to savableWindowFrame
	self.identifier          = nil; // This removes us from AllControllers and causes a release
}

- (void)showWindow:(id)sender
{
	if(_cppDocuments.empty())
	{
		document::document_ptr defaultDocument = document::create();
		self.cppDocuments = { defaultDocument };
		[self openAndSelectDocument:defaultDocument];
	}
	[self.window makeKeyAndOrderFront:sender];
}

- (void)makeTextViewFirstResponder:(id)sender { [self.window makeFirstResponder:self.textView]; }
- (void)close                                 { [self.window close]; }

- (IBAction)moveFocus:(id)sender
{
	if([self.window firstResponder] == self.textView)
	{
		self.fileBrowserVisible = YES;
		NSOutlineView* outlineView = self.fileBrowser.outlineView;
		[self.window makeFirstResponder:outlineView];
		if([outlineView numberOfSelectedRows] == 0)
		{
			for(NSUInteger row = 0; row < [outlineView numberOfRows]; ++row)
			{
				if([[outlineView delegate] respondsToSelector:@selector(outlineView:isGroupItem:)] && [[outlineView delegate] outlineView:outlineView isGroupItem:[outlineView itemAtRow:row]])
					continue;
				[outlineView selectRowIndexes:[NSIndexSet indexSetWithIndex:row] byExtendingSelection:NO];
				break;
			}
		}
	}
	else
	{
		[self makeTextViewFirstResponder:sender];
	}
}

// ==========================
// = Notification Callbacks =
// ==========================

- (void)userDefaultsDidChange:(NSNotification*)aNotification
{
	self.htmlOutputInWindow = [[[NSUserDefaults standardUserDefaults] stringForKey:kUserDefaultsHTMLOutputPlacementKey] isEqualToString:@"window"];
	self.disableFileBrowserWindowResize = [[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsDisableFileBrowserWindowResizeKey];
	self.autoRevealFile = [[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsAutoRevealFileKey];
	self.documentView.hideStatusBar = [[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsHideStatusBarKey];

	if(self.layoutView.fileBrowserOnRight != [[[NSUserDefaults standardUserDefaults] stringForKey:kUserDefaultsFileBrowserPlacementKey] isEqualToString:@"right"])
	{
		self.oldWindowFrame = self.newWindowFrame = NSZeroRect;
		self.layoutView.fileBrowserOnRight = !self.layoutView.fileBrowserOnRight;
	}
}

- (void)applicationDidBecomeActiveNotification:(NSNotification*)aNotification
{
	if(!_cppDocuments.empty())
		[self.textView performSelector:@selector(applicationDidBecomeActiveNotification:) withObject:aNotification];
}

- (void)applicationDidResignActiveNotification:(NSNotification*)aNotification
{
	static BOOL IsSaving = NO;
	if(std::exchange(IsSaving, YES))
		return;

	NSMutableArray* documentsToSave = [NSMutableArray array];
	for(auto doc : _cppDocuments)
	{
		if(doc->is_modified() && doc->path() != NULL_STR)
		{
			settings_t const settings = settings_for_path(doc->logical_path(), doc->file_type(), path::parent(doc->path()));
			if(settings.get(kSettingsSaveOnBlurKey, false))
			{
				if(doc == _selectedCppDocument)
					[_textView updateDocumentMetadata];
				[documentsToSave addObject:doc->document()];
			}
		}
	}

	[self saveDocumentsUsingEnumerator:[documentsToSave objectEnumerator] completionHandler:^(OakDocumentIOResult result){
		if(!_cppDocuments.empty())
			[self.textView performSelector:@selector(applicationDidResignActiveNotification:) withObject:aNotification];
		IsSaving = NO;
	}];
}

// =================
// = Close Methods =
// =================

+ (NSAlert*)saveAlertForDocuments:(NSArray<OakDocument*>*)someDocuments
{
	NSAlert* alert = [[NSAlert alloc] init];
	[alert setAlertStyle:NSWarningAlertStyle];
	if(someDocuments.count == 1)
	{
		OakDocument* document = someDocuments.firstObject;
		[alert setMessageText:[NSString stringWithFormat:@"Do you want to save the changes you made in the document “%@”?", document.displayName]];
		[alert setInformativeText:@"Your changes will be lost if you don’t save them."];
		[alert addButtons:@"Save", @"Cancel", @"Don’t Save", nil];
	}
	else
	{
		NSString* body = @"";
		for(OakDocument* document in someDocuments)
			body = [body stringByAppendingFormat:@"• “%@”\n", document.displayName];
		[alert setMessageText:@"Do you want to save documents with changes?"];
		[alert setInformativeText:body];
		[alert addButtons:@"Save All", @"Cancel", @"Don’t Save", nil];
	}
	return alert;
}

- (void)showCloseWarningUIForDocuments:(NSArray<OakDocument*>*)someDocuments completionHandler:(void(^)(BOOL canClose))callback
{
	if(!someDocuments.count)
		return callback(YES);

	if(someDocuments.count == 1)
	{
		for(size_t i = 0; i < _cppDocuments.size(); ++i)
		{
			if(someDocuments.firstObject == _cppDocuments[i]->document())
			{
				if(_selectedCppDocument != _cppDocuments[i])
				{
					self.selectedTabIndex = i;
					[self openAndSelectDocument:_cppDocuments[i]];
				}
				break;
			}
		}
	}

	NSAlert* alert = [DocumentWindowController saveAlertForDocuments:someDocuments];
	OakShowAlertForWindow(alert, self.window, ^(NSInteger returnCode){
		switch(returnCode)
		{
			case NSAlertFirstButtonReturn: /* "Save" */
			{
				[self saveDocumentsUsingEnumerator:[someDocuments objectEnumerator] completionHandler:^(OakDocumentIOResult result){
					callback(result == OakDocumentIOResultSuccess);
				}];
			}
			break;

			case NSAlertSecondButtonReturn: /* "Cancel" */
			{
				callback(NO);
			}
			break;

			case NSAlertThirdButtonReturn: /* "Don't Save" */
			{
				callback(YES);
			}
			break;
		}
	});
}

- (void)closeTabsAtIndexes:(NSIndexSet*)anIndexSet askToSaveChanges:(BOOL)askToSaveFlag createDocumentIfEmpty:(BOOL)createIfEmptyFlag
{
	if([anIndexSet count] == 0 || _cppDocuments.empty())
		return;

	std::vector<document::document_ptr> documentsToClose;
	for(NSUInteger index = [anIndexSet firstIndex]; index != NSNotFound; index = [anIndexSet indexGreaterThanIndex:index])
		documentsToClose.push_back(_cppDocuments[index]);

	if(askToSaveFlag)
	{
		NSMutableArray<OakDocument*>* documentsToSave = [NSMutableArray array];
		for(auto doc : documentsToClose)
		{
			if(doc->is_modified())
				[documentsToSave addObject:doc->document()];
		}

		if(documentsToSave.count)
		{
			[self showCloseWarningUIForDocuments:documentsToSave completionHandler:^(BOOL canClose){
				if(canClose)
				{
					[self closeTabsAtIndexes:anIndexSet askToSaveChanges:NO createDocumentIfEmpty:createIfEmptyFlag];
				}
				else
				{
					NSMutableIndexSet* newIndexes = [anIndexSet mutableCopy];
					for(NSUInteger index = [anIndexSet firstIndex]; index != NSNotFound; index = [anIndexSet indexGreaterThanIndex:index])
					{
						if(_cppDocuments[index]->is_modified())
							[newIndexes removeIndex:index];
					}
					[self closeTabsAtIndexes:newIndexes askToSaveChanges:YES createDocumentIfEmpty:createIfEmptyFlag];
				}
			}];
			return;
		}
	}

	std::set<oak::uuid_t> uuids;
	std::transform(documentsToClose.begin(), documentsToClose.end(), inserter(uuids, uuids.end()), [](document::document_ptr const& doc){ return doc->identifier(); });

	std::vector<document::document_ptr> newDocuments;
	NSUInteger newSelectedTabIndex = _selectedTabIndex;
	oak::uuid_t const selectedUUID = _cppDocuments[_selectedTabIndex]->identifier();
	for(auto document : _cppDocuments)
	{
		oak::uuid_t const& uuid = document->identifier();
		if(uuids.find(uuid) == uuids.end())
			newDocuments.push_back(document);
		if(selectedUUID == uuid)
			newSelectedTabIndex = newDocuments.empty() ? 0 : newDocuments.size() - 1;
	}

	if(createIfEmptyFlag && newDocuments.empty())
		newDocuments.push_back(document::create());

	self.cppDocuments     = newDocuments;
	self.selectedTabIndex = newSelectedTabIndex;

	if(!newDocuments.empty() && newDocuments[newSelectedTabIndex]->identifier() != selectedUUID)
		[self openAndSelectDocument:newDocuments[newSelectedTabIndex]];
}

- (IBAction)performCloseTab:(id)sender
{
	if(_cppDocuments.empty() || _cppDocuments.size() == 1 && (is_disposable(_selectedCppDocument) || !self.fileBrowserVisible))
		return [self performCloseWindow:sender];
	NSUInteger index = [sender isKindOfClass:[OakTabBarView class]] ? [sender tag] : _selectedTabIndex;
	[self closeTabsAtIndexes:[NSIndexSet indexSetWithIndex:index] askToSaveChanges:YES createDocumentIfEmpty:YES];
}

- (IBAction)performCloseSplit:(id)sender
{
	ASSERT(sender == self.layoutView.htmlOutputView);
	self.htmlOutputVisible = NO;
}

- (IBAction)performCloseWindow:(id)sender
{
	[self.window performClose:self];
}

- (IBAction)performCloseAllTabs:(id)sender
{
	NSMutableIndexSet* allTabs = [NSMutableIndexSet indexSetWithIndexesInRange:NSMakeRange(0, _cppDocuments.size())];
	for(size_t i = 0; i < _cppDocuments.size(); ++i)
	{
		if(_cppDocuments[i]->is_modified() && _cppDocuments[i]->path() == NULL_STR || [self isDocumentSticky:_cppDocuments[i]])
			[allTabs removeIndex:i];
	}
	[self closeTabsAtIndexes:allTabs askToSaveChanges:YES createDocumentIfEmpty:YES];
}

- (IBAction)performCloseOtherTabsXYZ:(id)sender
{
	NSUInteger tabIndex = [sender isKindOfClass:[OakTabBarView class]] ? [sender tag] : _selectedTabIndex;

	NSMutableIndexSet* otherTabs = [NSMutableIndexSet indexSet];
	for(size_t i = 0; i < _cppDocuments.size(); ++i)
	{
		if(i != tabIndex && (!_cppDocuments[i]->is_modified() || _cppDocuments[i]->path() != NULL_STR) && ![self isDocumentSticky:_cppDocuments[i]])
			[otherTabs addIndex:i];
	}
	[self closeTabsAtIndexes:otherTabs askToSaveChanges:YES createDocumentIfEmpty:YES];
}

- (IBAction)performCloseTabsToTheRight:(id)sender
{
	NSUInteger from = _selectedTabIndex + 1, to = _cppDocuments.size();
	if(from < to)
		[self closeTabsAtIndexes:[NSIndexSet indexSetWithIndexesInRange:NSMakeRange(from, to - from)] askToSaveChanges:YES createDocumentIfEmpty:YES];
}

- (void)saveProjectState
{
	if(self.treatAsProjectWindow)
		[[DocumentWindowController sharedProjectStateDB] setValue:[self sessionInfoIncludingUntitledDocuments:NO] forKey:self.projectPath];
}

- (BOOL)windowShouldClose:(id)sender
{
	if(!self.htmlOutputInWindow && _htmlOutputView.isRunningCommand)
	{
		[_htmlOutputView stopLoadingWithUserInteraction:YES completionHandler:^(BOOL didStop){
			if(didStop)
				[sender performSelector:@selector(performClose:) withObject:self afterDelay:0];
		}];
		return NO;
	}

	NSMutableArray<OakDocument*>* documentsToSave = [NSMutableArray array];
	for(auto doc : _cppDocuments)
	{
		if(doc->is_modified())
			[documentsToSave addObject:doc->document()];
	}

	if(!documentsToSave.count)
	{
		[self saveProjectState];
		return YES;
	}

	[self showCloseWarningUIForDocuments:documentsToSave completionHandler:^(BOOL canClose){
		if(canClose)
		{
			[self saveProjectState];
			[self.window close];
		}
	}];

	return NO;
}

- (void)fileManagerWillDeleteItemAtPath:(NSNotification*)aNotification
{
	NSDictionary* userInfo = [aNotification userInfo];
	NSString* path = userInfo[OakFileManagerPathKey];

	NSMutableIndexSet* indexSet = [NSMutableIndexSet indexSet];
	for(size_t i = 0; i < _cppDocuments.size(); ++i)
	{
		document::document_ptr doc = _cppDocuments[i];
		if(!doc->is_modified() && path::is_child(doc->path(), to_s(path)))
			[indexSet addIndex:i];
	}

	id oldFirstResponder = self.window.firstResponder;
	[self closeTabsAtIndexes:indexSet askToSaveChanges:NO createDocumentIfEmpty:YES];
	if(oldFirstResponder && oldFirstResponder != self.window.firstResponder)
		[self.window makeFirstResponder:oldFirstResponder];
}

- (void)fileBrowserDidDuplicateAtURLs:(NSNotification*)aNotification
{
	NSDictionary* userInfo = [aNotification userInfo];
	NSDictionary* urls = userInfo[OakFileBrowserURLMapKey];
	for(NSURL* url in urls)
	{
		if([url.path isEqualToString:self.documentPath])
			[self openItems:@[ @{ @"path" : [urls[url] path] } ] closingOtherTabs:NO];
	}
}

+ (void)saveSessionAndDetachBackups
{
	BOOL restoresSession = ![[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsDisableSessionRestoreKey];
	[DocumentWindowController saveSessionIncludingUntitledDocuments:restoresSession];
	for(DocumentWindowController* controller in [SortedControllers() reverseObjectEnumerator])
	{
		[controller saveProjectState];

		// Ensure we do not remove backup files, as they are used to restore untitled documents
		if(restoresSession)
		{
			for(auto document : controller.cppDocuments)
			{
				std::string const backupPath = document->backup_path();
				document->detach_backup();
				if(backupPath != NULL_STR && document->path() != NULL_STR)
					unlink(backupPath.c_str());
			}
		}
	}
}

- (NSArray<OakDocument*>*)documentsNeedingSaving
{
	BOOL restoresSession = ![[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsDisableSessionRestoreKey];

	NSMutableArray<OakDocument*>* res = [NSMutableArray array];
	for(auto doc : _cppDocuments)
	{
		if(doc->is_modified() && (doc->path() != NULL_STR || !restoresSession))
			[res addObject:doc->document()];
	}
	return res.count ? res : nil;
}

+ (void)saveControllersUsingEnumerator:(NSEnumerator*)anEnumerator completionHandler:(void(^)(OakDocumentIOResult result))callback
{
	if(DocumentWindowController* controller = [anEnumerator nextObject])
	{
		[controller saveDocumentsUsingEnumerator:[controller.documentsNeedingSaving objectEnumerator] completionHandler:^(OakDocumentIOResult result){
			if(result == OakDocumentIOResultSuccess)
				[self saveControllersUsingEnumerator:anEnumerator completionHandler:callback];
			else if(callback)
				callback(result);
		}];
	}
	else if(callback)
	{
		callback(OakDocumentIOResultSuccess);
	}
}

+ (NSApplicationTerminateReply)applicationShouldTerminate:(NSApplication*)sender
{
	NSMutableArray<DocumentWindowController*>* controllers = [NSMutableArray array];
	NSMutableArray<OakDocument*>* documents = [NSMutableArray array];
	for(DocumentWindowController* controller in SortedControllers())
	{
		if(NSArray* newDocs = controller.documentsNeedingSaving)
		{
			[controllers addObject:controller];
			[documents addObjectsFromArray:newDocs];
		}
	}

	if(controllers.count == 0)
	{
		[self saveSessionAndDetachBackups];
		return NSTerminateNow;
	}
	else if(controllers.count == 1)
	{
		DocumentWindowController* controller = controllers.firstObject;
		[controller showCloseWarningUIForDocuments:controller.documentsNeedingSaving completionHandler:^(BOOL canClose){
			if(canClose)
				[self saveSessionAndDetachBackups];
			[NSApp replyToApplicationShouldTerminate:canClose];
		}];
	}
	else
	{
		switch([[DocumentWindowController saveAlertForDocuments:documents] runModal])
		{
			case NSAlertFirstButtonReturn: /* "Save" */
			{
				[self saveControllersUsingEnumerator:[controllers objectEnumerator] completionHandler:^(OakDocumentIOResult result){
					if(result == OakDocumentIOResultSuccess)
						[self saveSessionAndDetachBackups];
					[NSApp replyToApplicationShouldTerminate:result == OakDocumentIOResultSuccess];
				}];
			}
			break;

			case NSAlertSecondButtonReturn: /* "Cancel" */
				return NSTerminateCancel;

			case NSAlertThirdButtonReturn: /* "Don't Save" */
				return NSTerminateNow;
		}
	}
	return NSTerminateLater;
}

// =====================
// = Document Tracking =
// =====================

- (void)trackDocument:(OakDocument*)document
{
	if(!document)
		return;

	NSUInteger trackCount = [_trackedDocuments[document.identifier] intValue];
	_trackedDocuments[document.identifier] = @(trackCount+1);
	if(trackCount == 0)
	{
		for(NSString* keyPath in @[ @"path", @"onDisk", @"documentEdited" ])
			[document addObserver:self forKeyPath:keyPath options:0 context:nullptr];
	}

	document.keepBackupFile = YES;
	[document open];
}

- (void)untrackDocument:(OakDocument*)document
{
	if(!document)
		return;

	NSUInteger trackCount = [_trackedDocuments[document.identifier] intValue];
	_trackedDocuments[document.identifier] = @(trackCount-1);
	if(trackCount == 1)
	{
		for(NSString* keyPath in @[ @"path", @"onDisk", @"documentEdited" ])
			[document removeObserver:self forKeyPath:keyPath];
	}

	[document close];
}

- (void)observeValueForKeyPath:(NSString*)keyPath ofObject:(id)anObject change:(NSDictionary*)change context:(void*)context
{
	if(self.selectedDocument && anObject && [self.selectedDocument isEqual:anObject])
	{
		OakDocument* document = anObject;
		if([keyPath isEqualToString:@"path"])
		{
			self.documentPath        = document.virtualPath ?: document.path;
			self.documentDisplayName = document.displayName;
			if(!self.projectPath)
				self.projectPath = [document.path stringByDeletingLastPathComponent];
		}
		else if([keyPath isEqualToString:@"onDisk"])
		{
			self.documentIsOnDisk = document.isOnDisk;
		}
		else if([keyPath isEqualToString:@"documentEdited"])
		{
			self.documentIsModified = document.isDocumentEdited;
		}
	}

	if([keyPath isEqualToString:@"path"] || [keyPath isEqualToString:@"documentEdited"])
	{
		[self updateFileBrowserStatus:self];
		[self.tabBarView reloadData];
		[[self class] scheduleSessionBackup:self];
	}
}

- (BOOL)isDocumentSticky:(document::document_ptr)aDocument
{
	return [_stickyDocumentIdentifiers containsObject:aDocument->document().identifier];
}

- (void)setDocument:(document::document_ptr)aDocument sticky:(BOOL)stickyFlag
{
	if(stickyFlag)
		_stickyDocumentIdentifiers = _stickyDocumentIdentifiers ?: [NSMutableSet set];

	if(stickyFlag)
			[_stickyDocumentIdentifiers addObject:aDocument->document().identifier];
	else	[_stickyDocumentIdentifiers removeObject:aDocument->document().identifier];
}

// ====================
// = Create Documents =
// ====================

- (IBAction)newDocumentInTab:(id)sender
{
	[self takeNewTabIndexFrom:[NSIndexSet indexSetWithIndex:_selectedTabIndex + 1]];
}

- (IBAction)newDocumentInDirectory:(id)sender
{
	if(!self.fileBrowserVisible)
		return;

	if(NSString* folder = [self.fileBrowser directoryForNewItems])
	{
		std::string path = "untitled";
		std::string fileType = settings_for_path(NULL_STR, "attr.untitled", to_s(folder)).get(kSettingsFileTypeKey, "text.plain");
		for(auto item : bundles::query(bundles::kFieldGrammarScope, fileType))
		{
			std::string const& ext = item->value_for_field(bundles::kFieldGrammarExtension);
			if(ext != NULL_STR)
				path = "untitled." + ext;
		}

		NSURL* url = [NSURL fileURLWithPath:[NSString stringWithCxxString:path::unique(path::join([folder fileSystemRepresentation], path))]];
		if([[OakFileManager sharedInstance] createFileAtURL:url view:self.fileBrowser.view])
		{
			document::document_ptr doc = document::create(to_s([url path]));
			doc->set_file_type(fileType);

			[self insertDocuments:{ doc } atIndex:_selectedTabIndex + 1 selecting:doc andClosing:[self disposableDocument]];

			// Using openAndSelectDocument: will move focus to OakTextView
			doc->sync_load();
			self.selectedCppDocument = doc;
			doc->close();

			[self.fileBrowser editURL:url];
		}
	}
}

- (IBAction)moveDocumentToNewWindow:(id)sender
{
	if(_cppDocuments.size() > 1)
		[self takeTabsToTearOffFrom:[NSIndexSet indexSetWithIndex:_selectedTabIndex]];
}

- (IBAction)mergeAllWindows:(id)sender
{
	std::vector<document::document_ptr> documents = _cppDocuments;
	for(DocumentWindowController* delegate in SortedControllers())
	{
		if(delegate != self && ![delegate.window isMiniaturized])
		{
			auto delegateDocuments = delegate.cppDocuments; // Returns by-value so each result is unique
			documents.insert(documents.end(), delegateDocuments.begin(), delegateDocuments.end());
		}
	}

	self.cppDocuments = documents;

	for(DocumentWindowController* delegate in SortedControllers())
	{
		if(delegate != self && ![delegate.window isMiniaturized])
			[delegate.window close];
	}
}

- (std::set<oak::uuid_t>)disposableDocument
{
	if(_selectedTabIndex < _cppDocuments.size() && is_disposable(_cppDocuments[_selectedTabIndex]))
		return { _cppDocuments[_selectedTabIndex]->identifier() };
	return { };
}

- (void)insertDocuments:(std::vector<document::document_ptr> const&)documents atIndex:(NSInteger)index selecting:(document::document_ptr const&)selectDocument andClosing:(std::set<oak::uuid_t> const&)closeDocuments
{
	std::set<oak::uuid_t> oldUUIDs, newUUIDs;
	std::transform(_cppDocuments.begin(), _cppDocuments.end(), inserter(oldUUIDs, oldUUIDs.end()), [](auto const& doc){ return doc->identifier(); });
	std::transform(documents.begin(), documents.end(), inserter(newUUIDs, newUUIDs.end()), [](auto const& doc){ return doc->identifier(); });
	std::for_each(closeDocuments.begin(), closeDocuments.end(), [&oldUUIDs](auto const& uuid){ oldUUIDs.erase(uuid); });

	BOOL shouldReorder = ![[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsDisableTabReorderingKey];
	std::vector<document::document_ptr> newDocuments;
	for(NSUInteger i = 0; i <= _cppDocuments.size(); ++i)
	{
		if(i == MIN(index, _cppDocuments.size()))
		{
			std::set<oak::uuid_t> didInsert;
			for(NSUInteger j = 0; j < documents.size(); ++j)
			{
				if(didInsert.find(documents[j]->identifier()) == didInsert.end() && (shouldReorder || oldUUIDs.find(documents[j]->identifier()) == oldUUIDs.end()))
				{
					newDocuments.push_back(documents[j]);
					didInsert.insert(documents[j]->identifier());
				}
			}
		}

		if(i == _cppDocuments.size())
			break;
		else if(shouldReorder && newUUIDs.find(_cppDocuments[i]->identifier()) != newUUIDs.end())
			continue;
		else if(closeDocuments.find(_cppDocuments[i]->identifier()) != closeDocuments.end())
			continue;

		newDocuments.push_back(_cppDocuments[i]);
	}

	self.cppDocuments     = newDocuments;
	self.selectedTabIndex = std::find_if(newDocuments.begin(), newDocuments.end(), [&selectDocument](auto const& doc){ return *doc == *selectDocument; }) - newDocuments.begin();
}

- (void)openItems:(NSArray*)items closingOtherTabs:(BOOL)closeOtherTabsFlag
{
	std::vector<document::document_ptr> documents;
	for(id item in items)
	{
		std::string const path  = to_s([item objectForKey:@"path"]);
		std::string const uuid  = to_s([item objectForKey:@"identifier"]);
		std::string const range = to_s([item objectForKey:@"selectionString"]);

		document::document_ptr doc;
		if(path == NULL_STR && oak::uuid_t::is_valid(uuid))
			doc = document::find(uuid);
		if(!doc && path != NULL_STR)
			doc = document::create(path);

		if(doc)
		{
			doc->set_recent_tracking(false);
			if(range != NULL_STR)
				doc->set_selection(range);
			documents.push_back(doc);
		}
	}

	if(documents.empty())
		return;

	std::set<oak::uuid_t> tabsToClose;
	if(closeOtherTabsFlag)
	{
		for(auto const& doc : _cppDocuments)
		{
			if(!doc->is_modified() && ![self isDocumentSticky:doc])
				tabsToClose.insert(doc->identifier());
		}
	}
	else
	{
		tabsToClose = [self disposableDocument];
	}

	[self insertDocuments:documents atIndex:_selectedTabIndex + 1 selecting:documents.back() andClosing:tabsToClose];
	[self openAndSelectDocument:documents.back()];

	if(self.tabBarView && ![[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsDisableTabAutoCloseKey])
	{
		NSInteger excessTabs = _cppDocuments.size() - std::max<NSUInteger>(self.tabBarView.countOfVisibleTabs, 8);
		if(excessTabs > 0)
		{
			std::multimap<NSInteger, size_t> ranked;
			for(size_t i = 0; i < _cppDocuments.size(); ++i)
				ranked.emplace(_cppDocuments[i]->lru(), i);

			std::set<oak::uuid_t> newUUIDs;
			std::transform(documents.begin(), documents.end(), inserter(newUUIDs, newUUIDs.end()), [](auto const& doc){ return doc->identifier(); });

			NSMutableIndexSet* indexSet = [NSMutableIndexSet indexSet];
			for(auto const& pair : ranked)
			{
				document::document_ptr doc = _cppDocuments[pair.second];
				if(!doc->is_modified() && ![self isDocumentSticky:doc] && doc->is_on_disk() && newUUIDs.find(doc->identifier()) == newUUIDs.end())
					[indexSet addIndex:pair.second];
				if([indexSet count] == excessTabs)
					break;
			}

			[self closeTabsAtIndexes:indexSet askToSaveChanges:NO createDocumentIfEmpty:NO];
		}
	}
}

// ================
// = Document I/O =
// ================

- (void)didOpenDocuemntInTextView:(OakTextView*)textView
{
	for(auto const& item : bundles::query(bundles::kFieldSemanticClass, "callback.document.did-open", [textView scopeContext], bundles::kItemTypeMost, oak::uuid_t(), false))
		[textView performBundleItem:item];
}

- (void)openAndSelectDocument:(document::document_ptr const&)aDocument
{
	document::document_ptr doc = aDocument;
	[doc->document() loadModalForWindow:self.window completionHandler:^(OakDocumentIOResult result, NSString* errorMessage, oak::uuid_t const& filterUUID){
		if(result == OakDocumentIOResultSuccess)
		{
			OakDocument* document = doc->document();
			BOOL showBundleSuggestions = ![[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsDisableBundleSuggestionsKey];
			if(!document.fileType && showBundleSuggestions)
			{
				NSArray<BundleGrammar*>* grammars = document.proposedGrammars;
				if(NSArray* excludedGrammars = [[NSUserDefaults standardUserDefaults] stringArrayForKey:kUserDefaultsGrammarsToNeverSuggestKey])
					grammars = [grammars filteredArrayUsingPredicate:[NSPredicate predicateWithFormat:@"!(identifier.UUIDString IN %@)", excludedGrammars]];
				if(_bundlesAlreadySuggested)
					grammars = [grammars filteredArrayUsingPredicate:[NSPredicate predicateWithFormat:@"!(bundle IN %@)", _bundlesAlreadySuggested]];

				if([grammars count] && network::can_reach_host([[[NSURL URLWithString:@(REST_API)] host] UTF8String]))
				{
					self.bundlesAlreadySuggested = [(_bundlesAlreadySuggested ?: @[ ]) arrayByAddingObject:[grammars firstObject].bundle];

					SelectGrammarViewController* installer = [[SelectGrammarViewController alloc] init];
					installer.documentDisplayName = document.path || document.customName ? document.displayName : nil;

					__weak __block id documentCloseObserver = [[NSNotificationCenter defaultCenter] addObserverForName:OakDocumentWillCloseNotification object:document queue:nil usingBlock:^(NSNotification*){
						[installer dismiss];
						[[NSNotificationCenter defaultCenter] removeObserver:documentCloseObserver];
					}];

					[installer showGrammars:grammars forView:_documentView completionHandler:^(SelectGrammarResponse response, BundleGrammar* grammar){
						if(response == SelectGrammarResponseInstall && grammar.bundle.isInstalled)
						{
							for(document::document_ptr cppDoc : _cppDocuments)
							{
								OakDocument* doc = cppDoc->document();
								if([doc isEqual:document] || [[doc proposedGrammars] containsObject:grammar])
									doc.fileType = grammar.fileType;
							}
						}
						else if(response == SelectGrammarResponseNever)
						{
							NSArray* excludedGrammars = [[NSUserDefaults standardUserDefaults] stringArrayForKey:kUserDefaultsGrammarsToNeverSuggestKey] ?: @[ ];
							[[NSUserDefaults standardUserDefaults] setObject:[excludedGrammars arrayByAddingObject:grammar.identifier.UUIDString] forKey:kUserDefaultsGrammarsToNeverSuggestKey];
						}

						if(id observer = documentCloseObserver)
							[[NSNotificationCenter defaultCenter] removeObserver:observer];
					}];
				}

				std::string const docAttributes = document.path ? "attr.file.unknown-type" : "attr.untitled";
				document.fileType = to_ns(settings_for_path(to_s(document.virtualPath ?: document.path), docAttributes, to_s(self.projectPath)).get(kSettingsFileTypeKey, "text.plain"));
			}

			[self makeTextViewFirstResponder:self];
			self.selectedCppDocument = doc;
			[self performSelector:@selector(didOpenDocuemntInTextView:) withObject:self.documentView.textView afterDelay:0];
			[doc->document() close];
		}
		else
		{
			if(filterUUID)
				show_command_error(to_s(errorMessage), filterUUID);

			// Close the tab that failed to open
			for(size_t i = 0; i < _cppDocuments.size(); ++i)
			{
				if(_cppDocuments[i]->identifier() == doc->identifier())
				{
					[self closeTabsAtIndexes:[NSIndexSet indexSetWithIndex:i] askToSaveChanges:NO createDocumentIfEmpty:self.fileBrowserVisible];
					break;
				}
			}

			if(_cppDocuments.empty())
				[self close];
		}
	}];
}

- (IBAction)saveDocument:(id)sender
{
	if(!_selectedCppDocument)
		return;

	if(_selectedCppDocument->path() != NULL_STR)
	{
		[self saveDocumentsUsingEnumerator:@[ _selectedCppDocument->document() ].objectEnumerator completionHandler:nil];
	}
	else
	{
		NSString* const suggestedFolder  = self.untitledSavePath;
		NSString* const suggestedName    = [_selectedCppDocument->document() displayNameWithExtension:YES];
		[OakSavePanel showWithPath:suggestedName directory:suggestedFolder fowWindow:self.window encoding:_selectedCppDocument->disk_encoding() completionHandler:^(NSString* path, encoding::type const& encoding){
			if(!path)
				return;

			std::vector<std::string> const& paths = path::expand_braces(to_s(path));
			ASSERT_LT(0, paths.size());

			_selectedCppDocument->set_path(paths[0]);
			_selectedCppDocument->set_disk_encoding(encoding);

			// if(_selectedCppDocument->identifier() == scratchDocument)
			// 	scratchDocument = oak::uuid_t();

			if(paths.size() > 1)
			{
				 // FIXME check if paths[0] already exists (overwrite)

				std::vector<document::document_ptr> documents = { _selectedCppDocument };
				std::transform(paths.begin() + 1, paths.end(), back_inserter(documents), [&encoding](std::string const& path) -> document::document_ptr {
					document::document_ptr doc = document::create(path);
					doc->set_disk_encoding(encoding);
					return doc;
				});

				[self insertDocuments:documents atIndex:_selectedTabIndex selecting:documents.front() andClosing:{ }];
			}

			[self saveDocumentsUsingEnumerator:@[ _selectedCppDocument->document() ].objectEnumerator completionHandler:nil];
		}];
	}
}

- (IBAction)saveDocumentAs:(id)sender
{
	if(!_selectedCppDocument)
		return;

	std::string const documentPath   = _selectedCppDocument->path();
	NSString* const suggestedFolder  = [NSString stringWithCxxString:path::parent(documentPath)] ?: self.untitledSavePath;
	NSString* const suggestedName    = [NSString stringWithCxxString:path::name(documentPath)]   ?: [_selectedCppDocument->document() displayNameWithExtension:YES];
	[OakSavePanel showWithPath:suggestedName directory:suggestedFolder fowWindow:self.window encoding:_selectedCppDocument->disk_encoding() completionHandler:^(NSString* path, encoding::type const& encoding){
		if(!path)
			return;
		_selectedCppDocument->set_path(to_s(path));
		_selectedCppDocument->set_disk_encoding(encoding);
		[self saveDocumentsUsingEnumerator:@[ _selectedCppDocument->document() ].objectEnumerator completionHandler:nil];
	}];
}

- (void)saveDocumentsUsingEnumerator:(NSEnumerator*)anEnumerator completionHandler:(void(^)(OakDocumentIOResult result))callback
{
	if(OakDocument* document = [anEnumerator nextObject])
	{
		id observerId = [[NSNotificationCenter defaultCenter] addObserverForName:OakDocumentWillShowAlertNotification object:document queue:nil usingBlock:^(NSNotification*){
			for(size_t i = 0; i < _cppDocuments.size(); ++i)
			{
				if(document.isLoaded && _cppDocuments[i]->document() == document)
				{
					if(_selectedCppDocument != _cppDocuments[i])
					{
						self.selectedTabIndex = i;
						self.selectedCppDocument = _cppDocuments[i];
					}

					if(NSApp.isActive && (self.window.isMiniaturized || !self.window.isKeyWindow))
						[self.window makeKeyAndOrderFront:self];

					break;
				}
			}
		}];

		[document saveModalForWindow:self.window completionHandler:^(OakDocumentIOResult result, NSString* errorMessage, oak::uuid_t const& filterUUID){
			[[NSNotificationCenter defaultCenter] removeObserver:observerId];
			if(result == OakDocumentIOResultSuccess)
			{
				[self saveDocumentsUsingEnumerator:anEnumerator completionHandler:callback];
			}
			else
			{
				if(result == OakDocumentIOResultFailure)
				{
					[self.window.attachedSheet orderOut:self];
					if(filterUUID)
							show_command_error(to_s(errorMessage), filterUUID, self.window);
					else	[[NSAlert tmAlertWithMessageText:[NSString stringWithFormat:@"The document “%@” could not be saved.", document.displayName] informativeText:(errorMessage ?: @"Please check Console output for reason.") buttons:@"OK", nil] beginSheetModalForWindow:self.window modalDelegate:nil didEndSelector:NULL contextInfo:NULL];
				}

				if(callback)
					callback(result);
			}
		}];
	}
	else
	{
		if(callback)
			callback(OakDocumentIOResultSuccess);
	}
}

- (IBAction)saveAllDocuments:(id)sender
{
	NSMutableArray* documentsToSave = [NSMutableArray array];
	for(auto document : _cppDocuments)
	{
		if(document->is_modified())
			[documentsToSave addObject:document->document()];
	}
	[self saveDocumentsUsingEnumerator:[documentsToSave objectEnumerator] completionHandler:nil];
}

- (void)saveAllEditedDocuments:(BOOL)includeAllFlag completionHandler:(void(^)(BOOL didSave))callback
{
	NSMutableArray* documentsToSave = [NSMutableArray array];
	if(includeAllFlag)
	{
		for(auto document : _cppDocuments)
		{
			if(document->is_modified() && document->path() != NULL_STR)
				[documentsToSave addObject:document->document()];
		}
	}
	else
	{
		if(_selectedCppDocument && (_selectedCppDocument->is_modified() || !_selectedCppDocument->is_on_disk()))
			[documentsToSave addObject:_selectedCppDocument->document()];
	}

	[self saveDocumentsUsingEnumerator:[documentsToSave objectEnumerator] completionHandler:^(OakDocumentIOResult result){
		callback(result == OakDocumentIOResultSuccess);
	}];
}

- (OakHTMLOutputView*)htmlOutputView:(BOOL)createFlag forIdentifier:(NSUUID*)identifier
{
	// if createFlag == YES then return (potential new) OakHTMLOutputView where isRunningCommand == NO.
	// If createFlag == NO and there is non-busy OakHTMLOutputView with commandIdentifier == identifier then return it
	// otherwise return busy OakHTMLOutputView with commandIdentifier == identifier or nil.

	if(!self.htmlOutputInWindow)
	{
		BOOL nonExistingOrNonBusy   = !self.htmlOutputView || !self.htmlOutputView.isRunningCommand;
		BOOL existsForOurIdentifier = self.htmlOutputView && [self.htmlOutputView.commandIdentifier isEqual:identifier];
		if(createFlag ? nonExistingOrNonBusy : existsForOurIdentifier)
		{
			self.htmlOutputVisible = YES;
			return self.htmlOutputView;
		}
	}

	NSMutableArray <OakHTMLOutputView*>* htmlOutputViews = [NSMutableArray array];
	if(self.htmlOutputWindowController)
		[htmlOutputViews addObject:self.htmlOutputWindowController.htmlOutputView];

	for(NSWindow* window in [NSApp orderedWindows])
	{
		if([window isVisible] && ![window isMiniaturized] && [window.delegate isKindOfClass:[HTMLOutputWindowController class]])
			[htmlOutputViews addObject:[(HTMLOutputWindowController*)window.delegate htmlOutputView]];
	}

	NSArray* allHTMLViews = [htmlOutputViews filteredArrayUsingPredicate:[NSPredicate predicateWithFormat:@"needsNewWebView == NO AND commandIdentifier == %@", identifier]];
	NSArray* nonBusyViews = [allHTMLViews filteredArrayUsingPredicate:[NSPredicate predicateWithFormat:@"isRunningCommand == NO"]];

	if(OakHTMLOutputView* view = [nonBusyViews firstObject])
	{
		return view;
	}
	else if(createFlag)
	{
		self.htmlOutputWindowController = [[HTMLOutputWindowController alloc] initWithIdentifier:identifier];
		return self.htmlOutputWindowController.htmlOutputView;
	}
	return [allHTMLViews firstObject];
}

- (void)updateEnvironment:(std::map<std::string, std::string>&)res forCommand:(OakCommand*)aCommand
{
	for(auto const& pair : [self variables])
		res[pair.first] = pair.second;

	if(aCommand.firstResponder == _fileBrowser)
	{
		NSURL* fileURL = [[_fileBrowser.selectedURLs filteredArrayUsingPredicate:[NSPredicate predicateWithFormat:@"isFileURL == YES"]] firstObject];
		res = bundles::scope_variables(res);
		res = variables_for_path(res, to_s(fileURL.path));
	}
	else if(aCommand.firstResponder != _textView)
	{
		if([aCommand.firstResponder respondsToSelector:@selector(updateEnvironment:)])
			[(id)aCommand.firstResponder updateEnvironment:res];
	}
	else // OakTextView
	{
		[_textView updateEnvironment:res];
	}
}

- (void)showDocument:(OakDocument*)aDocument
{
	[OakDocumentController.sharedInstance showDocument:aDocument inProject:[[NSUUID alloc] initWithUUIDString:self.identifier] bringToFront:YES];
}

// ================
// = Window Title =
// ================

- (void)updateProxyIcon
{
	if(self.documentPath && self.documentIsOnDisk)
	{
		OakFileIconImage* icon = [[OakFileIconImage alloc] initWithSize:NSMakeSize(16, 16)];
		icon.path      = self.documentPath;
		icon.scmStatus = self.documentSCMStatus;

		self.window.representedFilename = icon.path;
		[self.window standardWindowButton:NSWindowDocumentIconButton].image = icon;
	}
	else
	{
		self.window.representedFilename = @"";
		[self.window standardWindowButton:NSWindowDocumentIconButton].image = nil;
	}
}

- (void)updateWindowTitleAndRevealFile
{
	[self updateWindowTitle];

	if(self.autoRevealFile && self.fileBrowserVisible)
	{
		if(_selectedCppDocument && _selectedCppDocument->path() != NULL_STR)
			[self revealFileInProject:self];
	}
}

- (void)updateWindowTitle
{
	if(_selectedCppDocument && _documentDisplayName)
	{
		auto map = _selectedCppDocument->document_variables();
		auto const& scm = _documentSCMVariables.empty() ? _projectSCMVariables : _documentSCMVariables;
		map.insert(scm.begin(), scm.end());
		if(self.projectPath)
			map["projectDirectory"] = to_s(self.projectPath);

		std::string docDirectory = _selectedCppDocument->path() != NULL_STR ? path::parent(_selectedCppDocument->path()) : to_s(self.untitledSavePath);
		settings_t const settings = settings_for_path(_selectedCppDocument->logical_path(), _selectedCppDocument->file_type() + " " + to_s(self.scopeAttributes), docDirectory, map);
		self.window.title = [NSString stringWithCxxString:settings.get(kSettingsWindowTitleKey, to_s(self.documentDisplayName))];
	}
	else
	{
		self.window.title = @"«no documents»";
	}
}

- (void)updateExternalAttributes
{
	struct attribute_rule_t { std::string attribute; path::glob_t glob; std::string group; };
	static auto const rules = new std::vector<attribute_rule_t>
	{
		{ "attr.scm.svn",         ".svn",           "scm"     },
		{ "attr.scm.hg",          ".hg",            "scm"     },
		{ "attr.scm.git",         ".git",           "scm"     },
		{ "attr.scm.p4",          ".p4config",      "scm"     },
		{ "attr.project.ninja",   "build.ninja",    "build"   },
		{ "attr.project.make",    "Makefile",       "build"   },
		{ "attr.project.xcode",   "*.xcodeproj",    "build"   },
		{ "attr.project.rake",    "Rakefile",       "build"   },
		{ "attr.project.ant",     "build.xml",      "build"   },
		{ "attr.project.cmake",   "CMakeLists.txt", "build"   },
		{ "attr.project.maven",   "pom.xml",        "build"   },
		{ "attr.project.scons",   "SConstruct",     "build"   },
		{ "attr.project.lein",    "project.clj",    "build"   },
		{ "attr.project.cargo",   "Cargo.toml",     "build"   },
		{ "attr.project.swift",   "Package.swift",  "build"   },
		{ "attr.project.vagrant", "Vagrantfile",    "vagrant" },
		{ "attr.project.jekyll",  "_config.yml",    "jekyll"  },
		{ "attr.test.rspec",      ".rspec",         "test"    },
	};

	_externalScopeAttributes.clear();
	if(!_selectedCppDocument && !_projectPath)
		return;

	std::string const projectDir   = to_s(_projectPath ?: NSHomeDirectory());
	std::string const documentPath = _selectedCppDocument && _selectedCppDocument->path() != NULL_STR ? _selectedCppDocument->path() : path::join(projectDir, "dummy");

	dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_LOW, 0), ^{

		std::vector<std::string> res;
		std::set<std::string> groups;
		std::string dir = documentPath;
		do {

			dir = path::parent(dir);
			auto entries = path::entries(dir);

			for(auto rule : *rules)
			{
				if(groups.find(rule.group) != groups.end())
					continue;

				for(auto entry : entries)
				{
					if(rule.glob.does_match(entry->d_name))
					{
						res.push_back(rule.attribute);
						if(rule.group != NULL_STR)
						{
							groups.insert(rule.group);
							break;
						}
					}
				}
			}

		} while(path::is_child(dir, projectDir) && dir != projectDir);

		dispatch_async(dispatch_get_main_queue(), ^{
			std::string const currentProjectDir   = to_s(_projectPath ?: NSHomeDirectory());
			std::string const currentDocumentPath = _selectedCppDocument ? _selectedCppDocument->path() : path::join(projectDir, "dummy");
			if(projectDir == currentProjectDir && currentDocumentPath == currentDocumentPath)
				_externalScopeAttributes = res;
		});

	});
}

- (void)setProjectPath:(NSString*)newProjectPath
{
	if(_projectPath != newProjectPath && ![_projectPath isEqualToString:newProjectPath])
	{
		_projectPath = newProjectPath;
		if(_projectSCMInfo = scm::info(to_s(_projectPath)))
		{
			__weak DocumentWindowController* weakSelf = self;
			_projectSCMInfo->add_callback(^(scm::info_t const& info){
				weakSelf.projectSCMVariables = info.scm_variables();
			});
		}
		else
		{
			self.projectSCMVariables = std::map<std::string, std::string>();
		}

		_projectScopeAttributes.clear();

		std::string const customAttributes = settings_for_path(NULL_STR, text::join(_projectScopeAttributes, " "), to_s(_projectPath)).get(kSettingsScopeAttributesKey, NULL_STR);
		if(customAttributes != NULL_STR)
			_projectScopeAttributes.push_back(customAttributes);

		[self updateExternalAttributes];
		[self updateWindowTitleAndRevealFile];
	}
}

- (void)setDocumentPath:(NSString*)newDocumentPath
{
	if(_documentPath != newDocumentPath && !([_documentPath isEqualToString:newDocumentPath]) || _documentScopeAttributes.empty())
	{
		_documentPath = newDocumentPath;
		_documentScopeAttributes = text::split(file::path_attributes(to_s(_documentPath)), " ");

		std::string docDirectory = _documentPath ? path::parent(to_s(_documentPath)) : to_s(self.projectPath);

		if(_selectedCppDocument)
		{
			std::string const customAttributes = settings_for_path(to_s(_documentPath), _selectedCppDocument->file_type() + " " + text::join(_documentScopeAttributes, " "), docDirectory).get(kSettingsScopeAttributesKey, NULL_STR);
			if(customAttributes != NULL_STR)
				_documentScopeAttributes.push_back(customAttributes);
		}

		self.documentSCMStatus    = scm::status::unknown;
		self.documentSCMVariables = std::map<std::string, std::string>();

		if(_documentSCMInfo = scm::info(docDirectory))
		{
			__weak DocumentWindowController* weakSelf = self;
			_documentSCMInfo->add_callback(^(scm::info_t const& info){
				weakSelf.documentSCMStatus    = info.status(to_s(weakSelf.documentPath));
				weakSelf.documentSCMVariables = info.scm_variables();
			});
		}

		[self updateExternalAttributes];
		[self updateProxyIcon];
		[self updateWindowTitleAndRevealFile];
	}
}

- (void)setDocumentDisplayName:(NSString*)newDisplayName
{
	if(_documentDisplayName != newDisplayName && ![_documentDisplayName isEqualToString:newDisplayName])
	{
		_documentDisplayName = newDisplayName;
		[self updateWindowTitleAndRevealFile];
	}
}

- (void)setDocumentIsModified:(BOOL)newDocumentIsModified
{
	if(_documentIsModified != newDocumentIsModified)
	{
		_documentIsModified = newDocumentIsModified;
		self.window.documentEdited = _documentIsModified;
	}
}

- (void)setDocumentIsOnDisk:(BOOL)newDocumentIsOnDisk
{
	if(_documentIsOnDisk != newDocumentIsOnDisk)
	{
		_documentIsOnDisk = newDocumentIsOnDisk;
		[self updateProxyIcon];
	}
}

- (void)setDocumentSCMStatus:(scm::status::type)newDocumentSCMStatus
{
	if(_documentSCMStatus != newDocumentSCMStatus)
	{
		_documentSCMStatus = newDocumentSCMStatus;
		[self updateProxyIcon];
	}
}

- (void)setProjectSCMVariables:(std::map<std::string, std::string> const&)newVariables
{
	if(_projectSCMVariables != newVariables)
	{
		_projectSCMVariables = newVariables;
		[self updateWindowTitleAndRevealFile];
	}
}

- (void)setDocumentSCMVariables:(std::map<std::string, std::string> const&)newVariables
{
	if(_documentSCMVariables != newVariables)
	{
		_documentSCMVariables = newVariables;
		[self updateWindowTitleAndRevealFile];
	}
}

- (std::map<std::string, std::string> const&)projectSCMVariables
{
	return _projectSCMVariables;
}

- (std::map<std::string, std::string> const&)documentSCMVariables
{
	return _documentSCMVariables;
}

- (void)takeProjectPathFrom:(NSMenuItem*)aMenuItem
{
	if(NSString* path = [aMenuItem respondsToSelector:@selector(representedObject)] ? [aMenuItem representedObject] : nil)
		self.projectPath = self.defaultProjectPath = path;
}

// ========================
// = OakTextView Delegate =
// ========================

- (NSString*)scopeAttributes
{
	std::set<std::string> attributes;

	auto const& vars = _documentSCMVariables.empty() ? _projectSCMVariables : _documentSCMVariables;
	auto scmName = vars.find("TM_SCM_NAME");
	if(scmName != vars.end())
		attributes.insert("attr.scm." + scmName->second);
	auto branch = vars.find("TM_SCM_BRANCH");
	if(branch != vars.end())
		attributes.insert("attr.scm.branch." + branch->second);

	if(self.documentSCMStatus != scm::status::unknown)
		attributes.insert("attr.scm.status." + to_s(self.documentSCMStatus));

	attributes.insert(_documentScopeAttributes.begin(), _documentScopeAttributes.end());
	attributes.insert(_projectScopeAttributes.begin(), _projectScopeAttributes.end());
	attributes.insert(_externalScopeAttributes.begin(), _externalScopeAttributes.end());

	return [NSString stringWithCxxString:text::join(attributes, " ")];
}

// ==============
// = Properties =
// ==============

- (OakDocument*)selectedDocument
{
	return _selectedCppDocument ? _selectedCppDocument->document() : nil;
}

- (NSArray<OakDocument*>*)documents
{
	NSMutableArray* res = [NSMutableArray array];
	for(auto doc : _cppDocuments)
		[res addObject:doc->document()];
	return res;
}

- (void)setCppDocuments:(std::vector<document::document_ptr>)newDocuments
{
	for(auto document : newDocuments)
	{
		[self trackDocument:document->document()];

		// Avoid resetting directory when tearing off a tab (unless moved to new project)
		if(!document->document().path && (self.projectPath || !document->document().directory))
			document->document().directory = self.projectPath ?: self.defaultProjectPath;
	}

	for(OakDocument* document in self.documents)
		[self untrackDocument:document];

	_cppDocuments = newDocuments;

	if(_cppDocuments.size())
		[self.tabBarView reloadData];

	[self updateFileBrowserStatus:self];
	[[self class] scheduleSessionBackup:self];
}

- (void)setSelectedCppDocument:(document::document_ptr)newSelectedDocument
{
	ASSERT(!newSelectedDocument || newSelectedDocument->is_loaded());
	if(_selectedCppDocument == newSelectedDocument)
	{
		self.documentView.document = _selectedCppDocument ? _selectedCppDocument->document() : nil;
		return;
	}

	if(_selectedCppDocument)
		_selectedCppDocument->hide();
	if(newSelectedDocument)
		newSelectedDocument->show();

	[self trackDocument:newSelectedDocument ? newSelectedDocument->document() : nil];
	[self untrackDocument:self.selectedDocument];

	if(_selectedCppDocument = newSelectedDocument)
	{
		NSString* projectPath = self.defaultProjectPath ?: self.fileBrowser.path ?: [NSString stringWithCxxString:path::parent(_selectedCppDocument->path())];
		if(projectPath)
		{
			std::map<std::string, std::string> const map = { { "projectDirectory", to_s(projectPath) } };
			settings_t const settings = settings_for_path(NULL_STR, scope::scope_t(), to_s(projectPath), map);
			std::string const userProjectDirectory = settings.get(kSettingsProjectDirectoryKey, NULL_STR);
			if(path::is_absolute(userProjectDirectory))
				projectPath = [NSString stringWithCxxString:path::normalize(userProjectDirectory)];
		}
		else if(NSString* urlString = [[NSUserDefaults standardUserDefaults] stringForKey:kUserDefaultsInitialFileBrowserURLKey])
		{
			if(NSURL* url = [NSURL URLWithString:urlString])
				projectPath = [[url filePathURL] path];
		}

		self.projectPath         = projectPath;
		self.documentPath        = [NSString stringWithCxxString:_selectedCppDocument->logical_path()];
		self.documentDisplayName = [NSString stringWithCxxString:_selectedCppDocument->display_name()];
		self.documentIsModified  = _selectedCppDocument->is_modified();
		self.documentIsOnDisk    = _selectedCppDocument->is_on_disk();

		self.documentView.document = _selectedCppDocument->document();
		[[self class] scheduleSessionBackup:self];
	}
	else
	{
		self.projectPath         = nil;
		self.documentPath        = nil;
		self.documentDisplayName = nil;
		self.documentIsModified  = NO;
		self.documentIsOnDisk    = NO;
	}
}

- (void)setSelectedTabIndex:(NSUInteger)newSelectedTabIndex
{
	_selectedTabIndex = newSelectedTabIndex;
	[self.tabBarView setSelectedTab:newSelectedTabIndex];
}

- (void)setIdentifier:(NSString*)newIdentifier
{
	if(_identifier == newIdentifier || [_identifier isEqualToString:newIdentifier])
		return;

	NSString* oldIdentifier = _identifier;
	if(_identifier = newIdentifier)
		[AllControllers() setObject:self forKey:newIdentifier];

	if(oldIdentifier)
		[AllControllers() removeObjectForKey:oldIdentifier]; // This may release our object
}

// ===========================
// = OakTabBarViewDataSource =
// ===========================

- (NSUInteger)numberOfRowsInTabBarView:(OakTabBarView*)aTabBarView                         { return _cppDocuments.size(); }
- (NSString*)tabBarView:(OakTabBarView*)aTabBarView titleForIndex:(NSUInteger)anIndex      { return [NSString stringWithCxxString:_cppDocuments[anIndex]->display_name()]; }
- (NSString*)tabBarView:(OakTabBarView*)aTabBarView pathForIndex:(NSUInteger)anIndex       { return [NSString stringWithCxxString:_cppDocuments[anIndex]->path()] ?: @""; }
- (NSString*)tabBarView:(OakTabBarView*)aTabBarView identifierForIndex:(NSUInteger)anIndex { return [NSString stringWithCxxString:_cppDocuments[anIndex]->identifier()]; }
- (BOOL)tabBarView:(OakTabBarView*)aTabBarView isEditedAtIndex:(NSUInteger)anIndex         { return _cppDocuments[anIndex]->is_modified(); }

// ==============================
// = OakTabBarView Context Menu =
// ==============================

- (NSIndexSet*)tryObtainIndexSetFrom:(id)sender
{
	id res = [sender respondsToSelector:@selector(representedObject)] ? [sender representedObject] : sender;
	if([res isKindOfClass:[NSIndexSet class]])
		return res;
	else if(!_cppDocuments.empty())
		return [NSIndexSet indexSetWithIndex:self.selectedTabIndex];
	return nil;
}

- (void)takeNewTabIndexFrom:(id)sender
{
	if(NSIndexSet* indexSet = [self tryObtainIndexSetFrom:sender])
	{
		document::document_ptr doc = document::create();
		[self insertDocuments:{ doc } atIndex:[indexSet firstIndex] selecting:doc andClosing:{ }];
		[self openAndSelectDocument:doc];
	}
}

- (void)takeTabsToCloseFrom:(id)sender
{
	if(NSIndexSet* indexSet = [self tryObtainIndexSetFrom:sender])
		[self closeTabsAtIndexes:indexSet askToSaveChanges:YES createDocumentIfEmpty:YES];
}

- (void)takeTabsToTearOffFrom:(id)sender
{
	if(NSIndexSet* indexSet = [self tryObtainIndexSetFrom:sender])
	{
		std::vector<document::document_ptr> documents;
		for(NSUInteger index = [indexSet firstIndex]; index != NSNotFound; index = [indexSet indexGreaterThanIndex:index])
			documents.push_back(_cppDocuments[index]);

		if(documents.size() == 1)
		{
			DocumentWindowController* controller = [DocumentWindowController new];
			controller.cppDocuments = { documents[0] };
			if(path::is_child(documents[0]->path(), to_s(self.projectPath)))
				controller.defaultProjectPath = self.projectPath;
			[controller openAndSelectDocument:documents[0]];
			[controller showWindow:self];
			[self closeTabsAtIndexes:indexSet askToSaveChanges:NO createDocumentIfEmpty:YES];
		}
	}
}

- (IBAction)toggleSticky:(id)sender
{
	if(NSIndexSet* indexSet = [self tryObtainIndexSetFrom:sender])
	{
		std::vector<document::document_ptr> documents;
		for(NSUInteger index = [indexSet firstIndex]; index != NSNotFound; index = [indexSet indexGreaterThanIndex:index])
			[self setDocument:_cppDocuments[index] sticky:![self isDocumentSticky:_cppDocuments[index]]];
	}
}

- (NSMenu*)menuForTabBarView:(OakTabBarView*)aTabBarView
{
	NSInteger tabIndex = aTabBarView.tag;
	NSInteger total    = _cppDocuments.size();

	NSMutableIndexSet* newTabAtTab   = tabIndex == -1 ? [NSMutableIndexSet indexSetWithIndex:total] : [NSMutableIndexSet indexSetWithIndex:tabIndex + 1];
	NSMutableIndexSet* clickedTab    = tabIndex == -1 ? [NSMutableIndexSet indexSet] : [NSMutableIndexSet indexSetWithIndex:tabIndex];
	NSMutableIndexSet* otherTabs     = tabIndex == -1 ? [NSMutableIndexSet indexSet] : [NSMutableIndexSet indexSetWithIndexesInRange:NSMakeRange(0, total)];
	NSMutableIndexSet* rightSideTabs = tabIndex == -1 ? [NSMutableIndexSet indexSet] : [NSMutableIndexSet indexSetWithIndexesInRange:NSMakeRange(0, total)];

	if(tabIndex != -1)
	{
		[otherTabs removeIndex:tabIndex];
		[rightSideTabs removeIndexes:[NSIndexSet indexSetWithIndexesInRange:NSMakeRange(0, tabIndex + 1)]];
	}

	for(size_t i = 0; i < _cppDocuments.size(); ++i)
	{
		if([self isDocumentSticky:_cppDocuments[i]])
		{
			[otherTabs removeIndex:i];
			[rightSideTabs removeIndex:i];
		}
	}

	SEL closeSingleTabSelector = tabIndex == _selectedTabIndex ? @selector(performCloseTab:) : @selector(takeTabsToCloseFrom:);

	NSMenu* menu = [NSMenu new];
	[menu addItemWithTitle:@"New Tab"                 action:@selector(takeNewTabIndexFrom:)   keyEquivalent:@""];
	[menu addItemWithTitle:@"Move Tab to New Window"  action:@selector(takeTabsToTearOffFrom:) keyEquivalent:@""];
	[menu addItem:[NSMenuItem separatorItem]];
	[menu addItemWithTitle:@"Close Tab"               action:closeSingleTabSelector            keyEquivalent:@""];
	[menu addItemWithTitle:@"Close Other Tabs"        action:@selector(takeTabsToCloseFrom:)   keyEquivalent:@""];
	[menu addItemWithTitle:@"Close Tabs to the Right" action:@selector(takeTabsToCloseFrom:)   keyEquivalent:@""];
	[menu addItem:[NSMenuItem separatorItem]];
	[menu addItemWithTitle:@"Sticky"                  action:@selector(toggleSticky:)          keyEquivalent:@""];

	NSIndexSet* indexSets[] = { newTabAtTab, total > 1 ? clickedTab : [NSIndexSet indexSet], nil, clickedTab, otherTabs, rightSideTabs, nil, clickedTab };
	for(size_t i = 0; i < sizeofA(indexSets); ++i)
	{
		if(NSIndexSet* indexSet = indexSets[i])
			[[menu itemAtIndex:i] setRepresentedObject:indexSet];
	}

	return menu;
}

// =========================
// = OakTabBarViewDelegate =
// =========================

- (BOOL)tabBarView:(OakTabBarView*)aTabBarView shouldSelectIndex:(NSUInteger)anIndex
{
	[self openAndSelectDocument:_cppDocuments[anIndex]];
	self.selectedTabIndex = anIndex;
	return YES;
}

- (void)tabBarView:(OakTabBarView*)aTabBarView didDoubleClickIndex:(NSUInteger)anIndex
{
	if(_cppDocuments.size() > 1)
		[self takeTabsToTearOffFrom:[NSMutableIndexSet indexSetWithIndex:anIndex]];
}

- (void)tabBarViewDidDoubleClick:(OakTabBarView*)aTabBarView
{
	[self takeNewTabIndexFrom:[NSMutableIndexSet indexSetWithIndex:_cppDocuments.size()]];
}

// ================
// = Tab Dragging =
// ================

- (BOOL)performDropOfTabItem:(OakTabItem*)tabItem fromTabBar:(OakTabBarView*)sourceTabBar index:(NSUInteger)dragIndex toTabBar:(OakTabBarView*)destTabBar index:(NSUInteger)droppedIndex operation:(NSDragOperation)operation
{
	document::document_ptr srcDocument = document::find(to_s(tabItem.identifier));
	if(!srcDocument)
		return NO;

	[self insertDocuments:{ srcDocument } atIndex:droppedIndex selecting:_selectedCppDocument andClosing:{ srcDocument->identifier() }];

	if(operation == NSDragOperationMove && sourceTabBar != destTabBar)
	{
		for(DocumentWindowController* delegate in SortedControllers())
		{
			if(delegate == sourceTabBar.delegate)
			{
				if(delegate.fileBrowserVisible || delegate.cppDocuments.size() > 1)
						[delegate closeTabsAtIndexes:[NSIndexSet indexSetWithIndex:dragIndex] askToSaveChanges:NO createDocumentIfEmpty:YES];
				else	[delegate close];
				return YES;
			}
		}
	}

	return YES;
}

- (IBAction)selectNextTab:(id)sender            { self.selectedTabIndex = (_selectedTabIndex + 1) % _cppDocuments.size();                     [self openAndSelectDocument:_cppDocuments[_selectedTabIndex]]; }
- (IBAction)selectPreviousTab:(id)sender        { self.selectedTabIndex = (_selectedTabIndex + _cppDocuments.size() - 1) % _cppDocuments.size(); [self openAndSelectDocument:_cppDocuments[_selectedTabIndex]]; }
- (IBAction)takeSelectedTabIndexFrom:(id)sender { self.selectedTabIndex = [sender tag];                                                    [self openAndSelectDocument:_cppDocuments[_selectedTabIndex]]; }

// ==================
// = OakFileBrowser =
// ==================

- (void)fileBrowser:(OakFileBrowser*)aFileBrowser openURLs:(NSArray*)someURLs
{
	NSMutableArray* items = [NSMutableArray array];
	for(NSURL* url in someURLs)
	{
		if([url isFileURL])
			[items addObject:@{ @"path" : [url path] }];
	}
	[self openItems:items closingOtherTabs:OakIsAlternateKeyOrMouseEvent()];
}

- (void)fileBrowser:(OakFileBrowser*)aFileBrowser closeURL:(NSURL*)anURL
{
	if(![anURL isFileURL])
		return;

	std::string const path = to_s([anURL path]);
	auto documents = _cppDocuments;
	NSMutableIndexSet* indexSet = [NSMutableIndexSet indexSet];
	for(size_t i = 0; i < documents.size(); ++i)
	{
		if(path == documents[i]->path())
			[indexSet addIndex:i];
	}
	[self closeTabsAtIndexes:indexSet askToSaveChanges:YES createDocumentIfEmpty:YES];
}

- (void)setFileBrowserVisible:(BOOL)makeVisibleFlag
{
	if(_fileBrowserVisible != makeVisibleFlag)
	{
		_fileBrowserVisible = makeVisibleFlag;
		if(!self.fileBrowser && makeVisibleFlag)
		{
			self.fileBrowser = [OakFileBrowser new];
			self.fileBrowser.delegate = self;
			[self.fileBrowser setupViewWithState:_fileBrowserHistory];
			if(self.projectPath && !_fileBrowserHistory)
				self.fileBrowser.url = [NSURL fileURLWithPath:self.projectPath];
			[self updateFileBrowserStatus:self];
			if(self.layoutView.tabsAboveDocument)
				[self.tabBarView expand];

			[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(fileBrowserDidDuplicateAtURLs:) name:OakFileBrowserDidDuplicateURLs object:_fileBrowser];
		}

		if(!makeVisibleFlag && [[self.window firstResponder] isKindOfClass:[NSView class]] && [(NSView*)[self.window firstResponder] isDescendantOf:self.layoutView.fileBrowserView])
			[self makeTextViewFirstResponder:self];

		self.layoutView.fileBrowserView       = makeVisibleFlag ? self.fileBrowser.view : nil;
		self.layoutView.fileBrowserHeaderView = makeVisibleFlag ? self.fileBrowser.headerView : nil;

		if(makeVisibleFlag)
		{
			self.fileBrowser.nextResponder = self.fileBrowser.view.nextResponder;
			self.fileBrowser.view.nextResponder = self.fileBrowser;
			if(self.autoRevealFile && _selectedCppDocument && _selectedCppDocument->path() != NULL_STR)
				[self revealFileInProject:self];
		}

		if(!self.disableFileBrowserWindowResize && ([self.window styleMask] & NSFullScreenWindowMask) != NSFullScreenWindowMask)
		{
			NSRect windowFrame = self.window.frame;

			if(NSEqualRects(windowFrame, self.newWindowFrame))
			{
				windowFrame = self.oldWindowFrame;
			}
			else if(makeVisibleFlag)
			{
				NSRect screenFrame = [[self.window screen] visibleFrame];
				CGFloat minX = NSMinX(windowFrame);
				CGFloat maxX = NSMaxX(windowFrame);

				if(self.layoutView.fileBrowserOnRight)
						maxX += self.fileBrowserWidth + 1;
				else	minX -= self.fileBrowserWidth + 1;

				if(minX < NSMinX(screenFrame))
					maxX += NSMinX(screenFrame) - minX;
				if(maxX > NSMaxX(screenFrame))
					minX -= maxX - NSMaxX(screenFrame);

				minX = MAX(minX, NSMinX(screenFrame));
				maxX = MIN(maxX, NSMaxX(screenFrame));

				windowFrame.origin.x   = minX;
				windowFrame.size.width = maxX - minX;
			}
			else
			{
				windowFrame.size.width -= self.fileBrowserWidth + 1;
				if(!self.layoutView.fileBrowserOnRight)
					windowFrame.origin.x += self.fileBrowserWidth + 1;
			}

			self.oldWindowFrame = self.window.frame;
			[self.window setFrame:windowFrame display:YES];
			self.newWindowFrame = self.window.frame;
		}
	}
	[[self class] scheduleSessionBackup:self];
}

- (IBAction)toggleFileBrowser:(id)sender    { self.fileBrowserVisible = !self.fileBrowserVisible; }

- (void)updateFileBrowserStatus:(id)sender
{
	NSMutableArray* openURLs     = [NSMutableArray array];
	NSMutableArray* modifiedURLs = [NSMutableArray array];
	for(auto document : _cppDocuments)
	{
		if(document->path() != NULL_STR)
			[openURLs addObject:[NSURL fileURLWithPath:[NSString stringWithCxxString:document->path()]]];
		if(document->path() != NULL_STR && document->is_modified())
			[modifiedURLs addObject:[NSURL fileURLWithPath:[NSString stringWithCxxString:document->path()]]];
	}
	self.fileBrowser.openURLs     = openURLs;
	self.fileBrowser.modifiedURLs = modifiedURLs;
}

- (NSDictionary*)fileBrowserHistory         { return self.fileBrowser.sessionState ?: _fileBrowserHistory; }
- (CGFloat)fileBrowserWidth                 { return self.layoutView.fileBrowserWidth;   }
- (void)setFileBrowserWidth:(CGFloat)aWidth { self.layoutView.fileBrowserWidth = aWidth; }

- (IBAction)newFolder:(id)sender            { if(self.fileBrowser) [NSApp sendAction:_cmd to:self.fileBrowser from:sender]; }
- (IBAction)reload:(id)sender               { if(self.fileBrowser) [NSApp sendAction:_cmd to:self.fileBrowser from:sender]; }
- (IBAction)deselectAll:(id)sender          { if(self.fileBrowser) [NSApp sendAction:_cmd to:self.fileBrowser from:sender]; }

- (IBAction)revealFileInProject:(id)sender  { if(_selectedCppDocument) { self.fileBrowserVisible = YES; [self.fileBrowser selectURL:[NSURL fileURLWithPath:[NSString stringWithCxxString:_selectedCppDocument->path()]] withParentURL:self.projectPath ? [NSURL fileURLWithPath:self.projectPath] : nil]; } }
- (IBAction)goToProjectFolder:(id)sender    { self.fileBrowserVisible = YES; [self.fileBrowser goToURL:[NSURL fileURLWithPath:self.projectPath]]; }

- (IBAction)goBack:(id)sender               { self.fileBrowserVisible = YES; [NSApp sendAction:_cmd to:self.fileBrowser from:sender]; }
- (IBAction)goForward:(id)sender            { self.fileBrowserVisible = YES; [NSApp sendAction:_cmd to:self.fileBrowser from:sender]; }
- (IBAction)goToParentFolder:(id)sender     { self.fileBrowserVisible = YES; [NSApp sendAction:_cmd to:self.fileBrowser from:sender]; }

- (IBAction)goToComputer:(id)sender         { self.fileBrowserVisible = YES; [NSApp sendAction:_cmd to:self.fileBrowser from:sender]; }
- (IBAction)goToHome:(id)sender             { self.fileBrowserVisible = YES; [NSApp sendAction:_cmd to:self.fileBrowser from:sender]; }
- (IBAction)goToDesktop:(id)sender          { self.fileBrowserVisible = YES; [NSApp sendAction:_cmd to:self.fileBrowser from:sender]; }
- (IBAction)goToFavorites:(id)sender        { self.fileBrowserVisible = YES; [NSApp sendAction:_cmd to:self.fileBrowser from:sender]; }
- (IBAction)goToSCMDataSource:(id)sender    { self.fileBrowserVisible = YES; [NSApp sendAction:_cmd to:self.fileBrowser from:sender]; }
- (IBAction)orderFrontGoToFolder:(id)sender { self.fileBrowserVisible = YES; [NSApp sendAction:_cmd to:self.fileBrowser from:sender]; }

// ===============
// = HTML Output =
// ===============

- (NSSize)htmlOutputSize                    { return self.layoutView.htmlOutputSize;  }
- (void)setHtmlOutputSize:(NSSize)aSize     { self.layoutView.htmlOutputSize = aSize; }

- (BOOL)htmlOutputVisible
{
	return self.htmlOutputInWindow ? [self.htmlOutputWindowController.window isVisible] : self.layoutView.htmlOutputView != nil;
}

- (void)setHtmlOutputVisible:(BOOL)makeVisibleFlag
{
	if(self.htmlOutputVisible == makeVisibleFlag)
		return;

	if(makeVisibleFlag)
	{
		if(self.htmlOutputInWindow)
		{
			[self.htmlOutputWindowController showWindow:self];
		}
		else
		{
			if(!self.htmlOutputView || self.htmlOutputView.needsNewWebView)
				self.htmlOutputView = [[OakHTMLOutputView alloc] initWithFrame:NSZeroRect];
			self.layoutView.htmlOutputView = self.htmlOutputView;
		}
	}
	else
	{
		if(self.layoutView.htmlOutputView && [[self.window firstResponder] isKindOfClass:[NSView class]] && [(NSView*)[self.window firstResponder] isDescendantOf:self.layoutView.htmlOutputView])
			[self makeTextViewFirstResponder:self];

		if(self.layoutView.htmlOutputView)
				self.layoutView.htmlOutputView = nil;
		else	[self.htmlOutputWindowController close];
	}
}

- (void)setHtmlOutputInWindow:(BOOL)showInWindowFlag
{
	if(_htmlOutputInWindow == showInWindowFlag)
		return;

	if(_htmlOutputInWindow = showInWindowFlag)
	{
		self.layoutView.htmlOutputView = nil;
		self.htmlOutputView = nil;
	}
	else
	{
		self.htmlOutputWindowController = nil;
	}
}

- (IBAction)toggleHTMLOutput:(id)sender
{
	if(self.htmlOutputVisible && self.htmlOutputInWindow && ![self.htmlOutputWindowController.window isKeyWindow])
			[self.htmlOutputWindowController showWindow:self];
	else	self.htmlOutputVisible = !self.htmlOutputVisible;
}

// =============================
// = Opening Auxiliary Windows =
// =============================

- (void)positionWindow:(NSWindow*)aWindow
{
	if(![aWindow isVisible])
	{
		[aWindow layoutIfNeeded];
		NSRect frame  = [aWindow frame];
		NSRect parent = [self.window convertRectToScreen:[_textView convertRect:[_textView visibleRect] toView:nil]];

		frame.origin.x = NSMinX(parent) + round((NSWidth(parent)  - NSWidth(frame))  * 1 / 4);
		frame.origin.y = NSMinY(parent) + round((NSHeight(parent) - NSHeight(frame)) * 3 / 4);
		[aWindow setFrame:frame display:NO];
	}
}

- (NSUUID*)selectedDocumentUUID
{
	return _selectedCppDocument ? _selectedCppDocument->document().identifier : nil;
}

- (Find*)prepareAndReturnFindPanel
{
	Find* find = [Find sharedInstance];
	find.documentIdentifier = self.selectedDocumentUUID;
	find.projectFolder      = self.projectPath ?: self.untitledSavePath ?: NSHomeDirectory();
	find.projectIdentifier  = self.identifier;

	NSArray* items;
	if(self.fileBrowserVisible)
	{
		items = [[self.fileBrowser.selectedURLs filteredArrayUsingPredicate:[NSPredicate predicateWithFormat:@"isFileURL == YES"]] valueForKey:@"path"];
		if(items.count == 0)
			items = @[ self.fileBrowser.path ];
	}
	find.fileBrowserItems = items.count ? items : nil;

	return find;
}

- (IBAction)orderFrontFindPanel:(id)sender
{
	Find* find              = [Find sharedInstance];
	BOOL didOwnDialog       = [find.projectIdentifier isEqualToString:self.identifier];
	[self prepareAndReturnFindPanel];

	NSInteger mode = [sender respondsToSelector:@selector(tag)] ? [sender tag] : find_tags::in_document;
	if(mode == find_tags::in_document && ![[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsAlwaysFindInDocument] && [self.window isKeyWindow] && self.textView.hasMultiLineSelection)
		mode = find_tags::in_selection;

	switch(mode)
	{
		case find_tags::in_document:  find.searchTarget = FFSearchTargetDocument;  break;
		case find_tags::in_selection: find.searchTarget = FFSearchTargetSelection; break;
		case find_tags::in_folder:    return [find showFolderSelectionPanel:self]; break;

		case find_tags::in_project:
		{
			// Only reset search target if the dialog is not already showing potential search results from “Other…”
			if(!find.isVisible || !didOwnDialog || find.searchTarget == FFSearchTargetDocument || find.searchTarget == FFSearchTargetSelection)
			{
				BOOL fileBrowserHasFocus = [self.window.firstResponder respondsToSelector:@selector(isDescendantOf:)] && [(NSView*)self.window.firstResponder isDescendantOf:self.fileBrowser.view];
				find.searchTarget = fileBrowserHasFocus ? FFSearchTargetFileBrowserItems : FFSearchTargetProject;
			}
		}
		break;
	}
	[find showWindow:self];
}

- (IBAction)orderFrontFindPanelForFileBrowser:(id)sender
{
	Find* find = [self prepareAndReturnFindPanel];
	find.searchTarget = FFSearchTargetFileBrowserItems;
	[find showWindow:self];
}

- (IBAction)orderFrontRunCommandWindow:(id)sender
{
	OakRunCommandWindowController* runCommand = [OakRunCommandWindowController sharedInstance];
	[self positionWindow:runCommand.window];
	[runCommand showWindow:nil];
}

// ==================
// = OakFileChooser =
// ==================

- (IBAction)goToFile:(id)sender
{
	FileChooser* fc = [FileChooser sharedInstance];

	fc.path            = nil; // Disable potential work when updating filterString/currentDocument
	fc.filterString    = @"";
	fc.currentDocument = self.selectedDocumentUUID;
	fc.target          = self;
	fc.action          = @selector(fileChooserDidSelectItems:);
	fc.path            = self.projectPath ?: self.untitledSavePath ?: NSHomeDirectory();

	if(OakPasteboardEntry* entry = [[OakPasteboard pasteboardWithName:NSFindPboard] current])
	{
		std::string str = to_s(entry.string);
		if(regexp::search("\\A.*?(\\.|/).*?:\\d+\\z", str))
		{
			if([entry.string hasPrefix:fc.path])
					fc.filterString = [NSString stringWithCxxString:path::relative_to(str, to_s(fc.path))];
			else	fc.filterString = entry.string;
		}
	}

	[fc showWindowRelativeToFrame:[self.window convertRectToScreen:[self.textView convertRect:[self.textView visibleRect] toView:nil]]];
}

- (void)fileChooserDidSelectItems:(FileChooser*)sender
{
	ASSERT([sender respondsToSelector:@selector(selectedItems)]);
	[self openItems:[sender selectedItems] closingOtherTabs:OakIsAlternateKeyOrMouseEvent()];
}

// ===========
// = Methods =
// ===========

- (NSString*)untitledSavePath
{
	NSString* res = self.projectPath ?: (_selectedCppDocument ? to_ns(path::parent(_selectedCppDocument->path())) : nil);
	if(self.fileBrowserVisible)
	{
		NSArray* selectedURLs = self.fileBrowser.selectedURLs;
		if([selectedURLs count] == 1 && [[selectedURLs lastObject] isFileURL])
		{
			NSString* path = [[selectedURLs lastObject] path];
			res = path::is_directory(to_s(path)) ? path : [path stringByDeletingLastPathComponent];
		}
		else if(NSString* folder = self.fileBrowser.path)
		{
			res = folder;
		}
	}
	return res;
}

- (BOOL)treatAsProjectWindow
{
	return self.projectPath && (self.fileBrowserVisible || _cppDocuments.size() > 1);
}

- (NSPoint)positionForWindowUnderCaret
{
	return [self.textView positionForWindowUnderCaret];
}

- (void)performBundleItem:(bundles::item_ptr const&)anItem
{
	if(anItem->kind() == bundles::kItemTypeTheme)
	{
		[self.documentView setThemeWithUUID:[NSString stringWithCxxString:anItem->uuid()]];
	}
	else
	{
		[self showWindow:self];
		[self makeTextViewFirstResponder:self];
		[self.textView performBundleItem:anItem];
	}
}

- (IBAction)goToRelatedFile:(id)sender
{
	if(!_selectedCppDocument)
		return;

	std::string const documentPath = _selectedCppDocument->path();
	if(documentPath == NULL_STR)
		return (void)NSBeep();

	std::string const documentDir  = path::parent(documentPath);
	std::string const documentName = path::name(documentPath);
	std::string const documentBase = path::strip_extensions(documentName);

	std::set<std::string> candidates = { documentName };
	for(auto document : _cppDocuments)
	{
		if(documentDir == path::parent(document->path()) && documentBase == path::strip_extensions(path::name(document->path())))
			candidates.insert(path::name(document->path()));
	}

	auto map = _selectedCppDocument->document_variables();
	auto const& scm = _documentSCMVariables.empty() ? _projectSCMVariables : _documentSCMVariables;
	map.insert(scm.begin(), scm.end());
	if(self.projectPath)
		map["projectDirectory"] = to_s(self.projectPath);

	settings_t const settings = settings_for_path(_selectedCppDocument->logical_path(), _selectedCppDocument->file_type() + " " + to_s(self.scopeAttributes), path::parent(documentPath), map);
	std::string const customCandidate = settings.get(kSettingsRelatedFilePathKey, NULL_STR);

	if(customCandidate != NULL_STR && customCandidate != documentPath && (std::find_if(_cppDocuments.begin(), _cppDocuments.end(), [&customCandidate](document::document_ptr const& doc){ return customCandidate == doc->path(); }) != _cppDocuments.end() || path::exists(customCandidate)))
		return [self openItems:@[ @{ @"path" : [NSString stringWithCxxString:customCandidate] } ] closingOtherTabs:NO];

	for(auto const& entry : path::entries(documentDir))
	{
		std::string const name = entry->d_name;
		if(entry->d_type == DT_REG && documentBase == path::strip_extensions(name) && path::extensions(name) != "")
		{
			std::string const content = path::content(path::join(documentDir, name));
			if(utf8::is_valid(content.data(), content.data() + content.size()))
				candidates.insert(name);
		}
	}

	path::glob_t const excludeGlob(settings.get(kSettingsExcludeKey, ""));
	path::glob_t const binaryGlob(settings.get(kSettingsBinaryKey, ""));

	std::vector<std::string> v;
	for(auto const& name : candidates)
	{
		if(name == documentName || !binaryGlob.does_match(name) && !excludeGlob.does_match(name))
			v.push_back(name);
	}

	if(v.size() == 1)
	{
		if(customCandidate == NULL_STR || customCandidate == documentPath)
			return (void)NSBeep();
		v.push_back(customCandidate);
	}

	std::vector<std::string>::const_iterator it = std::find(v.begin(), v.end(), documentName);
	ASSERT(it != v.end());

	NSString* path = [NSString stringWithCxxString:path::join(documentDir, v[((it - v.begin()) + 1) % v.size()])];
	[self openItems:@[ @{ @"path" : path } ] closingOtherTabs:NO];
}

// ============================
// = Select Tab Menu Delegate =
// ============================

- (void)updateSelectTabMenu:(NSMenu*)aMenu
{
	if(![self.window isKeyWindow])
	{
		[aMenu addItemWithTitle:@"No Tabs" action:@selector(nop:) keyEquivalent:@""];
		return;
	}

	int i = 0;
	for(auto document : _cppDocuments)
	{
		NSMenuItem* item = [aMenu addItemWithTitle:[NSString stringWithCxxString:document->display_name()] action:@selector(takeSelectedTabIndexFrom:) keyEquivalent:i < 8 ? [NSString stringWithFormat:@"%c", '1' + i] : @""];
		item.tag     = i;
		item.toolTip = [[NSString stringWithCxxString:document->path()] stringByAbbreviatingWithTildeInPath];
		if(aMenu.propertiesToUpdate & NSMenuPropertyItemImage)
			item.image = document->document().icon;
		if(i == _selectedTabIndex)
			[item setState:NSOnState];
		else if(document->is_modified())
			[item setModifiedState:YES];
		++i;
	}

	if(i == 0)
	{
		[aMenu addItemWithTitle:@"No Tabs Open" action:@selector(nop:) keyEquivalent:@""];
	}
	else
	{
		[aMenu addItem:[NSMenuItem separatorItem]];

		NSMenuItem* item = [aMenu addItemWithTitle:@"Last Tab" action:@selector(takeSelectedTabIndexFrom:) keyEquivalent:@"9"];
		item.tag     = _cppDocuments.size()-1;
		item.toolTip = [NSString stringWithCxxString:_cppDocuments.back()->display_name()];
	}
}

// ====================
// = NSMenuValidation =
// ====================

- (BOOL)validateMenuItem:(NSMenuItem*)menuItem
{
	static std::set<SEL> const delegateToFileBrowser = {
		@selector(newFolder:), @selector(goBack:), @selector(goForward:),
		@selector(reload:), @selector(deselectAll:)
	};

	BOOL active = YES;
	if([menuItem action] == @selector(toggleFileBrowser:))
		[menuItem setTitle:self.fileBrowserVisible ? @"Hide File Browser" : @"Show File Browser"];
	else if([menuItem action] == @selector(toggleHTMLOutput:))
	{
		[menuItem setTitle:(!self.htmlOutputVisible || self.htmlOutputInWindow) ? @"Show HTML Output" : @"Hide HTML Output"];
		active = !self.htmlOutputInWindow || self.htmlOutputWindowController;
	}
	else if([menuItem action] == @selector(newDocumentInDirectory:))
		active = self.fileBrowserVisible && [self.fileBrowser directoryForNewItems] != nil;
	else if(delegateToFileBrowser.find([menuItem action]) != delegateToFileBrowser.end())
		active = self.fileBrowserVisible && [self.fileBrowser validateMenuItem:menuItem];
	else if([menuItem action] == @selector(moveDocumentToNewWindow:))
		active = _cppDocuments.size() > 1;
	else if([menuItem action] == @selector(selectNextTab:) || [menuItem action] == @selector(selectPreviousTab:))
		active = _cppDocuments.size() > 1;
	else if([menuItem action] == @selector(revealFileInProject:) || [menuItem action] == @selector(revealFileInProjectByExpandingAncestors:))
	{
		active = _selectedCppDocument && _selectedCppDocument->path() != NULL_STR;
		[menuItem setDynamicTitle:active ? [NSString stringWithFormat:@"Select “%@”", [NSString stringWithCxxString:_selectedCppDocument->display_name()]] : @"Select Document"];
	}
	else if([menuItem action] == @selector(goToProjectFolder:))
		active = self.projectPath != nil;
	else if([menuItem action] == @selector(goToParentFolder:))
		active = [self.window firstResponder] != self.textView;
	else if([menuItem action] == @selector(moveFocus:))
		[menuItem setTitle:self.window.firstResponder == self.textView ? @"Move Focus to File Browser" : @"Move Focus to Document"];
	else if([menuItem action] == @selector(takeProjectPathFrom:))
		[menuItem setState:[self.defaultProjectPath isEqualToString:[menuItem representedObject]] ? NSOnState : NSOffState];
	else if([menuItem action] == @selector(performCloseOtherTabsXYZ:))
		active = _cppDocuments.size() > 1;
	else if([menuItem action] == @selector(performCloseTabsToTheRight:))
		active = _selectedTabIndex + 1 < _cppDocuments.size();

	SEL tabBarActions[] = { @selector(performCloseTab:), @selector(takeNewTabIndexFrom::), @selector(takeTabsToCloseFrom:), @selector(takeTabsToTearOffFrom:), @selector(toggleSticky:) };
	if(oak::contains(std::begin(tabBarActions), std::end(tabBarActions), [menuItem action]))
	{
		if(NSIndexSet* indexSet = [self tryObtainIndexSetFrom:menuItem])
		{
			active = [indexSet count] != 0;
			if(active && [menuItem action] == @selector(toggleSticky:))
				[menuItem setState:[self isDocumentSticky:_cppDocuments[indexSet.firstIndex]] ? NSOnState : NSOffState];
		}
	}

	return active;
}

// =============
// = QuickLook =
// =============

// QLPreviewPanelController

- (BOOL)acceptsPreviewPanelControl:(QLPreviewPanel*)panel
{
	return self.fileBrowserVisible && [self.fileBrowser.selectedURLs count];
}

- (void)beginPreviewPanelControl:(QLPreviewPanel*)panel
{
	[QLPreviewPanel sharedPreviewPanel].delegate   = self;
	[QLPreviewPanel sharedPreviewPanel].dataSource = self;

	self.urlArrayForQuickLook = self.fileBrowser.selectedURLs;
}

- (void)endPreviewPanelControl:(QLPreviewPanel*)panel
{
	self.urlArrayForQuickLook = nil;
}

// QLPreviewPanelDelegate

- (NSRect)previewPanel:(QLPreviewPanel*)panel sourceFrameOnScreenForPreviewItem:(id <QLPreviewItem>)item
{
	return [_fileBrowser iconFrameForURL:item.previewItemURL];
}

- (BOOL)previewPanel:(QLPreviewPanel*)panel handleEvent:(NSEvent*)event
{
	if([event type] == NSKeyDown)
	{
		[self.fileBrowser.outlineView keyDown:event];
		NSArray* newSelection = self.fileBrowser.selectedURLs;
		if(![newSelection isEqualToArray:self.urlArrayForQuickLook])
		{
			self.urlArrayForQuickLook = newSelection;
			[panel reloadData];
		}
		return YES;
	}
	return NO;
}

// QLPreviewPanelDataSource

- (NSInteger)numberOfPreviewItemsInPreviewPanel:(QLPreviewPanel*)panel
{
	return self.urlArrayForQuickLook.count;
}

- (id <QLPreviewItem>)previewPanel:(QLPreviewPanel*)panel previewItemAtIndex:(NSInteger)index
{
	return [[QuickLookNSURLWrapper alloc] initWithURL:self.urlArrayForQuickLook[index]];
}

// ======================
// = Session Management =
// ======================

+ (void)initialize
{
	static dispatch_once_t onceToken = 0;
	dispatch_once(&onceToken, ^{
		for(NSString* notification in @[ NSWindowDidBecomeKeyNotification, NSWindowDidDeminiaturizeNotification, NSWindowDidExposeNotification, NSWindowDidMiniaturizeNotification, NSWindowDidMoveNotification, NSWindowDidResizeNotification, NSWindowWillCloseNotification ])
			[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(scheduleSessionBackup:) name:notification object:nil];
	});
}

+ (void)backupSessionFiredTimer:(NSTimer*)aTimer
{
	[self saveSessionIncludingUntitledDocuments:YES];
}

+ (void)scheduleSessionBackup:(id)sender
{
	static NSTimer* saveTimer;
	[saveTimer invalidate];
	saveTimer = [NSTimer scheduledTimerWithTimeInterval:0.5 target:self selector:@selector(backupSessionFiredTimer:) userInfo:nil repeats:NO];
}

+ (NSString*)sessionPath
{
	static NSString* const res = [NSString stringWithCxxString:path::join(oak::application_t::support("Session"), "Info.plist")];
	return res;
}

static NSUInteger DisableSessionSavingCount = 0;

+ (void)disableSessionSave { ++DisableSessionSavingCount; }
+ (void)enableSessionSave  { --DisableSessionSavingCount; }

+ (BOOL)restoreSession
{
	BOOL res = NO;
	++DisableSessionSavingCount;

	NSWindow* keyWindow;

	NSDictionary* session = [NSDictionary dictionaryWithContentsOfFile:[self sessionPath]];
	for(NSDictionary* project in session[@"projects"])
	{
		DocumentWindowController* controller = [DocumentWindowController new];
		[controller setupControllerForProject:project skipMissingFiles:NO];
		if(controller.cppDocuments.empty())
			continue;

		if(NSString* windowFrame = project[@"windowFrame"])
		{
			if([windowFrame hasPrefix:@"{"]) // Legacy NSRect
					[controller.window setFrame:NSRectFromString(windowFrame) display:NO];
			else	[controller.window setFrameFromString:windowFrame];
		}

		if([project[@"miniaturized"] boolValue])
		{
			[controller.window miniaturize:nil];
		}
		else
		{
			if([project[@"fullScreen"] boolValue])
				[controller.window toggleFullScreen:self];
			else if([project[@"zoomed"] boolValue])
				[controller.window zoom:self];

			[controller.window orderFront:self];
			keyWindow = controller.window;
		}

		res = YES;
	}

	[keyWindow makeKeyWindow];

	--DisableSessionSavingCount;
	return res;
}

- (void)setupControllerForProject:(NSDictionary*)project skipMissingFiles:(BOOL)skipMissing
{
	if(NSString* fileBrowserWidth = project[@"fileBrowserWidth"])
		self.fileBrowserWidth = [fileBrowserWidth floatValue];
	if(NSString* htmlOutputSize = project[@"htmlOutputSize"])
		self.htmlOutputSize = NSSizeFromString(htmlOutputSize);

	self.defaultProjectPath = project[@"projectPath"];
	self.projectPath        = project[@"projectPath"];
	self.fileBrowserHistory = project[@"fileBrowserState"];
	self.fileBrowserVisible = [project[@"fileBrowserVisible"] boolValue];

	std::vector<document::document_ptr> documents;
	NSInteger selectedTabIndex = 0;

	for(NSDictionary* info in project[@"documents"])
	{
		document::document_ptr doc;
		NSString* identifier = info[@"identifier"];
		if(!identifier || !(doc = document::find(to_s(identifier))))
		{
			NSString* path = info[@"path"];
			if(path && skipMissing && access([path fileSystemRepresentation], F_OK) != 0)
				continue;

			doc = document::create(to_s(path));
			if(NSString* fileType = info[@"fileType"])
				doc->set_file_type(to_s(fileType));
			if(NSString* displayName = info[@"displayName"])
				doc->set_custom_name(to_s(displayName));
			if([info[@"sticky"] boolValue])
				[self setDocument:doc sticky:YES];
		}

		if(doc->path() == NULL_STR)
			doc->show(); // Add to LRU-list

		doc->set_recent_tracking(false);
		documents.push_back(doc);

		if([info[@"selected"] boolValue])
			selectedTabIndex = documents.size() - 1;
	}

	if(documents.empty())
		documents.push_back(document::create());

	self.cppDocuments     = documents;
	self.selectedTabIndex = selectedTabIndex;

	[self openAndSelectDocument:documents[selectedTabIndex]];
}

- (NSDictionary*)sessionInfoIncludingUntitledDocuments:(BOOL)includeUntitled
{
	NSMutableDictionary* res = [NSMutableDictionary dictionary];

	if(NSString* projectPath = self.defaultProjectPath)
		res[@"projectPath"] = projectPath;
	if(NSDictionary* history = self.fileBrowserHistory)
		res[@"fileBrowserState"] = history;

	if(([self.window styleMask] & NSFullScreenWindowMask) == NSFullScreenWindowMask)
		res[@"fullScreen"] = @YES;
	else if(self.window.isZoomed)
		res[@"zoomed"] = @YES;
	else
		res[@"windowFrame"] = [self.window stringWithSavedFrame];

	res[@"miniaturized"]       = @([self.window isMiniaturized]);
	res[@"htmlOutputSize"]     = NSStringFromSize(self.htmlOutputSize);
	res[@"fileBrowserVisible"] = @(self.fileBrowserVisible);
	res[@"fileBrowserWidth"]   = @(self.fileBrowserWidth);

	NSMutableArray* docs = [NSMutableArray array];
	for(auto document : self.cppDocuments)
	{
		if(!includeUntitled && (document->path() == NULL_STR || !path::exists(document->path())))
			continue;

		NSMutableDictionary* doc = [NSMutableDictionary dictionary];
		if(document->is_modified() || document->path() == NULL_STR)
		{
			doc[@"identifier"] = [NSString stringWithCxxString:document->identifier()];
			if(document->is_loaded())
				document->backup();
		}
		if(document->path() != NULL_STR)
			doc[@"path"] = [NSString stringWithCxxString:document->path()];
		if(document->file_type() != NULL_STR) // TODO Only necessary when document.isBufferEmpty
			doc[@"fileType"] = [NSString stringWithCxxString:document->file_type()];
		if(document->display_name() != NULL_STR)
			doc[@"displayName"] = [NSString stringWithCxxString:document->display_name()];
		if(document == self.selectedCppDocument)
			doc[@"selected"] = @YES;
		if([self isDocumentSticky:document])
			doc[@"sticky"] = @YES;
		[docs addObject:doc];
	}
	res[@"documents"] = docs;
	res[@"lastRecentlyUsed"] = [NSDate date];
	return res;
}

+ (BOOL)saveSessionIncludingUntitledDocuments:(BOOL)includeUntitled
{
	if(DisableSessionSavingCount)
		return NO;

	NSArray* controllers = SortedControllers();
	if(controllers.count == 1)
	{
		DocumentWindowController* controller = controllers.firstObject;
		if(!controller.projectPath && !controller.fileBrowserVisible && controller.cppDocuments.size() == 1 && is_disposable(controller.selectedCppDocument))
			controllers = nil;
	}

	NSMutableArray* projects = [NSMutableArray array];
	for(DocumentWindowController* controller in [controllers reverseObjectEnumerator])
		[projects addObject:[controller sessionInfoIncludingUntitledDocuments:includeUntitled]];

	NSDictionary* session = @{ @"projects" : projects };
	return [session writeToFile:[self sessionPath] atomically:YES];
}

// ==========
// = Legacy =
// ==========

- (std::map<std::string, std::string>)variables
{
	std::map<std::string, std::string> res;
	if(self.fileBrowser)
		res = [self.fileBrowser variables];

	auto const& scmVars = _documentSCMVariables.empty() ? _projectSCMVariables : _documentSCMVariables;

	if(!scmVars.empty())
	{
		auto scmName = scmVars.find("TM_SCM_NAME");
		if(scmName != scmVars.end())
			res["TM_SCM_NAME"] = scmName->second;
	}
	else
	{
		for(auto const& attr : _externalScopeAttributes)
		{
			if(regexp::match_t const& m = regexp::search("^attr.scm.(?'TM_SCM_NAME'\\w+)$", attr))
			{
				res.insert(m.captures().begin(), m.captures().end());
				break;
			}
		}
	}

	if(NSString* projectDir = self.projectPath)
	{
		res["TM_PROJECT_DIRECTORY"] = [projectDir fileSystemRepresentation];
		res["TM_PROJECT_UUID"]      = to_s(self.identifier);
	}

	return res;
}

+ (instancetype)controllerForDocument:(OakDocument*)aDocument
{
	if(!aDocument)
		return nil;

	for(DocumentWindowController* delegate in SortedControllers())
	{
		if(delegate.fileBrowserVisible && aDocument.path && [aDocument.path hasPrefix:delegate.projectPath])
			return delegate;

		for(auto document : delegate.cppDocuments)
		{
			if([aDocument isEqual:document->document()])
				return delegate;
		}
	}
	return nil;
}

- (void)bringToFront
{
	[self showWindow:nil];
	if(![NSApp isActive])
	{
		__weak __block id observerId = [[NSNotificationCenter defaultCenter] addObserverForName:NSApplicationDidBecomeActiveNotification object:NSApp queue:nil usingBlock:^(NSNotification*){
			// If our window is not on the active desktop but another one is, the system gives focus to the wrong window.
			[self showWindow:nil];
			[[NSNotificationCenter defaultCenter] removeObserver:observerId];
		}];
		[NSApp activateIgnoringOtherApps:YES];
	}
}
@end

@implementation OakDocumentController (OakDocumentWindowControllerCategory)
- (DocumentWindowController*)findOrCreateController:(std::vector<document::document_ptr> const&)documents project:(NSUUID*)projectUUID
{
	ASSERT(!documents.empty());

	// =========================================
	// = Return requested window, if it exists =
	// =========================================

	if(projectUUID)
	{
		if(DocumentWindowController* res = AllControllers()[projectUUID.UUIDString])
			return res;

		if([projectUUID.UUIDString isEqual:@"00000000-0000-0000-0000-000000000000"])
			return [DocumentWindowController new];
	}

	// =========================================
	// = Find window with one of our documents =
	// =========================================

	std::set<oak::uuid_t> uuids;
	std::transform(documents.begin(), documents.end(), inserter(uuids, uuids.end()), [](document::document_ptr const& doc){ return doc->identifier(); });

	for(DocumentWindowController* candidate in SortedControllers())
	{
		for(auto document : candidate.cppDocuments)
		{
			if(uuids.find(document->identifier()) != uuids.end())
				return candidate;
		}
	}

	// ================================================================
	// = Find window with project folder closest to document’s parent =
	// ================================================================

	std::vector<document::document_ptr> documentsWithPath;
	std::copy_if(documents.begin(), documents.end(), back_inserter(documentsWithPath), [](document::document_ptr const& doc){ return doc->path() != NULL_STR; });

	std::set<std::string> parents;
	std::transform(documentsWithPath.begin(), documentsWithPath.end(), inserter(parents, parents.end()), [](document::document_ptr const& doc){ return path::parent(doc->path()); });

	std::map<size_t, DocumentWindowController*> candidates;
	for(DocumentWindowController* candidate in SortedControllers())
	{
		if(candidate.projectPath)
		{
			std::string const projectPath = to_s(candidate.projectPath);
			for(auto const& parent : parents)
			{
				if(path::is_child(parent, projectPath))
					candidates.emplace(parent.size() - projectPath.size(), candidate);
			}
		}
	}

	if(!candidates.empty())
		return candidates.begin()->second;

	// ==============================================
	// = Use frontmost window if a “scratch” window =
	// ==============================================

	if(DocumentWindowController* candidate = [SortedControllers() firstObject])
	{
		if(!candidate.fileBrowserVisible && candidate.cppDocuments.size() == 1 && is_disposable(candidate.selectedCppDocument))
			return candidate;
	}

	// ===================================
	// = Give up and create a new window =
	// ===================================

	DocumentWindowController* res = [DocumentWindowController new];

	if(!parents.empty()) // setup project folder for new window
	{
		std::vector<std::string> rankedParents(parents.begin(), parents.end());
		std::sort(rankedParents.begin(), rankedParents.end(), [](std::string const& lhs, std::string const& rhs){ return lhs.size() < rhs.size(); });
		res.defaultProjectPath = [NSString stringWithCxxString:rankedParents.front()];
	}

	return res;
}

- (DocumentWindowController*)controllerWithDocuments:(std::vector<document::document_ptr> const&)documents project:(NSUUID*)projectUUID
{
	DocumentWindowController* controller = [self findOrCreateController:documents project:projectUUID];
	auto documentToSelect = controller.cppDocuments.size() <= [controller disposableDocument].size() ? documents.front() : documents.back();
	[controller insertDocuments:documents atIndex:controller.selectedTabIndex + 1 selecting:documentToSelect andClosing:[controller disposableDocument]];
	return controller;
}

- (void)showDocument:(OakDocument*)aDocument andSelect:(text::range_t const&)range inProject:(NSUUID*)identifier bringToFront:(BOOL)bringToFront
{
	auto document = std::make_shared<document::document_t>(aDocument);
	if(range != text::range_t::undefined)
		document->set_selection(range);

	DocumentWindowController* controller = [self controllerWithDocuments:{ document } project:identifier];
	if(bringToFront)
		[controller bringToFront];
	else if(![controller.window isVisible])
		[controller.window orderWindow:NSWindowBelow relativeTo:[([NSApp keyWindow] ?: [NSApp mainWindow]) windowNumber]];
	[controller openAndSelectDocument:document];
}

- (void)showDocuments:(NSArray<OakDocument*>*)someDocument
{
	if(someDocument.count == 0)
		return;

	std::vector<document::document_ptr> documents;
	for(OakDocument* document in someDocument)
		documents.push_back(std::make_shared<document::document_t>(document));

	DocumentWindowController* controller = [self controllerWithDocuments:documents project:nil];
	[controller bringToFront];
	[controller openAndSelectDocument:controller.cppDocuments[controller.selectedTabIndex]];
}

- (void)showFileBrowserAtPath:(NSString*)aPath
{
	NSString* const folder = to_ns(path::resolve(to_s(aPath)));
	[[NSDocumentController sharedDocumentController] noteNewRecentDocumentURL:[NSURL fileURLWithPath:folder]];

	for(DocumentWindowController* candidate in SortedControllers())
	{
		if([folder isEqualToString:candidate.projectPath ?: candidate.defaultProjectPath])
			return [candidate bringToFront];
	}

	DocumentWindowController* controller = nil;
	for(DocumentWindowController* candidate in SortedControllers())
	{
		if(!candidate.fileBrowserVisible && candidate.cppDocuments.size() == 1 && is_disposable(candidate.selectedCppDocument))
		{
			controller = candidate;
			break;
		}
	}

	if(!controller)
		controller = [DocumentWindowController new];
	else if(controller.selectedCppDocument)
		[controller selectedCppDocument]->set_custom_name("not untitled"); // release potential untitled token used

	NSDictionary* project;
	if(![[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsDisableFolderStateRestore])
		project = [[DocumentWindowController sharedProjectStateDB] valueForKey:folder];

	if(project && [project[@"documents"] count])
	{
		[controller setupControllerForProject:project skipMissingFiles:YES];
	}
	else
	{
		controller.defaultProjectPath = folder;
		controller.fileBrowserVisible = YES;
		controller.cppDocuments       = { document::create() };
		controller.fileBrowser.url    = [NSURL fileURLWithPath:folder];

		[controller openAndSelectDocument:controller.cppDocuments[controller.selectedTabIndex]];
	}
	[controller bringToFront];
}
@end
