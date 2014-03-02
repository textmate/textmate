#import "DocumentController.h"
#import "ProjectLayoutView.h"
#import "DocumentOpenHelper.h"
#import "DocumentSaveHelper.h"
#import "DocumentCommand.h" // show_command_error
#import "OakRunCommandWindowController.h"
#import <OakAppKit/NSAlert Additions.h>
#import <OakAppKit/NSMenuItem Additions.h>
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/OakFileIconImage.h>
#import <OakAppKit/OakFileManager.h>
#import <OakAppKit/OakPasteboard.h>
#import <OakAppKit/OakSavePanel.h>
#import <OakAppKit/OakTabBarView.h>
#import <OakAppKit/OakWindowFrameHelper.h>
#import <OakFoundation/NSArray Additions.h>
#import <OakFoundation/NSString Additions.h>
#import <Preferences/Keys.h>
#import <OakTextView/OakDocumentView.h>
#import <OakFileBrowser/OakFileBrowser.h>
#import <HTMLOutputWindow/HTMLOutputWindow.h>
#import <OakFilterList/FileChooser.h>
#import <OakSystem/application.h>
#import <Find/Find.h>
#import <crash/info.h>
#import <file/path_info.h>
#import <io/entries.h>
#import <scm/scm.h>
#import <text/tokenize.h>
#import <text/utf8.h>
#import <ns/ns.h>
#import <oak/compat.h>
#import <kvdb/kvdb.h>

namespace find_tags { enum { in_document = 1, in_selection, in_project, in_folder }; } // From AppController.h

static NSString* const kUserDefaultsFindInSelectionByDefault = @"findInSelectionByDefault";
static NSString* const kUserDefaultsDisableFolderStateRestore = @"disableFolderStateRestore";
static NSString* const OakDocumentPboardType = @"OakDocumentPboardType"; // drag’n’drop of tabs
static BOOL IsInShouldTerminateEventLoop = NO;

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

@interface DocumentController () <NSWindowDelegate, OakTabBarViewDelegate, OakTabBarViewDataSource, OakTextViewDelegate, OakFileBrowserDelegate, OakWindowFrameHelperDelegate, QLPreviewPanelDelegate, QLPreviewPanelDataSource>
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

	static NSMutableDictionary* AllControllers ()
	{
		static NSMutableDictionary* res = [NSMutableDictionary new];
		return res;
	}

	static NSArray* SortedControllers ()
	{
		NSMutableArray* res = [NSMutableArray array];
		for(NSNumber* flag in @[ @NO, @YES ])
		{
			for(NSWindow* window in [NSApp orderedWindows])
			{
				if([window isMiniaturized] == [flag boolValue] && [window.delegate respondsToSelector:@selector(identifier)])
				{
					DocumentController* delegate = (DocumentController*)window.delegate;
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

	struct tracking_info_t : document::document_t::callback_t
	{
		tracking_info_t (DocumentController* self, document::document_ptr const& document) : _self(self), _document(document) { }
		~tracking_info_t () { ASSERT_EQ(_open_count, 0); }

		void track ()
		{
			if(++_open_count == 1)
			{
				_document->add_callback(this);
				// TODO Add kqueue watching of documents
			}

			if(!_did_open && _document->is_open())
			{
				_document->open();
				_did_open = true;
			}
		}

		bool untrack ()
		{
			if(_open_count == 1)
				_document->remove_callback(this);

			if(--_open_count == 0 && _did_open)
			{
				_document->close();
				_did_open = false;
			}
			return _open_count == 0;
		}

		void handle_document_event (document::document_ptr document, event_t event)
		{
			if(document && _self.selectedDocument && *document == *_self.selectedDocument)
			{
				switch(event)
				{
					case did_change_path:
						[_self setDocumentPath:[NSString stringWithCxxString:document->path()]];
						[_self setDocumentDisplayName:[NSString stringWithCxxString:document->display_name()]];
					break;

					case did_change_on_disk_status:  [_self setDocumentIsOnDisk:document->is_on_disk()];                      break;
					case did_change_modified_status: [_self setDocumentIsModified:document->is_modified()];                   break;
				}
			}

			switch(event)
			{
				case did_change_modified_status:
				case did_change_path:
					[_self updateFileBrowserStatus:nil];
					[_self.tabBarView reloadData];
					[[_self class] scheduleSessionBackup:nil];
				break;
			}
		}

	private:
		__weak DocumentController* _self;
		document::document_ptr _document;
		size_t _open_count = 0;
		bool _did_open = false;
	};

	static bool is_disposable (document::document_ptr const& doc)
	{
		return doc && !doc->is_modified() && !doc->is_on_disk() && doc->path() == NULL_STR && doc->buffer().empty();
	}

	static size_t merge_documents_splitting_at (std::vector<document::document_ptr> const& oldDocuments, std::vector<document::document_ptr> const& newDocuments, size_t splitAt, std::vector<document::document_ptr>& out, bool disableTabReordering = false)
	{
		if(newDocuments.empty())
		{
			std::copy(oldDocuments.begin(), oldDocuments.end(), back_inserter(out));
			return std::min(splitAt, out.size());
		}

		splitAt = std::min(splitAt, oldDocuments.size());
		if(disableTabReordering)
		{
			auto iter = std::find(oldDocuments.begin(), oldDocuments.end(), newDocuments.front());
			if(iter != oldDocuments.end())
				splitAt = iter - oldDocuments.begin();
		}

		std::set<oak::uuid_t> uuids;
		std::transform(newDocuments.begin(), newDocuments.end(), inserter(uuids, uuids.end()), [](document::document_ptr const& doc){ return doc->identifier(); });

		std::copy_if(oldDocuments.begin(), oldDocuments.begin() + splitAt, back_inserter(out), [&uuids](document::document_ptr const& doc){ return uuids.find(doc->identifier()) == uuids.end(); });
		std::copy(newDocuments.begin(), newDocuments.end(), back_inserter(out));	
		std::copy_if(oldDocuments.begin() + splitAt, oldDocuments.end(), back_inserter(out), [&uuids](document::document_ptr const& doc){ return uuids.find(doc->identifier()) == uuids.end(); });

		auto iter = std::find(out.begin(), out.end(), newDocuments.front());
		return iter - out.begin();
	}

	static std::vector<document::document_ptr> make_vector (document::document_ptr const& document)
	{
		return std::vector<document::document_ptr>(1, document);
	}

	static document::document_ptr create_untitled_document_in_folder (std::string const& suggestedFolder)
	{
		auto doc = document::from_content("", settings_for_path(NULL_STR, file::path_attributes(NULL_STR), suggestedFolder).get(kSettingsFileTypeKey, "text.plain"));
		auto const settings = settings_for_path(NULL_STR, doc->file_type(), suggestedFolder);
		doc->set_indent(text::indent_t(std::max(1, settings.get(kSettingsTabSizeKey, 4)), SIZE_T_MAX, settings.get(kSettingsSoftTabsKey, false)));
		return doc;
	}
}

@implementation DocumentController
{
	OBJC_WATCH_LEAKS(DocumentController);

	std::vector<document::document_ptr>    _documents;
	std::map<oak::uuid_t, tracking_info_t> _trackedDocuments;
	document::document_ptr                 _selectedDocument;
	command::runner_ptr                    _runner;

	scm::info_ptr                          _projectSCMInfo;
	std::map<std::string, std::string>     _projectSCMVariables;
	std::vector<std::string>               _projectScopeAttributes;
	std::vector<std::string>               _externalScopeAttributes;

	scm::info_ptr                          _documentSCMInfo;
	std::map<std::string, std::string>     _documentSCMVariables;
	std::vector<std::string>               _documentScopeAttributes;
}

+ (KVDB*)sharedProjectStateDB
{
	NSString* appSupport = [[NSSearchPathForDirectoriesInDomains(NSApplicationSupportDirectory, NSUserDomainMask, YES) objectAtIndex:0] stringByAppendingPathComponent:@"TextMate"];
	return [KVDB sharedDBUsingFile:@"RecentProjects.db" inDirectory:appSupport];
}

- (id)init
{
	if((self = [super init]))
	{
		self.identifier  = [NSString stringWithCxxString:oak::uuid_t().generate()];

		self.tabBarView = [[OakTabBarView alloc] initWithFrame:NSZeroRect];
		self.tabBarView.dataSource = self;
		self.tabBarView.delegate   = self;

		self.documentView = [[OakDocumentView alloc] init];
		self.textView = self.documentView.textView;
		self.textView.delegate = self;

		self.layoutView = [[ProjectLayoutView alloc] initWithFrame:NSZeroRect];
		self.layoutView.tabBarView   = self.tabBarView;
		self.layoutView.documentView = self.documentView;

		self.window = [[NSWindow alloc] initWithContentRect:NSZeroRect styleMask:(NSTitledWindowMask|NSClosableWindowMask|NSResizableWindowMask|NSMiniaturizableWindowMask|NSTexturedBackgroundWindowMask) backing:NSBackingStoreBuffered defer:NO];
		self.window.autorecalculatesKeyViewLoop = YES;
		self.window.collectionBehavior          = NSWindowCollectionBehaviorFullScreenPrimary;
		self.window.delegate                    = self;
		self.window.releasedWhenClosed          = NO;
		[self.window setContentBorderThickness:0 forEdge:NSMaxYEdge]; // top border
		[self.window setContentBorderThickness:0 forEdge:NSMinYEdge]; // bottom border
		[self.window setAutorecalculatesContentBorderThickness:NO forEdge:NSMaxYEdge];
		[self.window setAutorecalculatesContentBorderThickness:NO forEdge:NSMinYEdge];

		[self.layoutView setTranslatesAutoresizingMaskIntoConstraints:NO];
		[self.window.contentView addSubview:self.layoutView];
		[self.window.contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[view]|" options:0 metrics:nil views:@{ @"view" : self.layoutView }]];
		[self.window.contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[view]|" options:0 metrics:nil views:@{ @"view" : self.layoutView }]];

		[OakWindowFrameHelper windowFrameHelperWithWindow:self.window];

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

	self.tabBarView.dataSource  = nil;
	self.tabBarView.delegate    = nil;
	self.textView.delegate      = nil;
}

- (void)windowWillClose:(NSNotification*)aNotification
{
	self.documents          = std::vector<document::document_ptr>();
	self.selectedDocument   = document::document_ptr();
	self.window.delegate    = nil;
	self.fileBrowserVisible = NO; // Make window frame small as we no longer respond to savableWindowFrame
	self.identifier         = nil; // This removes us from AllControllers and causes a release
}

- (void)showWindow:(id)sender
{
	if(_documents.empty())
	{
		document::document_ptr defaultDocument = create_untitled_document_in_folder(to_s(self.untitledSavePath));
		self.documents = make_vector(defaultDocument);
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
	self.htmlOutputInWindow = [[[NSUserDefaults standardUserDefaults] objectForKey:kUserDefaultsHTMLOutputPlacementKey] isEqualToString:@"window"];
	self.disableFileBrowserWindowResize = [[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsDisableFileBrowserWindowResizeKey];
	self.autoRevealFile = [[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsAutoRevealFileKey];

	if(self.layoutView.fileBrowserOnRight != [[[NSUserDefaults standardUserDefaults] objectForKey:kUserDefaultsFileBrowserPlacementKey] isEqualToString:@"right"])
	{
		self.oldWindowFrame = self.newWindowFrame = NSZeroRect;
		self.layoutView.fileBrowserOnRight = !self.layoutView.fileBrowserOnRight;
	}
}

- (void)applicationDidBecomeActiveNotification:(NSNotification*)aNotification
{
	if(!_documents.empty())
		[self.textView performSelector:@selector(applicationDidBecomeActiveNotification:) withObject:aNotification];
}

- (void)applicationDidResignActiveNotification:(NSNotification*)aNotification
{
	if(!_documents.empty())
		[self.textView performSelector:@selector(applicationDidResignActiveNotification:) withObject:aNotification];
}

// =================
// = Close Methods =
// =================

- (void)showCloseWarningUIForDocuments:(std::vector<document::document_ptr> const&)someDocuments completionHandler:(void(^)(BOOL canClose))callback
{
	if(someDocuments.empty())
		return callback(YES);

	[[self.window attachedSheet] orderOut:self];
	NSAlert* alert = [[NSAlert alloc] init];
	[alert setAlertStyle:NSWarningAlertStyle];
	[alert addButtons:@"Save", @"Cancel", @"Don’t Save", nil];
	if(someDocuments.size() == 1)
	{
		document::document_ptr document = someDocuments.front();
		[alert setMessageText:[NSString stringWithCxxString:text::format("Do you want to save the changes you made in the document “%s”?", document->display_name().c_str())]];
		[alert setInformativeText:@"Your changes will be lost if you don’t save them."];
	}
	else
	{
		std::string body = "";
		for(auto document : someDocuments)
			body += text::format("• “%s”\n", document->display_name().c_str());
		[alert setMessageText:@"Do you want to save documents with changes?"];
		[alert setInformativeText:[NSString stringWithCxxString:body]];
	}

	bool windowModal = true;
	if(someDocuments.size() == 1)
	{
		NSUInteger index = 0;
		for(auto document : _documents)
		{
			if(*document == *someDocuments.front())
			{
				self.selectedTabIndex = index;
				[self openAndSelectDocument:document];
				break;
			}
			++index;
		}
	}
	else
	{
		std::set<oak::uuid_t> uuids;
		std::transform(_documents.begin(), _documents.end(), inserter(uuids, uuids.end()), [](document::document_ptr const& doc){ return doc->identifier(); });

		for(auto document : someDocuments)
		{
			if(uuids.find(document->identifier()) == uuids.end())
				windowModal = false;
		}
	}

	std::vector<document::document_ptr> documentsToSave(someDocuments);
	auto block = ^(NSInteger returnCode)
	{
		switch(returnCode)
		{
			case NSAlertFirstButtonReturn: /* "Save" */
			{
				if(IsInShouldTerminateEventLoop)
				{
					IsInShouldTerminateEventLoop = NO;
					[NSApp replyToApplicationShouldTerminate:NO];
				}

				[DocumentSaveHelper trySaveDocuments:documentsToSave forWindow:self.window defaultDirectory:self.untitledSavePath completionHandler:^(BOOL success){
					callback(success);
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
	};

	if(windowModal)
			OakShowAlertForWindow(alert, self.window, block);
	else	block([alert runModal]);
}

- (void)closeTabsAtIndexes:(NSIndexSet*)anIndexSet askToSaveChanges:(BOOL)askToSaveFlag createDocumentIfEmpty:(BOOL)createIfEmptyFlag
{
	if([anIndexSet count] == 0 || _documents.empty())
		return;

	crash_reporter_info_t crashInfo(text::format("close %lu documents with %zu open and index of selected being %zu.", [anIndexSet count], _documents.size(), _selectedTabIndex));
	crashInfo << to_s([anIndexSet description]);

	std::vector<document::document_ptr> documentsToClose;
	for(NSUInteger index = [anIndexSet firstIndex]; index != NSNotFound; index = [anIndexSet indexGreaterThanIndex:index])
		documentsToClose.push_back(_documents[index]);

	if(askToSaveFlag)
	{
		std::vector<document::document_ptr> documents;
		std::copy_if(documentsToClose.begin(), documentsToClose.end(), back_inserter(documents), [](document::document_ptr const& doc){ return doc->is_modified(); });

		if(!documents.empty())
		{
			[self showCloseWarningUIForDocuments:documents completionHandler:^(BOOL canClose){
				if(canClose)
				{
					[self closeTabsAtIndexes:anIndexSet askToSaveChanges:NO createDocumentIfEmpty:createIfEmptyFlag];
				}
				else
				{
					NSMutableIndexSet* newIndexes = [anIndexSet mutableCopy];
					for(NSUInteger index = [anIndexSet firstIndex]; index != NSNotFound; index = [anIndexSet indexGreaterThanIndex:index])
					{
						if(_documents[index]->is_modified())
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
	oak::uuid_t const selectedUUID = _documents[_selectedTabIndex]->identifier();
	for(auto document : _documents)
	{
		oak::uuid_t const& uuid = document->identifier();
		if(uuids.find(uuid) == uuids.end())
			newDocuments.push_back(document);
		if(selectedUUID == uuid)
			newSelectedTabIndex = newDocuments.empty() ? 0 : newDocuments.size() - 1;
	}

	crashInfo << text::format("keep %zu documents open, new selected index at %zu, create untitled %s", newDocuments.size(), newSelectedTabIndex, BSTR((createIfEmptyFlag && newDocuments.empty())));

	if(createIfEmptyFlag && newDocuments.empty())
		newDocuments.push_back(create_untitled_document_in_folder(to_s(self.untitledSavePath)));

	self.selectedTabIndex = newSelectedTabIndex;
	self.documents        = newDocuments;

	if(!newDocuments.empty() && newDocuments[newSelectedTabIndex]->identifier() != selectedUUID)
		[self openAndSelectDocument:newDocuments[newSelectedTabIndex]];
}

- (IBAction)performCloseTab:(id)sender
{
	if(_documents.empty() || _documents.size() == 1 && (is_disposable(_selectedDocument) || !self.fileBrowserVisible))
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
	NSMutableIndexSet* allTabs = [NSMutableIndexSet indexSetWithIndexesInRange:NSMakeRange(0, _documents.size())];
	for(size_t i = 0; i < _documents.size(); ++i)
	{
		if(_documents[i]->is_modified() && _documents[i]->path() == NULL_STR || _documents[i]->sticky())
			[allTabs removeIndex:i];
	}
	[self closeTabsAtIndexes:allTabs askToSaveChanges:YES createDocumentIfEmpty:YES];
}

- (IBAction)performCloseOtherTabs:(id)sender
{
	NSUInteger tabIndex = [sender isKindOfClass:[OakTabBarView class]] ? [sender tag] : _selectedTabIndex;

	NSMutableIndexSet* otherTabs = [NSMutableIndexSet indexSet];
	for(size_t i = 0; i < _documents.size(); ++i)
	{
		if(i != tabIndex && (!_documents[i]->is_modified() || _documents[i]->path() != NULL_STR) && !_documents[i]->sticky())
			[otherTabs addIndex:i];
	}
	[self closeTabsAtIndexes:otherTabs askToSaveChanges:YES createDocumentIfEmpty:YES];
}

- (IBAction)performCloseTabsToTheRight:(id)sender
{
	NSUInteger from = _selectedTabIndex + 1, to = _documents.size();
	if(from < to)
		[self closeTabsAtIndexes:[NSIndexSet indexSetWithIndexesInRange:NSMakeRange(from, to - from)] askToSaveChanges:YES createDocumentIfEmpty:YES];
}

- (BOOL)windowShouldClose:(id)sender
{
	[self.htmlOutputView stopLoading];

	std::vector<document::document_ptr> documents;
	std::copy_if(_documents.begin(), _documents.end(), back_inserter(documents), [](document::document_ptr const& doc){ return doc->is_modified(); });

	if(documents.empty())
	{
		if(self.treatAsProjectWindow)
			[[DocumentController sharedProjectStateDB] setValue:[self sessionInfoIncludingUntitledDocuments:NO] forKey:self.projectPath];
		return YES;
	}

	[self showCloseWarningUIForDocuments:documents completionHandler:^(BOOL canClose){
		if(canClose)
		{
			if(self.treatAsProjectWindow)
				[[DocumentController sharedProjectStateDB] setValue:[self sessionInfoIncludingUntitledDocuments:NO] forKey:self.projectPath];
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
	for(size_t i = 0; i < _documents.size(); ++i)
	{
		document::document_ptr doc = _documents[i];
		if(!doc->is_modified() && path::is_child(doc->path(), to_s(path)))
			[indexSet addIndex:i];
	}

	id oldFirstResponder = self.window.firstResponder;
	[self closeTabsAtIndexes:indexSet askToSaveChanges:NO createDocumentIfEmpty:YES];
	if(oldFirstResponder && oldFirstResponder != self.window.firstResponder)
		[self.window makeFirstResponder:oldFirstResponder];
}

+ (NSApplicationTerminateReply)applicationShouldTerminate:(NSApplication*)sender
{
	BOOL restoresSession = ![[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsDisableSessionRestoreKey];
	std::vector<document::document_ptr> documents;
	for(DocumentController* delegate in SortedControllers())
		std::copy_if(delegate.documents.begin(), delegate.documents.end(), back_inserter(documents), [&restoresSession](document::document_ptr const& doc){ return doc->is_modified() && (doc->path() != NULL_STR || !restoresSession); });

	if(documents.empty())
	{
		[DocumentController saveSessionIncludingUntitledDocuments:restoresSession];
		if(restoresSession)
		{
			// Ensure we do not remove backup files, as they are used to restore untitled documents
			for(DocumentController* controller in [SortedControllers() reverseObjectEnumerator])
			{
				for(auto document : controller.documents)
					document->detach_backup();
			}
		}
		return NSTerminateNow;
	}

	IsInShouldTerminateEventLoop = YES;

	DocumentController* controller = [SortedControllers() firstObject];
	[controller showCloseWarningUIForDocuments:documents completionHandler:^(BOOL canClose){
		if(canClose)
			[DocumentController saveSessionIncludingUntitledDocuments:NO];

		if(IsInShouldTerminateEventLoop)
			[NSApp replyToApplicationShouldTerminate:canClose];
		else if(canClose)
			[NSApp terminate:self];

		IsInShouldTerminateEventLoop = NO;
	}];

	return NSTerminateLater;
}

// =====================
// = Document Tracking =
// =====================

- (void)trackDocument:(document::document_ptr)aDocument
{
	if(aDocument)
	{
		auto iter = _trackedDocuments.find(aDocument->identifier());
		if(iter == _trackedDocuments.end())
			iter = _trackedDocuments.emplace(aDocument->identifier(), tracking_info_t(self, aDocument)).first;
		iter->second.track();
	}
}

- (void)untrackDocument:(document::document_ptr)aDocument
{
	if(aDocument)
	{
		auto iter = _trackedDocuments.find(aDocument->identifier());
		ASSERT(iter != _trackedDocuments.end());
		if(iter->second.untrack())
			_trackedDocuments.erase(iter);
	}
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
		if([[OakFileManager sharedInstance] createFileAtURL:url window:self.window])
		{
			document::document_ptr doc = document::create(to_s([url path]));
			doc->set_file_type(fileType);
			auto const settings = settings_for_path(doc->virtual_path(), doc->file_type(), path::parent(doc->path()));
			doc->set_indent(text::indent_t(std::max(1, settings.get(kSettingsTabSizeKey, 4)), SIZE_T_MAX, settings.get(kSettingsSoftTabsKey, false)));

			doc->open();
			[self setSelectedDocument:doc];
			doc->close();

			size_t selectedIndex = _selectedTabIndex;
			std::vector<document::document_ptr> documents = _documents;
			if(is_disposable(documents[selectedIndex]))
					documents[selectedIndex] = doc;
			else	documents.insert(documents.begin() + ++selectedIndex, doc);

			self.documents        = documents;
			self.selectedTabIndex = selectedIndex;

			[self.fileBrowser editURL:url];
		}
	}
}

- (IBAction)moveDocumentToNewWindow:(id)sender
{
	if(_documents.size() > 1)
		[self takeTabsToTearOffFrom:[NSIndexSet indexSetWithIndex:_selectedTabIndex]];
}

- (IBAction)mergeAllWindows:(id)sender
{
	std::vector<document::document_ptr> documents = _documents;
	for(DocumentController* delegate in SortedControllers())
	{
		if(delegate != self && ![delegate.window isMiniaturized])
			documents.insert(documents.end(), delegate.documents.begin(), delegate.documents.end());
	}

	self.documents = documents;

	for(DocumentController* delegate in SortedControllers())
	{
		if(delegate != self && ![delegate.window isMiniaturized])
			[delegate.window close];
	}
}

- (BOOL)disableTabReordering
{
	return [[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsDisableTabReorderingKey];;
}

- (void)openItems:(NSArray*)items closingOtherTabs:(BOOL)closeOtherTabsFlag
{
	std::vector<document::document_ptr> documents;
	for(id item in items)
	{
		std::string const path  = to_s((NSString*)[item objectForKey:@"path"]);
		std::string const uuid  = to_s((NSString*)[item objectForKey:@"identifier"]);
		std::string const range = to_s((NSString*)[item objectForKey:@"selectionString"]);

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

	std::vector<document::document_ptr> oldDocuments = _documents;
	NSUInteger split = _selectedTabIndex;

	std::set<oak::uuid_t> oldUUIDs, newUUIDs, actualNewUUIDs;
	std::transform(oldDocuments.begin(), oldDocuments.end(), inserter(oldUUIDs, oldUUIDs.end()), [](document::document_ptr const& doc){ return doc->identifier(); });
	std::transform(documents.begin(), documents.end(), inserter(newUUIDs, newUUIDs.end()), [](document::document_ptr const& doc){ return doc->identifier(); });
	std::set_difference(newUUIDs.begin(), newUUIDs.end(), oldUUIDs.begin(), oldUUIDs.end(), inserter(actualNewUUIDs, actualNewUUIDs.end()));

	if(!actualNewUUIDs.empty() && !oldDocuments.empty() && is_disposable(oldDocuments[split]))
			oldDocuments.erase(oldDocuments.begin() + split);
	else	++split;

	std::vector<document::document_ptr> newDocuments;
	split = merge_documents_splitting_at(oldDocuments, documents, split, newDocuments, [self disableTabReordering]);

	self.documents        = newDocuments;
	self.selectedTabIndex = split;

	if(!newDocuments.empty())
		[self openAndSelectDocument:newDocuments[split]];

	if(closeOtherTabsFlag)
	{
		std::set<oak::uuid_t> uuids;
		std::transform(documents.begin(), documents.end(), inserter(uuids, uuids.end()), [](document::document_ptr const& doc){ return doc->identifier(); });

		NSMutableIndexSet* indexSet = [NSMutableIndexSet indexSet];
		for(size_t i = 0; i < newDocuments.size(); ++i)
		{
			document::document_ptr doc = newDocuments[i];
			if(!doc->is_modified() && uuids.find(doc->identifier()) == uuids.end() && !_documents[i]->sticky())
				[indexSet addIndex:i];
		}
		[self closeTabsAtIndexes:indexSet askToSaveChanges:YES createDocumentIfEmpty:NO];
	}
	else
	{
		NSInteger excessTabs = _documents.size() - std::max<NSUInteger>(self.tabBarView.countOfVisibleTabs, 8);
		if(self.tabBarView && excessTabs > 0)
		{
			std::set<oak::uuid_t> uuids;
			std::transform(documents.begin(), documents.end(), inserter(uuids, uuids.end()), [](document::document_ptr const& doc){ return doc->identifier(); });

			std::multimap<oak::date_t, size_t> ranked;
			for(size_t i = 0; i < newDocuments.size(); ++i)
			{
				document::document_ptr doc = newDocuments[i];
				if(!doc->is_modified() && doc->is_on_disk() && uuids.find(doc->identifier()) == uuids.end() && !doc->sticky())
					ranked.emplace(doc->lru(), i);
			}

			NSMutableIndexSet* indexSet = [NSMutableIndexSet indexSet];
			for(auto const& pair : ranked)
			{
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

- (void)openAndSelectDocument:(document::document_ptr const&)aDocument
{
	document::document_ptr doc = aDocument;
	[[DocumentOpenHelper new] tryOpenDocument:doc forWindow:self.window completionHandler:^(std::string const& error, oak::uuid_t const& filterUUID){
		if(error == NULL_STR)
		{
			[self makeTextViewFirstResponder:self];
			[self setSelectedDocument:doc];
		}
		else
		{
			if(filterUUID)
				show_command_error(error, filterUUID);

			[self openAndSelectDocument:document::from_content("TODO Reselect previously open document")];
		}
	}];
}

- (IBAction)saveDocument:(id)sender
{
	if(!_selectedDocument)
		return;

	if(_selectedDocument->path() != NULL_STR)
	{
		[DocumentSaveHelper trySaveDocument:_selectedDocument forWindow:self.window defaultDirectory:nil completionHandler:nil];
	}
	else
	{
		NSString* const suggestedFolder  = self.untitledSavePath;
		NSString* const suggestedName    = DefaultSaveNameForDocument(_selectedDocument);
		encoding::type suggestedEncoding = _selectedDocument->encoding_for_save_as_path(to_s([suggestedFolder stringByAppendingPathComponent:suggestedName]));
		[OakSavePanel showWithPath:suggestedName directory:suggestedFolder fowWindow:self.window encoding:suggestedEncoding completionHandler:^(NSString* path, encoding::type const& encoding){
			if(!path)
				return;

			std::vector<std::string> const& paths = path::expand_braces(to_s(path));
			ASSERT_LT(0, paths.size());

			_selectedDocument->set_path(paths[0]);
			_selectedDocument->set_disk_encoding(encoding);

			// if(_selectedDocument->identifier() == scratchDocument)
			// 	scratchDocument = oak::uuid_t();

			if(paths.size() > 1)
			{
				 // FIXME check if paths[0] already exists (overwrite)

				std::vector<document::document_ptr> documents, newDocuments;
				std::transform(paths.begin() + 1, paths.end(), back_inserter(documents), [&encoding](std::string const& path) -> document::document_ptr {
					document::document_ptr doc = document::create(path);
					doc->set_disk_encoding(encoding);
					return doc;
				});

				merge_documents_splitting_at(_documents, documents, _selectedTabIndex + 1, newDocuments);
				self.documents = newDocuments;
			}

			[DocumentSaveHelper trySaveDocument:_selectedDocument forWindow:self.window defaultDirectory:nil completionHandler:nil];
		}];
	}
}

- (IBAction)saveDocumentAs:(id)sender
{
	if(!_selectedDocument)
		return;

	std::string const documentPath   = _selectedDocument->path();
	NSString* const suggestedFolder  = [NSString stringWithCxxString:path::parent(documentPath)] ?: self.untitledSavePath;
	NSString* const suggestedName    = [NSString stringWithCxxString:path::name(documentPath)]   ?: DefaultSaveNameForDocument(_selectedDocument);
	encoding::type suggestedEncoding = _selectedDocument->encoding_for_save_as_path(to_s([suggestedFolder stringByAppendingPathComponent:suggestedName]));
	[OakSavePanel showWithPath:suggestedName directory:suggestedFolder fowWindow:self.window encoding:suggestedEncoding completionHandler:^(NSString* path, encoding::type const& encoding){
		if(!path)
			return;
		_selectedDocument->set_path(to_s(path));
		_selectedDocument->set_disk_encoding(encoding);
		[DocumentSaveHelper trySaveDocument:_selectedDocument forWindow:self.window defaultDirectory:nil completionHandler:nil];
	}];
}

- (IBAction)saveAllDocuments:(id)sender
{
	std::vector<document::document_ptr> documentsToSave;
	for(auto document : _documents)
	{
		if(document->is_modified())
			documentsToSave.push_back(document);
	}
	[DocumentSaveHelper trySaveDocuments:documentsToSave forWindow:self.window defaultDirectory:self.untitledSavePath completionHandler:nil];
}

- (void)bundleItemPreExec:(pre_exec::type)preExec completionHandler:(void(^)(BOOL success))callback
{
	std::vector<document::document_ptr> documentsToSave;
	switch(preExec)
	{
		case pre_exec::save_document:
		{
			if(_selectedDocument && (_selectedDocument->is_modified() || !_selectedDocument->is_on_disk()))
				documentsToSave.push_back(_selectedDocument);
		}
		break;

		case pre_exec::save_project:
		{
			for(auto document : _documents)
			{
				if(document->is_modified() && document->path() != NULL_STR)
					documentsToSave.push_back(document);
			}
		}
		break;
	}

	if(!documentsToSave.empty())
	{
		[DocumentSaveHelper trySaveDocuments:documentsToSave forWindow:self.window defaultDirectory:self.untitledSavePath completionHandler:^(BOOL success){
			callback(success);
		}];
	}
	else
	{
		callback(YES);
	}
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
		if(_selectedDocument && _selectedDocument->path() != NULL_STR)
			[self revealFileInProject:self];
	}
}

- (void)updateWindowTitle
{
	if(_selectedDocument)
	{
		auto map = _selectedDocument->document_variables();
		auto const& scm = _documentSCMVariables.empty() ? _projectSCMVariables : _documentSCMVariables;
		map.insert(scm.begin(), scm.end());
		if(self.projectPath)
			map["projectDirectory"] = to_s(self.projectPath);

		std::string docDirectory = _selectedDocument->path() != NULL_STR ? path::parent(_selectedDocument->path()) : to_s(self.untitledSavePath);
		settings_t const settings = settings_for_path(_selectedDocument->virtual_path(), _selectedDocument->file_type() + " " + to_s(self.scopeAttributes), docDirectory, map);
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
		{ "attr.scm.svn",       ".svn",           "scm",   },
		{ "attr.scm.hg",        ".hg",            "scm",   },
		{ "attr.scm.git",       ".git",           "scm",   },
		{ "attr.scm.p4",        ".p4config",      "scm",   },
		{ "attr.project.ninja", "build.ninja",    "build", },
		{ "attr.project.make",  "Makefile",       "build", },
		{ "attr.project.xcode", "*.xcodeproj",    "build", },
		{ "attr.project.rake",  "Rakefile",       "build", },
		{ "attr.project.ant",   "build.xml",      "build", },
		{ "attr.project.cmake", "CMakeLists.txt", "build", },
		{ "attr.project.maven", "pom.xml",        "build", },
		{ "attr.project.scons", "SConstruct",     "build", },
		{ "attr.project.lein",  "project.clj",    "build", },
	};

	_externalScopeAttributes.clear();
	if(!_documentPath && !_projectPath)
		return;

	std::string const projectDir   = to_s(_projectPath ?: NSHomeDirectory());
	std::string const documentPath = _documentPath ? to_s(_documentPath) : path::join(projectDir, "dummy");

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
			std::string const currentDocumentPath = _documentPath ? to_s(_documentPath) : path::join(projectDir, "dummy");
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
			__weak DocumentController* weakSelf = self;
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

		std::string docDirectory = _documentPath ? path::parent(to_s(_documentPath)) : to_s(self.projectPath);

		_documentScopeAttributes = { text::format("attr.os-version.%zu.%zu.%zu", oak::os_major(), oak::os_minor(), oak::os_patch()) };
		if(_documentPath)
		{
			std::string const path = to_s(_documentPath);
			std::vector<std::string> revPath;
			for(auto const& token : text::tokenize(path.begin(), path.end(), '/'))
			{
				std::string tmp = token;
				for(auto const& subtoken : text::tokenize(tmp.begin(), tmp.end(), '.'))
				{
					if(subtoken.empty())
						continue;
					revPath.push_back(subtoken);
					std::replace(revPath.back().begin(), revPath.back().end(), ' ', '_');
				}
			}
			revPath.push_back("rev-path");
			revPath.push_back("attr");
			std::reverse(revPath.begin(), revPath.end());
			_documentScopeAttributes.push_back(text::join(revPath, "."));
		}
		else
		{
			_documentScopeAttributes.push_back("attr.untitled");
		}

		std::string const customAttributes = settings_for_path(to_s(_documentPath), text::join(_documentScopeAttributes, " "), docDirectory).get(kSettingsScopeAttributesKey, NULL_STR);
		if(customAttributes != NULL_STR)
			_documentScopeAttributes.push_back(customAttributes);

		if(_documentSCMInfo = scm::info(docDirectory))
		{
			__weak DocumentController* weakSelf = self;
			_documentSCMInfo->add_callback(^(scm::info_t const& info){
				weakSelf.documentSCMStatus    = info.status(to_s(weakSelf.documentPath));
				weakSelf.documentSCMVariables = info.scm_variables();
			});
		}
		else
		{
			self.documentSCMStatus    = scm::status::unknown;
			self.documentSCMVariables = std::map<std::string, std::string>();
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

- (void)setDocuments:(std::vector<document::document_ptr> const&)newDocuments
{
	for(auto document : newDocuments)
		[self trackDocument:document];
	for(auto document : _documents)
		[self untrackDocument:document];

	_documents = newDocuments;

	if(_documents.size())
		[self.tabBarView reloadData];

	[self updateFileBrowserStatus:self];
	[[self class] scheduleSessionBackup:self];
}

- (void)setSelectedDocument:(document::document_ptr const&)newSelectedDocument
{
	ASSERT(!newSelectedDocument || newSelectedDocument->is_open());
	if(_selectedDocument == newSelectedDocument)
	{
		[self.documentView setDocument:_selectedDocument];
		return;
	}

	[self trackDocument:newSelectedDocument];
	[self untrackDocument:_selectedDocument];

	if(_selectedDocument = newSelectedDocument)
	{
		NSString* projectPath = self.defaultProjectPath ?: self.fileBrowser.path ?: [NSString stringWithCxxString:path::parent(_selectedDocument->path())];
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
		self.documentPath        = [NSString stringWithCxxString:_selectedDocument->path()];
		self.documentDisplayName = [NSString stringWithCxxString:_selectedDocument->display_name()];
		self.documentIsModified  = _selectedDocument->is_modified();
		self.documentIsOnDisk    = _selectedDocument->is_on_disk();

		[self.documentView setDocument:_selectedDocument];
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

- (NSUInteger)numberOfRowsInTabBarView:(OakTabBarView*)aTabBarView                      { return _documents.size(); }
- (NSString*)tabBarView:(OakTabBarView*)aTabBarView titleForIndex:(NSUInteger)anIndex   { return [NSString stringWithCxxString:_documents[anIndex]->display_name()]; }
- (NSString*)tabBarView:(OakTabBarView*)aTabBarView toolTipForIndex:(NSUInteger)anIndex { return [NSString stringWithCxxString:path::with_tilde(_documents[anIndex]->path())] ?: @""; }
- (BOOL)tabBarView:(OakTabBarView*)aTabBarView isEditedAtIndex:(NSUInteger)anIndex      { return _documents[anIndex]->is_modified(); }

// ==============================
// = OakTabBarView Context Menu =
// ==============================

- (NSIndexSet*)tryObtainIndexSetFrom:(id)sender
{
	id res = [sender respondsToSelector:@selector(representedObject)] ? [sender representedObject] : sender;
	if([res isKindOfClass:[NSIndexSet class]])
		return res;
	else if(!_documents.empty())
		return [NSIndexSet indexSetWithIndex:self.selectedTabIndex];
	return nil;
}

- (void)takeNewTabIndexFrom:(id)sender
{
	if(NSIndexSet* indexSet = [self tryObtainIndexSetFrom:sender])
	{
		document::document_ptr doc = create_untitled_document_in_folder(to_s(self.untitledSavePath));
		doc->open();
		[self setSelectedDocument:doc];
		doc->close();

		std::vector<document::document_ptr> newDocuments;
		size_t pos = merge_documents_splitting_at(_documents, make_vector(doc), [indexSet firstIndex], newDocuments);
		self.documents        = newDocuments;
		self.selectedTabIndex = pos;
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
			documents.push_back(_documents[index]);

		if(documents.size() == 1)
		{
			DocumentController* controller = [DocumentController new];
			controller.documents = make_vector(documents[0]);
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
			_documents[index]->set_sticky(!_documents[index]->sticky());
	}
}

- (NSMenu*)menuForTabBarView:(OakTabBarView*)aTabBarView
{
	NSInteger tabIndex = aTabBarView.tag;
	NSInteger total    = _documents.size();

	NSMutableIndexSet* newTabAtTab   = tabIndex == -1 ? [NSMutableIndexSet indexSetWithIndex:total] : [NSMutableIndexSet indexSetWithIndex:tabIndex + 1];
	NSMutableIndexSet* clickedTab    = tabIndex == -1 ? [NSMutableIndexSet indexSet] : [NSMutableIndexSet indexSetWithIndex:tabIndex];
	NSMutableIndexSet* otherTabs     = tabIndex == -1 ? [NSMutableIndexSet indexSet] : [NSMutableIndexSet indexSetWithIndexesInRange:NSMakeRange(0, total)];
	NSMutableIndexSet* rightSideTabs = tabIndex == -1 ? [NSMutableIndexSet indexSet] : [NSMutableIndexSet indexSetWithIndexesInRange:NSMakeRange(0, total)];

	if(tabIndex != -1)
	{
		[otherTabs removeIndex:tabIndex];
		[rightSideTabs removeIndexes:[NSIndexSet indexSetWithIndexesInRange:NSMakeRange(0, tabIndex + 1)]];
	}

	for(size_t i = 0; i < _documents.size(); ++i)
	{
		if(_documents[i]->sticky())
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
	[self openAndSelectDocument:_documents[anIndex]];
	self.selectedTabIndex = anIndex;
	return YES;
}

- (void)tabBarView:(OakTabBarView*)aTabBarView didDoubleClickIndex:(NSUInteger)anIndex
{
	if(_documents.size() > 1)
		[self takeTabsToTearOffFrom:[NSMutableIndexSet indexSetWithIndex:anIndex]];
}

- (void)tabBarViewDidDoubleClick:(OakTabBarView*)aTabBarView
{
	[self takeNewTabIndexFrom:[NSMutableIndexSet indexSetWithIndex:_documents.size()]];
}

// ================
// = Tab Dragging =
// ================

- (void)setupPasteboard:(NSPasteboard*)aPasteboard forTabAtIndex:(NSUInteger)draggedTabIndex
{
	document::document_ptr document = _documents[draggedTabIndex];
	if(document->path() != NULL_STR)
	{
		[aPasteboard addTypes:@[ NSFilenamesPboardType ] owner:nil];
		[aPasteboard setPropertyList:@[ [NSString stringWithCxxString:document->path()] ] forType:NSFilenamesPboardType];
	}

	[aPasteboard addTypes:@[ OakDocumentPboardType ] owner:nil];
	[aPasteboard setPropertyList:@{
		@"index"       : @(draggedTabIndex),
		@"document"    : [NSString stringWithCxxString:document->identifier()],
		@"collection"  : self.identifier,
	} forType:OakDocumentPboardType];
}

- (BOOL)performTabDropFromTabBar:(OakTabBarView*)aTabBar atIndex:(NSUInteger)droppedIndex fromPasteboard:(NSPasteboard*)aPasteboard operation:(NSDragOperation)operation
{
	NSDictionary* plist = [aPasteboard propertyListForType:OakDocumentPboardType];
	oak::uuid_t docId   = to_s((NSString*)plist[@"document"]);

	std::vector<document::document_ptr> newDocuments;
	merge_documents_splitting_at(_documents, make_vector(document::find(docId)), droppedIndex, newDocuments);
	self.documents = newDocuments;

	if(_selectedDocument)
	{
		oak::uuid_t selectedUUID = _selectedDocument->identifier();
		auto iter = std::find_if(newDocuments.begin(), newDocuments.end(), [&selectedUUID](document::document_ptr const& doc){ return doc->identifier() == selectedUUID; });
		if(iter != newDocuments.end())
			self.selectedTabIndex = iter - newDocuments.begin();
	}

	oak::uuid_t srcProjectId = to_s((NSString*)plist[@"collection"]);
	if(operation == NSDragOperationMove && srcProjectId != to_s(self.identifier))
	{
		for(DocumentController* delegate in SortedControllers())
		{
			if(srcProjectId == oak::uuid_t(to_s(delegate.identifier)))
			{
				if(delegate.fileBrowserVisible || [delegate documents].size() > 1)
						[delegate closeTabsAtIndexes:[NSIndexSet indexSetWithIndex:[plist[@"index"] unsignedIntValue]] askToSaveChanges:NO createDocumentIfEmpty:YES];
				else	[delegate close];
				return YES;
			}
		}
	}

	return YES;
}

- (IBAction)selectNextTab:(id)sender            { self.selectedTabIndex = (_selectedTabIndex + 1) % _documents.size();                     [self openAndSelectDocument:_documents[_selectedTabIndex]]; }
- (IBAction)selectPreviousTab:(id)sender        { self.selectedTabIndex = (_selectedTabIndex + _documents.size() - 1) % _documents.size(); [self openAndSelectDocument:_documents[_selectedTabIndex]]; }
- (IBAction)takeSelectedTabIndexFrom:(id)sender { self.selectedTabIndex = [sender tag];                                                    [self openAndSelectDocument:_documents[_selectedTabIndex]]; }

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
	auto documents = _documents;
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
		}

		if(!makeVisibleFlag && [[self.window firstResponder] isKindOfClass:[NSView class]] && [(NSView*)[self.window firstResponder] isDescendantOf:self.layoutView.fileBrowserView])
			[self makeTextViewFirstResponder:self];

		self.layoutView.fileBrowserView = makeVisibleFlag ? self.fileBrowser.view : nil;

		if(makeVisibleFlag)
		{
			self.fileBrowser.nextResponder = self.fileBrowser.view.nextResponder;
			self.fileBrowser.view.nextResponder = self.fileBrowser;
			if(self.autoRevealFile && _selectedDocument && _selectedDocument->path() != NULL_STR)
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
	for(auto document : _documents)
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

- (IBAction)revealFileInProject:(id)sender  { if(_selectedDocument) { self.fileBrowserVisible = YES; [self.fileBrowser selectURL:[NSURL fileURLWithPath:[NSString stringWithCxxString:_selectedDocument->path()]] withParentURL:self.projectPath ? [NSURL fileURLWithPath:self.projectPath] : nil]; } }
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
	return self.layoutView.htmlOutputView || [self.htmlOutputWindowController.window isVisible];
}

- (void)setHtmlOutputVisible:(BOOL)makeVisibleFlag
{
	if(self.htmlOutputVisible == makeVisibleFlag)
		return;

	if(makeVisibleFlag)
	{
		if(self.htmlOutputInWindow)
		{
			[self.htmlOutputWindowController.window makeKeyAndOrderFront:self];
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

		[self.htmlOutputWindowController.window orderOut:self];
		self.layoutView.htmlOutputView = nil;
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
			[self.htmlOutputWindowController.window makeKeyAndOrderFront:self];
	else	self.htmlOutputVisible = !self.htmlOutputVisible;
}

- (BOOL)setCommandRunner:(command::runner_ptr const&)aRunner
{
	if(self.htmlOutputInWindow)
	{
		_runner = aRunner;

		if(!self.htmlOutputWindowController || [self.htmlOutputWindowController running] || self.htmlOutputWindowController.needsNewWebView)
				self.htmlOutputWindowController = [HTMLOutputWindowController HTMLOutputWindowWithRunner:_runner];
		else	[self.htmlOutputWindowController setCommandRunner:_runner];
	}
	else
	{
		if(_runner && _runner->running())
		{
			NSInteger choice = [[NSAlert alertWithMessageText:@"Stop current task first?" defaultButton:@"Stop Task" alternateButton:@"Cancel" otherButton:nil informativeTextWithFormat:@"There already is a task running. If you stop this then the task it is performing will not be completed."] runModal];
			if(choice != NSAlertDefaultReturn) /* "Stop" */
				return NO;
		}

		_runner = aRunner;

		self.htmlOutputVisible = YES;
		[self.window makeFirstResponder:self.htmlOutputView.webView];
		[self.htmlOutputView setEnvironment:_runner->environment()];
		[self.htmlOutputView loadRequest:URLRequestForCommandRunner(_runner) autoScrolls:_runner->auto_scroll_output()];
	}
	return YES;
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
		NSRect parent = [_window convertRectToScreen:[_textView convertRect:[_textView visibleRect] toView:nil]];

		frame.origin.x = NSMinX(parent) + round((NSWidth(parent)  - NSWidth(frame))  * 1 / 4);
		frame.origin.y = NSMinY(parent) + round((NSHeight(parent) - NSHeight(frame)) * 3 / 4);
		[aWindow setFrame:frame display:NO];
	}
}

- (NSString*)selectedDocumentUUID
{
	return _selectedDocument ? [NSString stringWithCxxString:_selectedDocument->identifier()] : nil;
}

- (IBAction)orderFrontFindPanel:(id)sender
{
	Find* find              = [Find sharedInstance];
	BOOL didOwnDialog       = [find.projectIdentifier isEqualToString:self.identifier];
	find.documentIdentifier = self.selectedDocumentUUID;
	find.projectFolder      = self.projectPath ?: self.untitledSavePath ?: NSHomeDirectory();
	find.projectIdentifier  = self.identifier;

	NSInteger mode = [sender respondsToSelector:@selector(tag)] ? [sender tag] : find_tags::in_document;
	if(mode == find_tags::in_document && [[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsFindInSelectionByDefault] && [self.window isKeyWindow] && self.textView.hasMultiLineSelection)
		mode = find_tags::in_selection;

	switch(mode)
	{
		case find_tags::in_document:  return [find showFindWindowFor:FFSearchInDocument];
		case find_tags::in_selection: return [find showFindWindowFor:FFSearchInSelection];
		case find_tags::in_folder:    return [find showFolderSelectionPanel:self];

		case find_tags::in_project:
		{
			BOOL fileBrowserHasFocus = [self.window.firstResponder respondsToSelector:@selector(isDescendantOf:)] && [(NSView*)self.window.firstResponder isDescendantOf:self.fileBrowser.view];
			NSString* searchFolder = fileBrowserHasFocus ? self.untitledSavePath : find.projectFolder;
			if(find.isVisible && find.searchFolder && didOwnDialog) // don’t reset search folder, as the user may have picked “Other…” and simply wants the results brought to front
				searchFolder = find.searchFolder;
			[find showFindWindowFor:searchFolder];
		}
		break;
	}
}

- (IBAction)orderFrontFindPanelForFileBrowser:(id)sender
{
	Find* find              = [Find sharedInstance];
	find.documentIdentifier = self.selectedDocumentUUID;
	find.projectFolder      = self.projectPath ?: self.untitledSavePath ?: NSHomeDirectory();
	find.projectIdentifier  = self.identifier;
	[find showFindWindowFor:self.untitledSavePath];
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

	fc.filterString    = @"";
	fc.openDocuments   = _documents;
	fc.currentDocument = _selectedDocument ? _selectedDocument->identifier() : oak::uuid_t();
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
	NSString* res = self.projectPath;
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
	return self.projectPath && (self.fileBrowserVisible || _documents.size() > 1);
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
	if(!_selectedDocument)
		return;

	std::string const documentPath = _selectedDocument->path();
	if(documentPath == NULL_STR)
		return (void)NSBeep();

	std::string const documentDir  = path::parent(documentPath);
	std::string const documentName = path::name(documentPath);
	std::string const documentBase = path::strip_extensions(documentName);

	std::set<std::string> candidates(&documentName, &documentName + 1);
	for(auto document : _documents)
	{
		if(documentDir == path::parent(document->path()) && documentBase == path::strip_extensions(path::name(document->path())))
			candidates.insert(path::name(document->path()));
	}

	auto map = _selectedDocument->document_variables();
	auto const& scm = _documentSCMVariables.empty() ? _projectSCMVariables : _documentSCMVariables;
	map.insert(scm.begin(), scm.end());
	if(self.projectPath)
		map["projectDirectory"] = to_s(self.projectPath);

	settings_t const settings = settings_for_path(_selectedDocument->virtual_path(), _selectedDocument->file_type() + " " + to_s(self.scopeAttributes), path::parent(documentPath), map);
	std::string const customCandidate = settings.get(kSettingsRelatedFilePathKey, NULL_STR);

	if(customCandidate != NULL_STR && customCandidate != documentPath && (std::find_if(_documents.begin(), _documents.end(), [&customCandidate](document::document_ptr const& doc){ return customCandidate == doc->path(); }) != _documents.end() || path::exists(customCandidate)))
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
	for(auto const& path : candidates)
	{
		if(path == documentPath || !binaryGlob.does_match(path) && !excludeGlob.does_match(path))
			v.push_back(path);
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

// ===========================
// = Go to Tab Menu Delegate =
// ===========================

- (void)updateGoToMenu:(NSMenu*)aMenu
{
	if(![self.window isKeyWindow])
	{
		[aMenu addItemWithTitle:@"No Tabs" action:@selector(nop:) keyEquivalent:@""];
		return;
	}

	int i = 0;
	for(auto document : _documents)
	{
		NSMenuItem* item = [aMenu addItemWithTitle:[NSString stringWithCxxString:document->display_name()] action:@selector(takeSelectedTabIndexFrom:) keyEquivalent:i < 9 ? [NSString stringWithFormat:@"%c", '0' + ((i+1) % 10)] : @""];
		item.tag     = i;
		item.toolTip = [[NSString stringWithCxxString:document->path()] stringByAbbreviatingWithTildeInPath];
		item.image   = [OakFileIconImage fileIconImageWithPath:[NSString stringWithCxxString:document->path()] isModified:document->is_modified()];
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

		NSMenuItem* item = [aMenu addItemWithTitle:@"Last Tab" action:@selector(takeSelectedTabIndexFrom:) keyEquivalent:@"0"];
		item.tag     = _documents.size()-1;
		item.toolTip = [NSString stringWithCxxString:_documents.back()->display_name()];
	}
}

// ====================
// = NSMenuValidation =
// ====================

- (BOOL)validateMenuItem:(NSMenuItem*)menuItem
{
	BOOL active = YES;
	if([menuItem action] == @selector(toggleFileBrowser:))
		[menuItem setTitle:self.fileBrowserVisible ? @"Hide File Browser" : @"Show File Browser"];
	else if([menuItem action] == @selector(toggleHTMLOutput:))
	{
		[menuItem setTitle:(!self.htmlOutputVisible || self.htmlOutputInWindow && ![self.htmlOutputWindowController.window isKeyWindow]) ? @"Show HTML Output" : @"Hide HTML Output"];
		active = !self.htmlOutputInWindow || self.htmlOutputWindowController;
	}
	else if([menuItem action] == @selector(newDocumentInDirectory:))
		active = self.fileBrowserVisible && [self.fileBrowser directoryForNewItems] != nil;
	else if([menuItem action] == @selector(newFolder:) || [menuItem action] == @selector(goBack:) || [menuItem action] == @selector(goForward:))
		active = self.fileBrowserVisible && [self.fileBrowser validateMenuItem:menuItem];
	else if([menuItem action] == @selector(moveDocumentToNewWindow:))
		active = _documents.size() > 1;
	else if([menuItem action] == @selector(selectNextTab:) || [menuItem action] == @selector(selectPreviousTab:))
		active = _documents.size() > 1;
	else if([menuItem action] == @selector(revealFileInProject:) || [menuItem action] == @selector(revealFileInProjectByExpandingAncestors:))
		active = _selectedDocument && _selectedDocument->path() != NULL_STR;
	else if([menuItem action] == @selector(goToProjectFolder:))
		active = self.projectPath != nil;
	else if([menuItem action] == @selector(goToParentFolder:))
		active = [self.window firstResponder] != self.textView;
	else if([menuItem action] == @selector(reload:) || [menuItem action] == @selector(deselectAll:))
		active = self.fileBrowserVisible;
	else if([menuItem action] == @selector(moveFocus:))
		[menuItem setTitle:self.window.firstResponder == self.textView ? @"Move Focus to File Browser" : @"Move Focus to Document"];
	else if([menuItem action] == @selector(takeProjectPathFrom:))
		[menuItem setState:[self.defaultProjectPath isEqualToString:[menuItem representedObject]] ? NSOnState : NSOffState];
	else if([menuItem action] == @selector(performCloseOtherTabs:))
		active = _documents.size() > 1;
	else if([menuItem action] == @selector(performCloseTabsToTheRight:))
		active = _selectedTabIndex + 1 < _documents.size();

	SEL tabBarActions[] = { @selector(performCloseTab:), @selector(takeNewTabIndexFrom::), @selector(takeTabsToCloseFrom:), @selector(takeTabsToTearOffFrom:), @selector(toggleSticky:) };
	if(oak::contains(std::begin(tabBarActions), std::end(tabBarActions), [menuItem action]))
	{
		if(NSIndexSet* indexSet = [self tryObtainIndexSetFrom:menuItem])
		{
			active = [indexSet count] != 0;
			if(active && [menuItem action] == @selector(toggleSticky:))
				[menuItem setState:_documents[[indexSet firstIndex]]->sticky() ? NSOnState : NSOffState];
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

	NSDictionary* session = [NSDictionary dictionaryWithContentsOfFile:[self sessionPath]];
	for(NSDictionary* project in session[@"projects"])
	{
		DocumentController* controller = [DocumentController new];
		[controller setupControllerForProject:project skipMissingFiles:NO];
		if(NSString* windowFrame = project[@"windowFrame"])
			[controller.window setFrame:NSRectFromString(windowFrame) display:NO];
		[controller showWindow:nil];
		if([project[@"miniaturized"] boolValue])
			[controller.window miniaturize:nil];
		else if([project[@"fullScreen"] boolValue])
			[controller.window toggleFullScreen:self];
		res = YES;
	}

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
	self.fileBrowserHistory = project[@"fileBrowserState"];
	self.fileBrowserVisible = [project[@"fileBrowserVisible"] boolValue];

	std::vector<document::document_ptr> documents;
	NSInteger selectedTabIndex = 0;

	for(NSDictionary* info in project[@"documents"])
	{
		document::document_ptr doc;
		NSString* identifier = info[@"identifier"];
		if(!identifier || !(doc = document::find(to_s(identifier), true)))
		{
			NSString* path = info[@"path"];
			if(path && skipMissing && access([path fileSystemRepresentation], F_OK) != 0)
				continue;

			doc = path ? document::create(to_s(path)) : create_untitled_document_in_folder(to_s(self.untitledSavePath));
			if(NSString* displayName = info[@"displayName"])
				doc->set_custom_name(to_s(displayName));
			if([info[@"sticky"] boolValue])
				doc->set_sticky(true);
		}

		doc->set_recent_tracking(false);
		documents.push_back(doc);

		if([info[@"selected"] boolValue])
			selectedTabIndex = documents.size() - 1;
	}

	if(documents.empty())
		documents.push_back(create_untitled_document_in_folder(to_s(self.untitledSavePath)));

	self.documents        = documents;
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
	else	res[@"windowFrame"] = NSStringFromRect([self.window frame]);

	res[@"miniaturized"]       = @([self.window isMiniaturized]);
	res[@"htmlOutputSize"]     = NSStringFromSize(self.htmlOutputSize);
	res[@"fileBrowserVisible"] = @(self.fileBrowserVisible);
	res[@"fileBrowserWidth"]   = @(self.fileBrowserWidth);

	NSMutableArray* docs = [NSMutableArray array];
	for(auto document : self.documents)
	{
		if(!includeUntitled && (document->path() == NULL_STR || !path::exists(document->path())))
			continue;

		NSMutableDictionary* doc = [NSMutableDictionary dictionary];
		if(document->is_modified() || document->path() == NULL_STR)
		{
			doc[@"identifier"] = [NSString stringWithCxxString:document->identifier()];
			if(document->is_open())
				document->backup();
		}
		if(document->path() != NULL_STR)
			doc[@"path"] = [NSString stringWithCxxString:document->path()];
		if(document->display_name() != NULL_STR)
			doc[@"displayName"] = [NSString stringWithCxxString:document->display_name()];
		if(document == self.selectedDocument)
			doc[@"selected"] = @YES;
		if(document->sticky())
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

	NSMutableArray* projects = [NSMutableArray array];
	for(DocumentController* controller in [SortedControllers() reverseObjectEnumerator])
		[projects addObject:[controller sessionInfoIncludingUntitledDocuments:includeUntitled]];

	NSDictionary* session = @{ @"projects" : projects };
	return [session writeToFile:[self sessionPath] atomically:YES];
}

// ================================
// = OakWindowFrameHelperDelegate =
// ================================

- (NSRect)savableWindowFrame
{
	NSRect res = [self.window frame];
	if(self.fileBrowserVisible)
		res.size.width -= self.fileBrowserWidth;
	return res;
}

// ==========
// = Legacy =
// ==========

- (std::map<std::string, std::string>)variables
{
	std::map<std::string, std::string> res;
	if(self.fileBrowser)
		res = [self.fileBrowser variables];

	if(NSString* projectDir = self.projectPath)
	{
		res["TM_PROJECT_DIRECTORY"] = [projectDir fileSystemRepresentation];
		res["TM_PROJECT_UUID"]      = to_s(self.identifier);
	}

	return res;
}

+ (instancetype)controllerForDocument:(document::document_ptr const&)aDocument
{
	if(!aDocument)
		return nil;

	for(DocumentController* delegate in SortedControllers())
	{
		if(delegate.fileBrowserVisible && aDocument->path() != NULL_STR && aDocument->path().find(to_s(delegate.projectPath)) == 0)
			return delegate;

		for(auto document : delegate.documents)
		{
			if(*document == *aDocument)
				return delegate;
		}
	}
	return nil;
}

+ (void)load
{
	static struct proxy_t : document::ui_proxy_t
	{
	private:
		static void bring_to_front (DocumentController* aController)
		{
			if([NSApp isHidden])
			{
				__weak __block id observerId = [[NSNotificationCenter defaultCenter] addObserverForName:NSApplicationDidUnhideNotification object:NSApp queue:nil usingBlock:^(NSNotification*){
					[aController showWindow:nil];
					SetFrontProcessWithOptions(&(ProcessSerialNumber){ 0, kCurrentProcess }, kSetFrontProcessFrontWindowOnly);
					[[NSNotificationCenter defaultCenter] removeObserver:observerId];
				}];
				[NSApp unhideWithoutActivation];
			}
			else
			{
				[aController showWindow:nil];
				SetFrontProcessWithOptions(&(ProcessSerialNumber){ 0, kCurrentProcess }, kSetFrontProcessFrontWindowOnly);
			}
		}

		static DocumentController* find_or_create_controller (std::vector<document::document_ptr> const& documents, oak::uuid_t const& projectUUID)
		{
			ASSERT(!documents.empty());

			// =========================================
			// = Return requested window, if it exists =
			// =========================================

			if(projectUUID != document::kCollectionAny)
			{
				if(DocumentController* res = AllControllers()[[NSString stringWithCxxString:projectUUID]])
					return res;

				DocumentController* res = [DocumentController new];
				res.identifier = [NSString stringWithCxxString:projectUUID];
				return res;
			}

			// =========================================
			// = Find window with one of our documents =
			// =========================================

			std::set<oak::uuid_t> uuids;
			std::transform(documents.begin(), documents.end(), inserter(uuids, uuids.end()), [](document::document_ptr const& doc){ return doc->identifier(); });

			for(DocumentController* candidate in SortedControllers())
			{
				for(auto document : candidate.documents)
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

			std::map<size_t, DocumentController*> candidates;
			for(DocumentController* candidate in SortedControllers())
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

			if(DocumentController* candidate = [SortedControllers() firstObject])
			{
				if(!candidate.fileBrowserVisible && candidate.documents.size() == 1 && is_disposable(candidate.selectedDocument))
					return candidate;
			}

			// ===================================
			// = Give up and create a new window =
			// ===================================

			DocumentController* res = [DocumentController new];

			if(!parents.empty()) // setup project folder for new window
			{
				std::vector<std::string> rankedParents(parents.begin(), parents.end());
				std::sort(rankedParents.begin(), rankedParents.end(), [](std::string const& lhs, std::string const& rhs){ return lhs.size() < rhs.size(); });
				res.defaultProjectPath = [NSString stringWithCxxString:rankedParents.front()];
			}

			return res;
		}

		static DocumentController* controller_with_documents (std::vector<document::document_ptr> const& documents, oak::uuid_t const& projectUUID = document::kCollectionAny)
		{
			DocumentController* controller = find_or_create_controller(documents, projectUUID);
			if(controller.documents.empty())
			{
				controller.documents = documents;
			}
			else
			{
				std::vector<document::document_ptr> oldDocuments = controller.documents;
				NSUInteger split = controller.selectedTabIndex;

				if(is_disposable(oldDocuments[split]))
						oldDocuments.erase(oldDocuments.begin() + split);
				else	++split;

				std::vector<document::document_ptr> newDocuments;
				split = merge_documents_splitting_at(oldDocuments, documents, split, newDocuments, true);
				controller.documents = newDocuments;
				controller.selectedTabIndex = split;
			}
			return controller;
		}

	public:
		void show_browser (std::string const& path) const
		{
			std::string const folder = path::resolve(path);
			[[NSDocumentController sharedDocumentController] noteNewRecentDocumentURL:[NSURL fileURLWithPath:[NSString stringWithCxxString:folder]]]; 

			for(DocumentController* candidate in SortedControllers())
			{
				if(folder == to_s(candidate.projectPath))
					return bring_to_front(candidate);
			}

			DocumentController* controller = nil;
			for(DocumentController* candidate in SortedControllers())
			{
				if(!candidate.fileBrowserVisible && candidate.documents.size() == 1 && is_disposable(candidate.selectedDocument))
				{
					controller = candidate;
					break;
				}
			}

			if(!controller)
				controller = [DocumentController new];
			else if(controller.selectedDocument)
				[controller selectedDocument]->set_custom_name("not untitled"); // release potential untitled token used

			BOOL disableFolderStateRestore = [[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsDisableFolderStateRestore];
			if(NSDictionary* project = (disableFolderStateRestore ? nil : [[DocumentController sharedProjectStateDB] valueForKey:[NSString stringWithCxxString:folder]]))
			{
				[controller setupControllerForProject:project skipMissingFiles:YES];
			}
			else
			{
				controller.defaultProjectPath = [NSString stringWithCxxString:folder];
				controller.fileBrowserVisible = YES;
				controller.documents          = make_vector(create_untitled_document_in_folder(folder));
				controller.fileBrowser.url    = [NSURL fileURLWithPath:[NSString stringWithCxxString:folder]];

				[controller openAndSelectDocument:[controller documents][controller.selectedTabIndex]];
			}
			bring_to_front(controller);
		}

		void show_documents (std::vector<document::document_ptr> const& documents) const
		{
			DocumentController* controller = controller_with_documents(documents);
			bring_to_front(controller);
			[controller openAndSelectDocument:[controller documents][controller.selectedTabIndex]];
		}

		void show_document (oak::uuid_t const& collection, document::document_ptr document, text::range_t const& range, bool bringToFront) const
		{
			if(range != text::range_t::undefined)
				document->set_selection(range);

			DocumentController* controller = controller_with_documents(make_vector(document), collection);
			if(bringToFront)
				bring_to_front(controller);
			else if(![controller.window isVisible])
				[controller.window orderWindow:NSWindowBelow relativeTo:[([NSApp keyWindow] ?: [NSApp mainWindow]) windowNumber]];
			[controller openAndSelectDocument:document];
		}

		void run (bundle_command_t const& command, ng::buffer_t const& buffer, ng::ranges_t const& selection, document::document_ptr document, std::map<std::string, std::string> const& env, std::string const& pwd)
		{
			run_impl(command, buffer, selection, document, env, pwd);
		}

	} proxy;

	document::set_ui_proxy(&proxy);
}
@end
