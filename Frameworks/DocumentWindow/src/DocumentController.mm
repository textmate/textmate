#import "DocumentController.h"
#import "ProjectLayoutView.h"
#import "DocumentOpenHelper.h"
#import "DocumentSaveHelper.h"
#import "DocumentCommand.h" // show_command_error
#import <OakAppKit/NSAlert Additions.h>
#import <OakAppKit/NSMenuItem Additions.h>
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/OakFileIconImage.h>
#import <OakAppKit/OakPasteboard.h>
#import <OakAppKit/OakSavePanel.h>
#import <OakAppKit/OakSubmenuController.h>
#import <OakAppKit/OakTabBarView.h>
#import <OakAppKit/OakWindowFrameHelper.h>
#import <OakFoundation/NSArray Additions.h>
#import <OakFoundation/NSString Additions.h>
#import <Preferences/Keys.h>
#import <OakTextView/OakDocumentView.h>
#import <OakFileBrowser/OakFileBrowser.h>
#import <HTMLOutputWindow/HTMLOutputWindow.h>
#import <OakFilterList/FileChooser.h>
#import <OakFilterList/SymbolChooser.h>
#import <OakSystem/application.h>
#import <Find/Find.h>
#import <file/path_info.h>
#import <io/entries.h>
#import <scm/scm.h>
#import <text/utf8.h>
#import <ns/ns.h>

namespace find_tags { enum { in_document = 1, in_selection, in_project, in_folder }; } // From AppController.h

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

@interface DocumentController () <NSWindowDelegate, OakTabBarViewDelegate, OakTabBarViewDataSource, OakTextViewDelegate, OakFileBrowserDelegate, QLPreviewPanelDelegate, QLPreviewPanelDataSource>
@property (nonatomic) ProjectLayoutView*          layoutView;
@property (nonatomic) OakTabBarView*              tabBarView;
@property (nonatomic) OakDocumentView*            documentView;
@property (nonatomic) OakTextView*                textView;
@property (nonatomic) OakFileBrowser*             fileBrowser;

@property (nonatomic) HTMLOutputWindowController* htmlOutputWindowController;
@property (nonatomic) OakHTMLOutputView*          htmlOutputView;
@property (nonatomic) BOOL                        htmlOutputInWindow;

@property (nonatomic) NSString*                   windowTitle;
@property (nonatomic) NSString*                   representedFile;
@property (nonatomic) BOOL                        isDocumentEdited;

@property (nonatomic) NSString*                   pathAttributes;
@property (nonatomic) NSString*                   projectPath;

@property (nonatomic) OakFilterWindowController*  filterWindowController;

@property (nonatomic) NSArray*                    urlArrayForQuickLook;

+ (void)scheduleSessionBackup:(id)sender;

- (void)makeTextViewFirstResponder:(id)sender;
- (void)updatePathDependentProperties;
- (void)updateFileBrowserStatus:(id)sender;
- (void)documentDidChange:(document::document_ptr const&)aDocument;

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
			switch(event)
			{
				case did_change_path:
				case did_change_on_disk_status:
				case did_change_modified_status:
					[_self documentDidChange:document];
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

	static size_t merge_documents_splitting_at (std::vector<document::document_ptr> const& oldDocuments, std::vector<document::document_ptr> const& newDocuments, size_t splitAt, std::vector<document::document_ptr>& out)
	{
		std::set<oak::uuid_t> uuids;
		std::transform(newDocuments.begin(), newDocuments.end(), inserter(uuids, uuids.end()), [](document::document_ptr const& doc){ return doc->identifier(); });

		splitAt = std::min(splitAt, oldDocuments.size());
		std::copy_if(oldDocuments.begin(), oldDocuments.begin() + splitAt, back_inserter(out), [&uuids](document::document_ptr const& doc){ return uuids.find(doc->identifier()) == uuids.end(); });
		size_t res = out.size();
		std::copy(newDocuments.begin(), newDocuments.end(), back_inserter(out));	
		std::copy_if(oldDocuments.begin() + splitAt, oldDocuments.end(), back_inserter(out), [&uuids](document::document_ptr const& doc){ return uuids.find(doc->identifier()) == uuids.end(); });
		return out.empty() ? 0 : std::min(res, out.size()-1);
	}

	static std::vector<document::document_ptr> make_vector (document::document_ptr const& document)
	{
		return std::vector<document::document_ptr>(1, document);
	}

	static document::document_ptr create_untitled_document_in_folder (std::string const& suggestedFolder)
	{
		return document::from_content("", settings_for_path(NULL_STR, file::path_attributes(NULL_STR), suggestedFolder).get(kSettingsFileTypeKey, "text.plain"));
	}
}

@implementation DocumentController
{
	OBJC_WATCH_LEAKS(DocumentController);

	std::vector<document::document_ptr>    _documents;
	std::map<oak::uuid_t, tracking_info_t> _trackedDocuments;
	document::document_ptr                 _selectedDocument;
	command::runner_ptr                    _runner;

	scm::info_ptr                          _scmInfo;
	scm::callback_t*                       _scmCallback;
}

- (id)init
{
	if((self = [super init]))
	{
		self.identifier  = [NSString stringWithCxxString:oak::uuid_t().generate()];
		self.windowTitle = @"untitled";
		self.htmlOutputInWindow = [[[NSUserDefaults standardUserDefaults] objectForKey:kUserDefaultsHTMLOutputPlacementKey] isEqualToString:@"window"];

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
		self.window.contentView                 = self.layoutView;
		self.window.delegate                    = self;
		self.window.releasedWhenClosed          = NO;
		[self.window bind:@"title" toObject:self withKeyPath:@"windowTitle" options:nil];
		[self.window bind:@"documentEdited" toObject:self withKeyPath:@"isDocumentEdited" options:nil];

		[OakWindowFrameHelper windowFrameHelperWithWindow:self.window];

		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(userDefaultsDidChange:) name:NSUserDefaultsDidChangeNotification object:[NSUserDefaults standardUserDefaults]];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(applicationDidBecomeActiveNotification:) name:NSApplicationDidBecomeActiveNotification object:NSApp];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(applicationDidResignActiveNotification:) name:NSApplicationDidResignActiveNotification object:NSApp];
	}
	return self;
}

- (void)dealloc
{
	[[NSNotificationCenter defaultCenter] removeObserver:self];

	if(_scmInfo)
		_scmInfo->remove_callback(_scmCallback);
	delete _scmCallback;

	self.tabBarView.dataSource  = nil;
	self.tabBarView.delegate    = nil;
	self.textView.delegate      = nil;
	self.filterWindowController = nil; // ensures we removeObserver: and set target to nil
}

- (void)windowWillClose:(NSNotification*)aNotification
{
	self.documents        = std::vector<document::document_ptr>();
	self.selectedDocument = document::document_ptr();

	[self.window unbind:@"title"];
	[self.window unbind:@"documentEdited"];
	self.window.delegate = nil;

	self.identifier = nil; // This removes us from AllControllers and causes a release
}

- (void)showWindow:(id)sender
{
	if(self.documents.empty())
	{
		document::document_ptr defaultDocument = create_untitled_document_in_folder(to_s(self.untitledSavePath));
		self.documents = make_vector(defaultDocument);
		[self openAndSelectDocument:defaultDocument];
	}
	[self.window makeKeyAndOrderFront:sender];
}

- (void)makeTextViewFirstResponder:(id)sender { [self.window makeFirstResponder:self.textView]; }
- (void)close                                 { [self.window close]; }

// ==========================
// = Notification Callbacks =
// ==========================

- (void)userDefaultsDidChange:(NSNotification*)aNotification
{
	self.htmlOutputInWindow = [[[NSUserDefaults standardUserDefaults] objectForKey:kUserDefaultsHTMLOutputPlacementKey] isEqualToString:@"window"];
}

- (void)applicationDidBecomeActiveNotification:(NSNotification*)aNotification
{
	if(!self.documents.empty())
		[self.textView performSelector:@selector(applicationDidBecomeActiveNotification:) withObject:aNotification];
	[self updatePathDependentProperties];
}

- (void)applicationDidResignActiveNotification:(NSNotification*)aNotification
{
	if(!self.documents.empty())
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
		for(auto document : self.documents)
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
		std::transform(self.documents.begin(), self.documents.end(), inserter(uuids, uuids.end()), [](document::document_ptr const& doc){ return doc->identifier(); });

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
				struct callback_t : document_save_callback_t
				{
					callback_t (void(^callback)(BOOL), size_t count) : _callback([callback copy]), _count(count) { }

					void did_save_document (document::document_ptr document, bool flag, std::string const& message, oak::uuid_t const& filter)
					{
						if(_callback && (_count == 1 || !flag))
							_callback(flag);

						if(--_count == 0 || !flag)
							delete this;
					}

				private:
					void(^_callback)(BOOL);
					size_t _count;
				};

				if(IsInShouldTerminateEventLoop)
				{
					IsInShouldTerminateEventLoop = NO;
					[NSApp replyToApplicationShouldTerminate:NO];
				}

				[DocumentSaveHelper trySaveDocuments:documentsToSave forWindow:self.window defaultDirectory:self.untitledSavePath andCallback:new callback_t(callback, documentsToSave.size())];
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
	std::vector<document::document_ptr> documentsToClose;
	for(NSUInteger index = [anIndexSet firstIndex]; index != NSNotFound; index = [anIndexSet indexGreaterThanIndex:index])
		documentsToClose.push_back([self documents][index]);

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
						if([self documents][index]->is_modified())
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
	NSUInteger newSelectedTabIndex = self.selectedTabIndex;
	oak::uuid_t const selectedUUID = [self documents][self.selectedTabIndex]->identifier();
	for(auto document : self.documents)
	{
		oak::uuid_t const& uuid = document->identifier();
		if(uuids.find(uuid) == uuids.end())
			newDocuments.push_back(document);
		if(selectedUUID == uuid)
			newSelectedTabIndex = newDocuments.empty() ? 0 : newDocuments.size() - 1;
	}

	if(createIfEmptyFlag && newDocuments.empty())
		newDocuments.push_back(create_untitled_document_in_folder(to_s(self.untitledSavePath)));

	self.selectedTabIndex = newSelectedTabIndex;
	self.documents        = newDocuments;

	if(!newDocuments.empty() && newDocuments[newSelectedTabIndex]->identifier() != selectedUUID)
		[self openAndSelectDocument:newDocuments[newSelectedTabIndex]];
}

- (IBAction)performCloseTab:(id)sender
{
	if(self.documents.empty() || self.documents.size() == 1 && (is_disposable(self.selectedDocument) || !self.fileBrowserVisible))
		return [self performCloseWindow:sender];
	NSUInteger index = [sender isKindOfClass:[OakTabBarView class]] ? [sender tag] : self.selectedTabIndex;
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

- (IBAction)performCloseOtherTabs:(id)sender
{
	NSUInteger tabIndex = [sender isKindOfClass:[OakTabBarView class]] ? [sender tag] : self.selectedTabIndex;

	NSMutableIndexSet* otherTabs = [NSMutableIndexSet indexSetWithIndexesInRange:NSMakeRange(0, self.documents.size())];
	[otherTabs removeIndex:tabIndex];
	[self closeTabsAtIndexes:otherTabs askToSaveChanges:YES createDocumentIfEmpty:YES];
}

- (BOOL)windowShouldClose:(id)sender
{
	[self.htmlOutputView stopLoading];

	std::vector<document::document_ptr> documents;
	std::copy_if(self.documents.begin(), self.documents.end(), back_inserter(documents), [](document::document_ptr const& doc){ return doc->is_modified(); });

	if(documents.empty())
		return YES;

	[self showCloseWarningUIForDocuments:documents completionHandler:^(BOOL canClose){
		if(canClose)
			[self.window close];
	}];

	return NO;
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
			iter = _trackedDocuments.insert(std::make_pair(aDocument->identifier(), tracking_info_t(self, aDocument))).first;
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

- (void)documentDidChange:(document::document_ptr const&)aDocument
{
	[self updatePathDependentProperties];
	[self updateFileBrowserStatus:self];
	[self.tabBarView reloadData];
	[[self class] scheduleSessionBackup:self];
}

// ====================
// = Create Documents =
// ====================

- (IBAction)newDocumentInTab:(id)sender
{
	[self takeNewTabIndexFrom:[NSIndexSet indexSetWithIndex:self.selectedTabIndex + 1]];
}

- (IBAction)moveDocumentToNewWindow:(id)sender
{
	if(self.documents.size() > 1)
		[self takeTabsToTearOffFrom:[NSIndexSet indexSetWithIndex:self.selectedTabIndex]];
}

- (IBAction)mergeAllWindows:(id)sender
{
	std::vector<document::document_ptr> documents = self.documents;
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

	std::vector<document::document_ptr> oldDocuments = self.documents;
	NSUInteger split = self.selectedTabIndex;

	if(!oldDocuments.empty() && is_disposable(oldDocuments[split]))
			oldDocuments.erase(oldDocuments.begin() + split);
	else	++split;

	std::vector<document::document_ptr> newDocuments;
	split = merge_documents_splitting_at(oldDocuments, documents, split, newDocuments);

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
			if(uuids.find(newDocuments[i]->identifier()) == uuids.end())
				[indexSet addIndex:i];
		}
		[self closeTabsAtIndexes:indexSet askToSaveChanges:YES createDocumentIfEmpty:NO];
	}
	else
	{
		NSInteger excessTabs = self.documents.size() - self.tabBarView.countOfVisibleTabs;
		if(self.tabBarView && excessTabs > 0)
		{
			std::set<oak::uuid_t> uuids;
			std::transform(documents.begin(), documents.end(), inserter(uuids, uuids.end()), [](document::document_ptr const& doc){ return doc->identifier(); });

			std::multimap<oak::date_t, size_t> ranked;
			for(size_t i = 0; i < newDocuments.size(); ++i)
			{
				document::document_ptr doc = newDocuments[i];
				if(!doc->is_modified() && doc->is_on_disk() && uuids.find(doc->identifier()) == uuids.end())
					ranked.insert(std::make_pair(doc->lru(), i));
			}

			NSMutableIndexSet* indexSet = [NSMutableIndexSet indexSet];
			iterate(pair, ranked)
			{
				[indexSet addIndex:pair->second];
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

namespace
{
	struct save_callback_t : document_save_callback_t
	{
		save_callback_t (DocumentController* self, size_t count) : _self(self), _count(count) { }

		void did_save_document (document::document_ptr document, bool flag, std::string const& message, oak::uuid_t const& filter)
		{
			if(flag)
				[_self documentDidChange:document];
			if(--_count == 0 || !flag)
				delete this;
		}

	private:
		__weak DocumentController* _self;
		size_t _count;
	};
}

- (IBAction)saveDocument:(id)sender
{
	if([self selectedDocument]->path() != NULL_STR)
	{
		[DocumentSaveHelper trySaveDocument:[self selectedDocument] forWindow:self.window defaultDirectory:nil andCallback:new save_callback_t(self, 1)];
	}
	else
	{
		NSString* const suggestedFolder  = self.untitledSavePath;
		NSString* const suggestedName    = DefaultSaveNameForDocument([self selectedDocument]);
		encoding::type suggestedEncoding = [self selectedDocument]->encoding_for_save_as_path(to_s([suggestedFolder stringByAppendingPathComponent:suggestedName]));
		[OakSavePanel showWithPath:suggestedName directory:suggestedFolder fowWindow:self.window encoding:suggestedEncoding completionHandler:^(NSString* path, encoding::type const& encoding){
			if(!path)
				return;

			std::vector<std::string> const& paths = path::expand_braces(to_s(path));
			ASSERT_LT(0, paths.size());

			[self selectedDocument]->set_path(paths[0]);
			[self selectedDocument]->set_disk_encoding(encoding);

			// if([self selectedDocument]->identifier() == scratchDocument)
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

				merge_documents_splitting_at(self.documents, documents, self.selectedTabIndex + 1, newDocuments);
				self.documents = newDocuments;
			}

			[DocumentSaveHelper trySaveDocument:self.selectedDocument forWindow:self.window defaultDirectory:nil andCallback:new save_callback_t(self, 1)];
			[self updatePathDependentProperties];
		}];
	}
}

- (IBAction)saveDocumentAs:(id)sender
{
	std::string const documentPath   = [self selectedDocument]->path();
	NSString* const suggestedFolder  = [NSString stringWithCxxString:path::parent(documentPath)] ?: self.untitledSavePath;
	NSString* const suggestedName    = [NSString stringWithCxxString:path::name(documentPath)]   ?: DefaultSaveNameForDocument([self selectedDocument]);
	encoding::type suggestedEncoding = [self selectedDocument]->encoding_for_save_as_path(to_s([suggestedFolder stringByAppendingPathComponent:suggestedName]));
	[OakSavePanel showWithPath:suggestedName directory:suggestedFolder fowWindow:self.window encoding:suggestedEncoding completionHandler:^(NSString* path, encoding::type const& encoding){
		if(!path)
			return;
		[self selectedDocument]->set_path(to_s(path));
		[self selectedDocument]->set_disk_encoding(encoding);
		[DocumentSaveHelper trySaveDocument:self.selectedDocument forWindow:self.window defaultDirectory:nil andCallback:new save_callback_t(self, 1)];
		[self updatePathDependentProperties];
	}];
}

- (IBAction)saveAllDocuments:(id)sender
{
	std::vector<document::document_ptr> documentsToSave;
	for(auto document : self.documents)
	{
		if(document->is_modified())
			documentsToSave.push_back(document);
	}
	[DocumentSaveHelper trySaveDocuments:documentsToSave forWindow:self.window defaultDirectory:self.untitledSavePath andCallback:new save_callback_t(self, documentsToSave.size())];
}

// ================
// = Window Title =
// ================

- (void)updateProxyIcon
{
	self.window.representedFilename = self.representedFile ?: @"";
	[self.window standardWindowButton:NSWindowDocumentIconButton].image = self.representedFile ? [OakFileIconImage fileIconImageWithPath:self.representedFile isModified:NO] : nil;
}

- (void)setRepresentedFile:(NSString*)aPath
{
	if(![_representedFile isEqualToString:aPath])
	{
		struct scm_callback_t : scm::callback_t
		{
			scm_callback_t (DocumentController* self) : _self(self) { }
	
			void status_changed (scm::info_t const& info, std::set<std::string> const& changedPaths)
			{
				if(changedPaths.find(to_s(_self.representedFile)) != changedPaths.end())
					[_self updateProxyIcon];
			}
	
		private:
			__weak DocumentController* _self;
		};

		if(_scmInfo)
		{
			_scmInfo->remove_callback(_scmCallback);
			_scmInfo.reset();
		}

		if(aPath && ![aPath isEqualToString:@""])
		{
			if(!_scmCallback)
				_scmCallback = new scm_callback_t(self);

			if(_scmInfo = scm::info(path::parent(to_s(aPath))))
				_scmInfo->add_callback(_scmCallback);
		}

		_representedFile = aPath;
		[self updateProxyIcon];
	}
}

- (void)updatePathDependentProperties
{
	document::document_ptr doc = self.selectedDocument;
	if(!doc)
	{
		self.windowTitle      = @"«no documents»";
		self.representedFile  = @"";
		self.isDocumentEdited = NO;
		return;
	}

	std::string docDirectory = doc->path() != NULL_STR ? path::parent(doc->path()) : to_s(self.untitledSavePath);
	self.pathAttributes = [NSString stringWithCxxString:file::path_attributes(doc->path(), docDirectory)];

	std::map<std::string, std::string> map;
	if(doc->path() == NULL_STR)
	{
		if(scm::info_ptr info = scm::info(docDirectory))
			map = info->variables();
	}

	if(NSString* projectPath = self.defaultProjectPath ?: self.fileBrowser.path ?: [NSString stringWithCxxString:path::parent(doc->path())])
		map["projectDirectory"] = to_s(projectPath);

	settings_t const settings = settings_for_path(doc->virtual_path(), doc->file_type() + " " + to_s(self.scopeAttributes), docDirectory, doc->variables(map, false));

	self.projectPath      = [NSString stringWithCxxString:settings.get(kSettingsProjectDirectoryKey, NULL_STR)];
	self.windowTitle      = [NSString stringWithCxxString:settings.get(kSettingsWindowTitleKey, doc->display_name())];
	self.representedFile  = doc->is_on_disk() ? [NSString stringWithCxxString:doc->path()] : nil;
	self.isDocumentEdited = doc->is_modified();
}

// ========================
// = OakTextView Delegate =
// ========================

- (NSString*)scopeAttributes
{
	return self.pathAttributes;
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
		[self.documentView setDocument:_selectedDocument];

	[self updatePathDependentProperties];
	[[self class] scheduleSessionBackup:self];
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
	id res = sender;
	if([sender respondsToSelector:@selector(representedObject)])
		res = [sender representedObject];
	return [res isKindOfClass:[NSIndexSet class]] ? res : nil;
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
		size_t pos = merge_documents_splitting_at(self.documents, make_vector(doc), [indexSet firstIndex], newDocuments);
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
			documents.push_back([self documents][index]);

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

- (NSMenu*)menuForTabBarView:(OakTabBarView*)aTabBarView
{
	NSInteger tabIndex = aTabBarView.tag;
	NSInteger total    = self.documents.size();

	NSMutableIndexSet* newTabAtTab   = tabIndex == -1 ? [NSMutableIndexSet indexSetWithIndex:total] : [NSMutableIndexSet indexSetWithIndex:tabIndex + 1];
	NSMutableIndexSet* clickedTab    = tabIndex == -1 ? [NSMutableIndexSet indexSet] : [NSMutableIndexSet indexSetWithIndex:tabIndex];
	NSMutableIndexSet* otherTabs     = tabIndex == -1 ? [NSMutableIndexSet indexSet] : [NSMutableIndexSet indexSetWithIndexesInRange:NSMakeRange(0, total)];
	NSMutableIndexSet* rightSideTabs = tabIndex == -1 ? [NSMutableIndexSet indexSet] : [NSMutableIndexSet indexSetWithIndexesInRange:NSMakeRange(0, total)];

	if(tabIndex != -1)
	{
		[otherTabs removeIndex:tabIndex];
		[rightSideTabs removeIndexes:[NSIndexSet indexSetWithIndexesInRange:NSMakeRange(0, tabIndex + 1)]];
	}

	SEL closeSingleTabSelector = tabIndex == self.selectedTabIndex ? @selector(performCloseTab:) : @selector(takeTabsToCloseFrom:);

	NSMenu* menu = [NSMenu new];
	[menu setAutoenablesItems:NO];

	[menu addItemWithTitle:@"New Tab"                 action:@selector(takeNewTabIndexFrom:)  keyEquivalent:@""];
	[menu addItem:[NSMenuItem separatorItem]];
	[menu addItemWithTitle:@"Close Tab"               action:closeSingleTabSelector           keyEquivalent:@""];
	[menu addItemWithTitle:@"Close Other Tabs"        action:@selector(takeTabsToCloseFrom:)  keyEquivalent:@""];
	[menu addItemWithTitle:@"Close Tabs to the Right" action:@selector(takeTabsToCloseFrom:)  keyEquivalent:@""];
	[menu addItem:[NSMenuItem separatorItem]];
	[menu addItemWithTitle:@"Move Tab to New Window"  action:@selector(takeTabsToTearOffFrom:) keyEquivalent:@""];

	NSIndexSet* indexSets[] = { newTabAtTab, nil, clickedTab, otherTabs, rightSideTabs, nil, total > 1 ? clickedTab : [NSIndexSet indexSet] };
	for(size_t i = 0; i < sizeofA(indexSets); ++i)
	{
		if(NSIndexSet* indexSet = indexSets[i])
		{
			if([indexSet count] == 0)
					[[menu itemAtIndex:i] setEnabled:NO];
			else	[[menu itemAtIndex:i] setRepresentedObject:indexSet];
		}
	}

	return menu;
}

// =========================
// = OakTabBarViewDelegate =
// =========================

- (BOOL)tabBarView:(OakTabBarView*)aTabBarView shouldSelectIndex:(NSUInteger)anIndex
{
	[self openAndSelectDocument:[self documents][anIndex]];
	self.selectedTabIndex = anIndex;
	return YES;
}

- (void)tabBarView:(OakTabBarView*)aTabBarView didDoubleClickIndex:(NSUInteger)anIndex
{
	if(self.documents.size() > 1)
		[self takeTabsToTearOffFrom:[NSMutableIndexSet indexSetWithIndex:anIndex]];
}

- (void)tabBarViewDidDoubleClick:(OakTabBarView*)aTabBarView
{
	[self takeNewTabIndexFrom:[NSMutableIndexSet indexSetWithIndex:self.documents.size()]];
}

// ================
// = Tab Dragging =
// ================

- (void)setupPasteboard:(NSPasteboard*)aPasteboard forTabAtIndex:(NSUInteger)draggedTabIndex
{
	document::document_ptr document = [self documents][draggedTabIndex];
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
	merge_documents_splitting_at(self.documents, make_vector(document::find(docId)), droppedIndex, newDocuments);
	self.documents = newDocuments;

	oak::uuid_t selectedUUID = [self selectedDocument]->identifier();
	auto iter = std::find_if(newDocuments.begin(), newDocuments.end(), [&selectedUUID](document::document_ptr const& doc){ return doc->identifier() == selectedUUID; });
	if(iter != newDocuments.end())
		self.selectedTabIndex = iter - newDocuments.begin();

	oak::uuid_t srcProjectId = to_s((NSString*)plist[@"collection"]);
	if(operation == NSDragOperationMove && srcProjectId != to_s(self.identifier))
	{
		for(DocumentController* delegate in SortedControllers())
		{
			if(srcProjectId == oak::uuid_t(to_s(delegate.identifier)))
				return [delegate closeTabsAtIndexes:[NSIndexSet indexSetWithIndex:[plist[@"index"] unsignedIntValue]] askToSaveChanges:NO createDocumentIfEmpty:YES], YES;
		}
	}

	return YES;
}

- (IBAction)selectNextTab:(id)sender            { self.selectedTabIndex = (_selectedTabIndex + 1) % _documents.size();                     [self openAndSelectDocument:_documents[_selectedTabIndex]]; }
- (IBAction)selectPreviousTab:(id)sender        { self.selectedTabIndex = (_selectedTabIndex + _documents.size() - 1) % _documents.size(); [self openAndSelectDocument:_documents[_selectedTabIndex]]; }
- (IBAction)takeSelectedTabIndexFrom:(id)sender { self.selectedTabIndex = [[OakSubmenuController sharedInstance] tagForSender:sender];     [self openAndSelectDocument:_documents[_selectedTabIndex]]; }

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
	auto documents = self.documents;
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
		self.layoutView.fileBrowserView = makeVisibleFlag ? self.fileBrowser.view : nil;

		if(makeVisibleFlag)
		{
			self.fileBrowser.nextResponder = self.fileBrowser.view.nextResponder;
			self.fileBrowser.view.nextResponder = self.fileBrowser;
		}
	}
	[[self class] scheduleSessionBackup:self];
}

- (IBAction)toggleFileBrowser:(id)sender    { self.fileBrowserVisible = !self.fileBrowserVisible; }

- (void)updateFileBrowserStatus:(id)sender
{
	NSMutableArray* openURLs     = [NSMutableArray array];
	NSMutableArray* modifiedURLs = [NSMutableArray array];
	for(auto document : self.documents)
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

- (IBAction)reload:(id)sender               { [NSApp sendAction:_cmd to:self.fileBrowser from:sender]; }

- (IBAction)revealFileInProject:(id)sender  { self.fileBrowserVisible = YES; [self.fileBrowser selectURL:[NSURL fileURLWithPath:[NSString stringWithCxxString:[self selectedDocument]->path()]] withParentURL:[NSURL fileURLWithPath:self.projectPath]]; }
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
			if(!self.htmlOutputView)
				self.htmlOutputView = [[OakHTMLOutputView alloc] initWithFrame:NSZeroRect];
			self.layoutView.htmlOutputView = self.htmlOutputView;
		}
	}
	else
	{
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
	self.htmlOutputVisible = !self.htmlOutputVisible;
}

- (BOOL)setCommandRunner:(command::runner_ptr const&)aRunner
{
	if(self.htmlOutputInWindow)
	{
		_runner = aRunner;

		if(!self.htmlOutputWindowController || [self.htmlOutputWindowController running])
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

- (void)setFilterWindowController:(OakFilterWindowController*)controller
{
	if(_filterWindowController != controller)
	{
		if(_filterWindowController)
		{
			[[NSNotificationCenter defaultCenter] removeObserver:self name:NSWindowWillCloseNotification object:_filterWindowController.window];
			_filterWindowController.target = nil;
			[_filterWindowController close];
		}

		if(_filterWindowController = controller)
			[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(filterWindowWillClose:) name:NSWindowWillCloseNotification object:_filterWindowController.window];
	}
}

- (void)filterWindowWillClose:(NSNotification*)notification
{
	self.filterWindowController = nil;
}

- (IBAction)orderFrontFindPanel:(id)sender
{
	Find* find              = [Find sharedInstance];
	find.documentIdentifier = [NSString stringWithCxxString:[self selectedDocument]->identifier()];
	find.projectFolder      = self.projectPath ?: self.untitledSavePath ?: NSHomeDirectory();
	find.projectIdentifier  = self.identifier;

	NSInteger mode = [sender respondsToSelector:@selector(tag)] ? [sender tag] : find_tags::in_document;
	if(mode == find_tags::in_folder)
		return [find showFolderSelectionPanel:self];

	if(mode == find_tags::in_document && self.textView.hasMultiLineSelection)
		mode = find_tags::in_selection;

	switch(mode)
	{
		case find_tags::in_document:
			find.searchScope = find::in::document;
		break;

		case find_tags::in_selection:
			find.searchScope = find::in::selection;
		break;

		case find_tags::in_project:
		{
			find.searchScope = find::in::folder;
			if(!find.isVisible)
			{
				BOOL fileBrowserHasFocus = [self.window.firstResponder respondsToSelector:@selector(isDescendantOf:)] && [(NSView*)self.window.firstResponder isDescendantOf:self.fileBrowser.view];
				find.searchFolder = fileBrowserHasFocus ? self.untitledSavePath : find.projectFolder;
			}
		}
		break;
	}
	[find showFindPanel:self];
}

- (IBAction)showSymbolChooser:(id)sender
{
	self.filterWindowController                         = [OakFilterWindowController new];
	self.filterWindowController.dataSource              = [SymbolChooser symbolChooserForDocument:[self selectedDocument]];
	self.filterWindowController.action                  = @selector(symbolChooserDidSelectItems:);
	self.filterWindowController.sendActionOnSingleClick = YES;
	[self.filterWindowController showWindowRelativeToWindow:self.window];
}

- (void)symbolChooserDidSelectItems:(id)sender
{
	[self openItems:[sender selectedItems] closingOtherTabs:NO];
}

// ==================
// = OakFileChooser =
// ==================

- (IBAction)goToFile:(id)sender
{
	FileChooser* fc = [FileChooser new];

	if(OakPasteboardEntry* entry = [[OakPasteboard pasteboardWithName:NSFindPboard] current])
	{
		std::string str = to_s(entry.string);
		if(regexp::search("\\A.*?(\\.|/).*?:\\d+\\z", str.data(), str.data() + str.size()))
			fc.filterString = entry.string;
	}

	fc.openDocuments   = _documents;
	fc.currentDocument = _selectedDocument ? _selectedDocument->identifier() : oak::uuid_t();
	fc.target          = self;
	fc.action          = @selector(fileChooserDidSelectItems:);
	fc.path            = self.projectPath ?: self.untitledSavePath ?: NSHomeDirectory();

	[fc showWindowRelativeToWindow:self.window];
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
		if([selectedURLs count] == 1 && [[selectedURLs lastObject] isFileURL] && path::is_directory([[[selectedURLs lastObject] path] fileSystemRepresentation]))
			res = [[selectedURLs lastObject] path];
		else if(NSString* folder = self.fileBrowser.path)
			res = folder;
	}
	return res;
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

- (IBAction)goToFileCounterpart:(id)sender
{
	std::string const documentPath = [self selectedDocument]->path();
	if(documentPath == NULL_STR)
		return (void)NSBeep();

	std::string const documentDir  = path::parent(documentPath);
	std::string const documentName = path::name(documentPath);
	std::string const documentBase = path::strip_extensions(documentName);

	std::set<std::string> candidates(&documentName, &documentName + 1);
	for(auto document : self.documents)
	{
		if(documentDir == path::parent(document->path()) && documentBase == path::strip_extensions(path::name(document->path())))
			candidates.insert(path::name(document->path()));
	}

	citerate(entry, path::entries(documentDir))
	{
		std::string const name = (*entry)->d_name;
		if((*entry)->d_type == DT_REG && documentBase == path::strip_extensions(name) && path::extensions(name) != "")
		{
			std::string const content = path::content(path::join(documentDir, name));
			if(utf8::is_valid(content.data(), content.data() + content.size()))
				candidates.insert(name);
		}
	}

	settings_t const settings = [self selectedDocument]->settings();
	path::glob_t const excludeGlob(settings.get(kSettingsExcludeKey, ""));
	path::glob_t const binaryGlob(settings.get(kSettingsBinaryKey, ""));

	std::vector<std::string> v;
	iterate(path, candidates)
	{
		if(*path == documentPath || !binaryGlob.does_match(*path) && !excludeGlob.does_match(*path))
			v.push_back(*path);
	}

	if(v.size() == 1)
		return (void)NSBeep();

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
	for(auto document : self.documents)
	{
		NSMenuItem* item = [aMenu addItemWithTitle:[NSString stringWithCxxString:document->display_name()] action:@selector(takeSelectedTabIndexFrom:) keyEquivalent:i < 10 ? [NSString stringWithFormat:@"%c", '0' + ((i+1) % 10)] : @""];
		item.tag = i;
		item.toolTip = [[NSString stringWithCxxString:document->path()] stringByAbbreviatingWithTildeInPath];
		if(i == self.selectedTabIndex)
			[item setState:NSOnState];
		else if(document->is_modified())
			[item setModifiedState:YES];
		++i;
	}

	if(i == 0)
		[aMenu addItemWithTitle:@"No Tabs Open" action:@selector(nop:) keyEquivalent:@""];
}

// ====================
// = NSMenuValidation =
// ====================

- (BOOL)validateMenuItem:(NSMenuItem*)menuItem;
{
	BOOL active = YES;
	if([menuItem action] == @selector(toggleFileBrowser:))
		[menuItem setTitle:self.fileBrowserVisible ? @"Hide File Browser" : @"Show File Browser"];
	else if([menuItem action] == @selector(toggleHTMLOutput:))
	{
		[menuItem setTitle:self.htmlOutputVisible ? @"Hide HTML Output" : @"Show HTML Output"];
		active = !self.htmlOutputInWindow || self.htmlOutputWindowController;
	}
	else if([menuItem action] == @selector(moveDocumentToNewWindow:))
		active = self.documents.size() > 1;
	else if([menuItem action] == @selector(revealFileInProject:) || [menuItem action] == @selector(revealFileInProjectByExpandingAncestors:))
		active = [self selectedDocument]->path() != NULL_STR;
	else if([menuItem action] == @selector(goToProjectFolder:))
		active = self.projectPath != nil;
	else if([menuItem action] == @selector(goToParentFolder:))
		active = [self.window firstResponder] != self.textView;
	else if([menuItem action] == @selector(reload:))
		active = self.fileBrowserVisible;
	return active;
}

// =============
// = QuickLook =
// =============

- (void)toggleQuickLookPreview:(id)sender
{
	if([QLPreviewPanel sharedPreviewPanelExists] && [[QLPreviewPanel sharedPreviewPanel] isVisible])
			[[QLPreviewPanel sharedPreviewPanel] orderOut:nil];
	else	[[QLPreviewPanel sharedPreviewPanel] makeKeyAndOrderFront:nil];
}

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
	static NSString* const WindowNotifications[] = { NSWindowDidBecomeKeyNotification, NSWindowDidDeminiaturizeNotification, NSWindowDidExposeNotification, NSWindowDidMiniaturizeNotification, NSWindowDidMoveNotification, NSWindowDidResizeNotification, NSWindowWillCloseNotification };
	iterate(notification, WindowNotifications)
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(scheduleSessionBackup:) name:*notification object:nil];
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

+ (BOOL)restoreSession
{
	BOOL res = NO;
	++DisableSessionSavingCount;

	NSDictionary* session = [NSDictionary dictionaryWithContentsOfFile:[self sessionPath]];
	for(NSDictionary* project in session[@"projects"])
	{
		DocumentController* controller = [DocumentController new];

		if(NSString* windowFrame = project[@"windowFrame"])
			[controller.window setFrame:NSRectFromString(windowFrame) display:NO];
		if(NSString* fileBrowserWidth = project[@"fileBrowserWidth"])
			controller.fileBrowserWidth = [fileBrowserWidth floatValue];
		if(NSString* htmlOutputSize = project[@"htmlOutputSize"])
			controller.htmlOutputSize = NSSizeFromString(htmlOutputSize);

		controller.defaultProjectPath = project[@"projectPath"];
		controller.fileBrowserHistory = project[@"fileBrowserState"];
		controller.fileBrowserVisible = [project[@"fileBrowserVisible"] boolValue];

		std::vector<document::document_ptr> documents;
		NSInteger selectedTabIndex = 0;

		for(NSDictionary* info in project[@"documents"])
		{
			document::document_ptr doc;
			NSString* identifier = info[@"identifier"];
			if(!identifier || !(doc = document::find(to_s(identifier), true)))
			{
				NSString* path = info[@"path"];
				doc = path ? document::create(to_s(path)) : create_untitled_document_in_folder(to_s(controller.untitledSavePath));
				if(NSString* displayName = info[@"displayName"])
					doc->set_custom_name(to_s(displayName));
			}

			doc->set_recent_tracking(false);
			documents.push_back(doc);

			if([info[@"selected"] boolValue])
				selectedTabIndex = documents.size() - 1;
		}

		if(documents.empty())
			documents.push_back(create_untitled_document_in_folder(to_s(controller.untitledSavePath)));

		controller.documents        = documents;
		controller.selectedTabIndex = selectedTabIndex;

		[controller openAndSelectDocument:documents[selectedTabIndex]];
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

+ (BOOL)saveSessionIncludingUntitledDocuments:(BOOL)includeUntitled
{
	if(DisableSessionSavingCount)
		return NO;

	NSMutableArray* projects = [NSMutableArray array];
	for(DocumentController* controller in [SortedControllers() reverseObjectEnumerator])
	{
		NSMutableDictionary* res = [NSMutableDictionary dictionary];

		if(NSString* projectPath = controller.defaultProjectPath)
			res[@"projectPath"] = projectPath;
		if(NSDictionary* history = controller.fileBrowserHistory)
			res[@"fileBrowserState"] = history;

		if(([controller.window styleMask] & NSFullScreenWindowMask) == NSFullScreenWindowMask)
				res[@"fullScreen"] = @YES;
		else	res[@"windowFrame"] = NSStringFromRect([controller.window frame]);

		res[@"miniaturized"]       = @([controller.window isMiniaturized]);
		res[@"htmlOutputSize"]     = NSStringFromSize(controller.htmlOutputSize);
		res[@"fileBrowserVisible"] = @(controller.fileBrowserVisible);
		res[@"fileBrowserWidth"]   = @(controller.fileBrowserWidth);

		NSMutableArray* docs = [NSMutableArray array];
		for(auto document : controller.documents)
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
			if(document == controller.selectedDocument)
				doc[@"selected"] = @YES;
			[docs addObject:doc];
		}
		res[@"documents"] = docs;
		[projects addObject:res];
	}

	NSDictionary* session = @{ @"projects" : projects };
	return [session writeToFile:[self sessionPath] atomically:YES];
}

// ==========
// = Legacy =
// ==========

- (void)updateVariables:(std::map<std::string, std::string>&)env
{
	[self.fileBrowser updateVariables:env];

	if(NSString* projectDir = self.projectPath)
	{
		env["TM_PROJECT_DIRECTORY"] = [projectDir fileSystemRepresentation];
		env["TM_PROJECT_UUID"]      = to_s(self.identifier);
	}

	if(auto theme = self.textView.theme)
	{
		if(auto themeItem = bundles::lookup(theme->uuid()))
		{
			if(!themeItem->paths().empty())
				env["TM_CURRENT_THEME_PATH"] = themeItem->paths().back();
		}
	}
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
				__block id observerId = [[NSNotificationCenter defaultCenter] addObserverForName:NSApplicationDidUnhideNotification object:NSApp queue:nil usingBlock:^(NSNotification*){
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
					iterate(parent, parents)
					{
						if(parent->find(projectPath) == 0 && (parent->size() == projectPath.size() || (*parent)[projectPath.size()] == '/'))
							candidates.insert(std::make_pair(parent->size() - projectPath.size(), candidate));
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
				split = merge_documents_splitting_at(oldDocuments, documents, split, newDocuments);
				controller.documents = newDocuments;
				controller.selectedTabIndex = split;
			}
			return controller;
		}

	public:
		void show_browser (std::string const& path) const
		{
			std::string const folder = path::resolve(path);

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

			controller.defaultProjectPath = [NSString stringWithCxxString:folder];
			controller.fileBrowserVisible = YES;
			controller.documents          = make_vector(create_untitled_document_in_folder(folder));
			controller.fileBrowser.url    = [NSURL fileURLWithPath:[NSString stringWithCxxString:folder]];

			[controller openAndSelectDocument:[controller documents][controller.selectedTabIndex]];
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

		void run (bundle_command_t const& command, ng::buffer_t const& buffer, ng::ranges_t const& selection, document::document_ptr document, std::map<std::string, std::string> const& env, document::run_callback_ptr callback)
		{
			::run(command, buffer, selection, document, env, callback);
		}

	} proxy;

	document::set_ui_proxy(&proxy);
}
@end
