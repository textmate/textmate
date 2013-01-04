#import "DocumentController.h"
#import "DocumentTabs.h"
#import "DocumentSaveHelper.h"
#import "DocumentCommand.h"
#import "ProjectLayoutView.h"
#import <HTMLOutputWindow/HTMLOutputWindow.h>

// #import "AppController.h" // find_tags
namespace find_tags
{
	enum
	{
		in_document = 1,
		in_selection,
		in_project,
		in_folder,
	};
}

#import <OakAppKit/NSMenuItem Additions.h>
#import <OakAppKit/OakSubmenuController.h>
#import <OakAppKit/OakSavePanel.h>
#import <OakFoundation/OakFoundation.h>
#import <OakFoundation/NSArray Additions.h>
#import <OakFoundation/NSString Additions.h>
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/NSAlert Additions.h>
#import <OakAppKit/NSMenu Additions.h>
#import <OakAppKit/NSWindow Additions.h>
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/OakFileIconImage.h>
#import <OakAppKit/OakWindowFrameHelper.h>
#import <OakAppKit/OakPasteboard.h>
#import <HTMLOutput/HTMLOutput.h>
#import <Find/Find.h>
#import <text/utf8.h>
#import <OakFilterList/OakFilterList.h>
#import <OakFilterList/OakFileChooser.h>
#import <OakFilterList/SymbolChooser.h>
#import <Preferences/Keys.h>
#import <ns/ns.h>
#import <plist/stl.h>
#import <io/entries.h>
#import <regexp/glob.h>
#import <document/session.h>
#import <document/collection.h>
#import <file/path_info.h>

OAK_DEBUG_VAR(DocumentController);

// ============================
// = Document Binding Support =
// ============================

@interface DocumentController ()
@property (nonatomic, retain) NSString* windowTitle;
@property (nonatomic, retain) NSString* representedFile;
@property (nonatomic, assign) BOOL isDocumentEdited;
@property (nonatomic, retain) DocumentController* retainedSelf;
@property (nonatomic, retain) OakFilterWindowController* filterWindowController;
@property (nonatomic, retain) NSString* pathAttributes;
- (id)initWithDocuments:(std::vector<document::document_ptr> const&)someDocuments;
- (void)updateProxyIcon;
- (void)takeTabsToTearOffFrom:(id)sender;
@end

@implementation DocumentController (UpdateDocumentBindingsPartOne)
- (void)addSessionInfoTo:(plist::array_t&)anArray includeUntitled:(BOOL)includeUntitled
{
	plist::dictionary_t res;

	res["windowFrame"]        = to_s(NSStringFromRect([self.window frame]));
	res["miniaturized"]       = [self.window isMiniaturized];
	res["htmlOutputSize"]     = to_s(NSStringFromSize(self.htmlOutputSize));
	res["fileBrowserVisible"] = self.fileBrowserVisible;
	res["fileBrowserWidth"]   = (int32_t)self.fileBrowserWidth;

	if(CFDictionaryRef fbState = (CFDictionaryRef)CFBridgingRetain(self.fileBrowserHistory))
	{
		res["fileBrowserState"] = plist::convert(fbState);
		CFRelease(fbState);
	}

	plist::array_t docs;
	iterate(tab, documentTabs)
	{
		document::document_ptr document = **tab;
		if(!includeUntitled && (document->path() == NULL_STR || !path::exists(document->path())))
			continue;

		plist::dictionary_t doc;
		if(document->is_modified() || document->path() == NULL_STR)
		{
			doc["identifier"] = std::string(document->identifier());
			if(document->is_open())
				document->backup();
		}
		if(document->path() != NULL_STR)
			doc["path"] = document->path();
		if(document->display_name() != NULL_STR)
			doc["displayName"] = document->display_name();
		if(tab - documentTabs.begin() == selectedTabIndex)
			doc["selected"] = true;
		docs.push_back(doc);
	}
	res["documents"] = docs;

	if(!docs.empty())
		anArray.push_back(res);
}

- (void)observeValueForKeyPath:(NSString*)keyPath ofObject:(id)object change:(NSDictionary*)change context:(void*)context
{
	if(object == fileBrowser)
		document::schedule_session_backup();
}
@end

// ============================

static document::document_ptr create_document (NSString* fileBrowserPath)
{
	document::document_ptr doc = document::from_content("", settings_for_path(NULL_STR, file::path_attributes(NULL_STR), to_s(fileBrowserPath)).get(kSettingsFileTypeKey, "text.plain"));
	doc->set_recent_tracking(false);
	return doc;
}

@implementation DocumentController
@synthesize filterWindowController;

+ (void)load
{
	static struct proxy_t : document::ui_proxy_t
	{
	// private:
		static void bring_to_front (DocumentController* aController)
		{
			if([NSApp isHidden])
			{
				__block id observerId = [[NSNotificationCenter defaultCenter] addObserverForName:NSApplicationDidUnhideNotification object:NSApp queue:nil usingBlock:^(NSNotification*){
					[[aController window] makeKeyAndOrderFront:nil];
					SetFrontProcessWithOptions(&(ProcessSerialNumber){ 0, kCurrentProcess }, kSetFrontProcessFrontWindowOnly);
					[[NSNotificationCenter defaultCenter] removeObserver:observerId];
				}];
				[NSApp unhideWithoutActivation];
			}
			else
			{
				[[aController window] makeKeyAndOrderFront:nil];
				SetFrontProcessWithOptions(&(ProcessSerialNumber){ 0, kCurrentProcess }, kSetFrontProcessFrontWindowOnly);
			}
		}

		static void close_scratch_project ()
		{
			for(NSWindow* window in [NSApp orderedWindows])
			{
				DocumentController* delegate = (DocumentController*)[window delegate];
				if([window isMiniaturized] || ![delegate isKindOfClass:[DocumentController class]])
					continue;

				if(delegate->documentTabs.size() == 1 && !delegate.fileBrowserVisible)
				{
					document::document_ptr document = *delegate->documentTabs[0];
					if(document->identifier() == delegate->scratchDocument && !document->is_modified() && document->path() == NULL_STR)
						[[delegate window] close];
				}

				break;
			}
		}

	public:
		void show_documents (std::vector<document::document_ptr> const& documents, std::string const& browserPath)
		{
			if(browserPath == NULL_STR && !documents.empty())
			{
				if(DocumentController* delegate = [DocumentController controllerForDocument:documents.back()])
				{
					[delegate addDocuments:documents andSelect:kSelectDocumentFirst closeOther:NO pruneTabBar:YES];
					bring_to_front(delegate);
				}
				else
				{
					close_scratch_project();
					bring_to_front([[DocumentController alloc] initWithDocuments:documents]);
				}
			}
			else if(DocumentController* delegate = [DocumentController controllerForPath:browserPath])
			{
				if(!documents.empty())
					[delegate addDocuments:documents andSelect:kSelectDocumentFirst closeOther:NO pruneTabBar:YES];
				bring_to_front(delegate);
			}
			else // if(browserPath != NULL_STR)
			{
				close_scratch_project();
				delegate = documents.empty() ? [[DocumentController alloc] init] : [[DocumentController alloc] initWithDocuments:documents];
				[delegate window];
				delegate.fileBrowserVisible = YES;
				[delegate->fileBrowser showURL:[NSURL fileURLWithPath:[NSString stringWithCxxString:path::resolve(browserPath)]]];
				[delegate synchronizeWindowTitle];
				bring_to_front(delegate);
				[[NSDocumentController sharedDocumentController] noteNewRecentDocumentURL:[NSURL fileURLWithPath:[NSString stringWithCxxString:path::resolve(browserPath)]]]; 
			}
		}

		void show_document (oak::uuid_t const& collection, document::document_ptr document, text::range_t const& range, bool bringToFront) const
		{
			DocumentController* delegate = nil;
			if(collection == document::kCollectionCurrent)
				delegate = [DocumentController controllerForDocument:document];
			else if(collection != document::kCollectionNew)
				delegate = [DocumentController controllerForUUID:collection];

			if(range != text::range_t::undefined)
				document->set_selection(range);

			if(delegate)
			{
				[delegate addDocuments:std::vector<document::document_ptr>(1, document) andSelect:kSelectDocumentFirst closeOther:NO pruneTabBar:YES];
				if(bringToFront)
					bring_to_front(delegate);
			}
			else
			{
				close_scratch_project();
				delegate = [[DocumentController alloc] initWithDocuments:std::vector<document::document_ptr>(1, document)];
				[delegate showWindow:nil];
			}
		}

		void run (bundle_command_t const& command, ng::buffer_t const& buffer, ng::ranges_t const& selection, document::document_ptr document, std::map<std::string, std::string> const& env, document::run_callback_ptr callback)
		{
			::run(command, buffer, selection, document, env, callback);
		}

		bool load_session (std::string const& path) const
		{
			bool res = false;
			plist::dictionary_t session = plist::load(path);
			plist::array_t projects;
			if(plist::get_key_path(session, "projects", projects))
			{
				iterate(project, projects)
				{
					NSInteger selectedTabIndex = 0;
					std::vector<document::document_ptr> documents;
					plist::array_t docsArray;
					if(plist::get_key_path(*project, "documents", docsArray))
					{
						iterate(document, docsArray)
						{
							document::document_ptr doc;
							std::string str;
							if(plist::get_key_path(*document, "identifier", str) && (doc = document::find(oak::uuid_t(str))))
								documents.push_back(doc);
							else if(plist::get_key_path(*document, "path", str))
								documents.push_back(document::create(str));
							else if(plist::get_key_path(*document, "displayName", str))
							{
								documents.push_back(document::create());
								documents.back()->set_custom_name(str);
							}
							else
								continue;

							documents.back()->set_recent_tracking(false);

							bool flag;
							if(plist::get_key_path(*document, "selected", flag) && flag)
								selectedTabIndex = documents.size() - 1;
						}
					}

					if(documents.empty())
						documents.push_back(document::create());

					DocumentController* controller = [[DocumentController alloc] initWithDocuments:documents];
					controller.selectedTabIndex = selectedTabIndex;

					plist::dictionary_t fileBrowserState;
					if(plist::get_key_path(*project, "fileBrowserState", fileBrowserState))
						controller.fileBrowserHistory = ns::to_dictionary(fileBrowserState);

					CGFloat size;
					if(plist::get_key_path(*project, "fileBrowserWidth", size))
						controller.fileBrowserWidth = size;

					std::string str;
					if(plist::get_key_path(*project, "htmlOutputSize", str))
						controller.htmlOutputSize = NSSizeFromString([NSString stringWithCxxString:str]);

					std::string windowFrame = NULL_STR;
					if(plist::get_key_path(*project, "windowFrame", windowFrame))
						[[controller window] setFrame:NSRectFromString([NSString stringWithCxxString:windowFrame]) display:NO];

					bool fileBrowserVisible = false;
					if(plist::get_key_path(*project, "fileBrowserVisible", fileBrowserVisible) && fileBrowserVisible)
						controller.fileBrowserVisible = YES;

					[controller showWindow:nil];

					bool isMiniaturized = false;
					if(plist::get_key_path(*project, "miniaturized", isMiniaturized) && isMiniaturized)
						[[controller window] miniaturize:nil];

					res = true;
				}
			}

			return res;
		}

		bool save_session (std::string const& path, bool includeUntitled) const
		{
			plist::array_t projects;
			for(NSWindow* window in [[[NSApp orderedWindows] reverseObjectEnumerator] allObjects])
			{
				DocumentController* controller = (DocumentController*)[window delegate];
				if([controller isKindOfClass:[DocumentController class]])
					[controller addSessionInfoTo:projects includeUntitled:includeUntitled];
			}

			plist::dictionary_t session;
			session["projects"] = projects;
			return plist::save(path, session);
		}

	} proxy;

	document::set_ui_proxy(&proxy);

	[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(applicationDidBecomeActiveNotification:) name:NSApplicationDidBecomeActiveNotification object:NSApp];
	[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(applicationDidResignActiveNotification:) name:NSApplicationDidResignActiveNotification object:NSApp];
}

+ (void)applicationDidBecomeActiveNotification:(NSNotification*)aNotification
{
	for(NSWindow* window in [NSApp orderedWindows])
	{
		DocumentController* controller = (DocumentController*)[window delegate];
		if([controller isKindOfClass:[DocumentController class]])
			[controller performSelector:@selector(applicationDidBecomeActiveNotification:) withObject:aNotification];
	}
}

+ (void)applicationDidResignActiveNotification:(NSNotification*)aNotification
{
	for(NSWindow* window in [NSApp orderedWindows])
	{
		DocumentController* controller = (DocumentController*)[window delegate];
		if([controller isKindOfClass:[DocumentController class]] && !controller->documentTabs.empty())
			[controller performSelector:@selector(applicationDidResignActiveNotification:) withObject:aNotification];
	}
}

- (void)applicationDidBecomeActiveNotification:(NSNotification*)aNotification
{
	if(!documentTabs.empty())
		[textView performSelector:@selector(applicationDidBecomeActiveNotification:) withObject:aNotification];
	[self synchronizeWindowTitle];
}

- (void)applicationDidResignActiveNotification:(NSNotification*)aNotification
{
	[textView performSelector:@selector(applicationDidResignActiveNotification:) withObject:aNotification];
}

// =========
// = Setup =
// =========

- (id)initWithDocuments:(std::vector<document::document_ptr> const&)someDocuments
{
	D(DBF_DocumentController, bug("%zu documents\n", someDocuments.size()););
	if(self = [super initWithWindow:[[NSWindow alloc] initWithContentRect:NSZeroRect styleMask:(NSTitledWindowMask|NSClosableWindowMask|NSResizableWindowMask|NSMiniaturizableWindowMask|NSTexturedBackgroundWindowMask) backing:NSBackingStoreBuffered defer:NO]])
	{
		_identifier = [NSString stringWithCxxString:oak::uuid_t().generate()];
		[self addDocuments:someDocuments andSelect:kSelectDocumentFirst closeOther:YES pruneTabBar:YES];

		layoutView = [[ProjectLayoutView alloc] initWithFrame:NSZeroRect];

		[self.window setCollectionBehavior:NSWindowCollectionBehaviorFullScreenPrimary];
		[self.window setContentView:layoutView];
		[self.window setDelegate:self];
		[self.window setAutorecalculatesKeyViewLoop:YES];
		[self.window bind:@"title" toObject:self withKeyPath:@"windowTitle" options:nil];
		[self.window bind:@"documentEdited" toObject:self withKeyPath:@"isDocumentEdited" options:nil];

		tabBarView = [[OakTabBarView alloc] initWithFrame:NSZeroRect];
		layoutView.tabBarView = tabBarView;

		[self windowDidLoad];
	}
	return self;
}

- (id)init
{
	D(DBF_DocumentController, bug("\n"););
	document::document_ptr document = document::create();
	if(self = [self initWithDocuments:std::vector<document::document_ptr>(1, document)])
		scratchDocument = document->identifier();
	return self;
}

+ (DocumentController*)controllerForDocument:(document::document_ptr const&)aDocument
{
	if(!aDocument)
		return nil;

	for(NSWindow* window in [NSApp orderedWindows])
	{
		DocumentController* delegate = (DocumentController*)[window delegate];
		if([delegate isKindOfClass:self])
		{
			if(delegate.fileBrowserVisible && aDocument->path() != NULL_STR && aDocument->path().find(to_s(delegate.projectPath)) == 0)
				return delegate;

			iterate(tab, delegate->documentTabs)
			{
				document::document_ptr document = **tab;
				if(*document == *aDocument)
					return delegate;
			}
		}
	}
	return nil;
}

+ (DocumentController*)controllerForPath:(std::string const&)aPath
{
	NSURL* url = [NSURL fileURLWithPath:[NSString stringWithCxxString:path::resolve(aPath)]];
	NSString* path = [url isFileURL] ? [url path] : nil;
	for(NSWindow* window in [NSApp orderedWindows])
	{
		DocumentController* delegate = (DocumentController*)[window delegate];
		if([delegate isKindOfClass:self])
		{
			if(delegate.fileBrowserVisible && [path isEqualToString:delegate.fileBrowserPath])
				return delegate;
		}
	}
	return nil;
}

+ (DocumentController*)controllerForUUID:(oak::uuid_t const&)aUUID
{
	for(NSWindow* window in [NSApp orderedWindows])
	{
		if([[window delegate] isKindOfClass:self] && oak::uuid_t(to_s([(DocumentController*)[window delegate] identifier])) == aUUID)
			return (DocumentController*)[window delegate];
	}
	return nil;
}

- (void)windowDidLoad
{
	D(DBF_DocumentController, bug("\n"););
	[OakWindowFrameHelper windowFrameHelperWithWindow:[self window]];

	documentView = [[OakDocumentView alloc] init];
	layoutView.documentView = documentView;

	textView = documentView.textView;
	textView.delegate = self;

	tabBarView.delegate   = self;
	tabBarView.dataSource = self;
	[tabBarView reloadData]; // FIXME this should be implicit

	self.selectedTabIndex = selectedTabIndex;
	[self updateProxyIcon];

	self.retainedSelf = self;
}

- (void)synchronizeWindowTitle
{
	if(selectedTabIndex >= documentTabs.size())
	{
		fprintf(stderr, "*** error: selected tab out of bounds: %zu >= %zu\n", selectedTabIndex, documentTabs.size());
		return;
	}

	document::document_ptr doc = [self selectedDocument];

	if(!doc)
	{
		fprintf(stderr, "*** error: no document (synchronizeWindowTitle)\n");
		return;
	}

	std::string docDirectory = doc->path() != NULL_STR ? path::parent(doc->path()) : to_s(self.untitledSavePath);

	std::map<std::string, std::string> map;
	if(doc->path() == NULL_STR)
	{
		if(scm::info_ptr info = scm::info(docDirectory))
		{
			std::string const& branch = info->branch();
			if(branch != NULL_STR)
				map["TM_SCM_BRANCH"] = branch;

			std::string const& name = info->scm_name();
			if(name != NULL_STR)
				map["TM_SCM_NAME"] = name;
		}
	}

	auto settings = settings_for_path(doc->virtual_path(), doc->file_type() + " " + to_s([self scopeAttributes]), docDirectory, doc->variables(map, false));

	self.windowTitle      = [NSString stringWithCxxString:settings.get(kSettingsWindowTitleKey, doc->display_name())];
	self.representedFile  = [NSString stringWithCxxString:doc->path()];
	self.isDocumentEdited = doc->is_modified();
	self.pathAttributes   = [NSString stringWithCxxString:file::path_attributes(doc->path(), docDirectory)];
}

- (void)updateProxyIcon
{
	struct scm_callback_t : scm::callback_t
	{
		scm_callback_t (DocumentController* controller) : controller(controller) { D(DBF_DocumentController, bug("%p\n", controller);); }
		~scm_callback_t ()                                                       { D(DBF_DocumentController, bug("%p\n", controller);); }

		void status_changed (scm::info_t const& info, std::set<std::string> const& changedPaths)
		{
			if(changedPaths.find(to_s(controller.representedFile)) != changedPaths.end())
				update();
		}

		void update ()
		{
			if([controller isWindowLoaded])
			{
				NSString* path = controller.representedFile;
				BOOL exists = [[NSFileManager defaultManager] fileExistsAtPath:path];
				[[controller window] setRepresentedFilename:exists ? path : @""];
				NSImage* image = exists ? [OakFileIconImage fileIconImageWithPath:path isModified:NO] : nil;
				[[[controller window] standardWindowButton:NSWindowDocumentIconButton] setImage:image];
			}
		}

	private:
		DocumentController* controller;
	};

	if(![self isWindowLoaded]) // we rely on windowDidLoad to call updateProxyIcon
		return;

	if(scmInfo && scmCallback)
	{
		scmInfo->remove_callback(scmCallback);
		delete scmCallback;
		scmCallback = NULL;
		scmInfo.reset();
	}

	if(self.representedFile)
	{
		scm_callback_t* cb = new scm_callback_t(self);
		cb->update();

		if(scmInfo = scm::info(path::parent(to_s(self.representedFile))))
		{
			scmInfo->add_callback(cb);
			scmCallback = cb;
		}
		else
		{
			delete cb;
		}
	}
	else if([self isWindowLoaded])
	{
		[[self window] setRepresentedFilename:@""];
	}
}

- (void)setRepresentedFile:(NSString*)aPath
{
	_representedFile = aPath;
	[self updateProxyIcon]; // FIXME Skip for unchanged path. Problem is updateProxyIcon not being called for file appearing/disappearing on disk.
}

- (void)swipeWithEvent:(NSEvent*)anEvent
{
	if([anEvent deltaX] == -1)
		[self selectNextTab:self];
	else if([anEvent deltaX] == +1)
		[self selectPreviousTab:self];
	else if([anEvent deltaY] == +1)
		[self goToFileCounterpart:self];
}

// ========================
// = OakTextView Delegate =
// ========================

- (NSString*)scopeAttributes
{
	return self.pathAttributes;
}

// =============================
// = OakTabBarView Data Source =
// =============================

- (NSUInteger)numberOfRowsInTabBarView:(OakTabBarView*)aTabBarView                      { return documentTabs.size(); }
- (NSString*)tabBarView:(OakTabBarView*)aTabBarView titleForIndex:(NSUInteger)anIndex   { return [NSString stringWithCxxString:documentTabs[anIndex]->_document->display_name()]; }
- (NSString*)tabBarView:(OakTabBarView*)aTabBarView toolTipForIndex:(NSUInteger)anIndex { return [NSString stringWithCxxString:path::with_tilde(documentTabs[anIndex]->_document->path())] ?: @""; }
- (BOOL)tabBarView:(OakTabBarView*)aTabBarView isEditedAtIndex:(NSUInteger)anIndex      { return documentTabs[anIndex]->_document->is_modified(); }

// ======================================
// = OakTabBarView Delegate / Selection =
// ======================================

- (BOOL)tabBarView:(OakTabBarView*)aTabBarView shouldSelectIndex:(NSUInteger)anIndex
{
	self.selectedTabIndex = anIndex;
	return YES;
}

- (void)tabBarView:(OakTabBarView*)aTabBarView didDoubleClickIndex:(NSUInteger)anIndex
{
	D(DBF_DocumentController, bug("\n"););
	[self moveDocumentToNewWindow:nil];
}

- (void)tabBarViewDidDoubleClick:(OakTabBarView*)aTabBarView
{
	D(DBF_DocumentController, bug("\n"););
	scratchDocument = oak::uuid_t();
	[self addDocuments:std::vector<document::document_ptr>(1, create_document(self.fileBrowserPath)) atIndex:documentTabs.size() andSelect:kSelectDocumentFirst closeOther:NO pruneTabBar:NO];
}

// =========================
// = Open/Create Documents =
// =========================

- (IBAction)newDocumentInTab:(id)sender
{
	D(DBF_DocumentController, bug("\n"););
	scratchDocument = oak::uuid_t();
	[self addDocuments:std::vector<document::document_ptr>(1, create_document(self.fileBrowserPath)) andSelect:kSelectDocumentFirst closeOther:NO pruneTabBar:YES];
}

- (BOOL)openFile:(NSString*)aPath
{
	D(DBF_DocumentController, bug("%s\n", [aPath UTF8String]););
	[self addDocuments:std::vector<document::document_ptr>(1, document::create(to_s(aPath))) andSelect:kSelectDocumentFirst closeOther:NO pruneTabBar:YES];
	return YES;
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
	citerate(doc, document::scanner_t::open_documents())
	{
		if(documentDir == path::parent((*doc)->path()) && documentBase == path::strip_extensions(path::name((*doc)->path())))
			candidates.insert(path::name((*doc)->path()));
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

	D(DBF_DocumentController, bug("%s (in %s) → %s\n", documentBase.c_str(), path::with_tilde(documentDir).c_str(), text::join(v).c_str()););
	if(v.size() == 1)
		return (void)NSBeep();

	std::vector<std::string>::const_iterator it = std::find(v.begin(), v.end(), documentName);
	ASSERT(it != v.end());
	[self openFile:[NSString stringWithCxxString:path::join(documentDir, v[((it - v.begin()) + 1) % v.size()])]];
}

- (void)openItems:(NSArray*)items closingOtherTabs:(BOOL)closeOtherTabs
{
	D(DBF_DocumentController, bug("%s (%s)\n", [items description].UTF8String, BSTR(closeOtherTabs)););
	if([items count] == 0)
		return;

	std::vector<document::document_ptr> documents;
	for(NSDictionary* item in items)
	{
		std::string const path = to_s((NSString*)[item objectForKey:@"path"]);
		std::string const uuid = to_s((NSString*)[item objectForKey:@"identifier"]);
		documents.push_back(path == NULL_STR && oak::uuid_t::is_valid(uuid) ? document::find(uuid) : document::create(path));
		documents.back()->set_recent_tracking(false);

		NSString* selectionString = [item objectForKey:@"selectionString"];
		if(NSNotEmptyString(selectionString))
			documents.back()->set_selection(to_s(selectionString));
	}

	[self addDocuments:documents andSelect:kSelectDocumentFirst closeOther:closeOtherTabs pruneTabBar:YES];
}

// OakFileBrowser delegate
- (void)fileBrowser:(OakFileBrowser*)aFileBrowser openURLs:(NSArray*)someURLs
{
	D(DBF_DocumentController, bug("%s\n", [[someURLs description] UTF8String]););
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
	if([anURL isFileURL])
		[self closeDocumentWithPath:[anURL path]];
}

// OakFileChooser delegate
- (void)fileChooserDidSelectItems:(id)sender
{
	D(DBF_DocumentController, bug("%s\n", [[[sender selectedItems] description] UTF8String]););
	[self openItems:[sender selectedItems] closingOtherTabs:OakIsAlternateKeyOrMouseEvent()];
}

- (void)fileChooserDidDescend:(id)sender
{
	ASSERT([sender respondsToSelector:@selector(selectedItems)]);
	ASSERT([[sender selectedItems] count] == 1);

	NSString* documentIdentifier = [[[sender selectedItems] lastObject] objectForKey:@"identifier"];
	fileChooserSourceIndex       = [[filterWindowController dataSource] sourceIndex];
	filterWindowController.dataSource              = [SymbolChooser symbolChooserForDocument:document::find(to_s(documentIdentifier))];
	filterWindowController.action                  = @selector(symbolChooserDidSelectItems:);
	filterWindowController.sendActionOnSingleClick = YES;
}

- (IBAction)mergeAllWindows:(id)sender
{
	std::set<oak::uuid_t> seenDocuments;
	iterate(tab, documentTabs)
	{
		document::document_ptr document = **tab;
		seenDocuments.insert(document->identifier());
	}

	NSMutableArray* delegates = [NSMutableArray array];
	std::vector<document::document_ptr> documents;
	for(NSWindow* window in [NSApp orderedWindows])
	{
		DocumentController* delegate = (DocumentController*)[window delegate];
		if(![window isMiniaturized] && [delegate isKindOfClass:[DocumentController class]] && delegate != self)
		{
			iterate(tab, delegate->documentTabs)
			{
				document::document_ptr document = **tab;
				if(seenDocuments.find(document->identifier()) != seenDocuments.end())
					continue;
				seenDocuments.insert(document->identifier());
				documents.push_back(document);
			}
			[delegates addObject:delegate];
		}
	}

	[self addDocuments:documents andSelect:kSelectDocumentNone closeOther:NO pruneTabBar:NO];

	for(DocumentController* delegate in delegates)
		[delegate close];
}

- (void)makeTextViewFirstResponder:(id)sender
{
	[[self window] makeFirstResponder:textView];
}

- (BOOL)htmlOutputIsVisible
{
	return htmlOutputView && [htmlOutputView superview];
}

- (void)toggleHTMLOutput:(id)sender
{
	if([self htmlOutputIsVisible])
	{
		layoutView.htmlOutputView = nil;
		[self.window makeFirstResponder:textView];
	}
	else
	{
		if(!htmlOutputView)
			htmlOutputView = [[OakHTMLOutputView alloc] initWithFrame:NSZeroRect];

		if(![self htmlOutputIsVisible])
			layoutView.htmlOutputView = htmlOutputView;
	}
}

- (BOOL)setCommandRunner:(command::runner_ptr const&)aRunner
{
	if(runner && runner->running())
	{
		NSInteger choice = [[NSAlert alertWithMessageText:@"Stop current task first?" defaultButton:@"Stop Task" alternateButton:@"Cancel" otherButton:nil informativeTextWithFormat:@"There already is a task running. If you stop this then the task it is performing will not be completed."] runModal];
		if(choice != NSAlertDefaultReturn) /* "Stop" */
			return NO;
	}

	runner = aRunner;

	if([[[NSUserDefaults standardUserDefaults] objectForKey:kUserDefaultsHTMLOutputPlacementKey] isEqualToString:@"window"])
	{
		[HTMLOutputWindowController HTMLOutputWindowWithRunner:runner];
	}
	else
	{
		if(![self htmlOutputIsVisible])
			[self toggleHTMLOutput:self];
		[self.window makeFirstResponder:htmlOutputView.webView];

		[htmlOutputView setEnvironment:runner->environment()];
		[htmlOutputView loadRequest:URLRequestForCommandRunner(runner) autoScrolls:runner->auto_scroll_output()];
	}

	return YES;
}

- (void)performBundleItem:(bundles::item_ptr const&)anItem
{
	if(anItem->kind() == bundles::kItemTypeTheme)
	{
		[documentView setThemeWithUUID:[NSString stringWithCxxString:anItem->uuid()]];
	}
	else
	{
		[self.window makeFirstResponder:textView];
		[self.window makeKeyAndOrderFront:self];
		[textView performBundleItem:anItem];
	}
}

- (NSPoint)positionForWindowUnderCaret
{
	return [textView positionForWindowUnderCaret];
}

// ===========================
// = Document Action Methods =
// ===========================

- (void)savePanelDidEnd:(OakSavePanel*)sheet path:(NSString*)aPath encoding:(encoding::type const&)encoding
{
	if(!aPath)
		return;

	if([self selectedDocument]->identifier() == scratchDocument)
		scratchDocument = oak::uuid_t();

	std::vector<std::string> const& paths = path::expand_braces(to_s(aPath));
	ASSERT_LT(0, paths.size());

	[self selectedDocument]->set_path(paths[0]); // FIXME check if document already exists (overwrite)
	[self selectedDocument]->set_disk_encoding(encoding);

	[DocumentSaveHelper trySaveDocument:self.selectedDocument forWindow:self.window defaultDirectory:nil andCallback:NULL];

	if(paths.size() > 1)
	{
		std::vector<document::document_ptr> documents;
		for(size_t i = 1; i < paths.size(); ++i)
		{
			documents.push_back(document::create(paths[i]));
			documents.back()->open(); // so that we find this when going to counterpart
			documents.back()->set_disk_encoding(encoding);
		}

		[self addDocuments:documents andSelect:kSelectDocumentNone closeOther:NO pruneTabBar:NO];

		iterate(doc, documents)
			(*doc)->close();
	}
}

- (IBAction)saveDocument:(id)sender
{
	D(DBF_DocumentController, bug("%s\n", [self selectedDocument]->path().c_str()););
	document::document_ptr doc = [self selectedDocument];
	if(doc->path() != NULL_STR)
			[DocumentSaveHelper trySaveDocument:doc forWindow:self.window defaultDirectory:nil andCallback:NULL];
	else	[OakSavePanel showWithPath:DefaultSaveNameForDocument(doc) directory:self.untitledSavePath fowWindow:self.window delegate:self encoding:doc->encoding_for_save_as_path(to_s([(self.untitledSavePath ?: @"") stringByAppendingPathComponent:DefaultSaveNameForDocument(doc)]))];
}

- (IBAction)saveDocumentAs:(id)sender
{
	D(DBF_DocumentController, bug("%s\n", [self selectedDocument]->path().c_str()););
	document::document_ptr doc = [self selectedDocument];
	std::string const documentPath = doc->path();
	NSString* documentFolder = [NSString stringWithCxxString:path::parent(documentPath)];
	NSString* documentName   = [NSString stringWithCxxString:path::name(documentPath)];
	[OakSavePanel showWithPath:(documentName ?: DefaultSaveNameForDocument(doc)) directory:(documentFolder ?: self.untitledSavePath) fowWindow:self.window delegate:self encoding:doc->encoding_for_save_as_path(doc->path() != NULL_STR ? doc->path() : to_s([(self.untitledSavePath ?: @"") stringByAppendingPathComponent:DefaultSaveNameForDocument(doc)]))];
}

- (IBAction)saveAllDocuments:(id)sender
{
	D(DBF_DocumentController, bug("\n"););
	std::vector<document::document_ptr> documents;
	iterate(tab, documentTabs)
	{
		document::document_ptr document = **tab;
		if(document->is_modified())
			documents.push_back(document);
	}
	[DocumentSaveHelper trySaveDocuments:documents forWindow:self.window defaultDirectory:self.untitledSavePath andCallback:NULL];
}
#if 0
- (IBAction)revertDocumentToSaved:(id)sender
{
	D(DBF_DocumentController, bug("%s\n", collection->current()->path().c_str()););
	if(collection->current()->path() != NULL_STR)
		collection->current()->load();
}
#endif
- (IBAction)revealFileInProject:(id)sender
{
	self.fileBrowserVisible = YES;
	[fileBrowser revealURL:[NSURL fileURLWithPath:[NSString stringWithCxxString:[self selectedDocument]->path()]]];
}
- (IBAction)navigateToFileInProject:(id)sender
{
	D(DBF_DocumentController, bug("%s\n", [self selectedDocument]->path().c_str()););
	self.fileBrowserVisible = YES;
	NSURL* currentDocumentURL = [NSURL fileURLWithPath:[NSString stringWithCxxString:[self selectedDocument]->path()]];
	if([fileBrowser.selectedURLs count] == 1 && [currentDocumentURL isEqualTo:[fileBrowser.selectedURLs lastObject]])
			[fileBrowser deselectAll:self];
	else	[fileBrowser showURL:currentDocumentURL];
}

- (IBAction)goToProjectFolder:(id)sender
{
	self.fileBrowserVisible = YES;
	[fileBrowser showURL:[NSURL fileURLWithPath:self.projectPath]];
}

- (IBAction)moveDocumentToNewWindow:(id)sender
{
	if(documentTabs.size() > 1)
		[self takeTabsToTearOffFrom:[NSIndexSet indexSetWithIndex:selectedTabIndex]];
}

// =========================
// = Close Window Warnings =
// =========================

- (void)closeTabAtIndex:(NSUInteger)tabIndex
{
	[self closeTabsAtIndexes:[NSIndexSet indexSetWithIndex:tabIndex] quiet:NO];
}

- (void)performCloseTabsAtIndexes:(NSIndexSet*)indexes
{
	[self closeTabsAtIndexes:indexes quiet:NO];
}

- (void)performCloseTab:(id)sender
{
	D(DBF_DocumentController, bug("\n"););
	ASSERT([sender isKindOfClass:[OakTabBarView class]]);

	if(documentTabs.size() == 1 && self.fileBrowserVisible)
	{
		document::document_ptr document = *documentTabs[0];
		if(!document->is_modified())
		{
			if(document->path() == NULL_STR)
			{
				return [[self window] performClose:self];
			}
			else
			{
				[self newDocumentInTab:nil];
				scratchDocument = [self selectedDocument]->identifier();
			}
		}
	}
	[self closeTabsAtIndexes:[NSIndexSet indexSetWithIndex:[sender tag]] quiet:NO];
}

- (void)performCloseSplit:(id)sender
{
	D(DBF_DocumentController, bug("\n"););
	if(htmlOutputView && sender == htmlOutputView)
	{
		if(runner && runner->running())
		{
			NSAlert* alert = [NSAlert alertWithMessageText:@"Stop task before closing?" defaultButton:@"Stop Task" alternateButton:@"Cancel" otherButton:nil informativeTextWithFormat:@"The job that the task is performing will not be completed."];
			OakShowAlertForWindow(alert, self.window, ^(NSInteger returnCode){
				if(returnCode == NSAlertDefaultReturn) /* "Stop" */
				{
					runner.reset();
					[htmlOutputView stopLoading];
					[self toggleHTMLOutput:self];
				}
			});
		}
		else
		{
			[self toggleHTMLOutput:self];
		}
	}
}

- (void)performCloseWindow:(id)sender
{
	D(DBF_DocumentController, bug("\n"););
	[self.window performClose:self];
}

- (void)performCloseOtherTabs:(id)sender
{
	D(DBF_DocumentController, bug("\n"););
	NSMutableIndexSet* indexSet = [NSMutableIndexSet indexSetWithIndexesInRange:NSMakeRange(0, documentTabs.size())];
	[indexSet removeIndex:([sender isKindOfClass:[OakTabBarView class]] ? [sender tag] : selectedTabIndex)];
	[self closeTabsAtIndexes:indexSet quiet:NO];
}

- (void)closeDocumentWithPath:(NSString*)aPath
{
	NSMutableIndexSet* set = [NSMutableIndexSet indexSet];
	for(size_t i = 0; i < documentTabs.size(); ++i)
	{
		document::document_ptr document = *documentTabs[i];
		if(document->path() == to_s(aPath))
			[set addIndex:i];
	}
	[self closeTabsAtIndexes:set quiet:NO];
}

- (BOOL)windowShouldClose:(id)sender
{
	D(DBF_DocumentController, bug("\n"););

	[htmlOutputView stopLoading];

	std::vector<document::document_ptr> documents;
	iterate(tab, documentTabs)
	{
		document::document_ptr document = **tab;
		if(document->is_modified())
			documents.push_back(document);
	}

	if(!documents.empty())
	{
		struct callback_t : close_warning_callback_t
		{
			callback_t (NSWindow* aWindow) : _window(aWindow) { }

			void can_close_documents (bool flag)
			{
				if(flag)
					[_window close];
				delete this;
			}
		private:
			NSWindow* _window;
		};
		return [self showCloseWarningUIForDocuments:documents andCallback:new callback_t(self.window)], NO;
	}

	return YES;
}

- (void)windowWillClose:(NSNotification*)aNotification
{
	D(DBF_DocumentController, bug("\n"););

	if(scmInfo && scmCallback)
	{
		scmInfo->remove_callback(scmCallback);
		delete scmCallback;
		scmCallback = NULL;
		scmInfo.reset();
	}

	[self.window unbind:@"title"];
	[self.window unbind:@"documentEdited"];
	self.window.delegate = nil;

	tabBarView.delegate   = nil;
	tabBarView.dataSource = nil;
	fileBrowser.delegate  = nil;

	self.filterWindowController = nil;

	documentView = nil;

	self.retainedSelf = nil;
}

// ========================
// = Tab Bar Context Menu =
// ========================

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
		[self addDocuments:std::vector<document::document_ptr>(1, create_document(self.fileBrowserPath)) atIndex:[indexSet firstIndex] andSelect:kSelectDocumentFirst closeOther:NO pruneTabBar:NO];
}

- (void)takeTabsToCloseFrom:(id)sender
{
	if(NSIndexSet* indexSet = [self tryObtainIndexSetFrom:sender])
		[self closeTabsAtIndexes:indexSet quiet:NO];
}

- (void)takeTabsToTearOffFrom:(id)sender
{
	if(NSIndexSet* indexSet = [self tryObtainIndexSetFrom:sender])
	{
		std::vector<document::document_ptr> documents;
		for(NSUInteger index = [indexSet firstIndex]; index != NSNotFound; index = [indexSet indexGreaterThanIndex:index])
			documents.push_back(*documentTabs[index]);
		DocumentController* delegate = [[DocumentController alloc] initWithDocuments:documents];
		[delegate showWindow:self];
		[self closeTabsAtIndexes:indexSet quiet:YES];
	}
}

- (NSMenu*)menuForTabBarView:(OakTabBarView*)aTabBarView
{
	NSInteger tabIndex = aTabBarView.tag;

	NSMutableIndexSet* newTabAtTab   = tabIndex == -1 ? [NSMutableIndexSet indexSetWithIndex:documentTabs.size()] : [NSMutableIndexSet indexSetWithIndex:tabIndex + 1];
	NSMutableIndexSet* clickedTab    = tabIndex == -1 ? [NSMutableIndexSet indexSet] : [NSMutableIndexSet indexSetWithIndex:tabIndex];
	NSMutableIndexSet* otherTabs     = tabIndex == -1 ? [NSMutableIndexSet indexSet] : [NSMutableIndexSet indexSetWithIndexesInRange:NSMakeRange(0, documentTabs.size())];
	NSMutableIndexSet* rightSideTabs = tabIndex == -1 ? [NSMutableIndexSet indexSet] : [NSMutableIndexSet indexSetWithIndexesInRange:NSMakeRange(0, documentTabs.size())];

	if(tabIndex != -1)
	{
		[otherTabs removeIndex:tabIndex];
		[rightSideTabs removeIndexes:[NSIndexSet indexSetWithIndexesInRange:NSMakeRange(0, tabIndex + 1)]];
	}

	NSMenu* menu = [NSMenu new];
	[menu setAutoenablesItems:NO];

	[menu addItemWithTitle:@"New Tab"                 action:@selector(takeNewTabIndexFrom:)  keyEquivalent:@""];
	[menu addItem:[NSMenuItem separatorItem]];
	[menu addItemWithTitle:@"Close Tab"               action:@selector(takeTabsToCloseFrom:)  keyEquivalent:@""];
	[menu addItemWithTitle:@"Close Other Tabs"        action:@selector(takeTabsToCloseFrom:)  keyEquivalent:@""];
	[menu addItemWithTitle:@"Close Tabs to the Right" action:@selector(takeTabsToCloseFrom:)  keyEquivalent:@""];
	[menu addItem:[NSMenuItem separatorItem]];
	[menu addItemWithTitle:@"Move Tab to New Window"  action:@selector(takeTabsToTearOffFrom:) keyEquivalent:@""];

	NSIndexSet* indexSets[] = { newTabAtTab, nil, clickedTab, otherTabs, rightSideTabs, nil, clickedTab };
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

// ================
// = Tab Dragging =
// ================

static NSString* const OakDocumentPboardType = @"OakDocumentPboardType";

- (void)setupPasteboard:(NSPasteboard*)aPasteboard forTabAtIndex:(NSUInteger)draggedTabIndex
{
	document::document_ptr document = *documentTabs[draggedTabIndex];
	if(document->path() != NULL_STR)
	{
		[aPasteboard addTypes:@[ NSFilenamesPboardType ] owner:nil];
		[aPasteboard setPropertyList:@[ [NSString stringWithCxxString:document->path()] ] forType:NSFilenamesPboardType];
	}

	[aPasteboard addTypes:@[ OakDocumentPboardType ] owner:nil];
	[aPasteboard setPropertyList:[NSDictionary dictionaryWithObjectsAndKeys:
		@(draggedTabIndex),                                      @"index",
		[NSString stringWithCxxString:document->identifier()],   @"document",
		self.identifier,                                         @"collection",
		nil] forType:OakDocumentPboardType];
}

- (BOOL)performTabDropFromTabBar:(OakTabBarView*)aTabBar atIndex:(NSUInteger)droppedIndex fromPasteboard:(NSPasteboard*)aPasteboard operation:(NSDragOperation)operation
{
	NSDictionary* plist    = [aPasteboard propertyListForType:OakDocumentPboardType];

	NSUInteger index       = [[plist objectForKey:@"index"] unsignedIntValue];
	oak::uuid_t docId      = to_s((NSString*)[plist objectForKey:@"document"]);
	oak::uuid_t projectId  = to_s((NSString*)[plist objectForKey:@"collection"]);
	oak::uuid_t identifier = to_s(self.identifier);

	NSInteger selectionConstant = (identifier == projectId && [self selectedDocument]->identifier() == docId) ? kSelectDocumentFirst : kSelectDocumentNone;
	[self addDocuments:std::vector<document::document_ptr>(1, document::find(docId)) atIndex:droppedIndex andSelect:selectionConstant closeOther:NO pruneTabBar:NO];

	if(operation == NSDragOperationMove && identifier != projectId)
	{
		for(NSWindow* window in [NSApp orderedWindows])
		{
			DocumentController* delegate = (DocumentController*)[window delegate];
			if([delegate isKindOfClass:[DocumentController class]] && oak::uuid_t(to_s([delegate identifier])) == projectId)
				return [delegate closeTabsAtIndexes:[NSIndexSet indexSetWithIndex:index] quiet:YES], YES;
		}
	}

	return YES;
}

// =============
// = Tear Down =
// =============

- (void)dealloc
{
	D(DBF_DocumentController, bug("\n"););
	// TODO remove document callbacks (not added at time of writing)
	[[NSNotificationCenter defaultCenter] removeObserver:self];
}

// ===============================
// = Documents Array Abstraction =
// ===============================

- (IBAction)selectNextTab:(id)sender            { self.selectedTabIndex = (selectedTabIndex + 1) % documentTabs.size();                       }
- (IBAction)selectPreviousTab:(id)sender        { self.selectedTabIndex = (selectedTabIndex + documentTabs.size() - 1) % documentTabs.size(); }
- (IBAction)takeSelectedTabIndexFrom:(id)sender { self.selectedTabIndex = [[OakSubmenuController sharedInstance] tagForSender:sender];        }

// ===========================
// = Go to Tab Menu Delegate =
// ===========================

- (void)updateGoToMenu:(NSMenu*)aMenu
{
	if(![[self window] isKeyWindow])
	{
		[aMenu addItemWithTitle:@"No Tabs" action:@selector(nop:) keyEquivalent:@""];
		return;
	}

	int i = 0;
	iterate(tab, documentTabs)
	{
		document::document_ptr document = **tab;

		D(DBF_DocumentController, bug("%s\n", document->display_name().c_str()););
		NSMenuItem* item = [aMenu addItemWithTitle:[NSString stringWithCxxString:document->display_name()] action:@selector(takeSelectedTabIndexFrom:) keyEquivalent:i < 10 ? [NSString stringWithFormat:@"%c", '0' + ((i+1) % 10)] : @""];
		item.tag = i;
		item.toolTip = [[NSString stringWithCxxString:document->path()] stringByAbbreviatingWithTildeInPath];
		if(i == selectedTabIndex)
			[item setState:NSOnState];
		else if(document->is_modified())
			[item setModifiedState:YES];

		++i;
	}

	if(i == 0)
		[aMenu addItemWithTitle:@"No Tabs Open" action:@selector(nop:) keyEquivalent:@""];
}

// =================================
// = Methods returning useful info =
// =================================

static std::string parent_or_home (std::string const& path)
{
	return path == NULL_STR ? path::home() : path::parent(path);
}

- (NSString*)documentPath
{
	return [NSString stringWithCxxString:documentTabs.empty() ? NULL_STR : parent_or_home([self selectedDocument]->path())];
}

- (NSString*)projectPath
{
	settings_t const& settings = documentTabs.empty() || [self selectedDocument]->path() == NULL_STR ? settings_for_path(NULL_STR, "", to_s(self.fileBrowserPath)) : [self selectedDocument]->settings();
	return [NSString stringWithCxxString:settings.get(kSettingsProjectDirectoryKey, NULL_STR)] ?: self.fileBrowserPath ?: self.documentPath;
}

- (NSString*)fileBrowserPath
{
	return fileBrowser ? fileBrowser.location : nil;
}

- (NSString*)untitledSavePath
{
	if(fileBrowser)
	{
		std::string selectedPath = NULL_STR;
		for(NSURL* url in fileBrowser.selectedURLs)
		{
			if(![url isFileURL])
				continue;

			std::string dir = [[url path] fileSystemRepresentation];
			if(path::is_directory(dir))
				return [url path];

			dir = path::parent(dir);
			if(selectedPath == NULL_STR)
				selectedPath = dir;

			while(selectedPath.size() != dir.size())
			{
				if(dir.size() > selectedPath.size())
						dir = path::parent(dir);
				else	selectedPath = path::parent(selectedPath);
			}
		}

		if(selectedPath != NULL_STR)
			return [NSString stringWithCxxString:selectedPath];

		return self.fileBrowserPath;
	}
	return nil;
}

// =============================
// = Opening Auxiliary Windows =
// =============================

- (IBAction)orderFrontFindPanel:(id)sender
{
	D(DBF_DocumentController, bug("\n"););
	Find* find              = [Find sharedInstance];
	find.documentIdentifier = [NSString stringWithCxxString:[self selectedDocument]->identifier()];
	find.projectFolder      = self.projectPath;
	find.projectIdentifier  = self.identifier;

	int mode = [sender respondsToSelector:@selector(tag)] ? [sender tag] : find_tags::in_document;
	if(mode == find_tags::in_folder)
		return [find showFolderSelectionPanel:self];

	if(mode == find_tags::in_document && textView.hasMultiLineSelection)
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
			find.searchFolder = find.projectFolder;
			find.searchScope  = find::in::folder;
			if(!find.isVisible)
			{
				find.searchFolder = find.projectFolder;

				if([self.window.firstResponder respondsToSelector:@selector(isDescendantOf:)] && [(NSView*)self.window.firstResponder isDescendantOf:fileBrowser.view])
				{
					// Take the search folder from the file browser if it is focused
					NSArray* selectedURLs = fileBrowser.selectedURLs;
					if([selectedURLs count] == 1 && [[selectedURLs lastObject] isFileURL] && path::is_directory([[[selectedURLs lastObject] path] fileSystemRepresentation]))
						find.searchFolder = [[selectedURLs lastObject] path];
					else if(fileBrowser.location)
						find.searchFolder = fileBrowser.location;
				}
			}
			break;
	}
	[find showFindPanel:self];
}

- (IBAction)showSymbolChooser:(id)sender
{
	D(DBF_DocumentController, bug("\n"););
	self.filterWindowController                    = [OakFilterWindowController filterWindow];
	filterWindowController.dataSource              = [SymbolChooser symbolChooserForDocument:[self selectedDocument]];
	filterWindowController.action                  = @selector(symbolChooserDidSelectItems:);
	filterWindowController.sendActionOnSingleClick = YES;
	[filterWindowController showWindowRelativeToWindow:self.window];
}

- (void)symbolChooserDidSelectItems:(id)sender
{
	D(DBF_DocumentController, bug("%s\n", [[[sender selectedItems] description] UTF8String]););
	[self openItems:[sender selectedItems] closingOtherTabs:NO];
}

static std::string file_chooser_glob (std::string const& path)
{
	settings_t const& settings = settings_for_path(NULL_STR, "", path);
	std::string const propertyKeys[] = { kSettingsIncludeFilesInFileChooserKey, kSettingsIncludeInFileChooserKey, kSettingsIncludeFilesKey, kSettingsIncludeKey };
	iterate(key, propertyKeys)
	{
		if(settings.has(*key))
			return settings.get(*key, NULL_STR);
	}
	return "*";
}

- (void)goToFile:(id)sender
{
	self.filterWindowController = [OakFilterWindowController filterWindow];

	OakFileChooser* dataSource = [OakFileChooser fileChooserWithPath:(self.fileBrowserPath ?: self.documentPath) projectPath:self.projectPath];
	dataSource.excludeDocumentWithIdentifier = [NSString stringWithCxxString:[self selectedDocument]->identifier()];
	dataSource.sourceIndex                   = fileChooserSourceIndex;
	dataSource.globString                    = [NSString stringWithCxxString:file_chooser_glob(to_s(dataSource.path))];

	if(OakPasteboardEntry* entry = [[OakPasteboard pasteboardWithName:NSFindPboard] current])
	{
		std::string str = [entry.string UTF8String] ?: "";
		if(regexp::search("\\A.*?\\..*?:\\d+\\z", str.data(), str.data() + str.size()))
			dataSource.filterString = entry.string;
	}

	filterWindowController.dataSource              = dataSource;
	filterWindowController.target                  = self;
	filterWindowController.allowsMultipleSelection = YES;
	filterWindowController.action                  = @selector(fileChooserDidSelectItems:);
	filterWindowController.accessoryAction         = @selector(fileChooserDidDescend:);
	fileChooserSourceIndex = NSNotFound;
	[filterWindowController showWindowRelativeToWindow:self.window];
}

- (void)setFilterWindowController:(OakFilterWindowController*)controller
{
	if(controller != filterWindowController)
	{
		if(filterWindowController)
		{
			if(fileChooserSourceIndex == NSNotFound)
				fileChooserSourceIndex = [[filterWindowController dataSource] sourceIndex];
			[[NSNotificationCenter defaultCenter] removeObserver:self name:NSWindowWillCloseNotification object:filterWindowController.window];
			filterWindowController.target = nil;
			[filterWindowController close];
		}

		if(filterWindowController = controller)
			[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(filterWindowWillClose:) name:NSWindowWillCloseNotification object:filterWindowController.window];
	}
}

- (void)filterWindowWillClose:(NSNotification*)notification
{
	self.filterWindowController = nil;
}

// ==============
// = Split view =
// ==============

- (void)setFileBrowserVisible:(BOOL)flag
{
	if(flag != _fileBrowserVisible)
	{
		_fileBrowserVisible = flag;
		if(!fileBrowser && flag)
		{
			D(DBF_DocumentController, bug("%s\n", [self.fileBrowserPath UTF8String]););

			fileBrowser = [OakFileBrowser new];
			fileBrowser.delegate = self;
			[fileBrowser setupViewWithState:_fileBrowserHistory];
			[self updateFileBrowserStatus:self];
		}

		layoutView.fileBrowserView = flag ? fileBrowser.view : nil;
	}
	document::schedule_session_backup();
}

- (IBAction)toggleFileBrowser:(id)sender
{
	self.fileBrowserVisible = !self.fileBrowserVisible;
}

- (NSDictionary*)fileBrowserHistory { return fileBrowser.sessionState ?: _fileBrowserHistory; }

- (CGFloat)fileBrowserWidth                 { return layoutView.fileBrowserWidth;   }
- (NSSize)htmlOutputSize                    { return layoutView.htmlOutputSize;     }
- (void)setFileBrowserWidth:(CGFloat)aWidth { layoutView.fileBrowserWidth = aWidth; }
- (void)setHtmlOutputSize:(NSSize)aSize     { layoutView.htmlOutputSize = aSize;    }

// ===========================
// = Forward to file browser =
// ===========================

- (IBAction)goBack:(id)sender               { self.fileBrowserVisible = YES; [NSApp sendAction:_cmd to:fileBrowser from:sender]; }
- (IBAction)goForward:(id)sender            { self.fileBrowserVisible = YES; [NSApp sendAction:_cmd to:fileBrowser from:sender]; }
- (IBAction)goToParentFolder:(id)sender     { self.fileBrowserVisible = YES; [NSApp sendAction:_cmd to:fileBrowser from:sender]; }

- (IBAction)goToComputer:(id)sender         { self.fileBrowserVisible = YES; [NSApp sendAction:_cmd to:fileBrowser from:sender]; }
- (IBAction)goToHome:(id)sender             { self.fileBrowserVisible = YES; [NSApp sendAction:_cmd to:fileBrowser from:sender]; }
- (IBAction)goToDesktop:(id)sender          { self.fileBrowserVisible = YES; [NSApp sendAction:_cmd to:fileBrowser from:sender]; }
- (IBAction)goToFavorites:(id)sender        { self.fileBrowserVisible = YES; [NSApp sendAction:_cmd to:fileBrowser from:sender]; }
- (IBAction)goToSCMDataSource:(id)sender    { self.fileBrowserVisible = YES; [NSApp sendAction:_cmd to:fileBrowser from:sender]; }
- (IBAction)orderFrontGoToFolder:(id)sender { self.fileBrowserVisible = YES; [NSApp sendAction:_cmd to:fileBrowser from:sender]; }

// ====================
// = NSMenuValidation =
// ====================

- (BOOL)validateMenuItem:(NSMenuItem*)menuItem;
{
	BOOL active = YES;
	if([menuItem action] == @selector(toggleFileBrowser:))
		[menuItem setTitle:self.fileBrowserVisible ? @"Hide File Browser" : @"Show File Browser"];
	else if([menuItem action] == @selector(moveDocumentToNewWindow:))
		active = documentTabs.size() > 1;
	else if([menuItem action] == @selector(navigateToFileInProject:) || [menuItem action] == @selector(revealFileInProject:))
		active = [self selectedDocument]->path() != NULL_STR;
	else if([menuItem action] == @selector(goToParentFolder:))
		active = [[self window] firstResponder] != textView;
	return active;
}
@end
