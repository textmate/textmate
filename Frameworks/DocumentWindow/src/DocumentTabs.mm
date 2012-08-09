#import "DocumentTabs.h"
#import "DocumentSaveHelper.h"
#import "DocumentCommand.h"
#import <document/document.h>
#import <OakTextView/OakDocumentView.h>
#import <OakFoundation/NSString Additions.h>
#import <OakAppKit/NSAlert Additions.h>
#import <document/collection.h>
#import <document/session.h>

OAK_DEBUG_VAR(DocumentController_Tabs);

@interface DocumentController ()
@property (nonatomic, retain) NSString* windowTitle;
@property (nonatomic, retain) NSString* representedFile;
@property (nonatomic, assign) BOOL isDocumentEdited;
@end

namespace
{
	struct callback_info_t
	{
		callback_info_t (std::vector<document::document_ptr> const& documents, close_warning_callback_t* callback) : documents(documents), callback(callback) { }
		std::vector<document::document_ptr> documents;
		close_warning_callback_t* callback;
	};
}

@implementation DocumentController (Tabs)
- (NSInteger)selectedTabIndex
{
	return selectedTabIndex;
}

- (void)setSelectedTabIndex:(NSInteger)newSelectedTabIndex
{
	D(DBF_DocumentController_Tabs, bug("%d → %d, window visible: %s\n", (int)selectedTabIndex, (int)newSelectedTabIndex, BSTR([self.window isVisible])););

	selectedTabIndex = newSelectedTabIndex;
	[tabBarView setSelectedTab:selectedTabIndex];
	document::schedule_session_backup();

	// This is also set after open succeeds
	settings_t const& settings = [self selectedDocument]->settings();
	self.windowTitle      = [NSString stringWithCxxString:settings.get("windowTitle", [self selectedDocument]->display_name())];
	self.representedFile  = [NSString stringWithCxxString:[self selectedDocument]->path()];
	self.isDocumentEdited = [self selectedDocument]->is_modified();

	if(windowHasLoaded)
		[[DocumentOpenHelper new] tryOpenDocument:self.selectedDocument forWindow:self.window delegate:self];
}

- (document::document_ptr const&)selectedDocument
{
	return *documentTabs[selectedTabIndex];
}

- (void)updateFileBrowserStatus:(id)sender
{
	NSMutableArray* openURLs     = [NSMutableArray array];
	NSMutableArray* modifiedURLs = [NSMutableArray array];
	iterate(tab, documentTabs)
	{
		document::document_ptr document = **tab;
		if(document->path() != NULL_STR)
			[openURLs addObject:[NSURL fileURLWithPath:[NSString stringWithCxxString:document->path()]]];
		if(document->path() != NULL_STR && document->is_modified())
			[modifiedURLs addObject:[NSURL fileURLWithPath:[NSString stringWithCxxString:document->path()]]];
	}
	fileBrowser.openURLs     = openURLs;
	fileBrowser.modifiedURLs = modifiedURLs;
}

- (void)documentDidChange:(document::document_ptr const&)aDocument
{
	D(DBF_DocumentController_Tabs, bug("\n"););
	if(*aDocument == *[self selectedDocument])
	{
		settings_t const& settings = [self selectedDocument]->settings();
		self.windowTitle      = [NSString stringWithCxxString:settings.get("windowTitle", [self selectedDocument]->display_name())];
		self.representedFile  = [NSString stringWithCxxString:[self selectedDocument]->path()];
		self.isDocumentEdited = [self selectedDocument]->is_modified();
	}
	[tabBarView reloadData];
	[self updateFileBrowserStatus:self];
	document::schedule_session_backup();
}

- (void)addDocuments:(std::vector<document::document_ptr> const&)someDocuments atIndex:(size_t)insertAt andSelect:(NSInteger)selectionConstant closeOther:(BOOL)closeOtherFlag pruneTabBar:(BOOL)pruneTabBarFlag
{
	D(DBF_DocumentController_Tabs, bug("%zu documents, close other %s, prune %s\n", someDocuments.size(), BSTR(closeOtherFlag), BSTR(pruneTabBarFlag)););

	oak::uuid_t selectedDocumentIdentifier;
	std::map<oak::uuid_t, document_tab_ptr> oldTabs;
	iterate(tab, documentTabs)
	{
		document::document_ptr document = **tab;
		oldTabs.insert(std::make_pair(document->identifier(), *tab));
		if(selectedTabIndex == tab - documentTabs.begin())
			selectedDocumentIdentifier = document->identifier();
	}

	std::set<oak::uuid_t> newDocs;
	std::vector<document_tab_ptr> wrapped;
	iterate(doc, someDocuments)
	{
		newDocs.insert((*doc)->identifier());
		std::map<oak::uuid_t, document_tab_ptr>::const_iterator oldTab = oldTabs.find((*doc)->identifier());
		if(oldTab != oldTabs.end())
		{
			wrapped.push_back(oldTab->second);
		}
		else
		{
			wrapped.push_back(document_tab_ptr(new document_tab_t(*doc)));
			wrapped.back()->add_callback(self);
		}
	}

	if(closeOtherFlag)
	{
		std::vector<document_tab_ptr> modifiedTabs;
		iterate(tab, documentTabs)
		{
			document::document_ptr document = **tab;
			if(document->is_modified())
				modifiedTabs.push_back(*tab);
		}
		documentTabs.swap(modifiedTabs);
		insertAt = 0;
	}
	else
	{
		if(scratchDocument)
		{
			iterate(tab, documentTabs)
			{
				document::document_ptr document = **tab;
				if(document->identifier() == scratchDocument && !document->is_modified() && document->path() == NULL_STR)
					newDocs.insert(scratchDocument); // causes scratch document to be closed
			}
		}

		size_t i = documentTabs.size();
		while(i-- != 0)
		{
			document::document_ptr doc = *documentTabs[i];
			if(newDocs.find(doc->identifier()) != newDocs.end())
			{
				documentTabs.erase(documentTabs.begin() + i);
				if(i < insertAt)
					--insertAt;
			}
		}
	}

	documentTabs.insert(documentTabs.begin() + insertAt, wrapped.begin(), wrapped.end());
	scratchDocument = oak::uuid_t();
	[tabBarView reloadData];
	[self updateFileBrowserStatus:self];

	switch(selectionConstant)
	{
		case kSelectDocumentFirst: self.selectedTabIndex = insertAt;                      break;
		case kSelectDocumentLast:  self.selectedTabIndex = insertAt + wrapped.size() - 1; break;
		case kSelectDocumentNone:
		{
			for(size_t i = 0; i < documentTabs.size(); ++i)
			{
				document::document_ptr doc = *documentTabs[i];
				if(doc->identifier() == selectedDocumentIdentifier)
					self.selectedTabIndex = i;
			}
		}
		break;
		default:                   self.selectedTabIndex = insertAt + selectionConstant;  break;
	}

	if(pruneTabBarFlag)
	{
		NSInteger excessTabs = documentTabs.size() - tabBarView.countOfVisibleTabs;
		if(tabBarView && excessTabs > 0)
		{
			std::multimap<oak::date_t, size_t> ranked;
			for(size_t i = 0; i < documentTabs.size(); ++i)
			{
				document::document_ptr doc = *documentTabs[i];
				if(!doc->is_modified() && doc->is_on_disk() && newDocs.find(doc->identifier()) == newDocs.end())
					ranked.insert(std::make_pair(doc->lru(), i));
			}

			NSMutableIndexSet* indexSet = [NSMutableIndexSet indexSet];
			iterate(pair, ranked)
			{
				[indexSet addIndex:pair->second];
				if([indexSet count] == excessTabs)
					break;
			}

			[self closeTabsAtIndexes:indexSet quiet:NO];
		}
	}
}

- (void)addDocuments:(std::vector<document::document_ptr> const&)someDocuments andSelect:(NSInteger)selectionConstant closeOther:(BOOL)closeOtherFlag pruneTabBar:(BOOL)pruneTabBarFlag
{
	[self addDocuments:someDocuments atIndex:(documentTabs.empty() ? 0 : selectedTabIndex + 1) andSelect:selectionConstant closeOther:closeOtherFlag pruneTabBar:pruneTabBarFlag];
}

- (void)closeTabsAtIndexes:(NSIndexSet*)anIndexSet quiet:(BOOL)quietFlag
{
	NSInteger newSelectedIndex = selectedTabIndex;

	if(quietFlag == NO)
	{
		std::vector<document::document_ptr> documents;
		NSUInteger i = documentTabs.size();
		while((i = [anIndexSet indexLessThanIndex:i]) != NSNotFound)
		{
			document::document_ptr document = *documentTabs[i];
			if(document->is_modified())
				documents.push_back(document);
		}

		if(!documents.empty())
		{
			struct callback_t : close_warning_callback_t
			{
				callback_t (DocumentController* controller, NSIndexSet* indexSet)
				{
					_self = [controller retain];
					_indexSet = [indexSet retain];
				}

				~callback_t ()
				{
					[_self release];
					[_indexSet release];
				}

				void can_close_documents (bool flag)
				{
					if(flag)
						[_self closeTabsAtIndexes:_indexSet quiet:YES];
					delete this;
				}
			private:
				DocumentController* _self;
				NSIndexSet* _indexSet;
			};
			return (void)[self showCloseWarningUIForDocuments:documents andCallback:new callback_t(self, anIndexSet)];
		}
	}

	NSUInteger i = documentTabs.size();
	while((i = [anIndexSet indexLessThanIndex:i]) != NSNotFound)
	{
		if(i <= newSelectedIndex && newSelectedIndex)
			--newSelectedIndex;
		documentTabs.erase(documentTabs.begin() + i);
	}

	if(documentTabs.empty())
		return [self close];

	[tabBarView reloadData];
	[self updateFileBrowserStatus:self];
	self.selectedTabIndex = newSelectedIndex;
}

// ==============================
// = DocumentOpenHelperDelegate =
// ==============================

- (void)documentOpenHelper:(DocumentOpenHelper*)documentOpenHelper didOpenDocument:(document::document_ptr const&)aDocument
{
	iterate(tab, documentTabs)
	{
		if(**tab == aDocument && !(*tab)->_did_open)
		{
			(*tab)->_document->open();
			(*tab)->_did_open = true;
		}
	}

	settings_t const& settings = aDocument->settings();
	self.windowTitle      = [NSString stringWithCxxString:settings.get("windowTitle", aDocument->display_name())];
	self.representedFile  = [NSString stringWithCxxString:aDocument->path()];
	self.isDocumentEdited = aDocument->is_modified();

	[documentView setDocument:aDocument];
	[self makeTextViewFirstResponder:self];

	[documentOpenHelper release];
}

- (void)documentOpenHelper:(DocumentOpenHelper*)documentOpenHelper failedToOpenDocument:(document::document_ptr const&)aDocument error:(std::string const&)aMessage usingFilter:(oak::uuid_t const&)filterUUID
{
	if(filterUUID)
		show_command_error(aMessage, filterUUID);

	NSMutableIndexSet* set = [NSMutableIndexSet indexSet];
	for(size_t i = 0; i < documentTabs.size(); ++i)
	{
		document::document_ptr document = *documentTabs[i];
		if(*document == *aDocument)
			[set addIndex:i];
	}
	[self closeTabsAtIndexes:set quiet:YES];

	[documentOpenHelper release];
}

// =======================
// = Save/Close Warnings =
// =======================

- (void)closeWarningDidEnd:(NSAlert*)alert returnCode:(NSInteger)returnCode contextInfo:(callback_info_t*)info
{
	switch(returnCode)
	{
		case NSAlertFirstButtonReturn: /* "Save" */
		{
			struct callback_t : document_save_callback_t
			{
				callback_t (close_warning_callback_t* callback, size_t count) : _callback(callback), _count(count) { }

				void did_save_document (document::document_ptr document, bool flag, std::string const& message, oak::uuid_t const& filter)
				{
					if(_callback && (_count == 1 || !flag))
						_callback->can_close_documents(flag);

					if(--_count == 0 || !flag)
						delete this;
				}

			private:
				close_warning_callback_t* _callback;
				size_t _count;
			};

			if(info->callback)
				info->callback->will_save_documents();

			[DocumentSaveHelper trySaveDocuments:info->documents forWindow:self.window defaultDirectory:self.untitledSavePath andCallback:new callback_t(info->callback, info->documents.size())];
		}
		break;

		case NSAlertSecondButtonReturn: /* "Cancel" */
		{
			if(info->callback)
				info->callback->can_close_documents(false);
		}
		break;

		case NSAlertThirdButtonReturn: /* "Don't Save" */
		{
			if(info->callback)
				info->callback->can_close_documents(true);
		}
		break;
	}
	delete info;
	[alert release];
}

- (void)showCloseWarningUIForDocuments:(std::vector<document::document_ptr> const&)someDocuments andCallback:(close_warning_callback_t*)aCallback
{
	D(DBF_DocumentController_Tabs, bug("%s — %zu documents\n", [self.window.title UTF8String], someDocuments.size()););

	if(someDocuments.empty())
	{
		if(aCallback)
			aCallback->can_close_documents(true);
		return;
	}

	[[self.window attachedSheet] orderOut:self];
	NSAlert* alert = [[NSAlert alloc] init];
	[alert setAlertStyle:NSWarningAlertStyle];
	[alert addButtons:@"Save", @"Cancel", @"Don’t Save", nil];
	if(someDocuments.size() == 1)
	{
		document::document_ptr document = someDocuments.front();
		[alert setMessageText:[NSString stringWithCxxString:text::format("Do you want to save the changes you made in the document “%s”?", document->display_name().c_str())]];
		[alert setInformativeText:@"Your changes will be lost if you don't save them."];
	}
	else
	{
		std::string body = "";
		iterate(document, someDocuments)
			body += text::format("• “%s”\n", (*document)->display_name().c_str());
		[alert setMessageText:@"Do you want to save documents with changes?"];
		[alert setInformativeText:[NSString stringWithCxxString:body]];
	}

	bool windowModal = true;
	if(someDocuments.size() == 1)
	{
		D(DBF_DocumentController_Tabs, bug("select ‘%s’\n", someDocuments.back()->display_name().c_str()););
		iterate(tab, documentTabs)
		{
			document::document_ptr document = **tab;
			if(someDocuments.front()->identifier() == document->identifier())
				self.selectedTabIndex = tab - documentTabs.begin();
		}
	}
	else
	{
		std::set<oak::uuid_t> docIdentifiers;
		iterate(tab, documentTabs)
		{
			document::document_ptr document = **tab;
			docIdentifiers.insert(document->identifier());
		}

		iterate(document, someDocuments)
		{
			if(docIdentifiers.find((*document)->identifier()) == docIdentifiers.end())
				windowModal = false;
		}
	}

	if(windowModal)
			[alert beginSheetModalForWindow:self.window modalDelegate:self didEndSelector:@selector(closeWarningDidEnd:returnCode:contextInfo:) contextInfo:new callback_info_t(someDocuments, aCallback)];
	else	[self closeWarningDidEnd:alert returnCode:[alert runModal] contextInfo:new callback_info_t(someDocuments, aCallback)];
}

// ===========================
// = Application Termination =
// ===========================

- (void)shutdownCleanup
{
	document::save_session(false);
	for(NSWindow* window in [NSApp orderedWindows])
	{
		DocumentController* delegate = (DocumentController*)[window delegate];
		if([delegate isKindOfClass:[DocumentController class]])
		{
			// it might be better to call [delegate close] but with auto release pools we can’t be 100% certain that this closes all documents (in this event loop cycle)
			[delegate->documentView setDocument:document::create()];
			delegate->documentTabs.clear();
		}
	}
}

- (NSApplicationTerminateReply)applicationShouldTerminate:(NSApplication*)sender
{
	D(DBF_DocumentController_Tabs, bug("%s\n", [self.window.title UTF8String]););
	DocumentController* controller = nil;

	std::set<oak::uuid_t> docIdentifiers;
	std::vector<document::document_ptr> documents;
	for(NSWindow* window in [NSApp orderedWindows])
	{
		DocumentController* delegate = (DocumentController*)[window delegate];
		if([delegate isKindOfClass:[DocumentController class]])
		{
			iterate(tab, delegate->documentTabs)
			{
				document::document_ptr document = **tab;
				if(document->is_modified() && docIdentifiers.find(document->identifier()) == docIdentifiers.end())
				{
					documents.push_back(document);
					docIdentifiers.insert(document->identifier());
					if(!controller)
						controller = delegate;
				}
			}
		}
	}

	if(!documents.empty())
	{
		struct callback_t : close_warning_callback_t
		{
			callback_t (DocumentController* self) : _self(self), _did_reply(false) { }

			void will_save_documents ()
			{
				[NSApp replyToApplicationShouldTerminate:NO];
				_did_reply = true;
			}

			void can_close_documents (bool flag)
			{
				if(!_did_reply && flag)
					[_self shutdownCleanup];

				if(!_did_reply)
					[NSApp replyToApplicationShouldTerminate:flag];
				else if(flag)
					[NSApp terminate:nil];
				delete this;
			}

		private:
			DocumentController* _self;
			bool _did_reply;
		};
		return [controller showCloseWarningUIForDocuments:documents andCallback:new callback_t(self)], NSTerminateLater;
	}

	[self shutdownCleanup];
	return NSTerminateNow;
}
@end