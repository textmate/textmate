#import "DocumentSaveHelper.h"
#import "DocumentCommand.h"
#import <OakFoundation/NSString Additions.h>
#import <OakAppKit/OakSavePanel.h>
#import <OakAppKit/OakEncodingPopUpButton.h>
#import <ns/ns.h>
#import <text/parse.h>
#import <regexp/glob.h>
#import <authorization/constants.h>
#import <document/collection.h>
#import <file/encoding.h>
#import <bundles/bundles.h>

OAK_DEBUG_VAR(DocumentController_SaveHelper);

NSString* DefaultSaveNameForDocument (document::document_ptr const& aDocument)
{
	citerate(item, bundles::query(bundles::kFieldGrammarScope, aDocument->file_type()))
	{
		std::string const& ext = (*item)->value_for_field(bundles::kFieldGrammarExtension);
		if(ext != NULL_STR)
			return [NSString stringWithCxxString:aDocument->display_name() + "." + ext];
	}
	return [NSString stringWithCxxString:aDocument->display_name()];
}

@interface DocumentSaveHelper ()
- (void)trySaveDocuments:(std::vector<document::document_ptr> const&)someDocuments forWindow:(NSWindow*)aWindow defaultDirectory:(NSString*)aFolder andCallback:(document_save_callback_t*)aCallback;
- (void)didSaveDocument:(document::document_ptr const&)aDocument success:(BOOL)flag error:(std::string const&)aMessage usingFilter:(oak::uuid_t const&)aFilter;
- (file::save_context_ptr const&)context;
- (void)setContext:(file::save_context_ptr const&)newContext;
@property (nonatomic, retain) NSString* saveFolder;
@property (nonatomic, assign) BOOL userAbort;
@end

namespace
{
	struct save_callback_t : document::save_callback_t
	{
		save_callback_t (document::document_ptr document, DocumentSaveHelper* self, NSWindow* window) : _document(document), _self(self), _window(window) { }

		void select_path (std::string const& path, io::bytes_ptr content, file::save_context_ptr context)
		{
			D(DBF_DocumentController_SaveHelper, bug("\n"););
			init(context);

			[OakSavePanel showWithPath:DefaultSaveNameForDocument(_document) directory:_self.saveFolder fowWindow:_window delegate:_self contextInfo:NULL];
		}

		void select_make_writable (std::string const& path, io::bytes_ptr content, file::save_context_ptr context)
		{
			D(DBF_DocumentController_SaveHelper, bug("\n"););
			init(context);

			// TODO “unlock file” checkbox (presently implied)
			NSAlert* alert = [[NSAlert alertWithMessageText:[NSString stringWithCxxString:text::format("The file “%s” is locked.", _document->display_name().c_str())] defaultButton:@"Overwrite" alternateButton:@"Cancel" otherButton:nil informativeTextWithFormat:@"Do you want to overwrite it anyway?"] retain];
			[alert beginSheetModalForWindow:_window modalDelegate:_self didEndSelector:@selector(makeWritableSheetDidEnd:returnCode:contextInfo:) contextInfo:NULL];
		}

		void obtain_authorization (std::string const& path, io::bytes_ptr content, osx::authorization_t auth, file::save_context_ptr context)
		{
			D(DBF_DocumentController_SaveHelper, bug("\n"););
			if(auth.obtain_right(kAuthRightName))
					context->set_authorization(auth);
			else	_self.userAbort = YES;
		}

		void select_encoding (std::string const& path, io::bytes_ptr content, std::string const& encoding, file::save_context_ptr context)
		{
			D(DBF_DocumentController_SaveHelper, bug("\n"););
			init(context);

			if(encoding != kCharsetNoEncoding)
			{
				// TODO transliteration / BOM check box
				NSAlert* alert = [[NSAlert alertWithMessageText:[NSString stringWithCxxString:text::format("Unable to save document using “%s” as encoding.", encoding.c_str())] defaultButton:@"Retry" alternateButton:@"Cancel" otherButton:nil informativeTextWithFormat:@"Please choose another encoding:"] retain];
				[alert setAccessoryView:[[OakEncodingPopUpButton new] autorelease]];
				[alert beginSheetModalForWindow:_window modalDelegate:_self didEndSelector:@selector(encodingSheetDidEnd:returnCode:contextInfo:) contextInfo:NULL];
				[[alert window] recalculateKeyViewLoop];
			}
			else
			{
				context->set_encoding(kCharsetUTF8);
			}
		}

		void did_save_document (document::document_ptr document, std::string const& path, bool success, std::string const& message, oak::uuid_t const& filter)
		{
			D(DBF_DocumentController_SaveHelper, bug("%s, %s\n", path.c_str(), BSTR(success)););
			[_self didSaveDocument:document success:success error:message usingFilter:filter];
		}

	private:
		void init (file::save_context_ptr context)
		{
			document::show(_document);
			for(NSWindow* window in [NSApp orderedWindows])
			{
				id delegate = [window delegate];
				if(![window isMiniaturized] && [delegate isKindOfClass:[[_window delegate] class]])
				{
					_window = [delegate window];
					break;
				}
			}

			[_window.attachedSheet orderOut:_self];
			[_self setContext:context];
		}

		document::document_ptr _document;
		DocumentSaveHelper* _self;
		NSWindow* _window;
	};
}

@implementation DocumentSaveHelper
@synthesize saveFolder, userAbort;

- (id)init
{
	D(DBF_DocumentController_SaveHelper, bug("\n"););
	if(self = [super init])
	{
	}
	return self;
}

+ (void)trySaveDocuments:(std::vector<document::document_ptr> const&)someDocuments forWindow:(NSWindow*)aWindow defaultDirectory:(NSString*)aFolder andCallback:(document_save_callback_t*)aCallback
{
	[[[[DocumentSaveHelper alloc] init] autorelease] trySaveDocuments:someDocuments forWindow:aWindow defaultDirectory:aFolder andCallback:aCallback];
}

+ (void)trySaveDocument:(document::document_ptr const&)aDocument forWindow:(NSWindow*)aWindow defaultDirectory:(NSString*)aFolder andCallback:(document_save_callback_t*)aCallback
{
	[DocumentSaveHelper trySaveDocuments:std::vector<document::document_ptr>(1, aDocument) forWindow:aWindow defaultDirectory:aFolder andCallback:aCallback];
}

- (void)dealloc
{
	D(DBF_DocumentController_SaveHelper, bug("\n"););
	[window release];
	[saveFolder release];
	[super dealloc];
}

- (file::save_context_ptr const&)context                         { return context; }
- (void)setContext:(file::save_context_ptr const&)newContext     { context = newContext; }

- (void)saveNextDocument
{
	if(documents.empty())
		return;

	[self retain]; // keep us retained until document is saved

	document::document_ptr document = documents.back();
	D(DBF_DocumentController_SaveHelper, bug("%s (%zu total)\n", document->display_name().c_str(), documents.size()););
	document->try_save(document::save_callback_ptr((document::save_callback_t*)new save_callback_t(document, self, window)));
}

- (void)trySaveDocuments:(std::vector<document::document_ptr> const&)someDocuments forWindow:(NSWindow*)aWindow defaultDirectory:(NSString*)aFolder andCallback:(document_save_callback_t*)aCallback
{
	documents  = someDocuments;
	window     = [aWindow retain];
	saveFolder = [aFolder retain];
	callback   = aCallback;
	std::reverse(documents.begin(), documents.end());
	[[NSNotificationCenter defaultCenter] postNotificationName:@"OakDocumentNotificationWillSave" object:self];
	[self saveNextDocument];
}

- (void)didSaveDocument:(document::document_ptr const&)aDocument success:(BOOL)flag error:(std::string const&)aMessage usingFilter:(oak::uuid_t const&)aFilter
{
	D(DBF_DocumentController_SaveHelper, bug("‘%s’, success %s, user abort %s\n", aDocument->path().c_str(), BSTR(flag), BSTR(userAbort)););
	if(!flag && !userAbort)
	{
		[window.attachedSheet orderOut:self];
		if(aFilter)
				show_command_error(aMessage, aFilter, window);
		else	[[NSAlert alertWithMessageText:[NSString stringWithCxxString:text::format("The document “%s” could not be saved.", aDocument->display_name().c_str())] defaultButton:@"OK" alternateButton:nil otherButton:nil informativeTextWithFormat:[NSString stringWithCxxString:aMessage] ?: @"Please check Console output for reason."] beginSheetModalForWindow:window modalDelegate:nil didEndSelector:NULL contextInfo:NULL];
	}

	if(callback)
		callback->did_save_document(aDocument, flag, aMessage, aFilter);
	documents.pop_back();
	if(flag)
		[self saveNextDocument];

	if(flag && [[window delegate] respondsToSelector:@selector(updateProxyIcon)])
		[[window delegate] performSelector:@selector(updateProxyIcon)]; // FIXME The delegate needs to update proxy icon based on “exists on disk” notifications from document_t

	[self release];
}

// ===================
// = Sheet Callbacks =
// ===================

- (void)savePanelDidEnd:(OakSavePanel*)sheet path:(NSString*)aPath contextInfo:(void*)info
{
	D(DBF_DocumentController_SaveHelper, bug("%s\n", to_s(aPath).c_str()););
	if(aPath)
	{
		documents.back()->set_path(to_s(aPath));
		context->set_path(to_s(aPath));
	}
	else
	{
		userAbort = YES;
	}
	context.reset();
}

- (void)makeWritableSheetDidEnd:(NSAlert*)alert returnCode:(NSInteger)returnCode contextInfo:(void*)info
{
	D(DBF_DocumentController_SaveHelper, bug("%s\n", BSTR(returnCode == NSAlertDefaultReturn)););
	file::save_context_ptr ctxt = context;
	[self setContext:file::save_context_ptr()];
	if(returnCode == NSAlertDefaultReturn)
			ctxt->set_make_writable(true);
	else	userAbort = YES;
	[alert release];
}

- (void)encodingSheetDidEnd:(NSAlert*)alert returnCode:(NSInteger)returnCode contextInfo:(void*)info
{
	D(DBF_DocumentController_SaveHelper, bug("\n"););
	file::save_context_ptr ctxt = context;
	[self setContext:file::save_context_ptr()];
	userAbort = returnCode != NSAlertDefaultReturn;
	if(!userAbort)
	{
		OakEncodingPopUpButton* popUp = (OakEncodingPopUpButton*)[alert accessoryView];
		std::string encoding = to_s(popUp.encoding);
		bool bom = encoding != "UTF-8" && encoding.find("UTF-") == 0;
		ctxt->set_encoding(encoding, bom);
	}
	[alert release];
}
@end
