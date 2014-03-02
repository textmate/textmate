#import "DocumentSaveHelper.h"
#import "DocumentCommand.h"
#import <OakFoundation/NSString Additions.h>
#import <OakAppKit/NSAlert Additions.h>
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/OakSavePanel.h>
#import <OakAppKit/OakEncodingPopUpButton.h>
#import <ns/ns.h>
#import <text/parse.h>
#import <regexp/glob.h>
#import <authorization/constants.h>
#import <document/collection.h>
#import <file/encoding.h>
#import <file/save.h>
#import <bundles/bundles.h>

OAK_DEBUG_VAR(DocumentController_SaveHelper);

NSString* DefaultSaveNameForDocument (document::document_ptr const& aDocument)
{
	for(auto const& item : bundles::query(bundles::kFieldGrammarScope, aDocument->file_type()))
	{
		std::string const& ext = item->value_for_field(bundles::kFieldGrammarExtension);
		if(ext != NULL_STR)
			return [NSString stringWithCxxString:aDocument->display_name() + "." + ext];
	}
	return [NSString stringWithCxxString:aDocument->display_name()];
}

@interface DocumentSaveHelper ()
{
	std::vector<document::document_ptr> documents;
	file::save_context_ptr context;
}
- (void)didSaveDocument:(document::document_ptr const&)aDocument success:(BOOL)flag error:(std::string const&)aMessage usingFilter:(oak::uuid_t const&)aFilter;
- (file::save_context_ptr const&)context;
- (void)setContext:(file::save_context_ptr const&)newContext;
@property (nonatomic) NSWindow* window;
@property (nonatomic) NSString* saveFolder;
@property (nonatomic) BOOL userAbort;
@property (nonatomic) BOOL failed;
@property (nonatomic, copy) void (^callback)(BOOL);
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

			[OakSavePanel showWithPath:DefaultSaveNameForDocument(_document) directory:_self.saveFolder fowWindow:_window encoding:_document->encoding_for_save_as_path(to_s([_self.saveFolder stringByAppendingPathComponent:DefaultSaveNameForDocument(_document)])) completionHandler:^(NSString* path, encoding::type const& encoding){
				D(DBF_DocumentController_SaveHelper, bug("%s\n", to_s(path).c_str()););
				if(path)
				{
					_document->set_path(to_s(path));
					_document->set_disk_encoding(encoding);
					context->set_path(to_s(path));
				}
				else
				{
					_self.userAbort = YES;
				}
				[_self setContext:file::save_context_ptr()];
			}];
		}

		void select_make_writable (std::string const& path, io::bytes_ptr content, file::save_context_ptr context)
		{
			D(DBF_DocumentController_SaveHelper, bug("\n"););
			init(context);

			// TODO “unlock file” checkbox (presently implied)
			NSAlert* alert = [NSAlert tmAlertWithMessageText:[NSString stringWithCxxString:text::format("The file “%s” is locked.", _document->display_name().c_str())] informativeText:@"Do you want to overwrite it anyway?" buttons:@"Overwrite", @"Cancel", nil];
			OakShowAlertForWindow(alert, _window, ^(NSInteger returnCode){
				if(returnCode == NSAlertFirstButtonReturn)
						context->set_make_writable(true);
				else	_self.userAbort = YES;
				[_self setContext:file::save_context_ptr()];
			});
		}

		void select_create_parent (std::string const& path, io::bytes_ptr content, file::save_context_ptr context)
		{
			D(DBF_DocumentController_SaveHelper, bug("\n"););
			init(context);

			NSAlert* alert = [NSAlert tmAlertWithMessageText:[NSString stringWithCxxString:text::format("No parent folder for “%s”.", _document->display_name().c_str())] informativeText:[NSString stringWithFormat:@"Do you wish to create a folder at “%@”?", [NSString stringWithCxxString:path::with_tilde(path::parent(path))]] buttons:@"Create Folder", @"Cancel", nil];
			OakShowAlertForWindow(alert, _window, ^(NSInteger returnCode){
				if(returnCode == NSAlertFirstButtonReturn)
						context->set_create_parent(true);
				else	_self.userAbort = YES;
				[_self setContext:file::save_context_ptr()];
			});
		}

		void obtain_authorization (std::string const& path, io::bytes_ptr content, osx::authorization_t auth, file::save_context_ptr context)
		{
			D(DBF_DocumentController_SaveHelper, bug("\n"););
			if(auth.obtain_right(kAuthRightName))
					context->set_authorization(auth);
			else	_self.userAbort = YES;
		}

		void select_charset (std::string const& path, io::bytes_ptr content, std::string const& charset, file::save_context_ptr context)
		{
			D(DBF_DocumentController_SaveHelper, bug("\n"););
			init(context);

			if(charset != kCharsetNoEncoding)
			{
				NSAlert* alert = [NSAlert tmAlertWithMessageText:[NSString stringWithCxxString:text::format("Unable to save document using “%s” as encoding.", charset.c_str())] informativeText:@"Please choose another encoding:" buttons:@"Retry", @"Cancel", nil];
				OakEncodingPopUpButton* encodingPopUp = [OakEncodingPopUpButton new];
				[alert setAccessoryView:encodingPopUp];
				OakShowAlertForWindow(alert, _window, ^(NSInteger returnCode){
					if(returnCode == NSAlertFirstButtonReturn)
							context->set_charset(to_s(encodingPopUp.encoding));
					else	_self.userAbort = YES;
					[_self setContext:file::save_context_ptr()];
				});
				[[alert window] recalculateKeyViewLoop];
			}
			else
			{
				context->set_charset(kCharsetUTF8);
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
- (id)init
{
	D(DBF_DocumentController_SaveHelper, bug("\n"););
	if(self = [super init])
	{
	}
	return self;
}

+ (void)trySaveDocuments:(std::vector<document::document_ptr> const&)someDocuments forWindow:(NSWindow*)aWindow defaultDirectory:(NSString*)aFolder completionHandler:(void(^)(BOOL success))callback
{
	[[[DocumentSaveHelper alloc] init] trySaveDocuments:someDocuments forWindow:aWindow defaultDirectory:aFolder completionHandler:callback];
}

+ (void)trySaveDocument:(document::document_ptr const&)aDocument forWindow:(NSWindow*)aWindow defaultDirectory:(NSString*)aFolder completionHandler:(void(^)(BOOL success))callback
{
	[DocumentSaveHelper trySaveDocuments:std::vector<document::document_ptr>(1, aDocument) forWindow:aWindow defaultDirectory:aFolder completionHandler:callback];
}

- (void)dealloc
{
	D(DBF_DocumentController_SaveHelper, bug("\n"););
}

- (file::save_context_ptr const&)context                         { return context; }
- (void)setContext:(file::save_context_ptr const&)newContext     { context = newContext; }

- (void)saveNextDocument
{
	if(documents.empty() || self.failed)
	{
		if(self.callback)
			self.callback(!self.failed);
		documents.clear();
		return;
	}

	document::document_ptr document = documents.back();
	D(DBF_DocumentController_SaveHelper, bug("%s (%zu total)\n", document->display_name().c_str(), documents.size()););
	document->try_save(std::make_shared<save_callback_t>(document, self, self.window));
}

- (void)trySaveDocuments:(std::vector<document::document_ptr> const&)someDocuments forWindow:(NSWindow*)aWindow defaultDirectory:(NSString*)aFolder completionHandler:(void(^)(BOOL success))callback
{
	documents       = someDocuments;
	self.window     = aWindow;
	self.saveFolder = aFolder;
	self.callback   = callback;

	std::reverse(documents.begin(), documents.end());
	[[NSNotificationCenter defaultCenter] postNotificationName:@"OakDocumentNotificationWillSave" object:self userInfo:@{ @"window" : self.window }];
	[self saveNextDocument];
}

- (void)didSaveDocument:(document::document_ptr const&)aDocument success:(BOOL)flag error:(std::string const&)aMessage usingFilter:(oak::uuid_t const&)aFilter
{
	D(DBF_DocumentController_SaveHelper, bug("‘%s’, success %s, user abort %s\n", aDocument->path().c_str(), BSTR(flag), BSTR(self.userAbort)););
	if(flag)
	{
		[[NSNotificationCenter defaultCenter] postNotificationName:@"OakDocumentNotificationDidSave" object:self userInfo:@{ @"window" : self.window }];
	}
	else if(!self.userAbort)
	{
		[self.window.attachedSheet orderOut:self];
		if(aFilter)
				show_command_error(aMessage, aFilter, self.window);
		else	[[NSAlert tmAlertWithMessageText:[NSString stringWithCxxString:text::format("The document “%s” could not be saved.", aDocument->display_name().c_str())] informativeText:([NSString stringWithCxxString:aMessage] ?: @"Please check Console output for reason.") buttons:@"OK", nil] beginSheetModalForWindow:self.window modalDelegate:nil didEndSelector:NULL contextInfo:NULL];
	}

	documents.pop_back();
	self.failed = self.failed || !flag || self.userAbort;
	[self saveNextDocument];
}
@end
