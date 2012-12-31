#import "DocumentOpenHelper.h"
#import "EncodingView.h"
#import <OakFoundation/NSString Additions.h>
#import <ns/ns.h>
#import <text/parse.h>
#import <oak/debug.h>

OAK_DEBUG_VAR(DocumentController_OpenHelper);

@interface DocumentOpenHelper ()
- (void)didOpenDocument:(document::document_ptr const&)aDocument;
- (void)failedToOpenDocument:(document::document_ptr const&)aDocument error:(std::string const&)aMessage usingFilter:(oak::uuid_t const&)filterUUID;
@end

namespace
{
	struct info_t
	{
		info_t (EncodingViewController* controller, file::open_context_ptr context) : controller(controller), context(context) { }

		EncodingViewController* controller;
		file::open_context_ptr context;
	};

	struct open_callback_t : document::open_callback_t
	{
		open_callback_t (DocumentOpenHelper* self, NSWindow* window) : _self(self), _window(window)
		{
			ASSERT(_window);
		}

		void select_charset (std::string const& path, io::bytes_ptr content, file::open_context_ptr context)
		{
			[_window.attachedSheet orderOut:_self];

			NSAlert* alert = [NSAlert alertWithMessageText:@"Unknown Encoding" defaultButton:@"Continue" alternateButton:@"Cancel" otherButton:nil informativeTextWithFormat:@"This file is not UTF-8 nor does it have any encoding information stored."];
			EncodingViewController* controller = [[EncodingViewController alloc] initWithFirst:content->begin() last:content->end()];
			[alert setAccessoryView:controller.view];
			[alert beginSheetModalForWindow:_window modalDelegate:_self didEndSelector:@selector(selectEncodingSheetDidEnd:returnCode:contextInfo:) contextInfo:new info_t(controller, context)];
			[[alert window] recalculateKeyViewLoop];
		}

		void select_file_type (std::string const& path, io::bytes_ptr content, file::open_context_ptr context)
		{
			if(path == NULL_STR)
			{
				context->set_file_type("text.plain");
			}
			else
			{
				[_window.attachedSheet orderOut:_self];

				FileTypeDialog* controller = [[FileTypeDialog alloc] initWithPath:[NSString stringWithCxxString:path] first:(content ? content->begin() : NULL) last:(content ? content->end() : NULL)];
				[controller beginSheetModalForWindow:_window modalDelegate:_self contextInfo:new info_t(nil, context)];
			}
		}

		void show_document (std::string const& path, document::document_ptr document)
		{
			[_self didOpenDocument:document];
			document->close();
		}

		void show_error (std::string const& path, document::document_ptr document, std::string const& message, oak::uuid_t const& filter)
		{
			[_self failedToOpenDocument:document error:message usingFilter:filter];
		}

	private:
		DocumentOpenHelper* _self;
		NSWindow* _window;
	};
}

@implementation DocumentOpenHelper
- (id)init
{
	if(self = [super init])
	{
	}
	return self;
}

- (void)tryOpenDocument:(document::document_ptr const&)aDocument forWindow:(NSWindow*)aWindow delegate:(id <DocumentOpenHelperDelegate>)aDelegate
{
	D(DBF_DocumentController_OpenHelper, bug("%s, already open %s\n", aDocument->display_name().c_str(), BSTR(aDocument->is_open())););

	self.delegate = aDelegate;
	if(aDocument->try_open(document::open_callback_ptr((document::open_callback_t*)new open_callback_t(self, aWindow))))
	{
		[self didOpenDocument:aDocument];
		aDocument->close();
	}
}

- (void)selectEncodingSheetDidEnd:(NSAlert*)alert returnCode:(NSInteger)returnCode contextInfo:(info_t*)info
{
	if(returnCode == NSAlertDefaultReturn)
		info->context->set_charset(text::split(to_s(info->controller.currentEncoding), " ")[0]);
	delete info;
}

- (void)fileTypeDialog:(FileTypeDialog*)fileTypeDialog didSelectFileType:(NSString*)aFileType contextInfo:(void*)info
{
	if(aFileType)
		((info_t*)info)->context->set_file_type(to_s(aFileType));
	delete (info_t*)info;
}

- (void)didOpenDocument:(document::document_ptr const&)aDocument
{
	D(DBF_DocumentController_OpenHelper, bug("%s\n", aDocument->display_name().c_str()););
	if(aDocument->recent_tracking() && aDocument->path() != NULL_STR)
		[[NSDocumentController sharedDocumentController] noteNewRecentDocumentURL:[NSURL fileURLWithPath:[NSString stringWithCxxString:aDocument->path()]]]; 
	if([self.delegate respondsToSelector:@selector(documentOpenHelper:didOpenDocument:)])
		[self.delegate documentOpenHelper:self didOpenDocument:aDocument];
}

- (void)failedToOpenDocument:(document::document_ptr const&)aDocument error:(std::string const&)aMessage usingFilter:(oak::uuid_t const&)filterUUID
{
	D(DBF_DocumentController_OpenHelper, bug("%s\n", aDocument->display_name().c_str()););
	if([self.delegate respondsToSelector:@selector(documentOpenHelper:failedToOpenDocument:error:usingFilter:)])
		[self.delegate documentOpenHelper:self failedToOpenDocument:aDocument error:aMessage usingFilter:filterUUID];
}
@end
