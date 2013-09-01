#import "DocumentOpenHelper.h"
#import "EncodingView.h"
#import "FileTypeDialog.h"
#import <OakAppKit/OakAppKit.h>
#import <OakFoundation/NSString Additions.h>
#import <ns/ns.h>
#import <text/parse.h>
#import <oak/debug.h>

OAK_DEBUG_VAR(DocumentController_OpenHelper);

@interface DocumentOpenHelper ()
@property (nonatomic, copy) void(^callback)(std::string const&, oak::uuid_t const&);

- (void)didOpenDocument:(document::document_ptr const&)aDocument;
- (void)failedToOpenDocument:(document::document_ptr const&)aDocument error:(std::string const&)aMessage usingFilter:(oak::uuid_t const&)filterUUID;
@end

namespace
{
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
			OakShowAlertForWindow(alert, _window, ^(NSInteger returnCode){
				if(returnCode == NSAlertDefaultReturn)
					context->set_charset(text::split(to_s(controller.currentEncoding), " ")[0]);
			});
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
				[controller beginSheetModalForWindow:_window completionHandler:^(NSString* fileType){
					if(fileType)
						context->set_file_type(to_s(fileType));
				}];
			}
		}

		void show_document (std::string const& path, document::document_ptr document)
		{
			if(path != NULL_STR)
			{
				auto const settings = settings_for_path(document->virtual_path(), document->file_type(), path::parent(path), document->document_variables());
				document->set_indent(text::indent_t(std::max(1, settings.get(kSettingsTabSizeKey, 4)), SIZE_T_MAX, settings.get(kSettingsSoftTabsKey, false)));
			}

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
- (void)tryOpenDocument:(document::document_ptr const&)aDocument forWindow:(NSWindow*)aWindow completionHandler:(void(^)(std::string const& error, oak::uuid_t const& filterUUID))aCompletionHandler
{
	D(DBF_DocumentController_OpenHelper, bug("%s, already open %s\n", aDocument->display_name().c_str(), BSTR(aDocument->is_open())););
	self.callback = aCompletionHandler;
	if(aDocument->try_open(std::make_shared<open_callback_t>(self, aWindow)))
	{
		[self didOpenDocument:aDocument];
		aDocument->close();
	}
}

- (void)didOpenDocument:(document::document_ptr const&)aDocument
{
	D(DBF_DocumentController_OpenHelper, bug("%s\n", aDocument->display_name().c_str()););
	if(aDocument->recent_tracking() && aDocument->path() != NULL_STR)
		[[NSDocumentController sharedDocumentController] noteNewRecentDocumentURL:[NSURL fileURLWithPath:[NSString stringWithCxxString:aDocument->path()]]]; 
	self.callback(NULL_STR, oak::uuid_t());
}

- (void)failedToOpenDocument:(document::document_ptr const&)aDocument error:(std::string const&)aMessage usingFilter:(oak::uuid_t const&)filterUUID
{
	D(DBF_DocumentController_OpenHelper, bug("%s\n", aDocument->display_name().c_str()););
	self.callback(aMessage == NULL_STR ? "unknown error" : aMessage, filterUUID);
}
@end
