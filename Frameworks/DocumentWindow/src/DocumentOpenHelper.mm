#import "DocumentOpenHelper.h"
#import "EncodingView.h"
#import "FileTypeDialog.h"
#import <OakAppKit/OakAppKit.h>
#import <OakFoundation/NSString Additions.h>
#import <encoding/encoding.h>
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
		open_callback_t (document::document_ptr const& document, DocumentOpenHelper* self, NSWindow* window) : _document(document), _self(self), _window(window)
		{
			ASSERT(_window);
		}

		void select_charset (std::string const& path, io::bytes_ptr content, file::open_context_ptr context)
		{
			[_window.attachedSheet orderOut:_self];

			EncodingWindowController* controller = [[EncodingWindowController alloc] initWithFirst:content->begin() last:content->end()];
			controller.displayName = [NSString stringWithCxxString:_document->display_name()];

			__block encoding::classifier_t db;
			static std::string const kEncodingFrequenciesPath = path::join(path::home(), "Library/Caches/com.macromates.TextMate/EncodingFrequencies.binary");
			db.load(kEncodingFrequenciesPath);

			std::multimap<double, std::string> probabilities;
			for(auto const& charset : db.charsets())
				probabilities.emplace(1 - db.probability(content->begin(), content->end(), charset), charset);
			if(!probabilities.empty() && probabilities.begin()->first < 1)
				controller.encoding = [NSString stringWithCxxString:probabilities.begin()->second];

			[controller.window layoutIfNeeded];
			OakShowSheetForWindow(controller.window, _window, ^(NSInteger returnCode){
				if(returnCode != NSRunAbortedResponse)
				{
					context->set_charset(to_s(controller.encoding));
					if(controller.trainClassifier)
					{
						db.learn(content->begin(), content->end(), to_s(controller.encoding));
						db.save(kEncodingFrequenciesPath);
					}
				}
			});
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
		document::document_ptr _document;
		DocumentOpenHelper* _self;
		NSWindow* _window;
	};
}

@implementation DocumentOpenHelper
- (void)tryOpenDocument:(document::document_ptr const&)aDocument forWindow:(NSWindow*)aWindow completionHandler:(void(^)(std::string const& error, oak::uuid_t const& filterUUID))aCompletionHandler
{
	D(DBF_DocumentController_OpenHelper, bug("%s, already open %s\n", aDocument->display_name().c_str(), BSTR(aDocument->is_open())););
	self.callback = aCompletionHandler;
	if(aDocument->try_open(std::make_shared<open_callback_t>(aDocument, self, aWindow)))
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
