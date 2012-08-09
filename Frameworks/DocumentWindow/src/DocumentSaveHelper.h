#import <document/document.h>
#import <file/save.h>

struct PUBLIC document_save_callback_t
{
	virtual ~document_save_callback_t () { }
	virtual void did_save_document (document::document_ptr document, bool flag, std::string const& message, oak::uuid_t const& filter) = 0;
};

@interface DocumentSaveHelper : NSObject
{
	std::vector<document::document_ptr> documents;
	NSWindow* window;
	NSString* saveFolder;
	document_save_callback_t* callback;
	file::save_context_ptr context;
	BOOL userAbort;
}
+ (void)trySaveDocuments:(std::vector<document::document_ptr> const&)someDocuments forWindow:(NSWindow*)aWindow defaultDirectory:(NSString*)aFolder andCallback:(document_save_callback_t*)aCallback;
+ (void)trySaveDocument:(document::document_ptr const&)aDocument forWindow:(NSWindow*)aWindow defaultDirectory:(NSString*)aFolder andCallback:(document_save_callback_t*)aCallback;
@end

NSString* DefaultSaveNameForDocument (document::document_ptr const& aDocument);
