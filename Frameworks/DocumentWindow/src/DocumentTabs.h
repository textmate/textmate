#import "DocumentController.h"

NSInteger const kSelectDocumentFirst = -1;
NSInteger const kSelectDocumentLast  = -2;
NSInteger const kSelectDocumentNone  = -3;

struct PUBLIC close_warning_callback_t
{
	virtual ~close_warning_callback_t () { }
	virtual void will_save_documents ()  { }
	virtual void can_close_documents (bool flag) = 0;
};

@interface DocumentController (Tabs)
- (NSInteger)selectedTabIndex;
- (void)setSelectedTabIndex:(NSInteger)newSelectedTabIndex;

- (document::document_ptr const&)selectedDocument;
- (void)documentDidChange:(document::document_ptr const&)aDocument;

- (void)addDocuments:(std::vector<document::document_ptr> const&)someDocuments andSelect:(NSInteger)selectionConstant closeOther:(BOOL)closeOtherFlag pruneTabBar:(BOOL)pruneTabBarFlag;
- (void)addDocuments:(std::vector<document::document_ptr> const&)someDocuments atIndex:(size_t)insertAt andSelect:(NSInteger)selectionConstant closeOther:(BOOL)closeOtherFlag pruneTabBar:(BOOL)pruneTabBarFlag;
- (void)closeTabsAtIndexes:(NSIndexSet*)anIndexSet quiet:(BOOL)quietFlag;
- (void)showCloseWarningUIForDocuments:(std::vector<document::document_ptr> const&)someDocuments andCallback:(close_warning_callback_t*)aCallback;
- (NSApplicationTerminateReply)applicationShouldTerminate:(NSApplication*)sender;

- (void)updateFileBrowserStatus:(id)sender;
@end

struct document_tab_t
{
	document_tab_t (document::document_ptr doc) : _document(doc), _did_open(false)
	{
		if(_document->is_open())
		{
			_document->open();
			_did_open = true;
		}
	}

	~document_tab_t ()
	{
		if(_callback)
			_document->remove_callback(_callback.get());
		if(_did_open)
			_document->close();
	}

	operator document::document_ptr const& () const { return _document; }

	struct callback_t : document::document_t::callback_t
	{
		callback_t (DocumentController* self) : _self(self) { }

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
		DocumentController* _self;
	};

	typedef std::tr1::shared_ptr<callback_t> callback_ptr;

	void add_callback (DocumentController* self)
	{
		ASSERT(!_callback);
		_callback.reset(new callback_t(self));
		_document->add_callback(_callback.get());
	}

	document::document_ptr _document;
	callback_ptr _callback;
	bool _did_open;
	bool _did_add_callback;
};
