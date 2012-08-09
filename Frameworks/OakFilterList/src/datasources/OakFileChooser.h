#ifndef OakFilterList_EXPORTS
#import <OakFilterList/OakFilterList.h>
#else
#import "../OakFilterList.h"
#endif
#import <OakFoundation/OakTimer.h>
#import <document/document.h>

struct file_chooser_t
{
	struct item_t
	{
		item_t () { }
		item_t (document::document_ptr document, std::string const& matchName, double rank, std::vector< std::pair<size_t, size_t> > const& matchRanges) : _document(document), _match_name(matchName), _rank(rank), _match_ranges(matchRanges) { }
		bool operator< (item_t const& rhs) const;

		document::document_ptr _document;
		std::string _match_name;
		double _rank;
		std::vector< std::pair<size_t, size_t> > _match_ranges;
		size_t _parents = 0;
	};

	void setup (std::string const& path, std::string const& globString, std::string const& rankString);
	void set_excluded_document (oak::uuid_t const& uuid);
	void set_path (std::string const& path);
	void set_filtering (std::string const& globString, std::string const& rankString);

	void set_documents (std::vector<document::document_ptr> const& documents);

	bool running () const;
	bool poll_scanner ();
	void wait () const;
	void stop_scanner ();

	std::string const& glob_string () const { return _glob_string; }
	std::string const& rank_string () const { return _rank_string; }

	std::vector<item_t> const& items () const { return _ranked_items; }

private:
	void add_documents (std::vector<document::document_ptr> const& documents);

	std::tr1::shared_ptr<document::scanner_t> _scanner;

	std::string _path        = NULL_STR;
	std::string _glob_string = NULL_STR;
	std::string _rank_string = NULL_STR;

	oak::uuid_t _excluded_document;

	std::vector<document::document_ptr> _all_documents;
	std::vector<document::document_ptr> _filtered_documents;
	std::vector<item_t> _ranked_items;
};

@interface OakFileChooser : NSObject <FilterListDataSource>
{
	OBJC_WATCH_LEAKS(OakFileChooser)
	NSString* _path;
	NSString* projectPath;

	NSViewController* viewController;

	file_chooser_t helper;
	document::document_ptr document;

	OakTimer* scannerProbeTimer;
	double pollInterval;
	NSUInteger sourceIndex;

	NSString* title;
}
+ (id)fileChooserWithPath:(NSString*)aPath projectPath:(NSString*)project;
@property (nonatomic, assign)   NSUInteger sourceIndex;
@property (nonatomic, retain)   NSString*  path;
@property (nonatomic, readonly) NSString*  projectPath;

@property (nonatomic, retain) NSString* filterString;
@property (nonatomic, retain) NSString* globString;
@property (nonatomic, retain) NSString* excludeDocumentWithIdentifier;
@end
