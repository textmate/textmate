#include <oak/misc.h>

@class FileItem;
@class OFBHeaderView;
@class OFBActionsView;

@interface FileBrowserView : NSView
@property (nonatomic) OFBHeaderView*  headerView;
@property (nonatomic) NSOutlineView*  outlineView;
@property (nonatomic) OFBActionsView* actionsView;

@property (nonatomic, weak) id target;
@property (nonatomic) SEL openAction;
@property (nonatomic) SEL closeAction;

@property (nonatomic) NSURL* URL;
@property (nonatomic) NSArray<NSURL*>* openURLs;
@property (nonatomic) NSArray<NSURL*>* modifiedURLs;

@property (nonatomic) BOOL showExcludedItems;

@property (nonatomic, readonly) FileItem* fileItem;
@property (nonatomic, readonly) NSArray<FileItem*>* selectedItems;
@property (nonatomic, readonly) NSArray<FileItem*>* previewableItems;
@property (nonatomic, readonly) NSURL* directoryURLForNewItems;

@property (nonatomic, readonly) NSSet<NSURL*>* expandedURLs;
@property (nonatomic, readonly) NSSet<NSURL*>* selectedURLs;

- (void)reload:(id)sender;
- (void)expandURLs:(NSArray<NSURL*>*)expandURLs selectURLs:(NSArray<NSURL*>*)selectURLs;
- (NSRect)imageRectOfItem:(FileItem*)item;

- (NSComparator)itemComparator;
- (NSArray<FileItem*>*)arrangeChildren:(NSArray<FileItem*>*)children inParent:(FileItem*)parentOrNil;
- (void)rearrangeChildrenInParent:(FileItem*)item;
- (void)updateDisambiguationSuffixInParent:(FileItem*)item;
@end

typedef NS_OPTIONS(NSUInteger, FBOperation) {
	FBOperationLink      = 0x0001,
	FBOperationCopy      = 0x0002,
	FBOperationDuplicate = 0x0004,
	FBOperationMove      = 0x0008,
	FBOperationRename    = 0x0010,
	FBOperationTrash     = 0x0020,
	FBOperationNewFile   = 0x0040,
	FBOperationNewFolder = 0x0080,
};

@interface FileBrowserView (DiskOperations)
- (NSArray<NSURL*>*)performOperation:(FBOperation)op withURLs:(NSDictionary<NSURL*, NSURL*>*)urls unique:(BOOL)makeUnique select:(BOOL)selectDestinationURLs;
- (NSArray<NSURL*>*)performOperation:(FBOperation)op sourceURLs:(NSArray<NSURL*>*)srcURLs destinationURLs:(NSArray<NSURL*>*)destURLs unique:(BOOL)makeUnique select:(BOOL)selectDestinationURLs;
@end
