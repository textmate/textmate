@class FSDataSource;

@interface FSOutlineViewDelegate : NSObject <NSOutlineViewDelegate>
{
	IBOutlet NSOutlineView* outlineView;
	IBOutlet FSDataSource* dataSource;
	NSArray* openURLs;
	NSArray* modifiedURLs;

	NSMutableSet* expandedURLs;
	NSMutableSet* selectedURLs;

	NSInteger itemsReloading;
	NSInteger suppressCollapsing;
	BOOL suppressAutoExpansion;

	NSMutableSet* recursiveExpandPaths;
	NSSet* pendingSelectURLs;
	NSURL* pendingEditURL;
	NSURL* pendingMakeVisibleURL;
	CGFloat pendingScrollOffset;
}
@property (nonatomic, retain) NSOutlineView* outlineView;
@property (nonatomic, retain) FSDataSource* dataSource;
@property (nonatomic, retain) NSArray* openURLs;
@property (nonatomic, retain) NSArray* modifiedURLs;

- (void)selectURLs:(NSArray*)someURLs expandChildren:(BOOL)expandAncestors;
- (void)editURL:(NSURL*)anURL;
- (void)scrollToOffset:(CGFloat)anOffset;
@end
