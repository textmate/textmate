@class FSDataSource;

@interface FSOutlineViewDelegate : NSObject
@property (nonatomic, retain) NSOutlineView* outlineView;
@property (nonatomic, retain) FSDataSource* dataSource;
@property (nonatomic, retain) NSArray* openURLs;
@property (nonatomic, retain) NSArray* modifiedURLs;

- (void)selectURLs:(NSArray*)someURLs expandChildren:(BOOL)expandAncestors;
- (void)editURL:(NSURL*)anURL;
- (void)scrollToOffset:(CGFloat)anOffset;
@end
