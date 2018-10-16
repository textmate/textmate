@protocol FileBrowserOutlineViewDelegate <NSOutlineViewDelegate>
- (void)outlineView:(NSOutlineView*)outlineView willExpandItem:(id)someItem expandChildren:(BOOL)flag;
- (void)outlineView:(NSOutlineView*)outlineView didExpandItem:(id)someItem expandChildren:(BOOL)flag;
- (void)outlineView:(NSOutlineView*)outlineView willCollapseItem:(id)someItem collapseChildren:(BOOL)flag;
- (void)outlineView:(NSOutlineView*)outlineView didCollapseItem:(id)someItem collapseChildren:(BOOL)flag;
- (void)outlineView:(NSOutlineView*)outlineView didTrashURLs:(NSArray<NSURL*>*)someURLs;
@end

@interface FileBrowserOutlineView : NSOutlineView
@end
