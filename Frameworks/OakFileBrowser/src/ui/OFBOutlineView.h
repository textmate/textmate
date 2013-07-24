#import <oak/debug.h>

@protocol OFBOutlineViewMenuDelegate
- (NSMenu*)menuForOutlineView:(NSOutlineView*)anOutlineView;
@end

@interface OFBOutlineView : NSOutlineView
{
	OBJC_WATCH_LEAKS(OFBOutlineView);

	BOOL fieldEditorWasUp;
	NSRect mouseHoverRect;
	NSIndexSet* draggedRows;
}
@property (nonatomic, weak) id <OFBOutlineViewMenuDelegate> menuDelegate;
@property (nonatomic) BOOL renderAsSourceList;
- (void)performEditSelectedRow:(id)sender;
@end
