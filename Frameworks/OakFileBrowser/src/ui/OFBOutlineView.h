#import <oak/debug.h>

@protocol OFBOutlineViewMenuDelegate;

@interface OFBOutlineView : NSOutlineView
{
	OBJC_WATCH_LEAKS(OFBOutlineView);

	id <OFBOutlineViewMenuDelegate> menuDelegate;
	BOOL fieldEditorWasUp;
	NSRect mouseHoverRect;
	NSIndexSet* draggedRows;
}
@property (nonatomic, assign) id <OFBOutlineViewMenuDelegate> menuDelegate;
- (void)performEditSelectedRow:(id)sender;
@end
