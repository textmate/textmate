#import <oak/debug.h>

@protocol OFBOutlineViewMenuDelegate;

@interface OFBOutlineView : NSOutlineView
{
	OBJC_WATCH_LEAKS(OFBOutlineView);

	BOOL fieldEditorWasUp;
	NSRect mouseHoverRect;
	NSIndexSet* draggedRows;
}
@property (nonatomic, weak) id <OFBOutlineViewMenuDelegate> menuDelegate;
- (void)performEditSelectedRow:(id)sender;
@end
