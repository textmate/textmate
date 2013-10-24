#import <oak/debug.h>

@protocol OFBOutlineViewMenuDelegate
- (NSMenu*)menuForOutlineView:(NSOutlineView*)anOutlineView;
@end

@interface OFBOutlineView : NSOutlineView
@property (nonatomic, weak) id <OFBOutlineViewMenuDelegate> menuDelegate;
@property (nonatomic) BOOL renderAsSourceList;
- (void)performEditSelectedRow:(id)sender;
@end
