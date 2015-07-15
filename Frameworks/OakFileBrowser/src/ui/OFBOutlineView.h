#import <oak/debug.h>

@protocol OFBOutlineViewMenuDelegate
- (NSMenu*)menuForOutlineView:(NSOutlineView*)anOutlineView;
@end

@interface OFBOutlineView : NSOutlineView
@property (nonatomic, weak) id <OFBOutlineViewMenuDelegate> menuDelegate;
@property (nonatomic) BOOL renderAsSourceList;
@property (nonatomic) BOOL recursiveRequest;
- (void)performEditSelectedRow:(id)sender;
@end
