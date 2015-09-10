#import <oak/debug.h>

@interface OFBOutlineView : NSOutlineView
@property (nonatomic) BOOL renderAsSourceList;
@property (nonatomic) BOOL recursiveRequest;
- (void)performEditSelectedRow:(id)sender;
@end
