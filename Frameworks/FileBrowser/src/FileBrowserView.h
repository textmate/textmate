#include <oak/misc.h>

@class OFBHeaderView;
@class OFBActionsView;

@interface FileBrowserView : NSView
@property (nonatomic) OFBHeaderView*  headerView;
@property (nonatomic) NSOutlineView*  outlineView;
@property (nonatomic) OFBActionsView* actionsView;
@end
