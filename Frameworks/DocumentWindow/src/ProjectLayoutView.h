#import <OakTabBarView/OakTabBarView.h>

@interface ProjectLayoutView : NSView
@property (nonatomic) NSView* documentView;
@property (nonatomic) NSView* fileBrowserView;
@property (nonatomic) NSView* htmlOutputView;

@property (nonatomic) CGFloat fileBrowserWidth;
@property (nonatomic) BOOL fileBrowserOnRight;

@property (nonatomic) NSSize htmlOutputSize;
@property (nonatomic) BOOL htmlOutputOnRight;
@end
