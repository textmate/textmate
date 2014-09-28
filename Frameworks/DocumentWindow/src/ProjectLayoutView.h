@interface ProjectLayoutView : NSView
@property (nonatomic) NSView* tabBarView;
@property (nonatomic) NSView* documentView;
@property (nonatomic) NSView* fileBrowserView;
@property (nonatomic) NSView* fileBrowserHeaderView;
@property (nonatomic) NSView* htmlOutputView;

@property (nonatomic) CGFloat fileBrowserWidth;
@property (nonatomic) BOOL fileBrowserOnRight;

@property (nonatomic) NSSize htmlOutputSize;
@property (nonatomic) BOOL htmlOutputOnRight;
@property (nonatomic) BOOL tabsAboveDocument;
@end
