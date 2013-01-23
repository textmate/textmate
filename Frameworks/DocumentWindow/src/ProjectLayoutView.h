@interface ProjectLayoutView : NSView
@property (nonatomic, retain) NSView* tabBarView;
@property (nonatomic, retain) NSView* documentView;
@property (nonatomic, retain) NSView* fileBrowserView;
@property (nonatomic, retain) NSView* htmlOutputView;

@property (nonatomic, assign) CGFloat fileBrowserWidth;
@property (nonatomic, assign) BOOL fileBrowserOnRight;

@property (nonatomic, assign) NSSize htmlOutputSize;
@property (nonatomic, assign) BOOL htmlOutputOnRight;
@property (nonatomic, assign) BOOL tabsAboveDocument;
@end
