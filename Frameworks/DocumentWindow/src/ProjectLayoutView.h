#import <Cocoa/Cocoa.h>

@interface ProjectLayoutView : NSSplitViewController
@property (nonatomic) NSView* documentView;
@property (nonatomic) NSView* fileBrowserView;
@property (nonatomic) NSView* htmlOutputView;

@property (nonatomic) CGFloat fileBrowserWidth;

@property (nonatomic) NSSize htmlOutputSize;
@end
