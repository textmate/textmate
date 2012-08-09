@interface HOAutoScroll : NSObject
{
	WebFrameView* webFrame;
	NSRect lastFrame, lastVisibleRect;
}
@property (nonatomic, retain) WebFrameView* webFrame;
@end
