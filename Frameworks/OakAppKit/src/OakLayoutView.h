#include "box.h"

struct OakResizeInfo
{
	int width, height;
	enum { kTopLeft, kTopRight, kBottomLeft, kBottomRight } corner;
	enum { kWidth, kHeight } adjustable;
};

@interface OakLayoutView : NSView
{
	box_t* box;
	std::map<NSView*, box_t*> views;
	std::multimap<NSView*, OakResizeInfo> resize_info;
}
- (void)addView:(NSView*)aView;
- (void)addView:(NSView*)aView atEdge:(NSRectEdge)anEdge ofView:(NSView*)otherView;
- (void)removeView:(NSView*)aView;
- (void)setLocked:(BOOL)flag forView:(NSView*)aView;
- (void)addResizeInfo:(OakResizeInfo)info forView:(NSView*)aView;
- (void)removeResizeInfoForView:(NSView*)aView;
@end
