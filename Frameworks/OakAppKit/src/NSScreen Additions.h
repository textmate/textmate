@interface NSScreen (Additions)
+ (NSScreen*)screenWithFrame:(NSRect)rect;
- (NSRect)restrainFrameToVisibleScreen:(NSRect)aFrame;
@end