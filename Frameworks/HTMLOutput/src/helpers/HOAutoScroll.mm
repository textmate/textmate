#import "HOAutoScroll.h"
#import <oak/debug.h>

@interface HOAutoScroll ()
@property (nonatomic) NSRect lastFrame;
@property (nonatomic) NSRect lastVisibleRect;
@end

@implementation HOAutoScroll
- (void)scrollViewToBottom:(NSView*)aView
{
	NSRect visibleRect = [aView visibleRect];
	visibleRect.origin.y = NSHeight([aView frame]) - NSHeight(visibleRect);
	[aView scrollRectToVisible:visibleRect];
}

- (void)dealloc
{
	self.webFrame = nil;
}

- (void)setWebFrame:(WebFrameView*)aWebFrame
{
	if(aWebFrame == _webFrame)
		return;

	if(_webFrame = aWebFrame)
	{
		[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(webViewDidChangeFrame:) name:NSViewFrameDidChangeNotification object:nil];
		[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(webViewDidChangeBounds:) name:NSViewBoundsDidChangeNotification object:nil];

		_lastFrame       = [[_webFrame documentView] frame];
		_lastVisibleRect = [[_webFrame documentView] visibleRect];
	}
	else
	{
		[NSNotificationCenter.defaultCenter removeObserver:self name:NSViewFrameDidChangeNotification object:nil];
		[NSNotificationCenter.defaultCenter removeObserver:self name:NSViewBoundsDidChangeNotification object:nil];
	}
}

- (void)webViewDidChangeBounds:(NSNotification*)aNotification
{
	NSClipView* clipView = [[[_webFrame documentView] enclosingScrollView] contentView];
	if(clipView != [aNotification object])
		return;

	_lastVisibleRect = [[clipView documentView] visibleRect];
}

- (void)webViewDidChangeFrame:(NSNotification*)aNotification
{
	NSView* view = [aNotification object];
	if(view != _webFrame && view != [_webFrame documentView])
		return;

	if(view == [_webFrame documentView])
	{
		if(NSMaxY(_lastVisibleRect) >= NSMaxY(_lastFrame))
		{
			[self scrollViewToBottom:view];
			_lastVisibleRect = [view visibleRect];
		}
		_lastFrame = [view frame];
	}

	if(view == _webFrame)
	{
		if(NSMaxY(_lastVisibleRect) >= NSMaxY(_lastFrame))
			[self scrollViewToBottom:[_webFrame documentView]];
	}
}
@end
