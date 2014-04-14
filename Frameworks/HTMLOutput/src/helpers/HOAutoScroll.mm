#import "HOAutoScroll.h"
#import <oak/debug.h>

OAK_DEBUG_VAR(HTMLOutput_AutoScroll);

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
	D(DBF_HTMLOutput_AutoScroll, bug("\n"););
	self.webFrame = nil;
}

- (void)setWebFrame:(WebFrameView*)aWebFrame
{
	D(DBF_HTMLOutput_AutoScroll, bug("%s\n", [aWebFrame description].UTF8String););
	if(aWebFrame == _webFrame)
		return;

	if(_webFrame = aWebFrame)
	{
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(webViewDidChangeFrame:) name:NSViewFrameDidChangeNotification object:nil];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(webViewDidChangeBounds:) name:NSViewBoundsDidChangeNotification object:nil];

		_lastFrame       = [[_webFrame documentView] frame];
		_lastVisibleRect = [[_webFrame documentView] visibleRect];
	}
	else
	{
		[[NSNotificationCenter defaultCenter] removeObserver:self name:NSViewFrameDidChangeNotification object:nil];
		[[NSNotificationCenter defaultCenter] removeObserver:self name:NSViewBoundsDidChangeNotification object:nil];
	}
}

- (void)webViewDidChangeBounds:(NSNotification*)aNotification
{
	NSClipView* clipView = [[[_webFrame documentView] enclosingScrollView] contentView];
	if(clipView != [aNotification object])
		return;

	D(DBF_HTMLOutput_AutoScroll, bug("bounds changed: %s → %s\n", NSStringFromRect(_lastVisibleRect).UTF8String, NSStringFromRect([[clipView documentView] visibleRect]).UTF8String););
	_lastVisibleRect = [[clipView documentView] visibleRect];
}

- (void)webViewDidChangeFrame:(NSNotification*)aNotification
{
	NSView* view = [aNotification object];
	if(view != _webFrame && view != [_webFrame documentView])
		return;

	if(view == [_webFrame documentView])
	{
		D(DBF_HTMLOutput_AutoScroll, bug("frame changed: %s → %s\n", NSStringFromRect(_lastFrame).UTF8String, NSStringFromRect([view frame]).UTF8String););
		if(NSMaxY(_lastVisibleRect) >= NSMaxY(_lastFrame))
		{
			D(DBF_HTMLOutput_AutoScroll, bug("scroll to bottom\n"););
			[self scrollViewToBottom:view];
			_lastVisibleRect = [view visibleRect];
		}
		_lastFrame = [view frame];
	}

	if(view == _webFrame)
	{
		D(DBF_HTMLOutput_AutoScroll, bug("vislble rect changed: %s → %s\n", NSStringFromRect(_lastVisibleRect).UTF8String, NSStringFromRect([[_webFrame documentView] visibleRect]).UTF8String););
		if(NSMaxY(_lastVisibleRect) >= NSMaxY(_lastFrame))
		{
			D(DBF_HTMLOutput_AutoScroll, bug("scroll to bottom\n"););
			[self scrollViewToBottom:[_webFrame documentView]];
		}
	}
}
@end
