#import "HOAutoScroll.h"
#import <oak/debug.h>

OAK_DEBUG_VAR(HTMLOutput_AutoScroll);

@implementation HOAutoScroll
@synthesize webFrame;

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
	[super dealloc];
}

- (void)setWebFrame:(WebFrameView*)aWebFrame
{
	D(DBF_HTMLOutput_AutoScroll, bug("%s\n", [aWebFrame description].UTF8String););
	if(aWebFrame == webFrame)
		return;

	[webFrame release];
	webFrame = [aWebFrame retain];

	if(webFrame)
	{
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(webViewDidChangeFrame:) name:NSViewFrameDidChangeNotification object:nil];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(webViewDidChangeBounds:) name:NSViewBoundsDidChangeNotification object:nil];

		lastFrame = [[webFrame documentView] frame];
		lastVisibleRect = [[webFrame documentView] visibleRect];
	}
	else
	{
		[[NSNotificationCenter defaultCenter] removeObserver:self name:NSViewFrameDidChangeNotification object:nil];
		[[NSNotificationCenter defaultCenter] removeObserver:self name:NSViewBoundsDidChangeNotification object:nil];
	}
}

- (void)webViewDidChangeBounds:(NSNotification*)aNotification
{
	NSClipView* clipView = [[[webFrame documentView] enclosingScrollView] contentView];
	if(clipView != [aNotification object])
		return;

	D(DBF_HTMLOutput_AutoScroll, bug("bounds changed: %s → %s\n", NSStringFromRect(lastVisibleRect).UTF8String, NSStringFromRect([[clipView documentView] visibleRect]).UTF8String););
	lastVisibleRect = [[clipView documentView] visibleRect];
}

- (void)webViewDidChangeFrame:(NSNotification*)aNotification
{
	NSView* view = [aNotification object];
	if(view != webFrame && view != [webFrame documentView])
		return;

	if(view == [webFrame documentView])
	{
		D(DBF_HTMLOutput_AutoScroll, bug("frame changed: %s → %s\n", NSStringFromRect(lastFrame).UTF8String, NSStringFromRect([view frame]).UTF8String););
		if(NSMaxY(lastVisibleRect) >= NSMaxY(lastFrame))
		{
			D(DBF_HTMLOutput_AutoScroll, bug("scroll to bottom\n"););
			[self scrollViewToBottom:view];
			lastVisibleRect = [view visibleRect];
		}
		lastFrame = [view frame];
	}
	
	if(view == webFrame)
	{
		D(DBF_HTMLOutput_AutoScroll, bug("vislble rect changed: %s → %s\n", NSStringFromRect(lastVisibleRect).UTF8String, NSStringFromRect([[webFrame documentView] visibleRect]).UTF8String););
		if(NSMaxY(lastVisibleRect) >= NSMaxY(lastFrame))
		{
			D(DBF_HTMLOutput_AutoScroll, bug("scroll to bottom\n"););
			[self scrollViewToBottom:[webFrame documentView]];
		}
	}
}
@end
