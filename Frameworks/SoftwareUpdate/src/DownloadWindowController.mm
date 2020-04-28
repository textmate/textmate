#import "DownloadWindowController.h"
#import <ns/ns.h>
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/NSImage Additions.h>
#import <OakFoundation/NSString Additions.h>
#import <oak/debug.h>

OAK_DEBUG_VAR(SoftwareUpdate_Download);

@implementation DownloadWindowController
- (id)init
{
	return [super initWithWindowNibName:@"DownloadProgress"];
}

- (void)windowDidLoad
{
	self.window.hidesOnDeactivate = YES;
	self.window.level             = NSStatusWindowLevel;
}

- (void)setShowUpdateBadge:(BOOL)flag
{
	D(DBF_SoftwareUpdate_Download, bug("%s\n", BSTR(flag)););
	if(_showUpdateBadge == flag)
		return;

	if(_showUpdateBadge = flag)
	{
		D(DBF_SoftwareUpdate_Download, bug("alter application icon\n"););
		if(NSImage* dlBadge = [NSImage imageNamed:@"Update Badge" inSameBundleAsClass:[self class]])
		{
			NSImage* appIcon = NSApp.applicationIconImage;
			NSApp.applicationIconImage = [NSImage imageWithSize:appIcon.size flipped:NO drawingHandler:^BOOL(NSRect dstRect){
				[appIcon drawInRect:dstRect fromRect:NSZeroRect operation:NSCompositingOperationSourceOver fraction:1];

				CGFloat minX = NSMinX(dstRect) + round(NSWidth(dstRect)  * 2 / 3), maxX = NSMaxX(dstRect);
				CGFloat minY = NSMinY(dstRect) + round(NSHeight(dstRect) * 2 / 3), maxY = NSMaxY(dstRect);
				[dlBadge drawInRect:NSMakeRect(minX, minY, maxX - minX, maxY - minY) fromRect:NSZeroRect operation:NSCompositingOperationSourceOver fraction:1];

				return YES;
			}];
		}
	}
	else
	{
		NSApp.applicationIconImage = nil;
	}
}

// ================
// = UI Callbacks =
// ================

- (IBAction)install:(id)sender
{
	D(DBF_SoftwareUpdate_Download, bug("\n"););
	if([self.delegate respondsToSelector:@selector(install:)])
		[self.delegate install:self];
}

- (IBAction)cancel:(id)sender
{
	D(DBF_SoftwareUpdate_Download, bug("\n"););
	if([self.delegate respondsToSelector:@selector(cancel:)])
		[self.delegate cancel:self];
}

- (void)windowWillClose:(NSNotification*)aNotification
{
	D(DBF_SoftwareUpdate_Download, bug("\n"););
	if([self.delegate respondsToSelector:@selector(windowWillClose:)])
		[self.delegate windowWillClose:self];
}
@end
