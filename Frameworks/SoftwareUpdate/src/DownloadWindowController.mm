#import "DownloadWindowController.h"
#import <ns/ns.h>
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/NSImage Additions.h>
#import <OakAppKit/OakImage.h>
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
		NSImage* appIcon = [NSApp applicationIconImage];
		NSImage* dlBadge = [[NSImage imageNamed:@"Update Badge" inSameBundleAsClass:[self class]] copy];
		[dlBadge setSize:NSMakeSize(appIcon.size.width / 4, appIcon.size.height / 4)];
		[NSApp setApplicationIconImage:[OakImage imageWithBase:appIcon badge:dlBadge edge:CGRectMaxXEdge]];
	}
	else
	{
		[NSApp setApplicationIconImage:nil];
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
