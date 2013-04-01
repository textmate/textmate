#import "OakUIConstructionFunctions.h"
#import "NSImage Additions.h"

NSFont* OakStatusBarFont ()
{
	return [NSFont controlContentFontOfSize:[NSFont smallSystemFontSize]];
}

NSFont* OakControlFont ()
{
	return [NSFont controlContentFontOfSize:[NSFont systemFontSizeForControlSize:NSRegularControlSize]];
}

NSTextField* OakCreateLabel (NSString* label)
{
	NSTextField* res = [[NSTextField alloc] initWithFrame:NSZeroRect];
	[[res cell] setWraps:NO];
	res.bezeled         = NO;
	res.bordered        = NO;
	res.drawsBackground = NO;
	res.editable        = NO;
	res.font            = [NSFont controlContentFontOfSize:[NSFont systemFontSize]];
	res.selectable      = NO;
	res.stringValue     = label;
	return res;
}

NSTextField* OakCreateSmallLabel (NSString* label)
{
	NSTextField* res = OakCreateLabel(label);
	res.font = [NSFont controlContentFontOfSize:[NSFont smallSystemFontSize]];
	return res;
}

NSButton* OakCreateButton (NSString* label, NSBezelStyle bezel)
{
	NSButton* res = [[NSButton alloc] initWithFrame:NSZeroRect];
	res.bezelStyle = bezel;
	res.buttonType = NSMomentaryPushInButton;
	res.font       = OakControlFont();
	res.title      = label;
	return res;
}

NSPopUpButton* OakCreatePopUpButton (BOOL pullsDown, NSString* initialItemTitle)
{
	NSPopUpButton* res = [[NSPopUpButton alloc] initWithFrame:NSZeroRect pullsDown:pullsDown];
	res.font = OakControlFont();
	if(initialItemTitle)
		[[res cell] setMenuItem:[[NSMenuItem alloc] initWithTitle:initialItemTitle action:@selector(nop:) keyEquivalent:@""]];
	return res;
}

NSPopUpButton* OakCreateStatusBarPopUpButton (NSString* initialItemTitle)
{
	NSPopUpButton* res = OakCreatePopUpButton(NO, initialItemTitle);
	[[res cell] setBackgroundStyle:NSBackgroundStyleRaised];
	res.font     = OakStatusBarFont();
	res.bordered = NO;
	return res;
}

NSComboBox* OakCreateComboBox ()
{
	NSComboBox* res = [[NSComboBox alloc] initWithFrame:NSZeroRect];
	res.font = OakControlFont();
	return res;
}

NSImageView* OakCreateDividerImageView ()
{
	NSImageView* res = [[NSImageView alloc] initWithFrame:NSZeroRect];
	[res setImage:[NSImage imageNamed:@"Divider" inSameBundleAsClass:[NSClassFromString(@"OakDividerLineView") class]]];
	return res;
}
