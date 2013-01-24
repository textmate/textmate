#import "OakAppKit.h"

NSString* const OakCursorDidHideNotification = @"OakCursorDidHideNotification";

@interface OakDividerLineView : NSBox
@property (nonatomic, retain) NSColor* primaryColor;
@property (nonatomic, retain) NSColor* secondaryColor;
@property (nonatomic)         BOOL usePrimaryColor;
@end

@implementation OakDividerLineView
- (void)viewWillMoveToWindow:(NSWindow*)newWindow
{
	if(!self.secondaryColor)
		return;

	if(self.window)
	{
		[[NSNotificationCenter defaultCenter] removeObserver:self name:NSWindowDidBecomeMainNotification object:self.window];
		[[NSNotificationCenter defaultCenter] removeObserver:self name:NSWindowDidResignMainNotification object:self.window];
	}

	if(newWindow)
	{
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(windowDidChangeMain:) name:NSWindowDidBecomeMainNotification object:newWindow];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(windowDidChangeMain:) name:NSWindowDidResignMainNotification object:newWindow];
	}

	self.usePrimaryColor = [newWindow isMainWindow];
}

- (void)windowDidChangeMain:(NSNotification*)aNotification
{
	self.usePrimaryColor = [self.window isMainWindow];
}

- (void)setUsePrimaryColor:(BOOL)flag
{
	if(_usePrimaryColor != flag)
	{
		_usePrimaryColor = flag;
		self.borderColor = flag ? self.primaryColor : self.secondaryColor;
	}
}
@end

NSBox* OakCreateViewWithColor (NSColor* color, NSColor* secondaryColor)
{
	OakDividerLineView* box = [[[OakDividerLineView alloc] initWithFrame:NSZeroRect] autorelease];
	box.boxType        = NSBoxCustom;
	box.borderType     = NSLineBorder;
	box.borderColor    = color;
	box.primaryColor   = color;
	box.secondaryColor = secondaryColor;
	return box;
}

void OakRunIOAlertPanel (char const* format, ...)
{
	va_list ap;
	va_start(ap, format);
	char* buf = NULL;
	vasprintf(&buf, format, ap);
	va_end(ap);
	NSRunAlertPanel(@(buf), @"Error: %s", @"OK", nil, nil, strerror(errno));
	free(buf);
}

BOOL OakIsAlternateKeyOrMouseEvent (NSUInteger flags, NSEvent* anEvent)
{
	return ([anEvent type] == NSLeftMouseUp || [anEvent type] == NSKeyDown) && (([anEvent modifierFlags] & flags) == flags);
}

@interface OakSheetCallbackDelegate : NSObject
@property (nonatomic, copy)   void(^callback)(NSInteger);
@property (nonatomic, retain) id retainedSelf;
@end

@implementation OakSheetCallbackDelegate
- (id)initWithBlock:(void(^)(NSInteger))aBlock
{
	if(self = [super init])
	{
		self.callback = aBlock;
		self.retainedSelf = self;
	}
	return self;
}

- (void)sheetDidEnd:(id)sheetOrAlert returnCode:(NSInteger)returnCode contextInfo:(void*)unused
{
	self.callback(returnCode);
	self.retainedSelf = nil;
}

- (void)dealloc
{
	self.callback = nil;
	[super dealloc];
}
@end

void OakShowSheetForWindow (NSWindow* sheet, NSWindow* window, void(^callback)(NSInteger))
{
	OakSheetCallbackDelegate* delegate = [[[OakSheetCallbackDelegate alloc] initWithBlock:callback] autorelease];
	[NSApp beginSheet:sheet modalForWindow:window modalDelegate:delegate didEndSelector:@selector(sheetDidEnd:returnCode:contextInfo:) contextInfo:NULL];
}

void OakShowAlertForWindow (NSAlert* alert, NSWindow* window, void(^callback)(NSInteger))
{
	OakSheetCallbackDelegate* delegate = [[[OakSheetCallbackDelegate alloc] initWithBlock:callback] autorelease];
	if(window)
			[alert beginSheetModalForWindow:window modalDelegate:delegate didEndSelector:@selector(sheetDidEnd:returnCode:contextInfo:) contextInfo:NULL];
	else	[delegate sheetDidEnd:alert returnCode:[alert runModal] contextInfo:NULL];
}
