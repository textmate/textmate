#import "OTVStatusBar.h"
#import <OakAppKit/NSImage Additions.h>

@interface OTVStatusBar ()
- (void)update;
@property (nonatomic, retain) NSTimer* recordingTimer;
@property (nonatomic, retain) NSImage* pulsedRecordingIndicator;
@end

const NSInteger BundleItemSelector = 1;

@implementation OTVStatusBar
@synthesize recordingTimer, pulsedRecordingIndicator, grammarName, symbolName, isMacroRecording, tabSize, softTabs, showResizeThumb;
@synthesize delegate;

- (void)update
{
	size_t line = caretPosition.min().line, column = caretPosition.min().column;

	std::string const lineNumberText = "Line: " + text::pad(line+1, 4) + "\u2003" /* Em Space */ + "Column: " + text::pad(column+1, 3);
	std::string const tabSizeText    = std::string(softTabs ? "Soft Tabs:" : "Tab Size:") + "\u2003" /* Em Space */ + text::pad(tabSize, 4);
	static NSImage* gearImage        = [[NSImage imageNamed:@"Statusbar Gear" inSameBundleAsClass:[self class]] retain];
	static NSImage* languageIcon     = [[NSImage imageNamed:@"Languages" inSameBundleAsClass:[self class]] retain];
	static NSImage* splitViewThumb   = [[NSImage imageNamed:@"Horizontal SplitView Thumb" inSameBundleAsClass:[self class]] retain];

	struct sb::cell_t const cellList[] =
	{
		sb::cell_t::info(lineNumberText),
		sb::cell_t::popup([grammarName UTF8String] ?: "-",     @selector(showLanguageSelector:),    self.delegate).set_image(languageIcon).size(110),
		sb::cell_t::popup(gearImage,                           @selector(showBundleItemSelector:),  self.delegate).set_tag(BundleItemSelector),
		sb::cell_t::popup(tabSizeText,                         @selector(showTabSizeSelector:),     self.delegate),
		sb::cell_t::popup([symbolName UTF8String] ?: "Symbol", @selector(showSymbolSelector:),      self.delegate).size(200, CGFLOAT_MAX),
		sb::cell_t::button(pulsedRecordingIndicator,           @selector(toggleMacroRecording:),    self.delegate).no_padding().size(17),
		showResizeThumb ? sb::cell_t::button(splitViewThumb, NULL, nil).no_padding().size(15) : sb::cell_t::info().size(15),
	};
	SetCells(self, cellList);
}

- (void)updateMacroRecordingAnimation:(NSTimer*)aTimer
{
	NSImage* startImage = [NSImage imageNamed:@"RecordingMacro" inSameBundleAsClass:[self class]];
	self.pulsedRecordingIndicator = [[[NSImage alloc] initWithSize:startImage.size] autorelease];

	[pulsedRecordingIndicator lockFocus];
	CGFloat fraction = oak::cap(0.00, 0.50 + 0.50 * sin(recordingTime), 1.0);
	[startImage drawAtPoint:NSZeroPoint fromRect:NSZeroRect operation:NSCompositeSourceOver fraction:fraction];
	[pulsedRecordingIndicator unlockFocus];

	[self update];
	recordingTime += 0.075;
}

// ==============
// = Properties =
// ==============

- (void)setGrammarName:(NSString*)newGrammarName
{
	[grammarName release];
	grammarName = [newGrammarName copy];
	[self update];
}

- (void)setSymbolName:(NSString*)newSymbolName
{
	[symbolName release];
	symbolName = [newSymbolName copy];
	[self update];
}

- (void)setRecordingTimer:(NSTimer*)aTimer
{
	if(aTimer != recordingTimer)
	{
		[recordingTimer invalidate];
		[recordingTimer release];
		recordingTimer = [aTimer retain];
	}
}

- (void)setIsMacroRecording:(BOOL)flag
{
	isMacroRecording = flag;
	if(isMacroRecording)
	{
		recordingTime = 0;
		self.recordingTimer = [NSTimer scheduledTimerWithTimeInterval:0.02 target:self selector:@selector(updateMacroRecordingAnimation:) userInfo:nil repeats:YES];
	}
	else
	{
		self.pulsedRecordingIndicator = nil;
		self.recordingTimer = nil;
	}
	[self update];
}

- (void)setCaretPosition:(std::string const&)range
{
	caretPosition = range;
	[self update];
}

- (void)setTabSize:(int32_t)size
{
	tabSize = size;
	[self update];
}

- (void)setSoftTabs:(BOOL)flag
{
	softTabs = flag;
	[self update];
}

- (void)setShowResizeThumb:(BOOL)flag
{
	showResizeThumb = flag;
	[self update];
}

- (void)dealloc
{
	self.grammarName = nil;
	self.symbolName = nil;
	self.recordingTimer = nil;
	[super dealloc];
}
@end
