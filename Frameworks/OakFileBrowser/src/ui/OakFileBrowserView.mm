#import "OakFileBrowserView.h"
#import "OFBPathInfoCell.h"
#import "OFBOutlineView.h"
#import "OFBHeaderView.h"
#import <OakAppKit/NSImage Additions.h>
#import <OakAppKit/OakFileIconImage.h>

@interface OakFileBrowserView ()
{
	OBJC_WATCH_LEAKS(OakFileBrowserView);

	// These two properties are retained only as subviews
	OFBOutlineView* outlineView;
	OFBHeaderView* headerView;

	id delegate;
	NSResponder* persistentNextResponder;

	// Header view
	BOOL canGoBackward;
	BOOL canGoForward;

	NSString* titleText;
	NSImage*  titleImage;
}
- (void)setupViews;
@end

OAK_DEBUG_VAR(FileBrowser_View);

@implementation OakFileBrowserView
@synthesize outlineView;
@synthesize persistentNextResponder;
@synthesize delegate, canGoBackward, canGoForward, titleText, titleImage;

// ==================
// = Setup/Teardown =
// ==================

- (id)initWithFrame:(NSRect)aFrame
{
	if(self = [super initWithFrame:aFrame])
	{
		[self setupViews];
	}
	return self;
}

- (void)dealloc
{
	[super dealloc];
}

- (NSSize)intrinsicContentSize
{
	return NSMakeSize(NSViewNoInstrinsicMetric, NSViewNoInstrinsicMetric);
}

- (void)setupViews
{
	ASSERT(!outlineView);

	NSScrollView* scrollView         = [[[NSScrollView alloc] initWithFrame:NSZeroRect] autorelease];
	scrollView.hasVerticalScroller   = YES;
	scrollView.hasHorizontalScroller = NO;
	scrollView.borderType            = NSNoBorder;
	[self addSubview:scrollView];

	outlineView                          = [[[OFBOutlineView alloc] initWithFrame:NSMakeRect(10, 10, scrollView.contentSize.width, scrollView.contentSize.height)] autorelease];
	outlineView.focusRingType            = NSFocusRingTypeNone;
	outlineView.allowsMultipleSelection  = YES;
	outlineView.autoresizesOutlineColumn = NO;
	outlineView.headerView               = nil;

	scrollView.documentView              = outlineView;

	headerView = [[[OFBHeaderView alloc] initWithFrame:NSZeroRect] autorelease];
	[self addSubview:headerView];

	NSCell* cell       = [[OFBPathInfoCell new] autorelease];
	cell.lineBreakMode = NSLineBreakByTruncatingMiddle;
	[cell setEditable:YES];

	NSTableColumn* tableColumn = [[NSTableColumn new] autorelease];
	[tableColumn setDataCell:cell];
	[outlineView addTableColumn:tableColumn];
	[outlineView setOutlineTableColumn:tableColumn];
	[outlineView sizeLastColumnToFit];

	NSDictionary* views = NSDictionaryOfVariableBindings(headerView, scrollView);
	for(id key in views)
		[views[key] setTranslatesAutoresizingMaskIntoConstraints:NO];
	[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[headerView(==scrollView)]|" options:0 metrics:nil views:views]];
	[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[headerView][scrollView]|"   options:0 metrics:nil views:views]];
}

// ========
// = View =
// ========

- (void)setPersistentNextResponder:(NSResponder*)aResponder
{
	if(aResponder != persistentNextResponder)
	{
		persistentNextResponder = aResponder;
		[self viewDidMoveToSuperview];
	}
}

- (void)setNextResponder:(NSResponder*)aResponder
{
	if(!self.persistentNextResponder)
		return [super setNextResponder:aResponder];

	if(aResponder != self.persistentNextResponder)
		self.persistentNextResponder.nextResponder = aResponder;
	[super setNextResponder:self.persistentNextResponder];
}

- (BOOL)isOpaque
{
	return YES;
}

- (BOOL)canBecomeKeyView
{
	return NO;
}

- (void)displayMenu:(NSMenu*)aMenu fromHeaderColumn:(fb::header_column)columnTag selectedIndex:(NSUInteger)index popup:(BOOL)popup
{
}

- (NSRect)iconFrameForEntry:(id)anEntry
{
	NSInteger rowIndex = [outlineView rowForItem:anEntry];
	if(rowIndex == -1)
		return NSZeroRect;
	NSRect r = [outlineView frameOfCellAtColumn:0 row:rowIndex];
	r.origin.x += 7.0; // FIXME some hard-coded values here
	r.origin.y -= 1.0;
	r.size = NSMakeSize(16, 16);
	r = [outlineView convertRect:r toView:nil];
	r.origin = [outlineView.window convertBaseToScreen:r.origin];
	return r;
}

// ===============
// = Header view =
// ===============

static inline NSImage* Image (NSString* name)   { return [NSImage imageNamed:name inSameBundleAsClass:[OakFileBrowserView class]]; }
static inline NSImage* Pressed (NSString* name) { return Image([NSString stringWithFormat:@"%@ Pressed", name]); }

- (void)updateHeaderView
{
	headerView.goBackButton.enabled    = self.canGoBackward;
	headerView.goForwardButton.enabled = self.canGoForward;
}

- (void)setCanGoBackward:(BOOL)flag      { canGoBackward = flag; [self updateHeaderView]; }
- (void)setCanGoForward:(BOOL)flag       { canGoForward = flag; [self updateHeaderView]; }
- (void)setTitleText:(NSString*)text     { [titleText autorelease]; titleText = [text retain]; [self updateHeaderView]; }
- (void)setTitleImage:(NSImage*)image
{
	[titleImage release];
	titleImage = [[NSImage alloc] initWithSize:NSMakeSize(15, 15)];
	[titleImage lockFocus];
	[image drawInRect:NSMakeRect(1, 1, 13, 13) fromRect:NSZeroRect operation:NSCompositeSourceOver fraction:1.0];
	[titleImage unlockFocus];
	[self updateHeaderView];
}

- (IBAction)clickHeaderCell:(id)sender { [delegate didClickHeaderColumn:sender]; }
- (IBAction)holdHeaderCell:(id)sender  { [delegate didTriggerMenuForHeaderColumn:sender]; }

- (void)swipeWithEvent:(NSEvent*)anEvent
{
	if([anEvent deltaX] == +1 && [delegate respondsToSelector:@selector(goBack:)])
		[delegate performSelector:@selector(goBack:) withObject:self];
	else if([anEvent deltaX] == -1 && [delegate respondsToSelector:@selector(goForward:)])
		[delegate performSelector:@selector(goForward:) withObject:self];
	else if([anEvent deltaY] == +1 && [delegate respondsToSelector:@selector(goToParentFolder:)])
		[delegate performSelector:@selector(goToParentFolder:) withObject:self];
}
@end
