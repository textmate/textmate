#import "FFFindWindow.h"

@interface FFFindWindow ()
- (BOOL)restoreSavedWindowFrame;

@property (nonatomic, retain) NSViewAnimation* resizeAnimation;
@end

@implementation FFFindWindow
// ============
// = Resizing =
// ============

@synthesize isExpanded, resizeAnimation;

- (void)setIsExpanded:(BOOL)flag
{
	if(flag == isExpanded)
		return;

	if(resizeAnimation && [resizeAnimation isAnimating])
		[resizeAnimation stopAnimation];

	NSRect targetWindowFrame = [self frame];

	if(flag == NO) // Hide Find All Results
	{
		expandedFindPanelSize = targetWindowFrame.size;
		targetWindowFrame.origin.y += NSHeight(targetWindowFrame) - shrinkedFindPanelSize.height;
		targetWindowFrame.size.height = shrinkedFindPanelSize.height;

		NSSize minSize = [self contentMinSize];
		minSize.height -= 100.0;
		[self setContentMinSize:minSize];

		NSSize size = [self contentMaxSize];
		size.height = maxContentHeight;
		[self setContentMaxSize:size];
	}
	else
	{
		if(![collapsibleView superview])
		{
			[collapsibleView setFrame:[collapsibleViewPlaceholder frame]];
			[[self contentView] replaceSubview:collapsibleViewPlaceholder with:collapsibleView];
		}

		shrinkedFindPanelSize = [self frame].size;

		NSSize size = [self contentMaxSize];
		size.height = FLT_MAX;
		[self setContentMaxSize:size];

		NSSize minSize = [self contentMinSize];
		minSize.height += 100.0;
		[self setContentMinSize:minSize];

		if(expandedFindPanelSize.height > 0.0)
		{
			targetWindowFrame.origin.y -= expandedFindPanelSize.height - NSHeight(targetWindowFrame);
			targetWindowFrame.size.height += expandedFindPanelSize.height - NSHeight(targetWindowFrame);
		}
		else
		{
			targetWindowFrame.origin.y -= 194.0;
			targetWindowFrame.size.height += 194.0;
		}
	}

	isExpanded = !isExpanded;

	if([self isVisible])
	{
		NSDictionary* animation = [NSDictionary dictionaryWithObjectsAndKeys:
			self, NSViewAnimationTargetKey,
			[NSValue valueWithRect:targetWindowFrame], NSViewAnimationEndFrameKey,
			nil];
		self.resizeAnimation = [[[NSViewAnimation alloc] initWithViewAnimations:@[ animation ]] autorelease];
		[resizeAnimation setDuration:0.25];
		[resizeAnimation startAnimation];
	}
	else
	{
		[self setFrame:targetWindowFrame display:YES animate:NO];
	}
}

// =================
// = Window Saving =
// =================

- (void)awakeFromNib
{
	maxContentHeight = [self contentMaxSize].height;

	if(![self restoreSavedWindowFrame])
		[self center];

	[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(windowFrameDidChange:) name:NSWindowDidResizeNotification object:nil];
	[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(windowFrameDidChange:) name:NSWindowDidMoveNotification object:nil];
}

- (void)dealloc
{
	[[NSNotificationCenter defaultCenter] removeObserver:self];
	[super dealloc];
}

static NSString* const SavedWindowFrameName = @"Find Panel Saved Dimensions";

- (BOOL)restoreSavedWindowFrame
{
	if(NSDictionary* storedFrame = [[NSUserDefaults standardUserDefaults] objectForKey:SavedWindowFrameName])
	{
		NSRect frame = NSRectFromString([storedFrame objectForKey:@"frame"]);
		[self setFrame:frame display:YES];
		expandedFindPanelSize = NSSizeFromString([storedFrame objectForKey:@"expandedSize"]);
		return YES;
	}
	return NO;
}

- (void)saveFrame
{
	NSRect frame = self.frame;
	if(self.isExpanded)
	{
		// Store the frame as it would be if the find all results were hidden (i.e. the default state)
		frame.size.height -= [collapsibleView frame].size.height;
		frame.origin.y    += [collapsibleView frame].size.height;
		expandedFindPanelSize = self.frame.size;
	}
	NSDictionary* dict = [NSDictionary dictionaryWithObjectsAndKeys:
		NSStringFromRect(frame), @"frame",
		NSStringFromSize(expandedFindPanelSize), @"expandedSize",
		nil];
	[[NSUserDefaults standardUserDefaults] setObject:dict forKey:SavedWindowFrameName];
}

- (void)windowFrameDidChange:(NSNotification*)notification	{ [self saveFrame]; }
@end
