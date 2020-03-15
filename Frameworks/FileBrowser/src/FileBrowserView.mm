#import "FileBrowserView.h"
#import "FileBrowserOutlineView.h"
#import "OFB/OFBHeaderView.h"
#import "OFB/OFBActionsView.h"
#import <OakAppKit/OakUIConstructionFunctions.h>

@interface FileBrowserView () <NSAccessibilityGroup>
@property (nonatomic) NSScrollView* scrollView;
@end

@implementation FileBrowserView
- (instancetype)initWithFrame:(NSRect)aRect
{
	if(self = [super initWithFrame:aRect])
	{
		self.accessibilityRole  = NSAccessibilityGroupRole;
		self.accessibilityLabel = @"File browser";

		_headerView    = [[OFBHeaderView alloc] initWithFrame:NSZeroRect];
		_actionsView   = [[OFBActionsView alloc] initWithFrame:NSZeroRect];

		_outlineView = [[FileBrowserOutlineView alloc] initWithFrame:NSZeroRect];
		_outlineView.accessibilityLabel       = @"Files";
		_outlineView.allowsMultipleSelection  = YES;
		_outlineView.autoresizesOutlineColumn = NO;
		_outlineView.focusRingType            = NSFocusRingTypeNone;
		_outlineView.headerView               = nil;

		[_outlineView setDraggingSourceOperationMask:NSDragOperationLink|NSDragOperationMove|NSDragOperationCopy forLocal:YES];
		[_outlineView setDraggingSourceOperationMask:NSDragOperationEvery forLocal:NO];
		[_outlineView registerForDraggedTypes:@[ NSFilenamesPboardType ]];

		NSTableColumn* tableColumn = [[NSTableColumn alloc] init];
		[_outlineView addTableColumn:tableColumn];
		[_outlineView setOutlineTableColumn:tableColumn];
		[_outlineView sizeLastColumnToFit];

		_scrollView = [[NSScrollView alloc] initWithFrame:NSZeroRect];
		_scrollView.borderType            = NSNoBorder;
		_scrollView.documentView          = _outlineView;
		_scrollView.hasHorizontalScroller = NO;
		_scrollView.hasVerticalScroller   = YES;

		NSView* dividerView = OakCreateHorizontalLine(OakBackgroundFillViewStyleDivider);

		NSDictionary* views = @{
			@"header":  _headerView,
			@"files":   _scrollView,
			@"divider": dividerView,
			@"actions": _actionsView,
		};

		OakAddAutoLayoutViewsToSuperview(views.allValues, self);
		[_headerView removeFromSuperview];
		[self addSubview:_headerView positioned:NSWindowAbove relativeTo:nil];

		OakSetupKeyViewLoop(@[ self, _headerView, _outlineView, _actionsView ], NO);

		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[files(==header,==divider,==actions)]|" options:NSLayoutFormatAlignAllLeft metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[header]-(>=0)-[divider]"               options:0 metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[files][divider][actions]|"             options:0 metrics:nil views:views]];

		NSEdgeInsets insets = _scrollView.contentInsets;
		insets.top += _headerView.fittingSize.height;
		_scrollView.automaticallyAdjustsContentInsets = NO;
		_scrollView.contentInsets = insets;

		_outlineView.backgroundColor = NSColor.clearColor;
		_scrollView.drawsBackground  = NO;
	}
	return self;
}
@end
