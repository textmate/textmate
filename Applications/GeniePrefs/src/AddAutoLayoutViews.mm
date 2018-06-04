#import "AddAutoLayoutViews.h"

void GenieAddAutoLayoutViewsToSuperview (NSDictionary* views, NSView* superview)
{
	for(NSString* key in [views allKeys])
	{
		__kindof NSView* view = views[key];
		if([view isEqual:[NSNull null]])
			continue;

		if([key hasSuffix:@"Label"] && [view isKindOfClass:[NSTextField class]])
		{
			[view setAlignment:NSTextAlignmentRight];
			[view setContentHuggingPriority:NSLayoutPriorityDefaultLow+1 forOrientation:NSLayoutConstraintOrientationHorizontal];
			[view setContentCompressionResistancePriority:NSLayoutPriorityDefaultHigh forOrientation:NSLayoutConstraintOrientationHorizontal];
		}

		[view setTranslatesAutoresizingMaskIntoConstraints:NO];
		[superview addSubview:view];
	}
}

void GenieSetupKeyViewLoop (NSArray<__kindof NSResponder*>* superviews, BOOL setFirstResponder)
{
	std::set<id> seen;
	for(NSView* candidate in superviews)
		seen.insert(candidate);

	NSMutableArray* views = [NSMutableArray new];
	for(NSView* view in superviews)
	{
		if([view isEqual:[NSNull null]])
			continue;

		[views addObject:view];
		NSView* subview = view;
		while((subview = subview.nextKeyView) && [subview isDescendantOf:view] && seen.insert(subview).second)
			[views addObject:subview];
	}

	if(views.count == 1)
	{
		[views.firstObject setNextKeyView:nil];
	}
	else
	{
		for(size_t i = 0; i < views.count; ++i)
			[views[i] setNextKeyView:views[(i + 1) % views.count]];
	}

	if(setFirstResponder)
	{
		if(NSView* view = views.firstObject)
			view.window.initialFirstResponder = view;
	}
}

NSScrollView* GenieCreateTextView (BOOL editable)
{
	NSTextView* textView = [[NSTextView alloc] initWithFrame:NSZeroRect];

	textView.autoresizingMask      = NSViewWidthSizable|NSViewHeightSizable;
	textView.horizontallyResizable = YES;
	textView.verticallyResizable   = YES;

	textView.allowsUndo            = editable;
	textView.editable              = editable;
	textView.richText              = NO;
	textView.usesFindBar           = YES;

	NSScrollView* scrollView = [[NSScrollView alloc] initWithFrame:NSZeroRect];
	scrollView.hasVerticalScroller = YES;
	scrollView.autohidesScrollers  = YES;
	scrollView.borderType          = NSBezelBorder;
	scrollView.documentView        = textView;

	NSDictionary* fontAttributes = @{
		NSFontAttributeName : [NSFont userFixedPitchFontOfSize:0],
	};

	NSAttributedString* content = [[NSAttributedString alloc] initWithString:@"\n\n\n\n" attributes:fontAttributes];
	[textView.textStorage setAttributedString:content];

	[textView sizeToFit];
	CGFloat height = MAX(NSHeight(textView.frame), content.size.height);

	NSLayoutConstraint* heightConstraint = [NSLayoutConstraint constraintWithItem:scrollView attribute:NSLayoutAttributeHeight relatedBy:NSLayoutRelationGreaterThanOrEqual toItem:nil attribute:NSLayoutAttributeNotAnAttribute multiplier:1 constant:height];
	[scrollView addConstraint:heightConstraint];

	return scrollView;
}
