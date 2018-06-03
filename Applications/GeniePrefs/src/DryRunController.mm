#import "DryRunController.h"
#import "AddAutoLayoutViews.h"
#import <GenieManager/GenieManager.h>
#import <GenieManager/GenieItem.h>

@interface ExplodedResultItem : NSObject
{
	id _originalItem;
	NSString* _title;
	BOOL _leaf;
	NSArray<ExplodedResultItem*>* _children;
}
@property (nonatomic, readonly) NSString* title;
@property (nonatomic, readonly) NSString* value;
@property (nonatomic, readonly, getter = isLeaf) BOOL leaf;
@property (nonatomic, readonly) NSArray<ExplodedResultItem*>* children;
@end

@implementation ExplodedResultItem
- (instancetype)initWithTitle:(NSString*)aTitle item:(id)anItem
{
	if(self = [super init])
	{
		_title        = aTitle;
		_originalItem = anItem;
		_leaf         = !([anItem respondsToSelector:@selector(allKeys)] || [anItem isKindOfClass:[NSArray class]]);
	}
	return self;
}

- (NSString*)value
{
	if([_originalItem respondsToSelector:@selector(allKeys)])
		return [NSString stringWithFormat:@"(%ld key/value pair%@)", [[_originalItem allKeys] count], [[_originalItem allKeys] count] == 1 ? @"" : @"s"];
	else if([_originalItem isKindOfClass:[NSArray class]])
		return [NSString stringWithFormat:@"(%ld item%@)", [_originalItem count], [_originalItem count] == 1 ? @"" : @"s"];
	else if([_originalItem isKindOfClass:[NSString class]])
		return _originalItem;
	else if([_originalItem respondsToSelector:@selector(stringValue)])
		return [_originalItem stringValue];
	else if([_originalItem respondsToSelector:@selector(descriptionWithLocale:)])
		return [_originalItem descriptionWithLocale:nil];

	return [_originalItem description];;
}

- (NSArray<ExplodedResultItem*>*)children
{
	if(!_leaf && !_children)
	{
		NSMutableArray* array = [NSMutableArray array];
		if([_originalItem respondsToSelector:@selector(allKeys)])
		{
			static NSArray* const kOrder = @[ @"uid", @"title", @"subtitle", @"match", @"file", @"url", @"icon" ];
			NSArray* keys = [[_originalItem allKeys] sortedArrayUsingComparator:^NSComparisonResult(id lhs, id rhs){
				NSInteger lhsIndex = [kOrder indexOfObject:lhs];
				NSInteger rhsIndex = [kOrder indexOfObject:rhs];
				if(lhsIndex != NSNotFound && rhsIndex != NSNotFound)
					return lhsIndex < rhsIndex ? NSOrderedAscending : (rhsIndex < lhsIndex ? NSOrderedDescending : NSOrderedSame);
				else if(lhsIndex == NSNotFound && rhsIndex == NSNotFound)
					return [lhs compare:rhs];
				else if(rhsIndex == NSNotFound)
					return NSOrderedAscending;
				else //if(lhsIndex == NSNotFound)
					return NSOrderedDescending;
			}];

			for(NSString* key in keys)
			{
				id object;
				if([key isEqualToString:@"children"] && [_originalItem respondsToSelector:@selector(children)])
					object = [_originalItem children];
				else if([_originalItem respondsToSelector:@selector(staticValueForKey:)])
					object = [_originalItem staticValueForKey:key];
				else
					object = _originalItem[key];

				[array addObject:[[ExplodedResultItem alloc] initWithTitle:key item:object]];
			}
		}
		else if([_originalItem isKindOfClass:[NSArray class]])
		{
			for(NSUInteger i = 0; i < [_originalItem count]; ++i)
			{
				NSString* title = [NSString stringWithFormat:@"Item %ld", i+1];

				id object = _originalItem[i];
				if([object respondsToSelector:@selector(title)])
					title = [title stringByAppendingFormat:@": %@", [object title]];

				[array addObject:[[ExplodedResultItem alloc] initWithTitle:title item:object]];
			}
		}
		_children = [array copy];
	}
	return _children;
}
@end

@interface ExplodePropertyListValueTransformer : NSValueTransformer
@end

@implementation ExplodePropertyListValueTransformer
+ (Class)transformedValueClass         { return [NSArray class]; }
+ (BOOL)allowsReverseTransformation    { return NO; }

- (NSArray*)transformedValue:(NSArray*)array
{
	ExplodedResultItem* dummy = [[ExplodedResultItem alloc] initWithTitle:@"root" item:array];
	return dummy.children;
}
@end

@interface DryRunViewController ()
{
	BOOL _didLoadView;
	NSTreeController* _treeController;
	NSOutlineView* _outlineView;
	GenieItem* _dataSourceItem;
	NSDate* _startQueryDate;
}
@property (nonatomic) NSArray* jsonItems;
@property (nonatomic) NSTextView* textView;
@property (nonatomic) NSAttributedString* consoleOutputAttributedString;
@end

static void* kResultItemsBinding = &kResultItemsBinding;

@implementation DryRunViewController
- (instancetype)initWithDataSourceItem:(GenieItem*)dataSourceItem
{
	if(self = [super init])
	{
		_dataSourceItem = dataSourceItem;
		[self startQuery];
	}
	return self;
}

- (void)dealloc
{
	NSLog(@"[%@ dealloc]", [self class]);
	[self stopQuery];
}

- (void)startQuery
{
	[_dataSourceItem addObserver:self forKeyPath:@"replacementItems" options:0 context:kResultItemsBinding];
	_dataSourceItem.live = YES;
	_startQueryDate = [NSDate date];
}

- (void)stopQuery
{
	[_dataSourceItem removeObserver:self forKeyPath:@"replacementItems" context:kResultItemsBinding];
	_dataSourceItem.live = NO;

	NSNumberFormatter* formatter = [NSNumberFormatter new];
	formatter.numberStyle = NSNumberFormatterDecimalStyle;
	formatter.maximumFractionDigits = 1;
	NSString* seconds = [formatter stringFromNumber:@([NSDate.date timeIntervalSinceDate:_startQueryDate])];
	NSString* items = [NSNumberFormatter localizedStringFromNumber:@(_dataSourceItem.replacementItems.count) numberStyle:NSNumberFormatterDecimalStyle];

	// TODO Update status text
	NSLog(@"%s %@ items in %@ seconds", sel_getName(_cmd), items, seconds);
}

- (void)observeValueForKeyPath:(NSString*)keyPath ofObject:(id)value change:(NSDictionary*)changeDictionary context:(void*)someContext
{
	if(someContext == kResultItemsBinding)
		[self stopQuery];
}

- (void)loadView
{
	if(!_didLoadView)
	{
		NSView* contentView = [[NSView alloc] initWithFrame:NSZeroRect];
		contentView.autoresizingMask = NSViewWidthSizable|NSViewHeightSizable;
		[self makeView:contentView];
		self.view = contentView;
	}
	_didLoadView = YES;
}

- (void)makeView:(NSView*)contentView
{
	_treeController = [[NSTreeController alloc] init];
	_treeController.childrenKeyPath = @"children";
	_treeController.leafKeyPath     = @"leaf";
	[_treeController bind:NSContentBinding toObject:self withKeyPath:@"dataSourceItem.replacementItems" options:@{ NSValueTransformerNameBindingOption: @"ExplodePropertyListValueTransformer" }];

	// ===============
	// = JSON Output =
	// ===============

	_outlineView = [[NSOutlineView alloc] initWithFrame:NSZeroRect];
	_outlineView.usesAlternatingRowBackgroundColors = YES;
	_outlineView.columnAutoresizingStyle = NSTableViewUniformColumnAutoresizingStyle;

	for(NSString* title in @[ @"Key", @"Value" ])
	{
		NSTableColumn* tableColumn = [[NSTableColumn alloc] initWithIdentifier:title];
		tableColumn.title = title;
		tableColumn.editable = NO;
		((NSCell*)tableColumn.dataCell).font = [NSFont userFixedPitchFontOfSize:0];
		[_outlineView addTableColumn:tableColumn];
	}

	_outlineView.outlineTableColumn = _outlineView.tableColumns.firstObject;

	NSScrollView* scrollView = [[NSScrollView alloc] initWithFrame:NSZeroRect];
	scrollView.hasVerticalScroller = YES;
	scrollView.autohidesScrollers  = YES;
	scrollView.borderType          = NSBezelBorder;
	scrollView.documentView        = _outlineView;

	NSDictionary* views = @{
		@"scrollView": scrollView,
	};

	NSView* jsonContentView = [[NSView alloc] initWithFrame:NSZeroRect];
	GenieAddAutoLayoutViewsToSuperview(views, jsonContentView);

	[jsonContentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[scrollView(>=500)]-|" options:0 metrics:nil views:views]];
	[jsonContentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-[scrollView(>=300)]-|" options:0 metrics:nil views:views]];

	[_outlineView bind:NSContentBinding                 toObject:_treeController withKeyPath:@"arrangedObjects" options:nil];
	[_outlineView bind:NSSelectionIndexPathsBinding     toObject:_treeController withKeyPath:NSSelectionIndexPathsBinding options:nil];
	[_outlineView.tableColumns[0] bind:NSValueBinding   toObject:_treeController withKeyPath:@"arrangedObjects.title" options:nil];
	[_outlineView.tableColumns[1] bind:NSValueBinding   toObject:_treeController withKeyPath:@"arrangedObjects.value" options:nil];

	NSTabViewItem* jsonTabViewItem = [[NSTabViewItem alloc] initWithIdentifier:@"Results"];
	jsonTabViewItem.label = @"Results";
	jsonTabViewItem.view = jsonContentView;
	jsonTabViewItem.initialFirstResponder = _outlineView;

	// ==================
	// = Console Output =
	// ==================

	NSScrollView* textScrollView = GenieCreateTextView(NO); // Not editable
	_textView = textScrollView.documentView;
	_textView.incrementalSearchingEnabled = YES;

	NSButton* onlyStdout = [NSButton checkboxWithTitle:@"Only show output sent to standard error" target:nil action:nil];

	views = @{
		@"textView":   textScrollView,
		@"onlyStdout": onlyStdout,
	};

	NSView* consoleContentView = [[NSView alloc] initWithFrame:NSZeroRect];
	GenieAddAutoLayoutViewsToSuperview(views, consoleContentView);

	[consoleContentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[textView(>=500)]-|" options:0 metrics:nil views:views]];
	[consoleContentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-[textView(>=300)]-[onlyStdout]-|" options:NSLayoutFormatAlignAllLeft|NSLayoutFormatAlignAllRight metrics:nil views:views]];

	// If we bind directly to NSTextView then it becomes editable
	// [textView bind:NSAttributedStringBinding toObject:_dataSourceItem withKeyPath:@"consoleOutput" options:0];
#if 0
	[self bind:@"consoleOutputAttributedString" toObject:_dataSourceItem withKeyPath:@"consoleOutput" options:0];

	[onlyStdout bind:NSValueBinding toObject:_dataSourceItem withKeyPath:@"consoleIncludesStandardOutput" options:@{ NSValueTransformerNameBindingOption: NSNegateBooleanTransformerName }];
#endif
	NSTabViewItem* consoleTabViewItem = [[NSTabViewItem alloc] initWithIdentifier:@"Console"];
	consoleTabViewItem.label = @"Console";
	consoleTabViewItem.view = consoleContentView;
	consoleTabViewItem.initialFirstResponder = _textView;

	// ==============
	// = TabBarView =
	// ==============

	NSTabView* tabView = [[NSTabView alloc] initWithFrame:NSZeroRect];
	[tabView addTabViewItem:jsonTabViewItem];
	[tabView addTabViewItem:consoleTabViewItem];

	NSTextField* statusText = [NSTextField labelWithString:@""];
	statusText.lineBreakMode = NSLineBreakByTruncatingTail;
	[statusText setContentHuggingPriority:NSLayoutPriorityDefaultLow+1 forOrientation:NSLayoutConstraintOrientationHorizontal];
	[statusText setContentCompressionResistancePriority:NSLayoutPriorityDefaultLow+1 forOrientation:NSLayoutConstraintOrientationHorizontal];

	NSProgressIndicator* busy = [[NSProgressIndicator alloc] initWithFrame:NSZeroRect];
	busy.style                = NSProgressIndicatorSpinningStyle;
	busy.controlSize          = NSControlSizeSmall;
	busy.displayedWhenStopped = NO;

	NSButton* closeButton = [NSButton buttonWithTitle:@"Close" target:self action:@selector(closeDryRunSheet:)];
	[closeButton setKeyEquivalent:@"\e"];
	[closeButton setContentHuggingPriority:NSLayoutPriorityDefaultHigh forOrientation:NSLayoutConstraintOrientationHorizontal];

	views = @{
		@"tabView":    tabView,
		@"busy":       busy,
		@"statusText": statusText,
		@"close":      closeButton,
	};

	GenieAddAutoLayoutViewsToSuperview(views, contentView);

	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[tabView]-|"                   options:0 metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[busy]-[statusText]-[close]-|" options:NSLayoutFormatAlignAllTop|NSLayoutFormatAlignAllBottom metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-[tabView]-[close]-|"           options:NSLayoutFormatAlignAllRight metrics:nil views:views]];

	[busy       bind:NSAnimateBinding toObject:self withKeyPath:@"dataSourceItem.busy"  options:nil];
#if 0
	[statusText bind:NSValueBinding   toObject:self withKeyPath:@"dataSourceItem.error" options:nil];
#endif
}

- (void)setConsoleOutputAttributedString:(NSAttributedString*)anAttributedString
{
	_consoleOutputAttributedString = anAttributedString;
	[_textView.textStorage setAttributedString:anAttributedString];
}

- (void)copy:(id)sender
{
	if(ExplodedResultItem* item = _treeController.selectedObjects.firstObject)
	{
		NSString* string = [NSString stringWithFormat:@"%@ %@", item.title, item.value];
		[[NSPasteboard generalPasteboard] clearContents];
		[[NSPasteboard generalPasteboard] writeObjects:@[ string ]];
	}
}

- (BOOL)validateMenuItem:(NSMenuItem*)aMenuItem
{
	if(aMenuItem.action == @selector(copy:))
		return _treeController.selectedObjects.firstObject ? YES : NO;
	return [super validateMenuItem:aMenuItem];
}

- (void)closeDryRunSheet:(id)sender
{
	[self.presentingViewController dismissViewController:self];
}
@end
