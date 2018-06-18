#import "DryRunController.h"
#import "AddAutoLayoutViews.h"
#import <GenieManager/GenieManager.h>
#import <GenieManager/GenieItem.h>
#import <OakFoundation/OakFoundation.h>

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

	NSString* _displayName;
	NSString* _scriptPath;
	NSData* _standardOutputData;
	NSData* _standardErrorData;
}
@property (nonatomic) NSArray* jsonItems;
@property (nonatomic) NSTextView* textView;

@property (nonatomic) NSArray* genieItems;
@property (nonatomic) NSString* errorString;
@property (nonatomic) BOOL consoleIncludesStandardOutput;
@property (nonatomic, getter = isRunning) BOOL running;
@end

@implementation DryRunViewController
- (instancetype)initWithDataSourceItem:(GenieItem*)dataSourceItem
{
	if(self = [super init])
	{
		[self startQueryForItem:dataSourceItem];
	}
	return self;
}

- (void)dealloc
{
	NSLog(@"[%@ dealloc]", [self class]);
}

- (void)startQueryForItem:(GenieItem*)dataSourceItem
{
	NSDate* startDate = [NSDate date];
	_displayName = dataSourceItem.displayName;

	__weak __block id observerId = [NSNotificationCenter.defaultCenter addObserverForName:GenieItemDidReceiveNewItemsNotification object:dataSourceItem queue:nil usingBlock:^(NSNotification* notification){
		[NSNotificationCenter.defaultCenter removeObserver:observerId];
		self.running = NO;

		NSDictionary* userInfo = notification.userInfo;
		if(OakIsEmptyString(userInfo[@"errorString"]))
		{
			NSNumberFormatter* formatter = [NSNumberFormatter new];
			formatter.numberStyle = NSNumberFormatterDecimalStyle;
			formatter.maximumFractionDigits = 1;
			NSString* seconds = [formatter stringFromNumber:@([[NSDate date] timeIntervalSinceDate:startDate])];

			NSString* countOfItems = [NSNumberFormatter localizedStringFromNumber:@(dataSourceItem.replacementItems.count) numberStyle:NSNumberFormatterDecimalStyle];
			self.errorString = [NSString stringWithFormat:@"Got %@ items in %@ seconds", countOfItems, seconds];
		}
		else
		{
			self.errorString = userInfo[@"errorString"];
		}

		self.genieItems = userInfo[@"items"] ? dataSourceItem.replacementItems : nil;

		_scriptPath         = userInfo[@"scriptPath"];
		_standardOutputData = userInfo[@"standardOutputData"];
		_standardErrorData  = userInfo[@"standardErrorData"];
		[self updateConsoleOutput];
	}];

	NSMutableDictionary* dict = [dataSourceItem.environment mutableCopy];
	dict[@"GENIE_DEBUG"] = @"1";
	dataSourceItem.environment = dict;

	self.running = YES;
	[dataSourceItem refreshDataSource];
}

- (void)updateConsoleOutput
{
	NSMutableAttributedString* res = [[NSMutableAttributedString alloc] init];

	NSDictionary* outputStyles = @{
		NSFontAttributeName: [NSFont userFixedPitchFontOfSize:0],
	};

	NSDictionary* errorStyles = @{
		NSFontAttributeName: [NSFont userFixedPitchFontOfSize:0],
		NSForegroundColorAttributeName : NSColor.redColor
	};

	NSArray<NSData*>* data         = @[ _standardErrorData ?: [NSData data], (_consoleIncludesStandardOutput ? _standardOutputData : nil) ?: [NSData data] ];
	NSArray<NSDictionary*>* styles = @[ errorStyles, outputStyles ];

	for(NSUInteger i = 0; i < MIN(data.count, styles.count); ++i)
	{
		if(NSString* string = [[NSString alloc] initWithData:data[i] encoding:NSUTF8StringEncoding])
		{
			if(_scriptPath && _displayName)
				string = [string stringByReplacingOccurrencesOfString:_scriptPath withString:_displayName];

			if(NSAttributedString* attributedString = [[NSAttributedString alloc] initWithString:string attributes:styles[i]])
			{
				[res appendAttributedString:attributedString];
				if(string.length && ![string hasSuffix:@"\n"])
					[res appendAttributedString:[[NSAttributedString alloc] initWithString:@"\n"]];
			}
		}
	}

	[_textView.textStorage setAttributedString:res];
}

- (void)setConsoleIncludesStandardOutput:(BOOL)newConsoleIncludesStandardOutput
{
	if(_consoleIncludesStandardOutput == newConsoleIncludesStandardOutput)
		return;

	_consoleIncludesStandardOutput = newConsoleIncludesStandardOutput;
	[self updateConsoleOutput];
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
	[_treeController bind:NSContentBinding toObject:self withKeyPath:@"genieItems" options:@{ NSValueTransformerNameBindingOption: @"ExplodePropertyListValueTransformer" }];

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

	[busy       bind:NSAnimateBinding toObject:self withKeyPath:@"running"                       options:nil];
	[statusText bind:NSValueBinding   toObject:self withKeyPath:@"errorString"                   options:nil];
	[onlyStdout bind:NSValueBinding   toObject:self withKeyPath:@"consoleIncludesStandardOutput" options:@{ NSValueTransformerNameBindingOption: NSNegateBooleanTransformerName }];
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
	return [super respondsToSelector:@selector(validateMenuItem:)] ? [super validateMenuItem:aMenuItem] : YES;
}

- (void)closeDryRunSheet:(id)sender
{
	[self.presentingViewController dismissViewController:self];
}
@end
