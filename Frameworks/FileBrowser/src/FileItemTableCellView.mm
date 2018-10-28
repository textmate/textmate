#import "FileItemTableCellView.h"
#import "FileItem.h"
#import <OakAppKit/OakUIConstructionFunctions.h>
#import <OakAppKit/OakFinderTag.h>

// Implemented in OakFileBrowser/FSOutlineViewDelegate.mm
@interface OakItemButtonsView : NSView
- (id)initWithCloseAction:(SEL)closeAction target:(id)target;
@end

@interface FileItemSelectBasenameCell : NSTextFieldCell
@end

@implementation FileItemSelectBasenameCell
- (void)selectWithFrame:(NSRect)aRect inView:(NSView*)aView editor:(NSText*)aText delegate:(id)someDelegate start:(NSInteger)start length:(NSInteger)length
{
	NSString* path = self.stringValue;
	if([self.objectValue respondsToSelector:@selector(firstObject)])
		path = [self.objectValue firstObject];
	NSString* basename = [path stringByDeletingPathExtension];
	[super selectWithFrame:aRect inView:aView editor:aText delegate:someDelegate start:start length:(start == 0 && basename ? MIN(basename.length, length) : length)];
}
@end

@implementation FileItem (FileItemWrapper)
+ (NSSet*)keyPathsForValuesAffectingEditingAndDisplayName
{
	return [NSSet setWithObjects:@"URL", @"displayName", nil];
}

- (NSArray*)editingAndDisplayName
{
	return @[ self.URL.lastPathComponent ?: @"", self.displayName ];
}

- (void)setEditingAndDisplayName:(NSArray*)unused
{
	// Because ‘editingAndDisplayName’ is bound to our text field then we receive updates when user edits the text field
}
@end

@interface FileItemFormatter : NSFormatter
@property (nonatomic, weak) NSTableCellView* tableCellView;
@end

@implementation FileItemFormatter
- (instancetype)initWithTableCellView:(NSTableCellView*)tableCellView
{
	if(self = [super init])
		_tableCellView = tableCellView;
	return self;
}

- (NSString*)stringForObjectValue:(id)aValue
{
	return [_tableCellView.objectValue editingAndDisplayName].lastObject;
}

- (NSString*)editingStringForObjectValue:(id)aValue
{
	return [_tableCellView.objectValue editingAndDisplayName].firstObject;
}

- (BOOL)getObjectValue:(id*)valueRef forString:(NSString*)aString errorDescription:(NSString**)errorRef
{
	*valueRef = aString;
	return YES;
}
@end

@interface FileItemTableCellView () <NSTextFieldDelegate>
@property (nonatomic) NSView* itemInfoButtons;
@end

@implementation FileItemTableCellView
- (instancetype)init
{
	if((self = [super initWithFrame:NSZeroRect]))
	{
		_openButton = [[NSButton alloc] initWithFrame:NSZeroRect];
		_openButton.refusesFirstResponder = YES;
		_openButton.buttonType            = NSMomentaryChangeButton;
		_openButton.bordered              = NO;
		_openButton.imagePosition         = NSImageOnly;

		[_openButton setContentHuggingPriority:NSLayoutPriorityRequired forOrientation:NSLayoutConstraintOrientationHorizontal];
		[_openButton setContentCompressionResistancePriority:NSLayoutPriorityRequired forOrientation:NSLayoutConstraintOrientationHorizontal];

		_itemInfoButtons = [[OakItemButtonsView alloc] initWithCloseAction:@selector(didClickCloseButton:) target:self];

		NSTextField* textField = OakCreateLabel(@"", [NSFont controlContentFontOfSize:0]);
		textField.cell = [[FileItemSelectBasenameCell alloc] initTextCell:@""];
		[textField.cell setWraps:NO];
		[textField.cell setLineBreakMode:NSLineBreakByTruncatingMiddle];
		textField.formatter = [[FileItemFormatter alloc] initWithTableCellView:self];

		NSDictionary* views = @{
			@"icon":     _openButton,
			@"filename": textField,
			@"close":    _itemInfoButtons
		};
		OakAddAutoLayoutViewsToSuperview([views allValues], self);

		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(4)-[icon]-(4)-[filename]-(4)-[close]-(0@750)-|" options:0 metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[close]|" options:0 metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:[filename]-(2)-|" options:NSLayoutFormatAlignAllLeading|NSLayoutFormatAlignAllTrailing metrics:nil views:views]];
		[self addConstraint:[NSLayoutConstraint constraintWithItem:self attribute:NSLayoutAttributeCenterY relatedBy:NSLayoutRelationEqual toItem:_openButton attribute:NSLayoutAttributeCenterY multiplier:1 constant:0]];

		[_openButton bind:NSImageBinding     toObject:self withKeyPath:@"objectValue.image"                 options:nil];
		[textField bind:NSValueBinding       toObject:self withKeyPath:@"objectValue.editingAndDisplayName" options:nil];
		[textField bind:NSEditableBinding    toObject:self withKeyPath:@"objectValue.canRename"             options:nil];
		[textField bind:NSToolTipBinding     toObject:self withKeyPath:@"objectValue.toolTip"               options:nil];
		[_itemInfoButtons bind:@"finderTags" toObject:self withKeyPath:@"objectValue.finderTags"            options:nil];
		[_itemInfoButtons bind:@"open"       toObject:self withKeyPath:@"objectValue.open"                  options:nil];

		self.textField = textField;
	}
	return self;
}

- (void)didClickCloseButton:(id)sender
{
	if(self.closeAction)
		[NSApp sendAction:self.closeAction to:self.target from:sender];
}

- (void)dealloc
{
	[_openButton unbind:NSImageBinding];
	[self.textField unbind:NSValueBinding];
	[self.textField unbind:NSEditableBinding];
	[self.textField unbind:NSToolTipBinding];
	[_itemInfoButtons unbind:@"finderTags"];
	[_itemInfoButtons unbind:@"open"];
}

- (void)resetCursorRects
{
	[self addCursorRect:_openButton.frame cursor:NSCursor.pointingHandCursor];
}
@end
