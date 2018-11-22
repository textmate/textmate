#import "FileItemTableCellView.h"
#import "FileItem.h"
#import <OakAppKit/OakUIConstructionFunctions.h>
#import <OakAppKit/OakFinderTag.h>

@interface FileItemAuxiliaryView : NSView
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

		_itemInfoButtons = [[FileItemAuxiliaryView alloc] initWithCloseAction:@selector(didClickCloseButton:) target:self];

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

@interface FileItemFinderTagsView : NSView
@property (nonatomic) NSArray<OakFinderTag*>* finderTags;
@end

@implementation FileItemFinderTagsView
- (void)setFinderTags:(NSArray<OakFinderTag*>*)newFinderTags
{
	if(![_finderTags isEqual:newFinderTags])
	{
		_finderTags = newFinderTags;
		[self setNeedsDisplay:YES];
	}
}

- (BOOL)isSelectedAndEmphasized
{
	NSView* view = self;
	while(view && ![view isKindOfClass:[NSTableRowView class]])
		view = [view superview];
	return [view isKindOfClass:[NSTableRowView class]] && ((NSTableRowView*)view).isSelected && ((NSTableRowView*)view).isEmphasized;
}

- (void)drawRect:(NSRect)aRect
{
	NSArray<OakFinderTag*>* tagsWithLabelColor = [_finderTags filteredArrayUsingPredicate:[NSPredicate predicateWithFormat:@"hasLabelColor == YES"]];

	auto fillAndStrokePath = ^(NSBezierPath* path, OakFinderTag* tag){

		NSColor* borderColor = tag.labelColor;
		NSColor* fillColor = nil;

		if(borderColor)
		{
			NSColor* rgbColor = [borderColor colorUsingColorSpace:NSColorSpace.sRGBColorSpace];
			CGFloat factor = 0.8;
			CGFloat r = 1 - factor*(1 - rgbColor.redComponent);
			CGFloat g = 1 - factor*(1 - rgbColor.greenComponent);
			CGFloat b = 1 - factor*(1 - rgbColor.blueComponent);

			fillColor = [NSColor colorWithSRGBRed:r green:g blue:b alpha:1.0];
		}
		else
		{
			borderColor = [NSColor secondaryLabelColor];
			fillColor = [NSColor clearColor];
		}

		[fillColor set];
		[path fill];

		self.isSelectedAndEmphasized ? [[NSColor whiteColor] set] : [borderColor set];
		[path stroke];
	};

	auto drawCrestent = ^(NSPoint center1, NSPoint center2, OakFinderTag* tag){
		[NSGraphicsContext saveGraphicsState];

		NSBezierPath* clippingPath = [NSBezierPath bezierPath];
		[clippingPath appendBezierPathWithArcWithCenter:center2 radius:5.0 startAngle:-100 endAngle:100];
		[clippingPath appendBezierPathWithArcWithCenter:center1 radius:5.5 startAngle:60 endAngle:300 clockwise:YES];
		[clippingPath addClip];

		NSBezierPath* path = [NSBezierPath bezierPath];
		[path appendBezierPathWithArcWithCenter:center2 radius:4.0 startAngle:0 endAngle:360];
		[path closePath];

		fillAndStrokePath(path, tag);

		[NSGraphicsContext restoreGraphicsState];
	};

	NSRect r = [self bounds];
	switch([tagsWithLabelColor count])
	{
		case 0: return;
		case 1:
		{
			NSBezierPath* path = [NSBezierPath bezierPath];
			[path appendBezierPathWithArcWithCenter:NSMakePoint(NSMidX(r), NSMidY(r)) radius:4.0 startAngle:0 endAngle:360];

			fillAndStrokePath(path, tagsWithLabelColor[0]);
			return;
		}
		case 2:
		{
			NSPoint center = NSMakePoint(NSMidX(r), NSMidY(r));

			NSPoint center1 = NSMakePoint(center.x - 2.0, center.y);
			NSPoint center2 = NSMakePoint(center.x + 2.0, center.y);

			drawCrestent(center1, center2, tagsWithLabelColor[0]);

			NSBezierPath* path = [NSBezierPath bezierPath];
			[path appendBezierPathWithArcWithCenter:center1 radius:4.0 startAngle:0 endAngle:360];
			fillAndStrokePath(path, tagsWithLabelColor[1]);
			return;
		}
		default:
		{
			NSPoint center = NSMakePoint(NSMidX(r), NSMidY(r));

			NSPoint center1 = NSMakePoint(center.x - 4.0, center.y);
			NSPoint center2 = NSMakePoint(center.x, center.y);
			NSPoint center3 = NSMakePoint(center.x + 4.0, center.y);

			NSUInteger lastIndex = [tagsWithLabelColor count] - 1;
			drawCrestent(center2, center3, tagsWithLabelColor[lastIndex - 2]);
			drawCrestent(center1, center2, tagsWithLabelColor[lastIndex - 1]);

			NSBezierPath* path = [NSBezierPath bezierPath];
			[path appendBezierPathWithArcWithCenter:center1 radius:4.0 startAngle:0 endAngle:360];
			fillAndStrokePath(path, tagsWithLabelColor[lastIndex]);
			return;
		}
	}
}

- (NSSize)intrinsicContentSize
{
	return NSMakeSize(20, 10);
}
@end

@interface FileItemAuxiliaryView ()
@property (nonatomic) NSArray<OakFinderTag*>* finderTags;
@property (nonatomic) BOOL open;

@property (nonatomic) SEL closeAction;
@property (nonatomic, weak) id target;

@property (nonatomic) FileItemFinderTagsView* labelSwatchView;
@property (nonatomic) NSButton* closeButton;
@property (nonatomic) NSMutableArray* myConstraints;
@end

@implementation FileItemAuxiliaryView
- (id)initWithCloseAction:(SEL)closeAction target:(id)target
{
	if(self = [super initWithFrame:NSZeroRect])
	{
		_closeAction = closeAction;
		_target      = target;

		[self setContentHuggingPriority:NSLayoutPriorityDefaultHigh forOrientation:NSLayoutConstraintOrientationHorizontal];
		[self setContentCompressionResistancePriority:NSLayoutPriorityDefaultHigh forOrientation:NSLayoutConstraintOrientationHorizontal];
	}
	return self;
}

- (void)setBackgroundStyle:(NSBackgroundStyle)newBackgroundStyle
{
	_closeButton.cell.backgroundStyle = newBackgroundStyle;
}

- (void)updateConstraints
{
	if(_myConstraints)
		[self removeConstraints:_myConstraints];
	_myConstraints = [NSMutableArray array];

	if(_labelSwatchView)
	{
		NSDictionary* views = @{ @"labelSwatch": _labelSwatchView };
		[_myConstraints addObject:[NSLayoutConstraint constraintWithItem:self attribute:NSLayoutAttributeCenterY relatedBy:NSLayoutRelationEqual toItem:_labelSwatchView attribute:NSLayoutAttributeCenterY multiplier:1 constant:0]];
		[_myConstraints addObjectsFromArray:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-(>=0)-[labelSwatch]-(>=0)-|" options:0 metrics:nil views:views]];
		[_myConstraints addObjectsFromArray:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[labelSwatch]-(24)-|" options:0 metrics:nil views:views]];
	}

	if(_closeButton)
	{
		NSDictionary* views = @{ @"closeButton": _closeButton };
		[_myConstraints addObject:[NSLayoutConstraint constraintWithItem:self attribute:NSLayoutAttributeCenterY relatedBy:NSLayoutRelationEqual toItem:_closeButton attribute:NSLayoutAttributeCenterY multiplier:1 constant:0]];
		[_myConstraints addObjectsFromArray:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-(>=0)-[closeButton]-(>=0)-|" options:0 metrics:nil views:views]];
		[_myConstraints addObjectsFromArray:[NSLayoutConstraint constraintsWithVisualFormat:@"H:[closeButton]-(8)-|" options:0 metrics:nil views:views]];
		if(!_labelSwatchView)
			[_myConstraints addObjectsFromArray:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[closeButton]" options:0 metrics:nil views:views]];
	}

	if(!_labelSwatchView && !_closeButton)
		[_myConstraints addObject:[NSLayoutConstraint constraintWithItem:self attribute:NSLayoutAttributeWidth relatedBy:NSLayoutRelationEqual toItem:nil attribute:NSLayoutAttributeNotAnAttribute multiplier:1 constant:0]];

	[self addConstraints:_myConstraints];
	[super updateConstraints];
}

- (NSArray<OakFinderTag*>*)finderTags
{
	return _labelSwatchView ? _labelSwatchView.finderTags : @[ ];
}

- (void)setFinderTags:(NSArray<OakFinderTag*>*)newFinderTags
{
	if([self.finderTags isEqual:newFinderTags])
		return;

	if(![newFinderTags count])
	{
		[_labelSwatchView removeFromSuperview];
		_labelSwatchView = nil;
		[self setNeedsUpdateConstraints:YES];
	}
	else if(!_labelSwatchView)
	{
		_labelSwatchView = [[FileItemFinderTagsView alloc] initWithFrame:NSZeroRect];
		OakAddAutoLayoutViewsToSuperview(@[ _labelSwatchView ], self);
		[self setNeedsUpdateConstraints:YES];
	}
	_labelSwatchView.finderTags = newFinderTags;
}

- (void)setNilValueForKey:(NSString*)aKey
{
	if([aKey isEqualToString:@"open"])
			[self setValue:@NO forKey:aKey];
	else	[super setNilValueForKey:aKey];
}

- (void)setOpen:(BOOL)flag
{
	if(_open == flag)
		return;

	if(!flag)
	{
		[_closeButton removeFromSuperview];
		_closeButton = nil;
	}
	else if(!_closeButton)
	{
		_closeButton = OakCreateCloseButton();
		_closeButton.refusesFirstResponder = YES;
		_closeButton.target = _target;
		_closeButton.action = _closeAction;

		OakAddAutoLayoutViewsToSuperview(@[ _closeButton ], self);
	}

	_open = flag;
	[self setNeedsUpdateConstraints:YES];
}
@end
