#import "FileItemTableCellView.h"
#import "FileItem.h"
#import <OakAppKit/OakUIConstructionFunctions.h>
#import <OakAppKit/OakFinderTag.h>
#import <TMFileReference/TMFileReference.h>

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

@interface FileItemFinderTagsView : NSView
@property (nonatomic) NSArray<OakFinderTag*>* finderTags;
@property (nonatomic) NSBackgroundStyle backgroundStyle;
@property (nonatomic) BOOL rightPadding;
@end

static void* kObjectValueURLObserverContext = &kObjectValueURLObserverContext;

@interface FileItemTableCellView () <NSTextFieldDelegate>
@property (nonatomic) FileItemFinderTagsView* finderTagsView;
@property (nonatomic) TMFileReference* fileReference;
@end

@implementation FileItemTableCellView
- (instancetype)init
{
	if((self = [super initWithFrame:NSZeroRect]))
	{
		_openButton = [[NSButton alloc] initWithFrame:NSZeroRect];
		_openButton.refusesFirstResponder = YES;
		_openButton.buttonType            = NSButtonTypeMomentaryChange;
		_openButton.bordered              = NO;
		_openButton.imagePosition         = NSImageOnly;
		_openButton.imageScaling          = NSImageScaleProportionallyUpOrDown;

		[_openButton.widthAnchor  constraintEqualToConstant:16].active = YES;
		[_openButton.heightAnchor constraintEqualToConstant:16].active = YES;

		NSTextField* textField = OakCreateLabel(@"", [NSFont controlContentFontOfSize:0]);
		textField.cell = [[FileItemSelectBasenameCell alloc] initTextCell:@""];
		[textField.cell setWraps:NO];
		[textField.cell setLineBreakMode:NSLineBreakByTruncatingMiddle];
		textField.formatter = [[FileItemFormatter alloc] initWithTableCellView:self];

		_finderTagsView = [[FileItemFinderTagsView alloc] initWithFrame:NSZeroRect];

		_closeButton = OakCreateCloseButton();
		_closeButton.refusesFirstResponder = YES;

		NSStackView* stackView = [NSStackView stackViewWithViews:@[
			_openButton, textField, _finderTagsView, _closeButton
		]];
		stackView.spacing = 4;

		[self addSubview:stackView];

		[textField setContentHuggingPriority:NSLayoutPriorityDefaultLow-1 forOrientation:NSLayoutConstraintOrientationHorizontal];

		[stackView.leadingAnchor  constraintEqualToAnchor:self.leadingAnchor  constant: 4].active = YES;
		[stackView.trailingAnchor constraintEqualToAnchor:self.trailingAnchor constant:-8].active = YES;
		[stackView.topAnchor      constraintEqualToAnchor:self.topAnchor      constant: 0].active = YES;
		[stackView.bottomAnchor   constraintEqualToAnchor:self.bottomAnchor   constant: 0].active = YES;

		[_openButton bind:NSImageBinding      toObject:self withKeyPath:@"fileReference.icon"                options:nil];
		[textField bind:NSValueBinding        toObject:self withKeyPath:@"objectValue.editingAndDisplayName" options:nil];
		[textField bind:NSEditableBinding     toObject:self withKeyPath:@"objectValue.canRename"             options:nil];
		[textField bind:NSToolTipBinding      toObject:self withKeyPath:@"objectValue.toolTip"               options:nil];
		[_finderTagsView bind:@"finderTags"   toObject:self withKeyPath:@"objectValue.finderTags"            options:nil];
		[_finderTagsView bind:@"rightPadding" toObject:self withKeyPath:@"fileReference.closable"            options:@{ NSValueTransformerNameBindingOption: NSNegateBooleanTransformerName }];
		[_closeButton bind:NSHiddenBinding    toObject:self withKeyPath:@"fileReference.closable"            options:@{ NSValueTransformerNameBindingOption: NSNegateBooleanTransformerName }];

		self.textField = textField;

		[self addObserver:self forKeyPath:@"objectValue.URL" options:NSKeyValueObservingOptionNew context:kObjectValueURLObserverContext];
	}
	return self;
}

- (void)setBackgroundStyle:(NSBackgroundStyle)newBackgroundStyle
{
	[_finderTagsView setBackgroundStyle:newBackgroundStyle];
	[super setBackgroundStyle:newBackgroundStyle];
}

- (void)dealloc
{
	[self removeObserver:self forKeyPath:@"objectValue.URL" context:kObjectValueURLObserverContext];

	[_openButton unbind:NSImageBinding];
	[self.textField unbind:NSValueBinding];
	[self.textField unbind:NSEditableBinding];
	[self.textField unbind:NSToolTipBinding];
	[_finderTagsView unbind:@"finderTags"];
	[_closeButton unbind:NSHiddenBinding];
}

- (void)observeValueForKeyPath:(NSString*)keyPath ofObject:(id)object change:(NSDictionary*)change context:(void*)context
{
	if(context == kObjectValueURLObserverContext)
	{
		NSURL* url = change[NSKeyValueChangeNewKey];
		self.fileReference = [url isKindOfClass:[NSURL class]] ? [TMFileReference fileReferenceWithURL:url] : nil;
	}
}

- (void)resetCursorRects
{
	[self addCursorRect:_openButton.frame cursor:NSCursor.pointingHandCursor];
}
@end

@implementation FileItemFinderTagsView
- (void)setFinderTags:(NSArray<OakFinderTag*>*)newFinderTags
{
	if([_finderTags isEqual:newFinderTags])
		return;

	_finderTags = newFinderTags;
	self.hidden = _finderTags.count == 0;
	[self setNeedsDisplay:YES];
}

- (void)setBackgroundStyle:(NSBackgroundStyle)newBackgroundStyle
{
	_backgroundStyle = newBackgroundStyle;
	[self setNeedsDisplay:YES];
}

- (void)setRightPadding:(BOOL)flag
{
	_rightPadding = flag;
	[self invalidateIntrinsicContentSize];
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

		if(_backgroundStyle == NSBackgroundStyleEmphasized)
				[NSColor.whiteColor set];
		else	[borderColor set];

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
	r.size.width -= _rightPadding ? 16 : 0;
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
	return NSMakeSize((_rightPadding ? 16 : 0) + 20, 10);
}
@end
