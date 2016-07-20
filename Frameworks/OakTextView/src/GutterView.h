extern NSString* GVColumnDataSourceDidChange;
extern NSString* GVLineNumbersColumnIdentifier;

struct GVLineRecord
{
	GVLineRecord (NSUInteger lineNumber = NSNotFound, NSUInteger softlineOffset = 0, CGFloat firstY = 0, CGFloat lastY = 0, CGFloat baseline = 0) : lineNumber(lineNumber), softlineOffset(softlineOffset), firstY(firstY), lastY(lastY), baseline(baseline) { }

	NSUInteger lineNumber;
	NSUInteger softlineOffset;
	CGFloat firstY;
	CGFloat lastY;
	CGFloat baseline;
};

@protocol GutterViewDelegate
- (GVLineRecord)lineRecordForPosition:(CGFloat)yPos;
- (GVLineRecord)lineFragmentForLine:(NSUInteger)aLine column:(NSUInteger)aColumn;
@end

typedef NS_ENUM(NSUInteger, GutterViewRowState) {
	GutterViewRowStateRegular = 0,
	GutterViewRowStatePressed,
	GutterViewRowStateRollover
};

@protocol GutterViewColumnDataSource
- (NSImage*)imageForLine:(NSUInteger)aLine inColumnWithIdentifier:(id)columnIdentifier state:(GutterViewRowState)rowState;
- (CGFloat)widthForColumnWithIdentifier:(id)columnIdentifier;
@end

@protocol GutterViewColumnDelegate
- (void)userDidClickColumnWithIdentifier:(id)columnIdentifier atLine:(NSUInteger)lineNumber;
@end

@interface GutterView : NSView
@property (nonatomic, weak) IBOutlet NSView* partnerView;
@property (nonatomic) NSFont* lineNumberFont;
@property (nonatomic, weak) id <GutterViewDelegate> delegate;
@property (nonatomic) NSColor* foregroundColor;
@property (nonatomic) NSColor* backgroundColor;
@property (nonatomic) NSColor* iconColor;
@property (nonatomic) NSColor* iconHoverColor;
@property (nonatomic) NSColor* iconPressedColor;
@property (nonatomic) NSColor* selectionForegroundColor;
@property (nonatomic) NSColor* selectionBackgroundColor;
@property (nonatomic) NSColor* selectionIconColor;
@property (nonatomic) NSColor* selectionIconHoverColor;
@property (nonatomic) NSColor* selectionIconPressedColor;
@property (nonatomic) NSColor* selectionBorderColor;
- (void)setHighlightedRange:(std::string const&)str;
- (void)reloadData:(id)sender;
- (void)insertColumnWithIdentifier:(NSString*)columnIdentifier atPosition:(NSUInteger)index dataSource:(id <GutterViewColumnDataSource>)columnDataSource delegate:(id <GutterViewColumnDelegate>)columnDelegate;
- (void)setVisibility:(BOOL)visible forColumnWithIdentifier:(NSString*)columnIdentifier;
- (BOOL)visibilityForColumnWithIdentifier:(NSString*)identifier;
@end
