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
- (GVLineRecord const&)lineRecordForPosition:(CGFloat)yPos;
- (GVLineRecord const&)lineFragmentForLine:(NSUInteger)aLine column:(NSUInteger)aColumn;
@end

@protocol GutterViewColumnDataSource
- (NSUInteger)stateForColumnWithIdentifier:(id)columnIdentifier atLine:(NSUInteger)lineNumber;
- (NSImage*)imageForState:(NSUInteger)aState forColumnWithIdentifier:(id)columnIdentifier;
@optional
- (NSImage*)hoverImageForState:(NSUInteger)aState forColumnWithIdentifier:(id)columnIdentifier;
- (NSImage*)pressedImageForState:(NSUInteger)aState forColumnWithIdentifier:(id)columnIdentifier;
@end

@protocol GutterViewColumnDelegate
- (void)userDidClickColumnWithIdentifier:(id)columnIdentifier atLine:(NSUInteger)lineNumber;
@end

@interface GutterView : NSView
@property (nonatomic, retain) IBOutlet NSView* partnerView;
@property (nonatomic, retain) NSFont* lineNumberFont;
@property (nonatomic, assign) id <GutterViewDelegate> delegate;
@property (nonatomic, retain) NSColor* foregroundColor;
@property (nonatomic, retain) NSColor* backgroundColor;
@property (nonatomic, retain) NSColor* iconColor;
@property (nonatomic, retain) NSColor* iconHoverColor;
@property (nonatomic, retain) NSColor* iconPressedColor;
@property (nonatomic, retain) NSColor* selectionForegroundColor;
@property (nonatomic, retain) NSColor* selectionBackgroundColor;
@property (nonatomic, retain) NSColor* selectionIconColor;
@property (nonatomic, retain) NSColor* selectionIconHoverColor;
@property (nonatomic, retain) NSColor* selectionIconPressedColor;
@property (nonatomic, retain) NSColor* selectionBorderColor;
- (void)setHighlightedRange:(std::string const&)str;
- (void)reloadData:(id)sender;
- (void)insertColumnWithIdentifier:(NSString*)columnIdentifier atPosition:(NSUInteger)index dataSource:(id <GutterViewColumnDataSource>)columnDataSource delegate:(id <GutterViewColumnDelegate>)columnDelegate;
- (void)setVisibility:(BOOL)visible forColumnWithIdentifier:(NSString*)columnIdentifier;
- (BOOL)visibilityForColumnWithIdentifier:(NSString*)identifier;
@end
