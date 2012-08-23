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
{
	IBOutlet NSView* partnerView;
	NSFont* lineNumberFont;
	NSColor* foregroundColor;
	NSColor* backgroundColor;
	NSColor* selectionForegroundColor;
	NSColor* selectionBackgroundColor;
	id <GutterViewDelegate> delegate;
	std::vector<struct data_source_t> columnDataSources;
	NSMutableSet* hiddenColumns;
	std::string highlightedRange;
	std::vector<CGRect> backgroundRects, borderRects;
	std::set<size_t> selectedLines;

	NSPoint mouseDownAtPoint;
	NSPoint mouseHoveringAtPoint;
}
@property (nonatomic, retain) NSView* partnerView;
@property (nonatomic, retain) NSFont* lineNumberFont;
@property (nonatomic, assign) id <GutterViewDelegate> delegate;
@property (nonatomic, retain) NSColor* foregroundColor;
@property (nonatomic, retain) NSColor* backgroundColor;
@property (nonatomic, retain) NSColor* selectionForegroundColor;
@property (nonatomic, retain) NSColor* selectionBackgroundColor;
- (void)setHighlightedRange:(std::string const&)str;
- (void)reloadData:(id)sender;
- (void)insertColumnWithIdentifier:(NSString*)columnIdentifier atPosition:(NSUInteger)index dataSource:(id <GutterViewColumnDataSource>)columnDataSource delegate:(id <GutterViewColumnDelegate>)columnDelegate;
- (void)setVisibility:(BOOL)visible forColumnWithIdentifier:(NSString*)columnIdentifier;
- (BOOL)visibilityForColumnWithIdentifier:(NSString*)identifier;
@end
