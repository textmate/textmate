#import "Printing.h"
#import "OakDocument Private.h"
#import <OakAppKit/src/OakUIConstructionFunctions.h>
#import <theme/src/theme.h>
#import <layout/src/layout.h>
#import <ns/src/ns.h>
#import <text/src/my_ctype.h>

@interface OakDocumentPrintOptionsViewController ()
{
	std::vector<oak::uuid_t> themeUUIDs;
}
@end

@interface OakDocumentPrintableView ()
{
	std::unique_ptr<ng::layout_t> layout;
	std::vector<CGRect> pageRects;
}
@property (nonatomic) OakDocument* document;
@property (nonatomic) NSString* themeUUID;
@property (nonatomic) NSString* fontName;
@property (nonatomic) CGFloat fontSize;
@property (nonatomic) CGFloat pageWidth;
@property (nonatomic) CGFloat pageHeight;
@property (nonatomic) BOOL myNeedsLayout;
@end

#ifndef CONSTRAINT
#define CONSTRAINT(str, align) [constraints addObjectsFromArray:[NSLayoutConstraint constraintsWithVisualFormat:str options:align metrics:nil views:views]]
#endif

// ============================
// = OakDocumentPrintableView =
// ============================

@implementation OakDocumentPrintableView
+ (void)initialize
{
	[NSUserDefaults.standardUserDefaults registerDefaults:@{
		@"OakPrintThemeUUID":       @(kMacClassicThemeUUID),
		@"OakPrintFontSize":        @(11),
		@"OakPrintHeaderAndFooter": @NO,
	}];
}

- (id)initWithDocument:(OakDocument*)aDocument fontName:(NSString*)aFontName
{
	if(self = [self initWithFrame:NSZeroRect])
	{
		_document = aDocument;
		_fontName = aFontName;
	}
	return self;
}

- (BOOL)isFlipped
{
	return YES;
}

- (NSString*)printJobTitle
{
	return _document.displayName;
}

- (BOOL)knowsPageRange:(NSRangePointer)range
{
	NSPrintInfo* info = [[NSPrintOperation currentOperation] printInfo];

	NSRect display = NSIntersectionRect(info.imageablePageBounds, (NSRect){ NSZeroPoint, info.paperSize });
	info.leftMargin   = NSMinX(display);
	info.rightMargin  = info.paperSize.width - NSMaxX(display);
	info.topMargin    = info.paperSize.height - NSMaxY(display);
	info.bottomMargin = NSMinY(display);

	self.pageWidth  = floor(info.paperSize.width - info.leftMargin - info.rightMargin);
	self.pageHeight = floor(info.paperSize.height - info.topMargin - info.bottomMargin);
	self.fontSize   = [[[info dictionary] objectForKey:@"OakPrintFontSize"] floatValue];
	self.themeUUID  = [[info dictionary] objectForKey:@"OakPrintThemeUUID"];

	[self updateLayout];
	[self setFrame:NSMakeRect(0, 0, self.pageWidth, layout->height())];

	range->location = 1;
	range->length   = pageRects.size();

	return YES;
}

- (NSRect)rectForPage:(NSInteger)pageNumber
{
	NSParameterAssert(0 < pageNumber && pageNumber <= pageRects.size());
	return pageRects[pageNumber-1];
}

- (void)drawRect:(NSRect)aRect
{
	NSEraseRect(aRect);
	if(![NSGraphicsContext currentContextDrawingToScreen] && layout)
		layout->draw(NSGraphicsContext.currentContext.CGContext, aRect, [self isFlipped], /* selection: */ ng::ranges_t(), /* highlight: */ ng::ranges_t(), /* draw background: */ false);
}

- (void)updateLayout
{
	if(!_myNeedsLayout)
		return;

	pageRects.clear();

	theme_ptr theme = parse_theme(bundles::lookup(to_s(self.themeUUID)));
	layout = std::make_unique<ng::layout_t>([_document buffer], theme, to_s(_fontName), _fontSize, /* softWrap: */ true);
	layout->set_viewport_size(CGSizeMake(self.pageWidth, self.pageHeight));
	layout->update_metrics(CGRectMake(0, 0, CGFLOAT_MAX, CGFLOAT_MAX));

	CGRect pageRect = CGRectMake(0, 0, self.pageWidth, self.pageHeight);
	while(true)
	{
		CGRect lineRect = layout->rect_at_index(layout->index_at_point(CGPointMake(NSMinX(pageRect), NSMaxY(pageRect))).index);
		if(NSMaxY(lineRect) <= NSMinY(pageRect))
			break;
		else if(CGRectContainsRect(pageRect, lineRect))
			pageRect.size.height = NSMaxY(lineRect) - NSMinY(pageRect);
		else
			pageRect.size.height = NSMinY(lineRect) - NSMinY(pageRect);
		pageRects.push_back(pageRect);

		pageRect.origin.y = NSMaxY(pageRect);
		pageRect.size.height = self.pageHeight;
	}

	_myNeedsLayout = NO;
}

- (void)setPageWidth:(CGFloat)newPageWidth    { if(_pageWidth  != newPageWidth)  { _myNeedsLayout = YES; _pageWidth  = newPageWidth;  } }
- (void)setPageHeight:(CGFloat)newPageHeight  { if(_pageHeight != newPageHeight) { _myNeedsLayout = YES; _pageHeight = newPageHeight; } }
- (void)setFontSize:(CGFloat)newFontSize      { if(_fontSize   != newFontSize)   { _myNeedsLayout = YES; _fontSize   = newFontSize;   } }
- (void)setThemeUUID:(NSString*)newThemeUUID  { if(![_themeUUID isEqualToString:newThemeUUID]) { _myNeedsLayout = YES; _themeUUID  = newThemeUUID; } }
@end

// =========================================
// = OakDocumentPrintOptionsViewController =
// =========================================

@implementation OakDocumentPrintOptionsViewController
- (id)init
{
	if((self = [super init]))
	{
		NSView* contentView = [[NSView alloc] initWithFrame:NSZeroRect];
		[contentView setTranslatesAutoresizingMaskIntoConstraints:NO];

		NSTextField* themesLabel    = OakCreateLabel(@"Theme:");
		NSPopUpButton* themes       = OakCreatePopUpButton();
		NSTextField* fontSizesLabel = OakCreateLabel(@"Font Size:");
		NSPopUpButton* fontSizes    = OakCreatePopUpButton();
		NSButton* printHeaders      = OakCreateCheckBox(@"Print header and footer");

		NSMenu* themesMenu = themes.menu;
		[themesMenu removeAllItems];

		std::multimap<std::string, bundles::item_ptr, text::less_t> ordered;
		for(auto item : bundles::query(bundles::kFieldAny, NULL_STR, scope::wildcard, bundles::kItemTypeTheme))
			ordered.emplace(item->name(), item);

		for(auto pair : ordered)
		{
			[themesMenu addItemWithTitle:to_ns(pair.first) action:NULL keyEquivalent:@""];
			themeUUIDs.push_back(pair.second->uuid());
		}

		if(ordered.empty())
			[themesMenu addItemWithTitle:@"No Themes Loaded" action:@selector(nop:) keyEquivalent:@""];

		NSMenu* fontSizesMenu = fontSizes.menu;
		[fontSizesMenu removeAllItems];
		for(NSInteger size = 4; size < 23; ++size)
			[fontSizesMenu addItemWithTitle:@(size).stringValue action:NULL keyEquivalent:@""];

		[themes bind:NSSelectedIndexBinding toObject:self withKeyPath:@"themeIndex" options:nil];
		[fontSizes bind:NSSelectedValueBinding toObject:self withKeyPath:@"printFontSize" options:nil];
		[printHeaders bind:NSValueBinding toObject:self withKeyPath:@"printHeaderAndFooter" options:nil];

		NSDictionary* views = @{
			@"themesLabel":    themesLabel,
			@"themes":         themes,
			@"fontSizesLabel": fontSizesLabel,
			@"fontSizes":      fontSizes,
			@"printHeaders":   printHeaders
		};

		OakAddAutoLayoutViewsToSuperview([views allValues], contentView);

		NSMutableArray* constraints = [NSMutableArray array];
		CONSTRAINT(@"H:|-(>=4)-[themesLabel]-[themes]-(>=4)-|",        NSLayoutFormatAlignAllBaseline);
		CONSTRAINT(@"H:|-(>=4)-[fontSizesLabel]-[fontSizes]-(>=4)-|",  NSLayoutFormatAlignAllBaseline);
		CONSTRAINT(@"H:[printHeaders]-(>=4)-|",                        0);
		CONSTRAINT(@"V:|-[themes]-[fontSizes]",                        NSLayoutFormatAlignAllLeft|NSLayoutFormatAlignAllRight);
		CONSTRAINT(@"V:[fontSizes]-[printHeaders]-|",                  NSLayoutFormatAlignAllLeft);
		[contentView addConstraints:constraints];

		contentView.frame = (NSRect){ NSZeroPoint, [contentView fittingSize] };
		self.view = contentView;
	}
	return self;
}

- (void)setRepresentedObject:(NSPrintInfo*)printInfo
{
	[super setRepresentedObject:printInfo];
	[self setThemeIndex:[self themeIndex]];
	[self setPrintFontSize:[self printFontSize]];
	[self setPrintHeaderAndFooter:[self printHeaderAndFooter]];
}

- (void)setThemeIndex:(NSInteger)anIndex
{
	if(anIndex < themeUUIDs.size())
	{
		NSPrintInfo* info = [self representedObject];
		[[info dictionary] setObject:to_ns(themeUUIDs[anIndex]) forKey:@"OakPrintThemeUUID"];
		[NSUserDefaults.standardUserDefaults setObject:to_ns(themeUUIDs[anIndex]) forKey:@"OakPrintThemeUUID"];
	}
}

- (NSInteger)themeIndex
{
	NSPrintInfo* info = [self representedObject];
	if(NSString* themeUUID = [[info dictionary] objectForKey:@"OakPrintThemeUUID"])
	{
		for(size_t i = 0; i < themeUUIDs.size(); ++i)
		{
			if(themeUUIDs[i] == to_s(themeUUID))
				return i;
		}
	}
	return 0;
}

- (void)setPrintHeaderAndFooter:(BOOL)flag
{
	NSPrintInfo* info = [self representedObject];
	[[info dictionary] setObject:@(flag) forKey:NSPrintHeaderAndFooter];
	[NSUserDefaults.standardUserDefaults setObject:@(flag) forKey:@"OakPrintHeaderAndFooter"];
}

- (BOOL)printHeaderAndFooter
{
	return [[[[self representedObject] dictionary] objectForKey:NSPrintHeaderAndFooter] boolValue];
}

- (void)setPrintFontSize:(NSNumber*)size
{
	NSPrintInfo* info = [self representedObject];
	[[info dictionary] setObject:size forKey:@"OakPrintFontSize"];
	[NSUserDefaults.standardUserDefaults setObject:size forKey:@"OakPrintFontSize"];
}

- (NSNumber*)printFontSize
{
	return [[[self representedObject] dictionary] objectForKey:@"OakPrintFontSize"];
}

- (NSSet*)keyPathsForValuesAffectingPreview
{
	return [NSSet setWithObjects:@"themeIndex", @"printFontSize", @"printHeaderAndFooter", nil];
}

- (NSArray*)localizedSummaryItems
{
	return @[ ]; // TODO
}

- (NSString*)title
{
	return @"TextMate";
}
@end
