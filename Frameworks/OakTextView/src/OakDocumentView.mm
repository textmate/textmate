#import "OakDocumentView.h"
#import <oak/TextMateResponderActions.h>
#import "GutterView.h"
#import "OTVStatusBar.h"
#import <document/src/OakDocument.h>
#import <file/src/type.h>
#import <text/src/my_ctype.h>
#import <text/src/parse.h>
#import <ns/src/ns.h>
#import <oak/debug.h>
#import <bundles/src/bundles.h>
#import <settings/src/settings.h>
#import <OakFilterList/src/SymbolChooser.h>
#import <OakFoundation/src/NSString Additions.h>
#import <OakAppKit/src/OakAppKit.h>
#import <OakAppKit/src/NSImage Additions.h>
#import <OakAppKit/src/OakToolTip.h>
#import <OakAppKit/src/OakPasteboardChooser.h>
#import <OakAppKit/src/OakPasteboard.h>
#import <OakAppKit/src/OakUIConstructionFunctions.h>
#import <OakAppKit/src/NSMenuItem Additions.h>
#import <BundleMenu/src/BundleMenu.h>

static NSString* const kUserDefaultsLineNumberScaleFactorKey = @"lineNumberScaleFactor";
static NSString* const kUserDefaultsLineNumberFontNameKey    = @"lineNumberFontName";

static NSString* const kBookmarksColumnIdentifier = @"bookmarks";
static NSString* const kFoldingsColumnIdentifier  = @"foldings";
static CGFloat const kCurrentLineHighlightAlphaMultiplier = 0.18;

static NSUInteger OTVSymbolTextIndent (NSString* symbol)
{
	NSUInteger indent = 0;
	while(indent < symbol.length)
	{
		unichar ch = [symbol characterAtIndex:indent];
		if(ch != ' ' && ch != '\t' && ch != 0x2003) // Em-space
			break;
		++indent;
	}
	return indent;
}

static NSString* OTVSymbolTitle (NSString* symbol)
{
	if(symbol.length == 0 || [symbol isEqualToString:@"-"])
		return @"";

	NSUInteger indent = OTVSymbolTextIndent(symbol);
	return [[symbol substringFromIndex:indent] stringByTrimmingCharactersInSet:NSCharacterSet.whitespaceAndNewlineCharacterSet];
}

static NSString* OTVUnescapeXMLString (NSString* str)
{
	NSMutableString* res = [str mutableCopy];
	[res replaceOccurrencesOfString:@"&lt;" withString:@"<" options:0 range:NSMakeRange(0, res.length)];
	[res replaceOccurrencesOfString:@"&gt;" withString:@">" options:0 range:NSMakeRange(0, res.length)];
	[res replaceOccurrencesOfString:@"&quot;" withString:@"\"" options:0 range:NSMakeRange(0, res.length)];
	[res replaceOccurrencesOfString:@"&apos;" withString:@"'" options:0 range:NSMakeRange(0, res.length)];
	[res replaceOccurrencesOfString:@"&amp;" withString:@"&" options:0 range:NSMakeRange(0, res.length)];
	return res;
}

static NSString* OTVPlistKeyForLine (NSString* line)
{
	NSRange keyOpen = [line rangeOfString:@"<key>"];
	if(keyOpen.location != NSNotFound)
	{
		NSUInteger valueStart = NSMaxRange(keyOpen);
		NSRange keyClose = [line rangeOfString:@"</key>" options:0 range:NSMakeRange(valueStart, line.length - valueStart)];
		if(keyClose.location != NSNotFound)
			return OTVUnescapeXMLString([line substringWithRange:NSMakeRange(valueStart, keyClose.location - valueStart)]);
	}

	NSString* trimmedLine = [line stringByTrimmingCharactersInSet:NSCharacterSet.whitespaceAndNewlineCharacterSet];
	if(trimmedLine.length == 0 || [trimmedLine hasPrefix:@"//"] || [trimmedLine hasPrefix:@"/*"] || [trimmedLine hasPrefix:@"*"] || [trimmedLine hasPrefix:@"}"])
		return nil;

	NSRange separator = [trimmedLine rangeOfString:@"="];
	if(separator.location == NSNotFound || separator.location == 0)
		return nil;

	NSString* key = [[trimmedLine substringToIndex:separator.location] stringByTrimmingCharactersInSet:NSCharacterSet.whitespaceAndNewlineCharacterSet];
	if(key.length >= 2)
	{
		unichar first = [key characterAtIndex:0];
		unichar last  = [key characterAtIndex:key.length - 1];
		if((first == '"' && last == '"') || (first == '\'' && last == '\''))
			key = [key substringWithRange:NSMakeRange(1, key.length - 2)];
	}
	return key.length ? key : nil;
}

static BOOL OTVLineContainsAnyString (NSString* line, NSArray<NSString*>* strings)
{
	for(NSString* str in strings)
	{
		if([line rangeOfString:str].location != NSNotFound)
			return YES;
	}
	return NO;
}

static BOOL OTVLooksLikePlist (NSString* content)
{
	NSUInteger length = std::min<NSUInteger>(content.length, 4096);
	NSString* prefix = [content substringToIndex:length];
	return [prefix rangeOfString:@"<plist"].location != NSNotFound || [prefix rangeOfString:@"DOCTYPE plist" options:NSCaseInsensitiveSearch].location != NSNotFound || [prefix rangeOfString:@"// !$*UTF8*$!"].location != NSNotFound;
}

static NSString* OTVOldStylePlistValueForLine (NSString* line)
{
	NSRange separator = [line rangeOfString:@"="];
	if(separator.location == NSNotFound)
		return @"";
	return [[line substringFromIndex:NSMaxRange(separator)] stringByTrimmingCharactersInSet:NSCharacterSet.whitespaceAndNewlineCharacterSet];
}

static NSString* OTVOldStylePlistScopePath (NSString* content, NSUInteger caretLine)
{
	NSMutableArray<NSString*>* containerStack = [NSMutableArray array];
	__block NSString* pendingKey = nil;
	__block NSString* currentKey = nil;
	__block NSString* res = @"";
	__block NSUInteger lineNumber = 0;

	[content enumerateSubstringsInRange:NSMakeRange(0, content.length) options:NSStringEnumerationByLines usingBlock:^(NSString* line, NSRange, NSRange, BOOL* stop){
		NSString* trimmedLine = [line stringByTrimmingCharactersInSet:NSCharacterSet.whitespaceAndNewlineCharacterSet];

		if([trimmedLine hasPrefix:@"}"] || [trimmedLine hasPrefix:@")"])
		{
			if(containerStack.count)
				[containerStack removeLastObject];
			pendingKey = nil;
			currentKey = nil;
		}

		NSString* title = OTVPlistKeyForLine(line);
		if(title.length)
		{
			pendingKey = title;
			currentKey = title;
		}

		NSString* value = title.length ? OTVOldStylePlistValueForLine(line) : trimmedLine;
		BOOL opensContainer = [value hasPrefix:@"{"] || [value hasPrefix:@"("];
		if(opensContainer && pendingKey.length)
		{
			[containerStack addObject:pendingKey];
			pendingKey = nil;
			currentKey = nil;
		}
		else if(pendingKey.length && value.length && ![value isEqualToString:@"="])
		{
			pendingKey = nil;
		}

		NSMutableArray* titles = [containerStack mutableCopy];
		if(currentKey.length)
			[titles addObject:currentKey];
		res = [titles componentsJoinedByString:@" > "];

		if(lineNumber >= caretLine)
		{
			*stop = YES;
			return;
		}

		if(!title.length && pendingKey.length && value.length)
		{
			currentKey = pendingKey;
			pendingKey = nil;
		}

		++lineNumber;
	}];

	return res;
}

static NSString* OTVXMLPlistScopePath (NSString* content, NSUInteger caretLine)
{
	NSMutableArray<NSString*>* containerStack = [NSMutableArray array];
	__block NSString* pendingKey = nil;
	__block NSString* res = @"";
	__block NSUInteger lineNumber = 0;

	[content enumerateSubstringsInRange:NSMakeRange(0, content.length) options:NSStringEnumerationByLines usingBlock:^(NSString* line, NSRange, NSRange, BOOL* stop){
		NSString* trimmedLine = [line stringByTrimmingCharactersInSet:NSCharacterSet.whitespaceAndNewlineCharacterSet];

		if([trimmedLine hasPrefix:@"</dict"] || [trimmedLine hasPrefix:@"</array"])
		{
			if(containerStack.count)
				[containerStack removeLastObject];
			pendingKey = nil;
		}

		NSString* key = OTVPlistKeyForLine(line);
		if(key.length)
			pendingKey = key;

		BOOL opensContainer = OTVLineContainsAnyString(line, @[ @"<dict>", @"<dict ", @"<array>", @"<array " ]);
		if(opensContainer && pendingKey.length)
		{
			[containerStack addObject:pendingKey];
			pendingKey = nil;
		}

		NSMutableArray* titles = [containerStack mutableCopy];
		if(pendingKey.length)
			[titles addObject:pendingKey];
		res = [titles componentsJoinedByString:@" > "];

		if(lineNumber >= caretLine)
		{
			*stop = YES;
			return;
		}

		BOOL scalarValue = OTVLineContainsAnyString(line, @[ @"<string>", @"<integer>", @"<real>", @"<date>", @"<data>", @"<true", @"<false" ]);
		if(scalarValue && !opensContainer)
			pendingKey = nil;

		++lineNumber;
	}];

	return res;
}

static NSString* OTVPlistScopePath (NSString* content, NSUInteger caretLine, BOOL forceOldStyle)
{
	if(!(forceOldStyle || OTVLooksLikePlist(content)))
		return @"";
	return !forceOldStyle && [content rangeOfString:@"<plist"].location != NSNotFound ? OTVXMLPlistScopePath(content, caretLine) : OTVOldStylePlistScopePath(content, caretLine);
}

@interface OTVScopeBar : OakBackgroundFillView
@property (nonatomic) NSString* scopePath;
@property (nonatomic) CGFloat gutterWidth;
@end

@implementation OTVScopeBar
{
	NSLayoutConstraint* _gutterWidthConstraint;
	NSTextField* _scopeField;
	NSView* _divider;
}

- (id)initWithFrame:(NSRect)aRect
{
	if(self = [super initWithFrame:aRect])
	{
		if(@available(macOS 26.0, *))
		{
			self.activeBackgroundColor   = NSColor.windowBackgroundColor;
			self.inactiveBackgroundColor = NSColor.windowBackgroundColor;
		}
		else
		{
			self.style = OakBackgroundFillViewStyleTitlebar;
		}
		self.accessibilityLabel = @"Scope Bar";

		_divider = OakCreateNSBoxSeparator();

		NSView* gutterSpacer = [[NSView alloc] initWithFrame:NSZeroRect];

		_scopeField = OakCreateLabel(@"", OakStatusBarFont(), NSTextAlignmentLeft, NSLineBreakByTruncatingTail);
		_scopeField.accessibilityLabel = @"Current Scope";
		[_scopeField setContentCompressionResistancePriority:NSLayoutPriorityDefaultLow forOrientation:NSLayoutConstraintOrientationHorizontal];

		if(@available(macOS 10.14, *))
			_scopeField.textColor = NSColor.secondaryLabelColor;

		NSDictionary* views = @{
			@"divider": _divider,
			@"gutter":  gutterSpacer,
			@"scope":   _scopeField,
		};

		OakAddAutoLayoutViewsToSuperview([views allValues], self);
		_gutterWidthConstraint = [gutterSpacer.widthAnchor constraintEqualToConstant:0];
		_gutterWidthConstraint.active = YES;
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[divider]|" options:0 metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[gutter]-(8)-[scope]-(8)-|" options:0 metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[gutter]|" options:0 metrics:nil views:views]];
		if(@available(macOS 26.0, *))
			[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-(0)-[scope(==16)]-(2)-[divider(==1)]|" options:0 metrics:nil views:views]];
		else
			[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-(3)-[scope(==16)]-(2)-[divider(==1)]|" options:0 metrics:nil views:views]];
	}
	return self;
}

- (void)setScopePath:(NSString*)scopePath
{
	if(_scopePath == scopePath || [_scopePath isEqualToString:scopePath])
		return;

	_scopePath = [scopePath copy];
	_scopeField.stringValue = _scopePath.length ? _scopePath : @"No Symbol";
	_scopeField.toolTip = _scopePath;
}

- (void)setGutterWidth:(CGFloat)gutterWidth
{
	if(_gutterWidth == gutterWidth)
		return;

	_gutterWidth = gutterWidth;
	_gutterWidthConstraint.constant = gutterWidth;
}
@end

@interface OakDocumentView () <NSAccessibilityGroup, NSFontChanging, NSMenuItemValidation, GutterViewDelegate, GutterViewColumnDataSource, GutterViewColumnDelegate, OTVStatusBarDelegate>
{
	GutterView* gutterView;
	NSMutableDictionary* gutterImages;

	NSScrollView* textScrollView;

	NSMutableArray* topAuxiliaryViews;
	NSMutableArray* bottomAuxiliaryViews;

	IBOutlet NSPanel* tabSizeSelectorPanel;
}
@property (nonatomic, readonly) OTVStatusBar* statusBar;
@property (nonatomic) OTVScopeBar* scopeBar;
@property (nonatomic) SymbolChooser* symbolChooser;
@property (nonatomic) NSArray* observedKeys;
- (void)updateScopeBarGutterWidth;
- (void)updateScopeBar;
- (void)updateStyle;
@end

@implementation OakDocumentView
- (GutterView*)gutterView
{
	return gutterView;
}

- (id)initWithFrame:(NSRect)aRect
{
	if(self = [super initWithFrame:aRect])
	{
		self.accessibilityRole  = NSAccessibilityGroupRole;
		self.accessibilityLabel = @"Editor";

		_textView = [[OakTextView alloc] initWithFrame:NSZeroRect];
		_textView.autoresizingMask = NSViewWidthSizable|NSViewHeightSizable;

		textScrollView = [[NSScrollView alloc] initWithFrame:NSZeroRect];
		textScrollView.hasVerticalScroller      = YES;
		textScrollView.verticalScrollElasticity = NSScrollElasticityAllowed;
		textScrollView.hasHorizontalScroller    = YES;
		textScrollView.autohidesScrollers       = YES;
		textScrollView.borderType               = NSNoBorder;
		textScrollView.documentView             = _textView;

		gutterView = [[GutterView alloc] initWithScrollView:textScrollView orientation:NSVerticalRuler];
		gutterView.partnerView = _textView;
		gutterView.delegate    = self;
		gutterView.clientView  = _textView;
		[gutterView insertColumnWithIdentifier:kBookmarksColumnIdentifier atPosition:0 dataSource:self delegate:self];
		[gutterView insertColumnWithIdentifier:kFoldingsColumnIdentifier atPosition:2 dataSource:self delegate:self];
		if([NSUserDefaults.standardUserDefaults boolForKey:@"DocumentView Disable Line Numbers"])
			[gutterView setVisibility:NO forColumnWithIdentifier:GVLineNumbersColumnIdentifier];

		textScrollView.hasVerticalRuler = YES;
		textScrollView.rulersVisible    = YES;
		textScrollView.verticalRulerView = gutterView;

		_statusBar = [[OTVStatusBar alloc] initWithFrame:NSZeroRect];
		_statusBar.delegate = self;
		_statusBar.target = self;
		_statusBar.navigationTarget = self.navigationTarget;

		self.scopeBar = [[OTVScopeBar alloc] initWithFrame:NSZeroRect];
		topAuxiliaryViews = [NSMutableArray new];
		bottomAuxiliaryViews = [NSMutableArray new];

		OakAddAutoLayoutViewsToSuperview(@[ textScrollView, _statusBar ], self);
		self.showsScopeBar = YES;
		OakSetupKeyViewLoop(@[ self, _textView, self.scopeBar, _statusBar ]);

		self.document = [OakDocument documentWithString:@"" fileType:@"text.plain" customName:@"placeholder"];

		self.observedKeys = @[ @"selectionString", @"symbol", @"recordingMacro", @"themeUUID" ];
		for(NSString* keyPath in self.observedKeys)
			[_textView addObserver:self forKeyPath:keyPath options:NSKeyValueObservingOptionInitial context:NULL];
	}
	return self;
}

- (BOOL)showsScopeBar
{
	return [topAuxiliaryViews containsObject:self.scopeBar];
}

- (void)setShowsScopeBar:(BOOL)showsScopeBar
{
	if(self.showsScopeBar == showsScopeBar)
		return;

	if(showsScopeBar)
	{
		[topAuxiliaryViews insertObject:self.scopeBar atIndex:0];
		OakAddAutoLayoutViewsToSuperview(@[ self.scopeBar ], self);
		if(self.document)
			[self updateScopeBar];
	}
	else
	{
		[topAuxiliaryViews removeObject:self.scopeBar];
		[self.scopeBar removeFromSuperview];
	}
	[self setNeedsUpdateConstraints:YES];
}

- (void)updateConstraints
{
	[self removeConstraints:[self constraints]];
	[super updateConstraints];
	[self updateScopeBarGutterWidth];

	NSMutableArray* stackedViews = [NSMutableArray array];
	[stackedViews addObjectsFromArray:topAuxiliaryViews];
	[stackedViews addObject:textScrollView];
	[stackedViews addObjectsFromArray:bottomAuxiliaryViews];

	if(_statusBar)
	{
		[stackedViews addObject:_statusBar];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[_statusBar]|" options:0 metrics:nil views:NSDictionaryOfVariableBindings(_statusBar)]];
	}

	[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[textScrollView(>=100)]|" options:0 metrics:nil views:NSDictionaryOfVariableBindings(textScrollView)]];
	[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[topView]" options:0 metrics:nil views:@{ @"topView": stackedViews[0] }]];
	[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:[bottomView]|" options:0 metrics:nil views:@{ @"bottomView": [stackedViews lastObject] }]];

	for(size_t i = 0; i < [stackedViews count]-1; ++i)
		[self addConstraint:[NSLayoutConstraint constraintWithItem:stackedViews[i] attribute:NSLayoutAttributeBottom relatedBy:NSLayoutRelationEqual toItem:stackedViews[i+1] attribute:NSLayoutAttributeTop multiplier:1 constant:0]];

	NSArray* array[] = { topAuxiliaryViews, bottomAuxiliaryViews };
	for(NSArray* views : array)
	{
		for(NSView* view in views)
			[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[view]|" options:0 metrics:nil views:NSDictionaryOfVariableBindings(view)]];
	}
}

- (void)setHideStatusBar:(BOOL)flag
{
	if(_hideStatusBar == flag)
		return;

	_hideStatusBar = flag;
	if(_hideStatusBar)
	{
		[_statusBar removeFromSuperview];
		_statusBar.delegate = nil;
		_statusBar.target = nil;
		_statusBar = nil;
	}
	else
	{
		_statusBar = [[OTVStatusBar alloc] initWithFrame:NSZeroRect];
		_statusBar.delegate = self;
		_statusBar.target = self;
		_statusBar.navigationTarget = self.navigationTarget;
		_statusBar.canNavigateBack = self.canNavigateBack;
		_statusBar.canNavigateForward = self.canNavigateForward;

		OakAddAutoLayoutViewsToSuperview(@[ _statusBar ], self);
	}
	[self setNeedsUpdateConstraints:YES];
}

- (void)setNavigationTarget:(id)newTarget
{
	_navigationTarget = newTarget;
	_statusBar.navigationTarget = newTarget;
}

- (void)setCanNavigateBack:(BOOL)flag
{
	_canNavigateBack = flag;
	_statusBar.canNavigateBack = flag;
}

- (void)setCanNavigateForward:(BOOL)flag
{
	_canNavigateForward = flag;
	_statusBar.canNavigateForward = flag;
}

- (CGFloat)lineHeight
{
	return round(std::min(1.5 * [_textView.font capHeight], [_textView.font ascender] - [_textView.font descender] + [_textView.font leading]));
}

- (NSImage*)gutterImage:(NSString*)aName
{
	id res = gutterImages[aName];
	if(!res)
	{
		gutterImages = gutterImages ?: [NSMutableDictionary new];

		NSImage* image = [aName hasPrefix:@"/"] ? [[NSImage alloc] initWithContentsOfFile:aName] : [NSImage imageNamed:aName inSameBundleAsClass:[self class]];
		if(!image && ![aName hasPrefix:@"/"] && ![aName hasSuffix:@" Template"])
			image = [NSImage imageNamed:[aName stringByAppendingString:@" Template"] inSameBundleAsClass:[self class]];

		if([aName hasPrefix:@"/"] && [[aName stringByDeletingPathExtension] hasSuffix:@" Template"])
			[image setTemplate:YES];

		if(image)
		{
			CGFloat imageWidth  = image.size.width;
			CGFloat imageHeight = image.size.height;

			CGFloat viewWidth   = [self widthForColumnWithIdentifier:nil];
			CGFloat viewHeight  = self.lineHeight;

			res = image = [image copy];

			if(imageWidth / imageHeight < viewWidth / viewHeight)
					image.size = NSMakeSize(round(viewHeight * imageWidth / imageHeight), viewHeight);
			else	image.size = NSMakeSize(viewWidth, round(viewWidth * imageHeight / imageWidth));
		}
		else
		{
			res = [NSNull null];
			NSLog(@"%s no image named ‘%@’", sel_getName(_cmd), aName);
		}

		gutterImages[aName] = res;
	}
	return res == [NSNull null] ? nil : res;
}

- (void)updateGutterViewFont:(id)sender
{
	CGFloat const scaleFactor = [NSUserDefaults.standardUserDefaults floatForKey:kUserDefaultsLineNumberScaleFactorKey] ?: 0.8;
	NSString* lineNumberFontName = [NSUserDefaults.standardUserDefaults stringForKey:kUserDefaultsLineNumberFontNameKey] ?: [_textView.font fontName];

	gutterImages = nil; // force image sizes to be recalculated
	gutterView.lineNumberFont = [NSFont fontWithName:lineNumberFontName size:round(scaleFactor * [_textView.font pointSize] * _textView.fontScaleFactor)];
	[gutterView reloadData:self];
	[self updateScopeBarGutterWidth];
}

- (IBAction)makeTextLarger:(id)sender
{
	_textView.fontScaleFactor += 0.1;
	[self updateGutterViewFont:self];
}

- (IBAction)makeTextSmaller:(id)sender
{
	if(_textView.fontScaleFactor > 0.1)
	{
		_textView.fontScaleFactor -= 0.1;
		[self updateGutterViewFont:self];
	}
}

- (IBAction)makeTextStandardSize:(id)sender
{
	_textView.fontScaleFactor = 1;
	[self updateGutterViewFont:self];
}

- (void)changeFont:(id)sender
{
	NSFont* defaultFont = [NSFont userFixedPitchFontOfSize:0];
	if(NSFont* newFont = [sender convertFont:_textView.font ?: defaultFont])
	{
		std::string fontName = [newFont.fontName isEqualToString:defaultFont.fontName] ? NULL_STR : to_s(newFont.fontName);
		settings_t::set(kSettingsFontNameKey, fontName);
		settings_t::set(kSettingsFontSizeKey, [newFont pointSize]);
		_textView.font = newFont;
		[self updateGutterViewFont:self];
	}
}

- (void)observeValueForKeyPath:(NSString*)aKeyPath ofObject:(id)observableController change:(NSDictionary*)changeDictionary context:(void*)userData
{
	if([aKeyPath isEqualToString:@"selectionString"])
	{
		NSString* str = [_textView valueForKey:@"selectionString"];
		[gutterView setHighlightedRange:to_s(str ?: @"1")];
		[_statusBar setSelectionString:str];
		_symbolChooser.selectionString = str;
	}
	else if([aKeyPath isEqualToString:@"symbol"])
	{
		_statusBar.symbolName = _textView.symbol;
		[self updateScopeBar];
	}
	else if([aKeyPath isEqualToString:@"recordingMacro"])
	{
		_statusBar.recordingMacro = _textView.isRecordingMacro;
	}
	else if([aKeyPath isEqualToString:@"fileType"])
	{
		_statusBar.fileType = self.document.fileType;
	}
	else if([aKeyPath isEqualToString:@"tabSize"])
	{
		_statusBar.tabSize = self.document.tabSize;
	}
	else if([aKeyPath isEqualToString:@"softTabs"])
	{
		_statusBar.softTabs = self.document.softTabs;
	}
	else if([aKeyPath isEqualToString:@"automaticallyDetectsIndentation"])
	{
		_statusBar.automaticallyDetectsIndentation = self.document.automaticallyDetectsIndentation;
	}
	else if([aKeyPath isEqualToString:@"themeUUID"])
	{
		//[self updateStyle];
	}
}

- (void)dealloc
{
	for(NSString* keyPath in self.observedKeys)
		[_textView removeObserver:self forKeyPath:keyPath];
	[NSNotificationCenter.defaultCenter removeObserver:self];

	self.document = nil;
	self.symbolChooser = nil;
}

- (void)updateScopeBar
{
	if(!self.showsScopeBar)
		return;

	text::selection_t sel(to_s(_textView.selectionString));
	text::pos_t caret = sel.last().max();

	NSMutableArray* scopeStack = [NSMutableArray array];
	[self.document enumerateCachedSymbolsUsingBlock:^(text::pos_t const& pos, NSString* symbol, NSUInteger lineIndent, BOOL* stop){
		if(caret < pos)
		{
			*stop = YES;
			return;
		}
		if([symbol isEqualToString:@"-"])
			return;

		NSUInteger symbolIndent = OTVSymbolTextIndent(symbol);
		NSUInteger indent       = std::max(symbolIndent, lineIndent);

		NSString* title = OTVSymbolTitle(symbol);
		if(title.length == 0)
			return;

		while(scopeStack.count)
		{
			NSNumber* lastIndent = [scopeStack.lastObject objectForKey:@"indent"];
			if(lastIndent.unsignedIntegerValue < indent)
				break;
			[scopeStack removeLastObject];
		}

		[scopeStack addObject:@{ @"indent": @(indent), @"title": title }];
	}];

	NSMutableArray* titles = [NSMutableArray arrayWithCapacity:scopeStack.count];
	for(NSDictionary* item in scopeStack)
		[titles addObject:item[@"title"]];

	self.scopeBar.scopePath = [titles componentsJoinedByString:@" > "];
	if(titles.count <= 1)
	{
		NSString* scope = [NSString stringWithCxxString:to_s([_textView scopeContext].right)];
		NSString* fileType = self.document.fileType ?: @"";
		BOOL oldStylePlist = [scope rangeOfString:@"source.plist"].location != NSNotFound || [fileType rangeOfString:@"source.plist"].location != NSNotFound;
		NSString* content = self.document.content ?: @"";
		NSString* plistScopePath = OTVPlistScopePath(content, caret.line, oldStylePlist);
		if(plistScopePath.length)
			self.scopeBar.scopePath = plistScopePath;
	}
	if(self.scopeBar.scopePath.length == 0)
		self.scopeBar.scopePath = OTVSymbolTitle(_textView.symbol);
}

- (void)updateScopeBarGutterWidth
{
	self.scopeBar.gutterWidth = (textScrollView.rulersVisible && textScrollView.hasVerticalRuler) ? gutterView.requiredThickness : 0;
}

- (void)setDocument:(OakDocument*)aDocument
{
	NSArray* const documentKeys = @[ @"fileType", @"tabSize", @"softTabs", @"automaticallyDetectsIndentation" ];

	OakDocument* oldDocument = self.document;
	if(oldDocument)
	{
		for(NSString* key in documentKeys)
			[oldDocument removeObserver:self forKeyPath:key];
		[NSNotificationCenter.defaultCenter removeObserver:self name:OakDocumentMarksDidChangeNotification object:oldDocument];
	}

	if(aDocument)
	{
		[aDocument loadModalForWindow:self.window completionHandler:nullptr];
		[aDocument reloadIndentationSettings];
	}

	if((_document = aDocument))
	{
		[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(documentMarksDidChange:) name:OakDocumentMarksDidChangeNotification object:self.document];
		for(NSString* key in documentKeys)
			[self.document addObserver:self forKeyPath:key options:NSKeyValueObservingOptionInitial context:nullptr];
	}

	[_textView setDocument:self.document];
	[gutterView reloadData:self];
	[self updateScopeBarGutterWidth];
	[self updateStyle];

	if(_symbolChooser)
	{
		_symbolChooser.TMDocument      = self.document;
		_symbolChooser.selectionString = _textView.selectionString;
	}

	if(oldDocument)
		[oldDocument close];
}

- (void)updateStyle
{
	if(theme_ptr theme = _textView.theme)
	{
		NSColor* editorBackgroundColor = [NSColor colorWithCGColor:theme->background(to_s(self.document.fileType))];
		[textScrollView setBackgroundColor:editorBackgroundColor];
		[textScrollView setScrollerKnobStyle:theme->is_dark() ? NSScrollerKnobStyleLight : NSScrollerKnobStyleDark];

		if(@available(macOS 10.14, *))
		{
			[_textView setIbeamCursor:NSCursor.IBeamCursor];
		}
		else
		{
			if(theme->is_dark())
			{
				NSImage* whiteIBeamImage = [NSImage imageNamed:@"IBeam white" inSameBundleAsClass:[self class]];		
				[whiteIBeamImage setSize:NSCursor.IBeamCursor.image.size];
				[_textView setIbeamCursor:[[NSCursor alloc] initWithImage:whiteIBeamImage hotSpot:NSMakePoint(4, 9)]];
			}
			else
			{
				[_textView setIbeamCursor:NSCursor.IBeamCursor];
			}
		}

		[self updateGutterViewFont:self]; // trigger update of gutter view’s line number font
		auto const& styles = theme->gutter_styles();

		gutterView.foregroundColor           = [NSColor colorWithCGColor:styles.foreground];
		gutterView.backgroundColor           = editorBackgroundColor;
		gutterView.iconColor                 = [NSColor colorWithCGColor:styles.icons];
		gutterView.iconHoverColor            = [NSColor colorWithCGColor:styles.iconsHover];
		gutterView.iconPressedColor          = [NSColor colorWithCGColor:styles.iconsPressed];
		gutterView.selectionForegroundColor  = [NSColor colorWithCGColor:styles.selectionForeground];
		NSColor* selectionColor              = [NSColor colorWithCGColor:theme->styles_for_scope(to_s(self.document.fileType)).selection()];
		gutterView.selectionBackgroundColor  = [selectionColor colorWithAlphaComponent:kCurrentLineHighlightAlphaMultiplier * selectionColor.alphaComponent];
		gutterView.selectionIconColor        = [NSColor colorWithCGColor:styles.selectionIcons];
		gutterView.selectionIconHoverColor   = [NSColor colorWithCGColor:styles.selectionIconsHover];
		gutterView.selectionIconPressedColor = [NSColor colorWithCGColor:styles.selectionIconsPressed];
		gutterView.selectionBorderColor      = [NSColor colorWithCGColor:styles.selectionBorder];
		[gutterView setNeedsDisplay:YES];
	}
}

- (IBAction)toggleLineNumbers:(id)sender
{
	BOOL isVisibleFlag = ![gutterView visibilityForColumnWithIdentifier:GVLineNumbersColumnIdentifier];
	[gutterView setVisibility:isVisibleFlag forColumnWithIdentifier:GVLineNumbersColumnIdentifier];
	if(isVisibleFlag)
			[NSUserDefaults.standardUserDefaults removeObjectForKey:@"DocumentView Disable Line Numbers"];
	else	[NSUserDefaults.standardUserDefaults setObject:@YES forKey:@"DocumentView Disable Line Numbers"];
	[self updateScopeBarGutterWidth];
}

- (BOOL)validateMenuItem:(NSMenuItem*)aMenuItem
{
	if([aMenuItem action] == @selector(toggleLineNumbers:))
		[aMenuItem setTitle:[gutterView visibilityForColumnWithIdentifier:GVLineNumbersColumnIdentifier] ? @"Hide Line Numbers" : @"Show Line Numbers"];
	else if([aMenuItem action] == @selector(takeTabSizeFrom:))
		[aMenuItem setState:_textView.tabSize == [aMenuItem tag] ? NSControlStateValueOn : NSControlStateValueOff];
	else if([aMenuItem action] == @selector(showTabSizeSelectorPanel:))
	{
		static NSInteger const predefined[] = { 2, 3, 4, 8 };
		if(oak::contains(std::begin(predefined), std::end(predefined), _textView.tabSize))
		{
			[aMenuItem setTitle:@"Other…"];
			[aMenuItem setState:NSControlStateValueOff];
		}
		else
		{
			[aMenuItem setDynamicTitle:[NSString stringWithFormat:@"Other (%zd)…", _textView.tabSize]];
			[aMenuItem setState:NSControlStateValueOn];
		}
	}
	else if([aMenuItem action] == @selector(setIndentAutomatically:))
		[aMenuItem setState:self.document.automaticallyDetectsIndentation ? NSControlStateValueOn : NSControlStateValueOff];
	else if([aMenuItem action] == @selector(setIndentWithTabs:))
		[aMenuItem setState:!self.document.automaticallyDetectsIndentation && !_textView.softTabs ? NSControlStateValueOn : NSControlStateValueOff];
	else if([aMenuItem action] == @selector(setIndentWithSpaces:))
		[aMenuItem setState:!self.document.automaticallyDetectsIndentation && _textView.softTabs ? NSControlStateValueOn : NSControlStateValueOff];
	else if([aMenuItem action] == @selector(takeGrammarUUIDFrom:))
	{
		NSString* uuidString = [aMenuItem representedObject];
		if(bundles::item_ptr bundleItem = bundles::lookup(to_s(uuidString)))
		{
			bool selectedGrammar = to_s(self.document.fileType) == bundleItem->value_for_field(bundles::kFieldGrammarScope);
			[aMenuItem setState:selectedGrammar ? NSControlStateValueOn : NSControlStateValueOff];
		}
	}
	return YES;
}

// ===================
// = Auxiliary Views =
// ===================

- (void)addAuxiliaryView:(NSView*)aView atEdge:(NSRectEdge)anEdge
{
	topAuxiliaryViews    = topAuxiliaryViews    ?: [NSMutableArray new];
	bottomAuxiliaryViews = bottomAuxiliaryViews ?: [NSMutableArray new];
	if(anEdge == NSMinYEdge)
			[bottomAuxiliaryViews addObject:aView];
	else	[topAuxiliaryViews addObject:aView];
	OakAddAutoLayoutViewsToSuperview(@[ aView ], self);
	[self setNeedsUpdateConstraints:YES];
}

- (void)removeAuxiliaryView:(NSView*)aView
{
	if([topAuxiliaryViews containsObject:aView])
		[topAuxiliaryViews removeObject:aView];
	else if([bottomAuxiliaryViews containsObject:aView])
		[bottomAuxiliaryViews removeObject:aView];
	else
		return;
	[aView removeFromSuperview];
	[self setNeedsUpdateConstraints:YES];
}

// ======================
// = Pasteboard History =
// ======================

- (void)showClipboardHistory:(id)sender
{
	OakPasteboardChooser* chooser = [OakPasteboardChooser sharedChooserForPasteboard:OakPasteboard.generalPasteboard];
	chooser.action = @selector(paste:);
	[chooser showWindowRelativeToFrame:[self.window convertRectToScreen:[_textView convertRect:[_textView visibleRect] toView:nil]]];
}

- (void)showFindHistory:(id)sender
{
	OakPasteboardChooser* chooser = [OakPasteboardChooser sharedChooserForPasteboard:OakPasteboard.findPasteboard];
	chooser.action          = @selector(findNext:);
	chooser.alternateAction = @selector(orderFrontFindPanelForProject:);
	[chooser showWindowRelativeToFrame:[self.window convertRectToScreen:[_textView convertRect:[_textView visibleRect] toView:nil]]];
}

// ==================
// = Symbol Chooser =
// ==================

- (void)selectAndCenter:(NSString*)aSelectionString
{
	_textView.selectionString = aSelectionString;
	[_textView centerSelectionInVisibleArea:self];
}

- (void)setSymbolChooser:(SymbolChooser*)aSymbolChooser
{
	if(_symbolChooser == aSymbolChooser)
		return;

	if(_symbolChooser)
	{
		[NSNotificationCenter.defaultCenter removeObserver:self name:NSWindowWillCloseNotification object:_symbolChooser.window];

		_symbolChooser.target     = nil;
		_symbolChooser.TMDocument = nil;
	}

	if((_symbolChooser = aSymbolChooser))
	{
		_symbolChooser.target          = self;
		_symbolChooser.action          = @selector(symbolChooserDidSelectItems:);
		_symbolChooser.filterString    = @"";
		_symbolChooser.TMDocument      = self.document;
		_symbolChooser.selectionString = _textView.selectionString;

		[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(symbolChooserWillClose:) name:NSWindowWillCloseNotification object:_symbolChooser.window];
	}
}

- (void)symbolChooserWillClose:(NSNotification*)aNotification
{
	self.symbolChooser = nil;
}

- (IBAction)showSymbolChooser:(id)sender
{
	self.symbolChooser = SymbolChooser.sharedInstance;
	[self.symbolChooser showWindowRelativeToFrame:[self.window convertRectToScreen:[_textView convertRect:[_textView visibleRect] toView:nil]]];
}

- (void)symbolChooserDidSelectItems:(id)sender
{
	for(id item in [sender selectedItems])
		[self selectAndCenter:[item selectionString]];
}

// =======================
// = Status bar delegate =
// =======================

- (void)takeGrammarUUIDFrom:(id)sender
{
	if(bundles::item_ptr item = bundles::lookup(to_s([sender representedObject])))
		[_textView performBundleItem:item];
}

- (void)goToSymbol:(id)sender
{
	[self selectAndCenter:[sender representedObject]];
}

- (void)showSymbolSelector:(NSPopUpButton*)symbolPopUp
{
	NSMenu* symbolMenu = symbolPopUp.menu;
	[symbolMenu removeAllItems];

	text::selection_t sel(to_s(_textView.selectionString));
	text::pos_t caret = sel.last().max();

	__block NSInteger index = 0;
	[self.document enumerateSymbolsUsingBlock:^(text::pos_t const& pos, NSString* symbol){
		if([symbol isEqualToString:@"-"])
		{
			[symbolMenu addItem:[NSMenuItem separatorItem]];
		}
		else
		{
			NSUInteger indent = 0;
			while(indent < symbol.length && [symbol characterAtIndex:indent] == 0x2003) // Em-space
				++indent;

			NSMenuItem* item = [symbolMenu addItemWithTitle:[symbol substringFromIndex:indent] action:@selector(goToSymbol:) keyEquivalent:@""];
			[item setIndentationLevel:indent];
			[item setTarget:self];
			[item setRepresentedObject:to_ns(pos)];
		}

		if(pos <= caret)
			++index;
	}];

	if(symbolMenu.numberOfItems == 0)
		[symbolMenu addItemWithTitle:@"No symbols to show for current document." action:@selector(nop:) keyEquivalent:@""];

	[symbolPopUp selectItemAtIndex:(index ? index-1 : 0)];
}

- (void)showBundlesMenu:(id)sender
{
	if(!self.statusBar)
		return NSBeep();

	[NSApp sendAction:_cmd to:self.statusBar from:self];
}

- (void)showBundleItemSelector:(NSPopUpButton*)bundleItemsPopUp
{
	NSMenu* bundleItemsMenu = bundleItemsPopUp.menu;
	[bundleItemsMenu removeAllItems];

	std::multimap<std::string, bundles::item_ptr, text::less_t> ordered;
	for(auto item : bundles::query(bundles::kFieldAny, NULL_STR, scope::wildcard, bundles::kItemTypeBundle))
		ordered.emplace(item->name(), item);

	NSMenuItem* selectedItem = nil;
	for(auto pair : ordered)
	{
		bool selectedGrammar = false;
		for(auto item : bundles::query(bundles::kFieldGrammarScope, to_s(self.document.fileType), scope::wildcard, bundles::kItemTypeGrammar, pair.second->uuid(), true, true))
			selectedGrammar = true;
		if((!selectedGrammar && pair.second->hidden_from_user()) || pair.second->menu().empty())
			continue;

		NSMenuItem* menuItem = [bundleItemsMenu addItemWithTitle:[NSString stringWithCxxString:pair.first] action:NULL keyEquivalent:@""];
		menuItem.submenu = [[NSMenu alloc] initWithTitle:[NSString stringWithCxxString:pair.second->uuid()]];
		menuItem.submenu.delegate = BundleMenuDelegate.sharedInstance;

		if(selectedGrammar)
		{
			[menuItem setState:NSControlStateValueOn];
			selectedItem = menuItem;
		}
	}

	if(ordered.empty())
		[bundleItemsMenu addItemWithTitle:@"No Bundles Loaded" action:@selector(nop:) keyEquivalent:@""];

	if(selectedItem)
		[bundleItemsPopUp selectItem:selectedItem];
}

- (NSUInteger)tabSize
{
	return _textView.tabSize;
}

- (void)setTabSize:(NSUInteger)newTabSize
{
	_textView.tabSize = newTabSize;
	settings_t::set(kSettingsTabSizeKey, (size_t)newTabSize, to_s(self.document.fileType));
}

- (IBAction)takeTabSizeFrom:(id)sender
{
	ASSERT([sender respondsToSelector:@selector(tag)]);
	if([sender tag] > 0)
		self.tabSize = [sender tag];
}

- (IBAction)setIndentWithSpaces:(id)sender
{
	settings_t::set(kSettingsSoftTabsKey, true, to_s(self.document.fileType));
	[self.document reloadIndentationSettings];
}

- (IBAction)setIndentWithTabs:(id)sender
{
	settings_t::set(kSettingsSoftTabsKey, false, to_s(self.document.fileType));
	[self.document reloadIndentationSettings];
}

- (IBAction)setIndentAutomatically:(id)sender
{
	settings_t::set(kSettingsSoftTabsKey, "auto", to_s(self.document.fileType));
	[self.document reloadIndentationSettings];
}

- (IBAction)showTabSizeSelectorPanel:(id)sender
{
	if(!tabSizeSelectorPanel)
		[[NSBundle bundleForClass:[self class]] loadNibNamed:@"TabSizeSetting" owner:self topLevelObjects:NULL];
	[tabSizeSelectorPanel makeKeyAndOrderFront:self];
}

- (void)toggleMacroRecording:(id)sender    { [_textView toggleMacroRecording:sender]; }

// =============================
// = GutterView Delegate Proxy =
// =============================

- (GVLineRecord)lineRecordForPosition:(CGFloat)yPos                              { return [_textView lineRecordForPosition:yPos];               }
- (GVLineRecord)lineFragmentForLine:(NSUInteger)aLine column:(NSUInteger)aColumn { return [_textView lineFragmentForLine:aLine column:aColumn]; }
- (NSColor*)backgroundColorForLineRecord:(GVLineRecord)lineRecord                { return [_textView backgroundColorForLine:lineRecord.lineNumber]; }
- (void)gutterView:(GutterView*)gutterView drawBackgroundDecorationsInRect:(NSRect)dirtyRect
{
	[_textView drawDiagnosticBackgroundInGutterRect:dirtyRect bounds:gutterView.bounds];
}

// =========================
// = GutterView DataSource =
// =========================

- (CGFloat)widthForColumnWithIdentifier:(id)columnIdentifier
{
	return floor((self.lineHeight-1) / 2) * 2 + 1;
}

- (NSImage*)imageForLine:(NSUInteger)lineNumber inColumnWithIdentifier:(id)columnIdentifier state:(GutterViewRowState)rowState
{
	if([columnIdentifier isEqualToString:kBookmarksColumnIdentifier])
	{
		__block std::map<size_t, NSString*> gutterImageName;

		[self.document enumerateBookmarksAtLine:lineNumber block:^(text::pos_t const& pos, NSString* type, NSString* payload){
			if(payload.length != 0)
				gutterImageName.emplace(0, type);
			else if([type isEqualToString:OakDocumentBookmarkIdentifier])
				gutterImageName.emplace(1, rowState != GutterViewRowStateRegular ? @"Bookmark Hover Remove Template" : @"Bookmark Template");
			else if(rowState == GutterViewRowStateRegular)
				gutterImageName.emplace(2, type);
		}];

		if(rowState != GutterViewRowStateRegular)
			gutterImageName.emplace(3, @"Bookmark Hover Add Template");

		if(!gutterImageName.empty())
			return [self gutterImage:gutterImageName.begin()->second];
	}
	else if([columnIdentifier isEqualToString:kFoldingsColumnIdentifier])
	{
		switch([_textView foldingStateForLine:lineNumber])
		{
			case kFoldingNone:      return nil;
			case kFoldingTop:       return [self gutterImage:rowState == GutterViewRowStateRegular ? @"Folding Top Template"       : @"Folding Top Hover Template"];
			case kFoldingCollapsed: return [self gutterImage:rowState == GutterViewRowStateRegular ? @"Folding Collapsed Template" : @"Folding Collapsed Hover Template"];
			case kFoldingBottom:    return [self gutterImage:rowState == GutterViewRowStateRegular ? @"Folding Bottom Template"    : @"Folding Bottom Hover Template"];
		}
	}
	return nil;
}

// =============================
// = Bookmark Submenu Delegate =
// =============================

- (void)takeBookmarkFrom:(id)sender
{
	if([sender respondsToSelector:@selector(representedObject)])
		[self selectAndCenter:[sender representedObject]];
}

- (void)updateBookmarksMenu:(NSMenu*)aMenu
{
	[self.document enumerateBookmarksUsingBlock:^(text::pos_t const& pos, NSString* excerpt){
		NSString* prefix = to_ns(text::pad(pos.line+1, 4) + ": ");
		NSMenuItem* item = [aMenu addItemWithTitle:[prefix stringByAppendingString:excerpt] action:@selector(takeBookmarkFrom:) keyEquivalent:@""];
		[item setRepresentedObject:to_ns(pos)];
	}];

	BOOL hasBookmarks = aMenu.numberOfItems;
	if(hasBookmarks)
		[aMenu addItem:[NSMenuItem separatorItem]];
	[aMenu addItemWithTitle:@"Clear Bookmarks" action:hasBookmarks ? @selector(clearAllBookmarks:) : @selector(nop:) keyEquivalent:@""];
}

// =======================
// = GutterView Delegate =
// =======================

- (void)userDidClickColumnWithIdentifier:(id)columnIdentifier atLine:(NSUInteger)lineNumber
{
	if([columnIdentifier isEqualToString:kBookmarksColumnIdentifier])
	{
		__block std::vector<text::pos_t> bookmarks;
		__block NSMutableArray* content = [NSMutableArray array];

		[self.document enumerateBookmarksAtLine:lineNumber block:^(text::pos_t const& pos, NSString* type, NSString* payload){
			if(payload.length != 0)
				[content addObject:payload];
			else if([type isEqualToString:OakDocumentBookmarkIdentifier])
				bookmarks.push_back(pos);
		}];

		if(content.count == 0)
		{
			if(bookmarks.empty())
					[self.document setMarkOfType:OakDocumentBookmarkIdentifier atPosition:text::pos_t(lineNumber, 0) content:nil];
			else	[self.document removeMarkOfType:OakDocumentBookmarkIdentifier atPosition:bookmarks.front()];
		}
		else
		{
			NSView* popoverContainerView = [[NSView alloc] initWithFrame:NSZeroRect];

			NSTextField* textField = OakCreateLabel([content componentsJoinedByString:@"\n"]);
			OakAddAutoLayoutViewsToSuperview(@[ textField ], popoverContainerView);

			NSDictionary* views = NSDictionaryOfVariableBindings(textField);
			[popoverContainerView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(5)-[textField]-(5)-|" options:0 metrics:0 views:views]];
			[popoverContainerView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-(10)-[textField]-(10)-|" options:0 metrics:0 views:views]];

			NSViewController* viewController = [NSViewController new];
			viewController.view = popoverContainerView;

			NSPopover* popover = [NSPopover new];
			popover.behavior = NSPopoverBehaviorTransient;
			popover.contentViewController = viewController;

			GVLineRecord record = [self lineFragmentForLine:lineNumber column:0];
			NSRect rect = NSMakeRect(0, record.firstY, [self widthForColumnWithIdentifier:columnIdentifier], record.lastY - record.firstY);
			[popover showRelativeToRect:rect ofView:gutterView preferredEdge:NSMaxXEdge];
		}
	}
	else if([columnIdentifier isEqualToString:kFoldingsColumnIdentifier])
	{
		[_textView toggleFoldingAtLine:lineNumber recursive:OakIsAlternateKeyOrMouseEvent()];
		[NSNotificationCenter.defaultCenter postNotificationName:GVColumnDataSourceDidChange object:self];
	}
}

- (void)clearAllBookmarks:(id)sender
{
	[self.document removeAllMarksOfType:OakDocumentBookmarkIdentifier];
}

- (void)documentMarksDidChange:(NSNotification*)aNotification
{
	[NSNotificationCenter.defaultCenter postNotificationName:GVColumnDataSourceDidChange object:self];
}

// ============
// = Printing =
// ============

- (void)printDocument:(id)sender
{
	[self.document runPrintOperationModalForWindow:self.window fontName:_textView.font.fontName];
}
@end
