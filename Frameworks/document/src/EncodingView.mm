#import "EncodingView.h"
#import <OakFoundation/src/NSString Additions.h>
#import <OakAppKit/src/OakEncodingPopUpButton.h>
#import <OakAppKit/src/OakUIConstructionFunctions.h>
#import <text/src/hexdump.h>
#import <text/src/utf8.h>
#import <text/src/transcode.h>
#import <oak/oak.h>
#import <oak/debug.h>
#import <ns/src/ns.h>

template <typename _InputIter>
size_t newline_size (_InputIter first, _InputIter const& last)
{
	for(auto str : { "\r\n", "\n", "\r" })
	{
		if(oak::has_prefix(first, last, str, str + strlen(str)))
			return strlen(str);
	}
	return 0;
}

static void append (NSMutableAttributedString* dst, char const* first, char const* last, NSDictionary* styles)
{
	NSString* str = [NSString stringWithUTF8String:first length:last - first] ?: @"\uFFFD";
	[dst appendAttributedString:[[NSAttributedString alloc] initWithString:str attributes:styles]];
}

static NSAttributedString* convert_and_highlight (char const* first, char const* last, std::string const& encodeFrom = "UTF-8", std::string const& encodeTo = "UTF-8", bool* success = nullptr)
{
	std::set<ptrdiff_t> offsets;
	auto lastPos = first;
	for(auto it = first; it != last; ++it)
	{
		if(*it > 0x7F)
		{
			if(++lastPos != it)
				offsets.insert(std::distance(first, it));
			lastPos = it;
		}
	}

	text::transcode_t transcode(encodeFrom, encodeTo);
	if(!transcode)
		return nil;

	std::string dst;

	std::set<size_t> decodedOffsets;
	size_t from = 0;
	for(size_t to : offsets)
	{
		transcode(first + from, first + to, back_inserter(dst));
		decodedOffsets.insert(dst.size());
		from = to;
	}
	transcode(transcode(first + from, last, back_inserter(dst)));

	if(success)
		*success = transcode.invalid_count() == 0;

	NSMutableAttributedString* output = [[NSMutableAttributedString alloc] init];

	NSDictionary* regularStyle = @{
		NSFontAttributeName:            [NSFont userFixedPitchFontOfSize:0],
		NSForegroundColorAttributeName: [NSColor grayColor],
	};
	NSDictionary* lineHighlightStyle = @{
		NSFontAttributeName:            [NSFont userFixedPitchFontOfSize:0],
		NSForegroundColorAttributeName: [NSColor grayColor],
		NSBackgroundColorAttributeName: [NSColor colorWithCalibratedWhite:0.9 alpha:1],
	};
	NSDictionary* characterHighlightStyle = @{
		NSFontAttributeName:            [NSFont userFixedPitchFontOfSize:0],
		NSBackgroundColorAttributeName: [NSColor colorWithCalibratedWhite:0.9 alpha:1],
	};

	size_t bol = 0;
	auto offset = decodedOffsets.begin();
	for(size_t eol = 0; eol < dst.size(); ++eol)
	{
		static std::string const newlines[] = { "\r\n", "\n", "\r" };

		auto it = std::find_if(std::begin(newlines), std::end(newlines), [&](std::string const& str){ return oak::has_prefix(dst.begin() + eol, dst.end(), str.begin(), str.end()); });
		if(it == std::end(newlines))
			continue;

		size_t crlf = it->size();
		if(offset != decodedOffsets.end() && *offset < eol)
		{
			while(offset != decodedOffsets.end() && *offset < eol)
			{
				if(*offset < bol)
				{
					++offset;
					continue;
				}

				append(output, dst.data() + bol, dst.data() + *offset, lineHighlightStyle);

				bol = *offset;
				while(bol != dst.size() && dst[bol] > 0x7F)
					++bol;

				append(output, dst.data() + *offset, dst.data() + bol, characterHighlightStyle);

				++offset;
			}
			append(output, dst.data() + bol, dst.data() + eol + crlf, lineHighlightStyle);
		}
		else
		{
			append(output, dst.data() + bol, dst.data() + eol + crlf, regularStyle);
		}

		bol = eol + crlf;
		eol += crlf - 1;
	}

	if(bol < dst.size())
		append(output, dst.data() + bol, dst.data() + dst.size(), regularStyle);

	return output;
}

static NSTextView* MyCreateTextView ()
{
	NSTextView* res = [[NSTextView alloc] initWithFrame:NSZeroRect];
	[res setVerticallyResizable:YES];
	[res setHorizontallyResizable:YES];
	[res setAutoresizingMask:NSViewWidthSizable|NSViewHeightSizable];
	[[res textContainer] setWidthTracksTextView:NO];
	[[res textContainer] setContainerSize:NSMakeSize(CGFLOAT_MAX, CGFLOAT_MAX)];
	return res;
}

@interface EncodingContentView : NSView
@property (nonatomic) id delegate;
@end

@implementation EncodingContentView
- (NSSize)intrinsicContentSize
{
	return NSMakeSize(NSViewNoIntrinsicMetric, NSViewNoIntrinsicMetric);
}

- (void)updateConstraints
{
	[super updateConstraints];
	[self.delegate updateConstraints];
}
@end

@interface EncodingWindowController () <NSWindowDelegate, NSTextViewDelegate>
{
	NSData* _data;
}
@property (nonatomic) NSObjectController* objectController;
@property (nonatomic) NSTextField* title;
@property (nonatomic) NSTextField* explanation;
@property (nonatomic) NSTextField* label;
@property (nonatomic) OakEncodingPopUpButton* popUpButton;
@property (nonatomic) NSScrollView* scrollView;
@property (nonatomic) NSTextView* textView;
@property (nonatomic) NSButton* learnCheckBox;
@property (nonatomic) NSButton* openButton;
@property (nonatomic) NSButton* cancelButton;
@property (nonatomic) EncodingContentView* contentView;
@property (nonatomic) NSMutableArray* myConstraints;
@end

@implementation EncodingWindowController
- (instancetype)initWithData:(NSData*)data
{
	if(self = [super initWithWindow:[[NSWindow alloc] initWithContentRect:NSZeroRect styleMask:(NSWindowStyleMaskTitled|NSWindowStyleMaskClosable|NSWindowStyleMaskResizable|NSWindowStyleMaskMiniaturizable) backing:NSBackingStoreBuffered defer:NO]])
	{
		_data            = data;
		_encoding        = @"ISO-8859-1";
		_displayName     = @"untitled";
		_trainClassifier = YES;

		self.objectController = [[NSObjectController alloc] initWithContent:self];

		self.title         = OakCreateLabel(@"Unknown Encoding", [NSFont boldSystemFontOfSize:0]);
		self.explanation   = OakCreateLabel();
		self.label         = OakCreateLabel(@"Encoding:");
		self.popUpButton   = [[OakEncodingPopUpButton alloc] initWithFrame:NSZeroRect pullsDown:NO];
		self.scrollView    = [[NSScrollView alloc] initWithFrame:NSZeroRect];
		self.textView      = MyCreateTextView();
		self.learnCheckBox = OakCreateCheckBox(@"Use document for training encoding classifier");
		self.openButton    = OakCreateButton(@"Open");
		self.cancelButton  = OakCreateButton(@"Cancel");

		[self.label setContentHuggingPriority:NSLayoutPriorityRequired forOrientation:NSLayoutConstraintOrientationHorizontal];
		[self.popUpButton setContentHuggingPriority:NSLayoutPriorityDefaultLow forOrientation:NSLayoutConstraintOrientationHorizontal];

		self.scrollView.hasVerticalScroller   = YES;
		self.scrollView.hasHorizontalScroller = YES;
		self.scrollView.autohidesScrollers    = YES;
		self.scrollView.borderType            = NSBezelBorder;
		self.scrollView.documentView          = self.textView;

		self.textView.editable          = NO;
		self.textView.delegate          = self;
		self.openButton.action          = @selector(performOpenDocument:);
		self.cancelButton.action        = @selector(performCancelOperation:);
		self.cancelButton.keyEquivalent = @"\e";

		EncodingContentView* contentView = [[EncodingContentView alloc] initWithFrame:NSZeroRect];
		[contentView setDelegate:self];
		[contentView setAutoresizingMask:NSViewWidthSizable|NSViewHeightSizable];
		self.contentView = contentView;

		OakAddAutoLayoutViewsToSuperview([[self allViews] allValues], contentView);

		[self.window.contentView addSubview:contentView];
		self.window.defaultButtonCell = self.openButton.cell;
		self.window.delegate = self;

		[self.popUpButton   bind:@"encoding"      toObject:_objectController withKeyPath:@"content.encoding"           options:nil];
		[self.learnCheckBox bind:NSValueBinding   toObject:_objectController withKeyPath:@"content.trainClassifier"    options:nil];
		[self.openButton    bind:NSEnabledBinding toObject:_objectController withKeyPath:@"content.acceptableEncoding" options:nil];

		[self updateTextView];
	}
	return self;
}

- (void)beginSheetModalForWindow:(NSWindow*)aWindow completionHandler:(void(^)(NSModalResponse))callback
{
	[self.window layoutIfNeeded];
	[aWindow beginSheet:self.window completionHandler:callback];
}

- (BOOL)textView:(NSTextView*)aTextView doCommandBySelector:(SEL)aSelector
{
	BOOL res = aSelector == @selector(insertNewline:) && !aTextView.editable && self.window.defaultButtonCell;
	if(res)
		[self.window.defaultButtonCell performClick:self];
	return res;
}

- (NSDictionary*)allViews
{
	return @{
		@"title":       self.title,
		@"explanation": self.explanation,
		@"label":       self.label,
		@"popUp":       self.popUpButton,
		@"textView":    self.scrollView,
		@"learn":       self.learnCheckBox,
		@"open":        self.openButton,
		@"cancel":      self.cancelButton
	};
}

#ifndef CONSTRAINT
#define CONSTRAINT(str, align) [_myConstraints addObjectsFromArray:[NSLayoutConstraint constraintsWithVisualFormat:str options:align metrics:nil views:views]]
#endif

- (void)updateConstraints
{
	if(_myConstraints)
		[self.contentView removeConstraints:_myConstraints];
	_myConstraints = [NSMutableArray array];

	NSDictionary* views = [self allViews];

	CONSTRAINT(@"H:|-[title]-|",                          NSLayoutFormatAlignAllBaseline);
	CONSTRAINT(@"H:|-[explanation]-|",                    NSLayoutFormatAlignAllBaseline);
	CONSTRAINT(@"H:|-[label]-[popUp]-|",                  NSLayoutFormatAlignAllBaseline);
	CONSTRAINT(@"H:|-[textView(>=100)]-|",                0);
	CONSTRAINT(@"H:|-[learn]-|",                          0);
	CONSTRAINT(@"H:[cancel]-[open]-|",                    NSLayoutFormatAlignAllBaseline);
	CONSTRAINT(@"V:|-[title]-[explanation]-[popUp]-[textView(>=100)]-[learn]-[open]-|", NSLayoutFormatAlignAllRight);

	[self.contentView addConstraints:_myConstraints];
}

- (void)setDisplayName:(NSString*)aString
{
	_displayName = aString;
	self.explanation.stringValue = [NSString stringWithFormat:@"The file “%@” contains characters with unknown encoding.\nPlease select the encoding which should be used to open the file.\nThe contents of the file is shown below with the relevant lines highlighted.\nBefore proceeding, check that the chosen encoding makes the preview look correct.", _displayName];
}

- (void)updateTextView
{
	bool couldConvert = true;
	char const* bytes = (char const*)_data.bytes;
	[[self.textView textStorage] setAttributedString:convert_and_highlight(bytes, bytes + MIN(_data.length, 256*1024), to_s(self.encodingNoBOM), "UTF-8", &couldConvert)];
	self.acceptableEncoding = couldConvert;
}

- (void)setEncoding:(NSString*)anEncoding
{
	if([_encoding isEqualToString:anEncoding])
		return;
	_encoding = anEncoding;
	[self updateTextView];
}

- (NSString*)encodingNoBOM
{
	return [_encoding stringByReplacingOccurrencesOfString:@"//BOM" withString:@""];
}

- (void)cleanup
{
	self.contentView.delegate     = nil;
	self.objectController.content = nil;
}

- (IBAction)performOpenDocument:(id)sender
{
	[self.window.sheetParent endSheet:self.window returnCode:NSModalResponseOK];
	[self cleanup];
}

- (IBAction)performCancelOperation:(id)sender
{
	[self.window.sheetParent endSheet:self.window returnCode:NSModalResponseCancel];
	[self cleanup];
}
@end
