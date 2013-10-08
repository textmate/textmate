#import "EncodingView.h"
#import <OakFoundation/NSString Additions.h>
#import <OakAppKit/OakEncodingPopUpButton.h>
#import <OakAppKit/OakUIConstructionFunctions.h>
#import <text/hexdump.h>
#import <text/utf8.h>
#import <oak/oak.h>
#import <oak/debug.h>
#import <ns/ns.h>

static void grow (char*& outBuf, size_t& outBufSize, std::string& dst, size_t& copied)
{
	dst.resize(dst.size() * 3 / 2);
	outBuf     = &dst.front() + copied;
	outBufSize = dst.size() - copied;
}

static bool convert_range (iconv_t cd, char const* src, size_t from, size_t to, std::string& dst, size_t& copied)
{
	char* outBuf      = &dst.front() + copied;
	size_t outBufSize = dst.size() - copied;

	char* inBuf       = (char*)src + from;
	size_t inBufSize  = to - from;

	bool res = true;
	while(inBufSize)
	{
		size_t prevOutBufSize = outBufSize;
		size_t rc = iconv(cd, &inBuf, &inBufSize, &outBuf, &outBufSize);
		copied += prevOutBufSize - outBufSize;

		if(rc == (size_t)-1)
		{
			if(errno == EINVAL || errno == EILSEQ)
			{
				res = false;

				++inBuf;
				--inBufSize;

				while(iconv(cd, nullptr, nullptr, &outBuf, &outBufSize) == (size_t)-1 && errno == E2BIG)
					grow(outBuf, outBufSize, dst, copied);

				static char const kReplacementChar[]     = "\uFFFD";
				static char const kReplacementCharLength = strlen(kReplacementChar);
				if(outBufSize < kReplacementCharLength)
					grow(outBuf, outBufSize, dst, copied);

				memcpy(outBuf, kReplacementChar, kReplacementCharLength);
				outBuf += kReplacementCharLength;
				outBufSize -= kReplacementCharLength;
				copied += kReplacementCharLength;
			}
			else if(errno == E2BIG)
			{
				grow(outBuf, outBufSize, dst, copied);
			}
			else
			{
				res = false;
				fprintf(stderr, "iconv: %s after %zu bytes, %zu bytes left (unexpected)\n", strerror(errno), copied, inBufSize);
				break;
			}
		}
	}
	return res;
}

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

	iconv_t cd = iconv_open(encodeTo.c_str(), encodeFrom.c_str());
	if(cd == (iconv_t)(-1))
		return nil;

	std::string dst(std::distance(first, last), ' ');
	size_t copied = 0;

	bool couldConvert = true;
	std::set<size_t> decodedOffsets;
	size_t from = 0;
	for(size_t to : offsets)
	{
		couldConvert = convert_range(cd, first, from, to, dst, copied) && couldConvert;
		decodedOffsets.insert(copied);
		from = to;
	}
	couldConvert = convert_range(cd, first, from, std::distance(first, last), dst, copied) && couldConvert;
	dst.resize(copied);

	if(success)
		*success = couldConvert;

	iconv_close(cd);

	NSMutableAttributedString* output = [[NSMutableAttributedString alloc] init];

	NSDictionary* regularStyle = @{
		NSFontAttributeName : [NSFont userFixedPitchFontOfSize:0],
		NSForegroundColorAttributeName : [NSColor grayColor],
	};
	NSDictionary* lineHighlightStyle = @{
		NSFontAttributeName : [NSFont userFixedPitchFontOfSize:0],
		NSForegroundColorAttributeName : [NSColor grayColor],
		NSBackgroundColorAttributeName : [NSColor colorWithCalibratedWhite:0.9 alpha:1],
	};
	NSDictionary* characterHighlightStyle = @{
		NSFontAttributeName : [NSFont userFixedPitchFontOfSize:0],
		NSBackgroundColorAttributeName : [NSColor colorWithCalibratedWhite:0.9 alpha:1],
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

	return output;
}

static NSTextField* MyCreateLabel (NSString* label, NSFont* font = nil)
{
	NSTextField* res = OakCreateLabel(label);
	res.alignment = NSLeftTextAlignment;
	if(font)
		res.font = font;
	return res;
}

static NSTextView* MyCreateTextView ()
{
	NSTextView* res = [[NSTextView alloc] initWithFrame:NSZeroRect];
	[res setVerticallyResizable:YES];
	[res setHorizontallyResizable:NO];
	[res setAutoresizingMask:NSViewWidthSizable];
	[[res textContainer] setWidthTracksTextView:YES];
	return res;
}

@interface EncodingContentView : NSView
{
	OBJC_WATCH_LEAKS(EncodingContentView);
}
@property (nonatomic, assign) id delegate;
@end

@implementation EncodingContentView
- (NSSize)intrinsicContentSize
{
	return NSMakeSize(NSViewNoInstrinsicMetric, NSViewNoInstrinsicMetric);
}

- (void)updateConstraints
{
	[super updateConstraints];
	[self.delegate updateConstraints];
}
@end

@interface EncodingWindowController () <NSWindowDelegate>
{
	OBJC_WATCH_LEAKS(EncodingWindowController);
	char const* first;
	char const* last;
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
- (id)initWithFirst:(char const*)firstPointer last:(char const*)lastPointer
{
	if(self = [super initWithWindow:[[NSWindow alloc] initWithContentRect:NSZeroRect styleMask:(NSTitledWindowMask|NSClosableWindowMask|NSResizableWindowMask|NSMiniaturizableWindowMask) backing:NSBackingStoreBuffered defer:NO]])
	{
		first = firstPointer;
		last  = lastPointer;

		_encoding        = @"ISO-8859-1";
		_displayName     = @"untitled";
		_trainClassifier = YES;

		self.objectController = [[NSObjectController alloc] initWithContent:self];

		self.title         = MyCreateLabel(@"Unknown Encoding", [NSFont boldSystemFontOfSize:0]);
		self.explanation   = MyCreateLabel(@"");
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
		self.scrollView.hasHorizontalScroller = NO;
		self.scrollView.borderType            = NSBezelBorder;
		self.scrollView.documentView          = self.textView;

		self.textView.editable          = NO;
		self.openButton.action          = @selector(performOpenDocument:);
		self.cancelButton.action        = @selector(performCancelOperation:);
		self.cancelButton.keyEquivalent = @"\e";

		EncodingContentView* contentView = [[EncodingContentView alloc] initWithFrame:NSZeroRect];
		[contentView setDelegate:self];
		[contentView setAutoresizingMask:NSViewWidthSizable|NSViewHeightSizable];
		self.contentView = contentView;

		for(NSView* view in [[self allViews] allValues])
		{
			[view setTranslatesAutoresizingMaskIntoConstraints:NO];
			[contentView addSubview:view];
		}

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

- (NSDictionary*)allViews
{
	return @{
		@"title"       : self.title,
		@"explanation" : self.explanation,
		@"label"       : self.label,
		@"popUp"       : self.popUpButton,
		@"textView"    : self.scrollView,
		@"learn"       : self.learnCheckBox,
		@"open"        : self.openButton,
		@"cancel"      : self.cancelButton
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
	[[self.textView textStorage] setAttributedString:convert_and_highlight(first, last, to_s(self.encoding), "UTF-8", &couldConvert)];
	self.acceptableEncoding = couldConvert;
}

- (void)setEncoding:(NSString*)anEncoding
{
	if([_encoding isEqualToString:anEncoding])
		return;
	_encoding = anEncoding;
	[self updateTextView];
}

- (void)cleanup
{
	self.contentView.delegate     = nil;
	self.objectController.content = nil;
}

- (IBAction)performOpenDocument:(id)sender
{
	[self.window orderOut:self];
	[NSApp endSheet:self.window returnCode:NSRunStoppedResponse];
	[self cleanup];
}

- (IBAction)performCancelOperation:(id)sender
{
	[self.window orderOut:self];
	[NSApp endSheet:self.window returnCode:NSRunAbortedResponse];
	[self cleanup];
}
@end
