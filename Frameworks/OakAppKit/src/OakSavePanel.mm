#import "OakSavePanel.h"
#import "OakEncodingPopUpButton.h"
#import "OakUIConstructionFunctions.h"
#import "NSSavePanel Additions.h"
#import <OakFoundation/OakStringListTransformer.h>
#import <settings/settings.h>
#import <oak/oak.h>
#import <ns/ns.h>
#import <crash/info.h>

@interface OakEncodingSaveOptionsViewController : NSViewController <NSOpenSavePanelDelegate>
{
	OBJC_WATCH_LEAKS(OakEncodingSaveOptionsViewController);
	encoding::type _encodingOptions;
}
@property (nonatomic) NSString* lineEndings;
@property (nonatomic) NSString* encoding;
@property (nonatomic) BOOL useByteOrderMark;
@property (nonatomic) BOOL canUseByteOrderMark;
@property (nonatomic) NSSavePanel* savePanel;
@end

@implementation OakEncodingSaveOptionsViewController
- (void)dealloc
{
	if(_savePanel.delegate == self)
		_savePanel.delegate = nil;
}

- (id)initWithEncodingOptions:(encoding::type const&)someEncodingOptions
{
	if(self = [super init])
		_encodingOptions = someEncodingOptions;
	return self;
}

- (void)loadView
{
	static dispatch_once_t onceToken = 0;
	dispatch_once(&onceToken, ^{
		[OakStringListTransformer createTransformerWithName:@"OakLineEndingsTransformer" andObjectsArray:@[ @"\n", @"\r", @"\r\n" ]];
	});

	NSPopUpButton* encodingPopUpButton    = [[OakEncodingPopUpButton alloc] initWithFrame:NSZeroRect pullsDown:NO];
	NSPopUpButton* lineEndingsPopUpButton = [[NSPopUpButton alloc] initWithFrame:NSZeroRect pullsDown:NO];
	NSButton* bomCheckBox                 = OakCreateCheckBox(@"Add byte order mark");

	[encodingPopUpButton setContentCompressionResistancePriority:NSLayoutPriorityDefaultLow forOrientation:NSLayoutConstraintOrientationHorizontal];

	OakSetAccessibilityLabel(encodingPopUpButton, @"encoding");
	OakSetAccessibilityLabel(lineEndingsPopUpButton, @"line endings");

	NSArray* titles = @[ @"LF", @"CR", @"CRLF" ];
	for(NSUInteger i = 0; i < [titles count]; ++i)
		[[lineEndingsPopUpButton.menu addItemWithTitle:titles[i] action:nil keyEquivalent:@""] setTag:i];

	NSDictionary* views = @{
		@"encodingLabel"    : OakCreateLabel(@"Encoding:"),
		@"encodingPopUp"    : encodingPopUpButton,
		@"lineEndingsPopUp" : lineEndingsPopUpButton,
		@"bomCheckBox"      : bomCheckBox,
	};

	NSView* containerView = [[NSView alloc] initWithFrame:NSZeroRect];
	for(NSView* view in [views allValues])
	{
		[view setTranslatesAutoresizingMaskIntoConstraints:NO];
		[containerView addSubview:view];
	}

	[containerView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[encodingLabel]-[encodingPopUp]-[lineEndingsPopUp]-(>=0)-|" options:NSLayoutFormatAlignAllBaseline metrics:nil views:views]];
	[containerView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:[bomCheckBox]-(>=0)-|" options:NSLayoutFormatAlignAllBaseline metrics:nil views:views]];
	[containerView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-(8)-[encodingPopUp]-[bomCheckBox]-(8)-|" options:NSLayoutFormatAlignAllLeading metrics:nil views:views]];

	containerView.frame = (NSRect){ NSZeroPoint, [containerView fittingSize] };
	self.view = containerView;

	[encodingPopUpButton bind:@"encoding" toObject:self withKeyPath:@"encoding" options:nil];
	[lineEndingsPopUpButton bind:@"selectedTag" toObject:self withKeyPath:@"lineEndings" options:@{ NSValueTransformerNameBindingOption: @"OakLineEndingsTransformer" }];
	[bomCheckBox bind:NSEnabledBinding toObject:self withKeyPath:@"canUseByteOrderMark" options:nil];
	[bomCheckBox bind:NSValueBinding toObject:self withKeyPath:@"useByteOrderMark" options:nil];
}

- (void)setEncoding:(NSString*)newEncoding
{
	if([_encoding isEqualToString:newEncoding])
		return;
	_encoding = newEncoding;
	self.canUseByteOrderMark = _encodingOptions.supports_byte_order_mark(to_s(newEncoding));
	self.useByteOrderMark    = _canUseByteOrderMark && to_s(newEncoding) != kCharsetUTF8;
}

- (void)updateSettings:(encoding::type const&)encoding
{
	self.lineEndings      = [NSString stringWithCxxString:encoding.newlines()];
	self.encoding         = [NSString stringWithCxxString:encoding.charset()];
	self.useByteOrderMark = encoding.byte_order_mark();
}

- (encoding::type)encodingForURL:(NSURL*)anURL
{
	encoding::type res = _encodingOptions;

	settings_t const& settings = settings_for_path(to_s([[anURL filePathURL] path]));
	if(res.charset() == kCharsetNoEncoding)
	{
		res.set_charset(settings.get(kSettingsEncodingKey, kCharsetUTF8));
		res.set_byte_order_mark(settings.get(kSettingsUseBOMKey, res.byte_order_mark()));
	}

	if(res.newlines() == NULL_STR)
		res.set_newlines(settings.get(kSettingsLineEndingsKey, "\n"));

	return res;
}

- (void)panel:(NSSavePanel*)sender didChangeToDirectoryURL:(NSURL*)anURL
{
	crash_reporter_info_t info("NSSavePanel did change to directory: " + to_s([anURL path]));
	[self updateSettings:[self encodingForURL:[sender URL]]];
}
@end

@implementation OakSavePanel
+ (void)showWithPath:(NSString*)aPathSuggestion directory:(NSString*)aDirectorySuggestion fowWindow:(NSWindow*)aWindow encoding:(encoding::type const&)encoding completionHandler:(void(^)(NSString* path, encoding::type const& encoding))aCompletionHandler
{
	OakEncodingSaveOptionsViewController* optionsViewController = [[OakEncodingSaveOptionsViewController alloc] initWithEncodingOptions:encoding];
	if(!optionsViewController)
		return;

	[[aWindow attachedSheet] orderOut:self]; // incase there already is a sheet showing (like “Do you want to save?”)

	NSSavePanel* savePanel = [NSSavePanel savePanel];
	optionsViewController.savePanel = savePanel;
	[savePanel setTreatsFilePackagesAsDirectories:YES];
	if(aDirectorySuggestion)
		[savePanel setDirectoryURL:[NSURL fileURLWithPath:aDirectorySuggestion]];
	[savePanel setNameFieldStringValue:[aPathSuggestion lastPathComponent]];
	[savePanel setAccessoryView:optionsViewController.view];
	[optionsViewController updateSettings:[optionsViewController encodingForURL:[savePanel URL]]];
	crash_reporter_info_t info("Setup NSSavePanel delegate with path: " + to_s([[savePanel URL] path]));
	savePanel.delegate = optionsViewController;
	[savePanel beginSheetModalForWindow:aWindow completionHandler:^(NSInteger result) {
		crash_reporter_info_t info("Clear NSSavePanel delegate");
		savePanel.delegate = nil;
		NSString* path = result == NSOKButton ? [[savePanel.URL filePathURL] path] : nil;
		encoding::type encoding(to_s(optionsViewController.lineEndings), to_s(optionsViewController.encoding), optionsViewController.useByteOrderMark);
		aCompletionHandler(path, encoding);
	}];

	// Deselect Extension
	if([savePanel.firstResponder isKindOfClass:[NSTextView class]])
	{
		NSTextView* tw = (NSTextView*)savePanel.firstResponder;
		NSRange extRange = [tw.textStorage.string rangeOfString:@"."];
		if(extRange.location != NSNotFound)
			[tw setSelectedRange:NSMakeRange(0, extRange.location)];
	}
}
@end
