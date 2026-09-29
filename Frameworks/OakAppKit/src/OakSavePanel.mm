#import "OakSavePanel.h"
#import "OakEncodingPopUpButton.h"
#import "OakUIConstructionFunctions.h"
#import "NSSavePanel Additions.h"
#import <OakFoundation/src/OakStringListTransformer.h>
#import <settings/src/settings.h>
#import <oak/oak.h>
#import <ns/src/ns.h>

@interface OakEncodingSaveOptionsViewController : NSViewController <NSOpenSavePanelDelegate>
{
	encoding::type _encodingOptions;
}
@property (nonatomic) NSString* fileType;
@property (nonatomic) NSString* lineEndings;
@property (nonatomic) NSString* encoding;
@property (nonatomic) NSSavePanel* savePanel;
@end

@implementation OakEncodingSaveOptionsViewController
+ (void)initialize
{
	[OakStringListTransformer createTransformerWithName:@"OakLineEndingsTransformer" andObjectsArray:@[ @"\n", @"\r", @"\r\n" ]];
}

- (void)dealloc
{
	if(_savePanel.delegate == self)
		_savePanel.delegate = nil;
}

- (id)initWithEncodingOptions:(encoding::type const&)someEncodingOptions fileType:(NSString*)aFileType
{
	if(self = [super init])
	{
		_encodingOptions = someEncodingOptions;
		_fileType = aFileType;
	}
	return self;
}

- (void)loadView
{
	NSPopUpButton* encodingPopUpButton    = [[OakEncodingPopUpButton alloc] initWithFrame:NSZeroRect pullsDown:NO];
	NSPopUpButton* lineEndingsPopUpButton = [[NSPopUpButton alloc] initWithFrame:NSZeroRect pullsDown:NO];

	[encodingPopUpButton setContentCompressionResistancePriority:NSLayoutPriorityDefaultLow forOrientation:NSLayoutConstraintOrientationHorizontal];

	encodingPopUpButton.accessibilityLabel    = @"Encoding";
	lineEndingsPopUpButton.accessibilityLabel = @"Line endings";

	NSArray* titles = @[ @"LF", @"CR", @"CRLF" ];
	for(NSUInteger i = 0; i < [titles count]; ++i)
		[[lineEndingsPopUpButton.menu addItemWithTitle:titles[i] action:nil keyEquivalent:@""] setTag:i];

	NSDictionary* views = @{
		@"encodingLabel":    OakCreateLabel(@"Encoding:"),
		@"encodingPopUp":    encodingPopUpButton,
		@"lineEndingsPopUp": lineEndingsPopUpButton,
	};

	NSView* containerView = [[NSView alloc] initWithFrame:NSZeroRect];
	OakAddAutoLayoutViewsToSuperview([views allValues], containerView);

	[containerView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[encodingLabel]-[encodingPopUp]-[lineEndingsPopUp]-(>=20)-|" options:NSLayoutFormatAlignAllBaseline metrics:nil views:views]];
	[containerView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-(8)-[encodingPopUp]-(8)-|" options:NSLayoutFormatAlignAllLeading metrics:nil views:views]];

	containerView.frame = (NSRect){ NSZeroPoint, [containerView fittingSize] };
	self.view = containerView;

	[encodingPopUpButton bind:@"encoding" toObject:self withKeyPath:@"encoding" options:nil];
	[lineEndingsPopUpButton bind:NSSelectedTagBinding toObject:self withKeyPath:@"lineEndings" options:@{ NSValueTransformerNameBindingOption: @"OakLineEndingsTransformer" }];
}

- (void)updateSettings:(encoding::type const&)encoding
{
	self.lineEndings      = [NSString stringWithCxxString:encoding.newlines()];
	self.encoding         = [NSString stringWithCxxString:encoding.charset()];
}

- (encoding::type)encodingForURL:(NSURL*)anURL
{
	encoding::type res = _encodingOptions;

	settings_t const& settings = settings_for_path(to_s([[anURL filePathURL] path]), to_s(_fileType));
	if(res.charset() == kCharsetNoEncoding)
		res.set_charset(settings.get(kSettingsEncodingKey, kCharsetUTF8));

	if(res.newlines() == NULL_STR)
		res.set_newlines(settings.get(kSettingsLineEndingsKey, "\n"));

	return res;
}

- (void)panel:(NSSavePanel*)sender didChangeToDirectoryURL:(NSURL*)anURL
{
	[self updateSettings:[self encodingForURL:[sender URL]]];
}
@end

@implementation OakSavePanel
+ (void)showWithPath:(NSString*)aPathSuggestion directory:(NSString*)aDirectorySuggestion fowWindow:(NSWindow*)aWindow encoding:(encoding::type const&)encoding fileType:(NSString*)aFileType completionHandler:(void(^)(NSString* path, encoding::type const& encoding))aCompletionHandler
{
	OakEncodingSaveOptionsViewController* optionsViewController = [[OakEncodingSaveOptionsViewController alloc] initWithEncodingOptions:encoding fileType:aFileType];
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
	savePanel.delegate = optionsViewController;
	[savePanel beginSheetModalForWindow:aWindow completionHandler:^(NSModalResponse result) {
		savePanel.delegate = nil;
		NSString* path = result == NSModalResponseOK ? [[savePanel.URL filePathURL] path] : nil;
		encoding::type encoding(to_s(optionsViewController.lineEndings), to_s(optionsViewController.encoding));
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
