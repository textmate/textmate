#import "OakSavePanel.h"
#import "OakEncodingPopUpButton.h"
#import "NSSavePanel Additions.h"
#import <OakFoundation/OakStringListTransformer.h>
#import <settings/settings.h>
#import <oak/oak.h>
#import <ns/ns.h>

@interface OakEncodingSaveOptionsViewController : NSViewController <NSOpenSavePanelDelegate>
{
	OBJC_WATCH_LEAKS(OakEncodingSaveOptionsViewController);
	IBOutlet OakEncodingPopUpButton* encodingPopUpButton;

	encoding::type _encodingOptions;
}
@property (nonatomic) NSString* lineEndings;
@property (nonatomic) NSString* encoding;
@property (nonatomic) BOOL useByteOrderMark;
@property (nonatomic, readonly) BOOL canUseByteOrderMark;
@end

@implementation OakEncodingSaveOptionsViewController
+ (NSSet*)keyPathsForValuesAffectingCanUseByteOrderMark { return [NSSet setWithObject:@"encoding"]; }
+ (NSSet*)keyPathsForValuesAffectingUseByteOrderMark    { return [NSSet setWithObject:@"encoding"]; }

+ (void)initialize
{
	static dispatch_once_t onceToken = 0;
	dispatch_once(&onceToken, ^{
		[OakStringListTransformer createTransformerWithName:@"OakLineEndingsTransformer" andObjectsArray:@[ @"\n", @"\r", @"\r\n" ]];
		[OakStringListTransformer createTransformerWithName:@"OakLineEndingsListTransformer" andObjectsArray:@[ @"\n", @"\r", @"\r\n" ]];
	});
}

- (id)initWithEncodingOptions:(encoding::type const&)someEncodingOptions
{
	if(self = [super initWithNibName:@"EncodingSaveOptions" bundle:[NSBundle bundleForClass:[self class]]])
		_encodingOptions = someEncodingOptions;
	return self;
}

- (void)loadView
{
	[super loadView];
	[encodingPopUpButton bind:@"encoding" toObject:self withKeyPath:@"encoding" options:nil];
}

- (BOOL)canUseByteOrderMark { return _encodingOptions.supports_byte_order_mark(to_s(self.encoding)); }

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
	savePanel.delegate = optionsViewController;
	[savePanel setTreatsFilePackagesAsDirectories:YES];
	if(aDirectorySuggestion)
		[savePanel setDirectoryURL:[NSURL fileURLWithPath:aDirectorySuggestion]];
	[savePanel setNameFieldStringValue:[aPathSuggestion lastPathComponent]];
	[savePanel setAccessoryView:optionsViewController.view];
	[optionsViewController updateSettings:[optionsViewController encodingForURL:[savePanel URL]]];
	[savePanel beginSheetModalForWindow:aWindow completionHandler:^(NSInteger result) {
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
