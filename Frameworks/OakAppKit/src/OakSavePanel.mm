#import "OakSavePanel.h"
#import "OakEncodingPopUpButton.h"
#import "NSSavePanel Additions.h"
#import <OakFoundation/OakStringListTransformer.h>
#import <oak/oak.h>
#import <ns/ns.h>

@interface OakEncodingSaveOptionsViewController : NSViewController
{
	OBJC_WATCH_LEAKS(OakEncodingSaveOptionsViewController);
	IBOutlet OakEncodingPopUpButton* encodingPopUpButton;

	encoding::type _encodingOptions;
}
@property (nonatomic) NSString* lineEndings;
@property (nonatomic) NSString* encoding;
@property (nonatomic) BOOL useByteOrderMark;
@property (nonatomic, readonly) BOOL canUseByteOrderMark;
@property (nonatomic, readonly) encoding::type const& encodingOptions;
@end

@implementation OakEncodingSaveOptionsViewController
+ (NSSet*)keyPathsForValuesAffectingCanUseByteOrderMark { return [NSSet setWithObject:@"encoding"]; }
+ (NSSet*)keyPathsForValuesAffectingUseByteOrderMark    { return [NSSet setWithObject:@"encoding"]; }

+ (void)initialize
{
	[OakStringListTransformer createTransformerWithName:@"OakLineEndingsTransformer" andObjectsArray:@[ @"\n", @"\r", @"\r\n" ]];
	[OakStringListTransformer createTransformerWithName:@"OakLineEndingsListTransformer" andObjectsArray:@[ @"\n", @"\r", @"\r\n" ]];
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

- (BOOL)canUseByteOrderMark { return _encodingOptions.supports_byte_order_mark(_encodingOptions.charset()); }

- (NSString*)lineEndings    { return [NSString stringWithCxxString:_encodingOptions.newlines()]; }
- (NSString*)encoding       { return [NSString stringWithCxxString:_encodingOptions.charset()]; }
- (BOOL)useByteOrderMark    { return _encodingOptions.byte_order_mark(); }

- (void)setLineEndings:(NSString*)newLineEndings      { _encodingOptions.set_newlines(to_s(newLineEndings)); }
- (void)setEncoding:(NSString*)newEncoding            { _encodingOptions.set_charset(to_s(newEncoding)); }
- (void)setUseByteOrderMark:(BOOL)newUseByteOrderMark { _encodingOptions.set_byte_order_mark(newUseByteOrderMark); }
@end

@implementation OakSavePanel
+ (void)showWithPath:(NSString*)aPathSuggestion directory:(NSString*)aDirectorySuggestion fowWindow:(NSWindow*)aWindow encoding:(encoding::type const&)encoding completionHandler:(void(^)(NSString* path, encoding::type const& encoding))aCompletionHandler
{
	OakEncodingSaveOptionsViewController* optionsViewController = [[OakEncodingSaveOptionsViewController alloc] initWithEncodingOptions:encoding];
	if(!optionsViewController)
		return;

	[[aWindow attachedSheet] orderOut:self]; // incase there already is a sheet showing (like “Do you want to save?”)

	NSSavePanel* savePanel = [NSSavePanel savePanel];
	[savePanel setTreatsFilePackagesAsDirectories:YES];
	if(aDirectorySuggestion)
		[savePanel setDirectoryURL:[NSURL fileURLWithPath:aDirectorySuggestion]];
	[savePanel setNameFieldStringValue:[aPathSuggestion lastPathComponent]];
	[savePanel setAccessoryView:optionsViewController.view];
	[savePanel beginSheetModalForWindow:aWindow completionHandler:^(NSInteger result) {
		NSString* path = result == NSOKButton ? [[savePanel.URL filePathURL] path] : nil;
		aCompletionHandler(path, optionsViewController.encodingOptions);
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
