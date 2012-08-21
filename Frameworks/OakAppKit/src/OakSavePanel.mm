#import "OakSavePanel.h"
#import "OakEncodingPopUpButton.h"
#import "NSSavePanel Additions.h"
#import <OakFoundation/OakStringListTransformer.h>
#import <oak/oak.h>
#import <ns/ns.h>

@interface OakEncodingSaveOptionsViewController : NSViewController
{
	IBOutlet OakEncodingPopUpButton* encodingPopUpButton;

	NSString* encoding;
	NSString* lineEndings;
	BOOL useByteOrderMark;
}
@property (nonatomic, retain) NSString* lineEndings;
@property (nonatomic, retain) NSString* encoding;
@property (nonatomic, assign) BOOL useByteOrderMark;
@property (nonatomic, readonly) BOOL canUseByteOrderMark;
@end

@implementation OakEncodingSaveOptionsViewController
@synthesize encoding, lineEndings, useByteOrderMark;

+ (NSSet*)keyPathsForValuesAffectingCanUseByteOrderMark { return [NSSet setWithObject:@"encoding"]; }

+ (void)initialize
{
	[OakStringListTransformer createTransformerWithName:@"OakLineEndingsTransformer" andObjectsArray:@[ @"\n", @"\r", @"\r\n" ]];
	[OakStringListTransformer createTransformerWithName:@"OakLineEndingsListTransformer" andObjectsArray:@[ @"\n", @"\r", @"\r\n" ]];
}

- (id)init
{
	if(self = [super initWithNibName:@"EncodingSaveOptions" bundle:[NSBundle bundleForClass:[self class]]])
	{
		self.encoding    = @"UTF-8";
		self.lineEndings = @"\n";
	}
	return self;
}

- (void)dealloc
{
	[self unbind:@"encoding"];
	[super dealloc];
}

- (void)loadView
{
	[super loadView];
	encodingPopUpButton.encoding = encoding;
	[self bind:@"encoding" toObject:encodingPopUpButton withKeyPath:@"encoding" options:nil];
}

- (BOOL)canUseByteOrderMark
{
	return [self.encoding hasPrefix:@"UTF-"];
}

- (void)setEncoding:(NSString*)newEncoding
{
	if(encoding != newEncoding && ![encoding isEqualToString:newEncoding])
	{
		[encoding autorelease];
		encoding = [newEncoding retain];

		self.useByteOrderMark = [self canUseByteOrderMark] && ![encoding isEqualToString:@"UTF-8"];
	}
}
@end

@implementation OakSavePanel
- (id)initWithPath:(NSString*)aPathSuggestion directory:(NSString*)aDirectorySuggestion fowWindow:(NSWindow*)aWindow delegate:(id)aDelegate encoding:(std::string const&)encoding newlines:(std::string const&)newlines useBOM:(BOOL)useBOM
{
	if((self = [super init]))
	{
		optionsViewController = [OakEncodingSaveOptionsViewController new];
		if(!optionsViewController)
		{
			[self release];
			return nil;
		}

		optionsViewController.encoding         = [NSString stringWithCxxString:encoding] ?: @"UTF-8";
		optionsViewController.lineEndings      = [NSString stringWithCxxString:newlines] ?: @"\n";
		optionsViewController.useByteOrderMark = useBOM;

		[[aWindow attachedSheet] orderOut:self]; // incase there already is a sheet showing (like “Do you want to save?”)

		NSSavePanel* savePanel = [NSSavePanel savePanel];
		[savePanel setTreatsFilePackagesAsDirectories:YES];
		if(aDirectorySuggestion)
			[savePanel setDirectoryURL:[NSURL fileURLWithPath:aDirectorySuggestion]];
		[savePanel setNameFieldStringValue:[aPathSuggestion lastPathComponent]];
		[savePanel setAccessoryView:optionsViewController.view];
		[savePanel beginSheetModalForWindow:aWindow completionHandler:^(NSInteger result) {
			NSString* path = result == NSOKButton ? [[savePanel.URL filePathURL] path] : nil;
			[aDelegate savePanelDidEnd:self path:path encoding:to_s(optionsViewController.encoding) newlines:to_s(optionsViewController.lineEndings) useBOM:optionsViewController.useByteOrderMark];
			[self release];
		}];
		[savePanel deselectExtension];
	}
	return self;
}

- (void)dealloc
{
	[optionsViewController release];
	[super dealloc];
}

+ (void)showWithPath:(NSString*)aPathSuggestion directory:(NSString*)aDirectorySuggestion fowWindow:(NSWindow*)aWindow delegate:(id)aDelegate encoding:(std::string const&)encoding newlines:(std::string const&)newlines useBOM:(BOOL)useBOM
{
	[[OakSavePanel alloc] initWithPath:aPathSuggestion directory:aDirectorySuggestion fowWindow:aWindow delegate:aDelegate encoding:encoding newlines:newlines useBOM:useBOM];
}
@end
