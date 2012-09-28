#import "AboutWindowController.h"
#import <OakAppKit/OakSubmenuController.h>
#import <OakFoundation/NSString Additions.h>
#import <updater/updater.h>

static NSString* const kUserDefaultsReleaseNotesDigestKey = @"releaseNotesDigest";

static NSData* Digest (NSString* someString)
{
	char const* str = [someString UTF8String];
	char md[SHA_DIGEST_LENGTH];
	CC_SHA1((unsigned char*)str, strlen(str), (unsigned char*)md);
	return [NSData dataWithBytes:md length:sizeof(md)];
}

// ============================
// = JavaScript Bridge Object =
// ============================

@interface AboutWindowJSBridge : NSObject
{
	NSString* version;
}
@end

@implementation AboutWindowJSBridge
+ (BOOL)isSelectorExcludedFromWebScript:(SEL)aSelector { return YES; }
+ (BOOL)isKeyExcludedFromWebScript:(char const*)name   { return strcmp(name, "version") != 0; }
+ (NSString*)webScriptNameForSelector:(SEL)aSelector   { return nil; }
+ (NSString*)webScriptNameForKey:(char const*)name     { return @(name); }

- (NSString*)version
{
	return [[NSBundle mainBundle] objectForInfoDictionaryKey:@"CFBundleShortVersionString"];
}
@end

// ============================

@interface AboutWindowController ()
- (void)didClickToolbarItem:(id)sender;
@property (nonatomic, assign) NSToolbar* toolbar;
@property (nonatomic, assign) WebView* webView;
@end

@implementation AboutWindowController
+ (BOOL)shouldShowChangesWindow
{
	NSURL* url = [[NSBundle mainBundle] URLForResource:@"Changes" withExtension:@"html"];
	if(NSString* releaseNotes = [NSString stringWithContentsOfURL:url encoding:NSUTF8StringEncoding error:NULL])
	{
		NSData* lastDigest    = [[NSUserDefaults standardUserDefaults] dataForKey:kUserDefaultsReleaseNotesDigestKey];
		NSData* currentDigest = Digest(releaseNotes);
		if(lastDigest)
		{
			if(![lastDigest isEqualToData:currentDigest])
				return YES;
		}
		[[NSUserDefaults standardUserDefaults] setObject:currentDigest forKey:kUserDefaultsReleaseNotesDigestKey];
	}
	return NO;
}

- (id)init
{
	NSRect visibleRect = [[NSScreen mainScreen] visibleFrame];
	NSRect rect = NSMakeRect(0, 0, std::min<CGFloat>(700, NSWidth(visibleRect)), std::min<CGFloat>(800, NSHeight(visibleRect)));

	CGFloat dy = NSHeight(visibleRect) - NSHeight(rect);

	rect.origin.y = round(NSMinY(visibleRect) + dy*3/4);
	rect.origin.x = NSMaxY(visibleRect) - NSMaxY(rect);

	NSWindow* win = [[[NSWindow alloc] initWithContentRect:rect styleMask:(NSTitledWindowMask|NSClosableWindowMask|NSResizableWindowMask|NSMiniaturizableWindowMask) backing:NSBackingStoreBuffered defer:NO] autorelease];
	if((self = [super initWithWindow:win]))
	{
		NSToolbar* toolbar = [[[NSToolbar alloc] initWithIdentifier:@"About TextMate"] autorelease];
		self.toolbar = toolbar;
		[toolbar setAllowsUserCustomization:NO];
		[toolbar setDisplayMode:NSToolbarDisplayModeLabelOnly];
		[toolbar setDelegate:self];
		[win setToolbar:toolbar];

		NSView* contentView = [[[NSView alloc] initWithFrame:NSZeroRect] autorelease];
		[win setTitle:@"About TextMate"];
		[win setContentView:contentView];
		[win setFrameAutosaveName:@"BundlesReleaseNotes"];
		[win setDelegate:self];
		[win setAutorecalculatesKeyViewLoop:YES];
		[win setReleasedWhenClosed:NO];

		WebView* webView = [[[WebView alloc] initWithFrame:[contentView bounds]] autorelease];
		self.webView = webView;
		webView.translatesAutoresizingMaskIntoConstraints = NO;
		webView.frameLoadDelegate = self;
		webView.policyDelegate = self;
		[contentView addSubview:webView];

		NSDictionary* views = NSDictionaryOfVariableBindings(webView);
		[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[webView(>=200)]|" options:NSLayoutFormatAlignAllTop     metrics:nil views:views]];
		[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[webView(>=200)]|" options:NSLayoutFormatAlignAllLeading metrics:nil views:views]];
	}
	return self;
}

- (void)dealloc
{
	[self.webView setFrameLoadDelegate:nil];
	[[self.webView mainFrame] stopLoading];
	[super dealloc];
}

- (void)showAboutWindow:(id)sender
{
	[self didClickToolbarItem:@"About"];
	[self showWindow:self];
}

- (void)showChangesWindow:(id)sender
{
	[self didClickToolbarItem:@"Changes"];
	[self showWindow:self];

	NSURL* url = [[NSBundle mainBundle] URLForResource:@"Changes" withExtension:@"html"];
	if(NSString* releaseNotes = [NSString stringWithContentsOfURL:url encoding:NSUTF8StringEncoding error:NULL])
		[[NSUserDefaults standardUserDefaults] setObject:Digest(releaseNotes) forKey:kUserDefaultsReleaseNotesDigestKey];
}

// ====================
// = Toolbar Delegate =
// ====================

- (void)didClickToolbarItem:(id)sender
{
	NSString* identifier = nil;
	if([sender respondsToSelector:@selector(itemIdentifier)])
		identifier = [sender itemIdentifier];
	else if([sender respondsToSelector:@selector(representedObject)])
		identifier = [sender representedObject];
	else if([sender isKindOfClass:[NSMenu class]])
		identifier = [[OakSubmenuController sharedInstance] representedObjectForSender:sender];
	else if([sender respondsToSelector:@selector(description)])
		identifier = [sender description];

	if(identifier)
	{
		NSDictionary* items = @{
			@"About"         : @"About",
			@"Changes"       : @"Changes",
			@"Bundles"       : @"Bundles",
			@"Registration"  : @"Registration",
			@"Legal"         : @"Legal",
			@"Contributions" : @"Contributions"
		};

		if(NSString* name = items[identifier])
		{
			if(NSURL* url = [[NSBundle mainBundle] URLForResource:name withExtension:@"html"])
				[[self.webView mainFrame] loadRequest:[NSURLRequest requestWithURL:url cachePolicy:NSURLRequestReloadIgnoringCacheData timeoutInterval:60]];

			[self.window setTitle:identifier];
			if(![sender isKindOfClass:[NSToolbarItem class]])
				[self.toolbar setSelectedItemIdentifier:identifier];
		}
	}
}

- (NSToolbarItem*)toolbar:(NSToolbar*)aToolbar itemForItemIdentifier:(NSString*)anIdentifier willBeInsertedIntoToolbar:(BOOL)flag
{
	NSToolbarItem* res = [[[NSToolbarItem alloc] initWithItemIdentifier:anIdentifier] autorelease];
	[res setLabel:anIdentifier];
	[res setTarget:self];
	[res setAction:@selector(didClickToolbarItem:)];
	return res;
}

- (NSArray*)toolbarAllowedItemIdentifiers:(NSToolbar*)aToolbar
{
	return @[ @"About", @"Changes", @"Bundles", NSToolbarFlexibleSpaceItemIdentifier, @"Registration", @"Legal", @"Contributions" ];
}

- (NSArray*)toolbarDefaultItemIdentifiers:(NSToolbar*)aToolbar
{
	return [self toolbarAllowedItemIdentifiers:aToolbar];
}

- (NSArray*)toolbarSelectableItemIdentifiers:(NSToolbar*)aToolbar
{
	return @[ @"About", @"Changes", @"Bundles", @"Registration", @"Legal", @"Contributions" ];
}

// ====================

- (void)updateGoToMenu:(NSMenu*)aMenu
{
	if(![[self window] isKeyWindow])
	{
		[aMenu addItemWithTitle:@"No Tabs" action:@selector(nop:) keyEquivalent:@""];
		return;
	}

	char key = '0';
	for(NSString* label in [self toolbarSelectableItemIdentifiers:self.toolbar])
	{
		NSMenuItem* item = [aMenu addItemWithTitle:label action:@selector(didClickToolbarItem:) keyEquivalent:key++ < '9' ? [NSString stringWithFormat:@"%c", key] : @""];
		[item setRepresentedObject:label];
		[item setTarget:self];
		[item setState:[label isEqualToString:[self.toolbar selectedItemIdentifier]] ? NSOnState : NSOffState];
	}
}

// ====================

- (void)webView:(WebView*)aWebView didFinishLoadForFrame:(WebFrame*)aFrame
{
	if(![[self.toolbar selectedItemIdentifier] isEqualToString:@"Bundles"])
		return;

	bool first = true;
	NSMutableString* str = [NSMutableString stringWithString:@"{\"bundles\":["];
	for(std::string path : bundles_db::release_notes())
	{
		if(NSString* content = [NSString stringWithContentsOfFile:[NSString stringWithCxxString:path] encoding:NSUTF8StringEncoding error:NULL])
		{
			NSError* err = NULL;
			if(![NSJSONSerialization JSONObjectWithData:[content dataUsingEncoding:NSUTF8StringEncoding] options:0 error:&err])
			{
				NSLog(@"%s: %@", path.c_str(), err.localizedDescription);
				continue;
			}

			if(!first)
				[str appendString:@","];
			first = false;
			[str appendString:content];
		}
	}
	[str appendString:@"]}"];

	WebScriptObject* scriptObject = [aWebView windowScriptObject];
	[scriptObject callWebScriptMethod:@"setJSON" withArguments:@[ str ]];
}

- (void)webView:(WebView*)sender didClearWindowObject:(WebScriptObject*)windowScriptObject forFrame:(WebFrame*)frame
{
	AboutWindowJSBridge* bridge = [[[AboutWindowJSBridge alloc] init] autorelease];
	[windowScriptObject setValue:bridge forKey:@"TextMate"];
}

- (void)webView:(WebView*)sender decidePolicyForNavigationAction:(NSDictionary*)actionInformation request:(NSURLRequest*)request frame:(WebFrame*)frame decisionListener:(id <WebPolicyDecisionListener>)listener
{
	if(![[request.URL scheme] isEqualToString:@"file"] && [[NSWorkspace sharedWorkspace] openURL:request.URL])
		[listener ignore];
	else if([NSURLConnection canHandleRequest:request])
		[listener use];
}
@end
