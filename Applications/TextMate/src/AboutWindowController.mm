#import "AboutWindowController.h"
#import <OakAppKit/OakUIConstructionFunctions.h>
#import <OakFoundation/OakFoundation.h>
#import <OakFoundation/NSString Additions.h>
#import <BundlesManager/BundlesManager.h>
#import <license/LicenseManager.h>
#import <ns/ns.h>

static NSString* const kUserDefaultsReleaseNotesDigestKey = @"releaseNotesDigest";

static NSData* Digest (NSString* someString)
{
	char const* str = [someString UTF8String];
	char md[CC_SHA1_DIGEST_LENGTH];
	CC_SHA1((unsigned char*)str, strlen(str), (unsigned char*)md);
	return [NSData dataWithBytes:md length:sizeof(md)];
}

// ============================
// = JavaScript Bridge Object =
// ============================

@interface AboutWindowJSBridge : NSObject
{
	NSString* version;
	NSString* copyright;
	NSString* licensees;
}
- (void)addLicense;
@end

@implementation AboutWindowJSBridge
+ (BOOL)isSelectorExcludedFromWebScript:(SEL)aSelector { return aSelector != @selector(addLicense) && aSelector != @selector(themeColors); }
+ (BOOL)isKeyExcludedFromWebScript:(char const*)name   { return strcmp(name, "version") != 0 && strcmp(name, "copyright") != 0 && strcmp(name, "licensees") != 0; }
+ (NSString*)webScriptNameForSelector:(SEL)aSelector   { return NSStringFromSelector(aSelector); }
+ (NSString*)webScriptNameForKey:(char const*)name     { return @(name); }

- (NSString*)version
{
	return [[NSBundle mainBundle] objectForInfoDictionaryKey:@"CFBundleShortVersionString"];
}

- (NSString*)copyright
{
	return [[NSBundle mainBundle] objectForInfoDictionaryKey:@"NSHumanReadableCopyright"];
}

- (NSString*)licensees
{
	return LicenseManager.sharedInstance.owner;
}

- (NSString*)themeColors
{
	NSString* css = @":root {"
		" --textColor:       #000000ff;"
		" --backgroundColor: #f1f1f1ff;"
		" --shadowColor:     #ffffffc0;"
	"}";

	if(@available(macos 10.14, *))
	{
		NSAppearanceName appearanceName = [NSApp.effectiveAppearance bestMatchFromAppearancesWithNames:@[ NSAppearanceNameAqua, NSAppearanceNameDarkAqua ]];
		if([appearanceName isEqualToString:NSAppearanceNameDarkAqua])
		{
			css = @":root {"
				" --textColor:       #ffffffff;"
				" --backgroundColor: #1e1e1eff;"
				" --shadowColor:     #1e1e1ec0;"
			"}";
		}
	}
	return css;
}

- (void)addLicense
{
	[LicenseManager.sharedInstance showAddLicenseWindow:self];
}
@end

// ====================================
// = Auto-reload on appearance change =
// ====================================

@interface AboutWindowWebView : WebView
@property (nonatomic, copy) NSURL* lastURLRequested;
@end

@implementation AboutWindowWebView
- (void)loadRequest:(NSURLRequest*)urlRequest
{
	self.lastURLRequested = urlRequest.URL;
	[self.mainFrame loadRequest:urlRequest];
}

- (void)viewDidChangeEffectiveAppearance
{
	[super viewDidChangeEffectiveAppearance];
	if(NSURL* url = self.lastURLRequested)
		[self.mainFrame loadRequest:[NSURLRequest requestWithURL:url cachePolicy:NSURLRequestReloadIgnoringCacheData timeoutInterval:60]];
}
@end

// ====================================

@interface AboutWindowController () <NSWindowDelegate, NSToolbarDelegate, WebFrameLoadDelegate, WebPolicyDelegate>
@property (nonatomic) NSToolbar* toolbar;
@property (nonatomic) AboutWindowWebView* webView;
@property (nonatomic) NSString* selectedPage;
@end

@implementation AboutWindowController
+ (instancetype)sharedInstance
{
	static AboutWindowController* sharedInstance = [self new];
	return sharedInstance;
}

+ (void)showChangesIfUpdated
{
	NSURL* url = [[NSBundle mainBundle] URLForResource:@"Changes" withExtension:@"html"];
	dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_LOW, 0), ^{
		if(NSString* releaseNotes = [NSString stringWithContentsOfURL:url encoding:NSUTF8StringEncoding error:NULL])
		{
			NSData* lastDigest    = [[NSUserDefaults standardUserDefaults] dataForKey:kUserDefaultsReleaseNotesDigestKey];
			NSData* currentDigest = Digest(releaseNotes);
			dispatch_async(dispatch_get_main_queue(), ^{
				if(lastDigest && ![lastDigest isEqualToData:currentDigest])
					[[AboutWindowController sharedInstance] showChangesWindow:self];
				[[NSUserDefaults standardUserDefaults] setObject:currentDigest forKey:kUserDefaultsReleaseNotesDigestKey];
			});
		}
	});
}

- (id)init
{
	NSRect visibleRect = [[NSScreen mainScreen] visibleFrame];
	NSRect rect = NSMakeRect(0, 0, std::min<CGFloat>(700, NSWidth(visibleRect)), std::min<CGFloat>(800, NSHeight(visibleRect)));

	CGFloat dy = NSHeight(visibleRect) - NSHeight(rect);

	rect.origin.y = round(NSMinY(visibleRect) + dy*3/4);
	rect.origin.x = NSMaxY(visibleRect) - NSMaxY(rect);

	NSWindow* win = [[NSWindow alloc] initWithContentRect:rect styleMask:(NSWindowStyleMaskTitled|NSWindowStyleMaskClosable|NSWindowStyleMaskResizable|NSWindowStyleMaskMiniaturizable) backing:NSBackingStoreBuffered defer:NO];
	if((self = [super initWithWindow:win]))
	{
		self.toolbar = [[NSToolbar alloc] initWithIdentifier:@"About TextMate"];
		[self.toolbar setAllowsUserCustomization:NO];
		[self.toolbar setDisplayMode:NSToolbarDisplayModeLabelOnly];
		[self.toolbar setDelegate:self];
		[win setToolbar:self.toolbar];

		NSView* contentView = [[NSView alloc] initWithFrame:NSZeroRect];
		[win setTitle:@"About TextMate"];
		[win setContentView:contentView];
		[win setFrameAutosaveName:@"BundlesReleaseNotes"];
		[win setDelegate:self];
		[win setAutorecalculatesKeyViewLoop:YES];

		self.webView = [[AboutWindowWebView alloc] initWithFrame:[contentView bounds]];
		self.webView.drawsBackground = NO;
		self.webView.translatesAutoresizingMaskIntoConstraints = NO;
		self.webView.frameLoadDelegate = self;
		self.webView.policyDelegate = self;
		[contentView addSubview:self.webView];

		NSString* const kAboutWindowPreferencesIdentifier = @"About Window Preferences Identifier";
		WebPreferences* webViewPrefs = [[WebPreferences alloc] initWithIdentifier:kAboutWindowPreferencesIdentifier];
		webViewPrefs.plugInsEnabled = NO;
		webViewPrefs.usesPageCache  = NO;
		webViewPrefs.cacheModel     = WebCacheModelDocumentViewer;
		self.webView.preferencesIdentifier = kAboutWindowPreferencesIdentifier;

		NSDictionary* views = @{ @"webView": self.webView };
		[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[webView(>=200)]|" options:NSLayoutFormatAlignAllTop     metrics:nil views:views]];
		[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[webView(>=200)]|" options:NSLayoutFormatAlignAllLeading metrics:nil views:views]];
	}
	return self;
}

- (void)dealloc
{
	_webView.policyDelegate    = nil;
	_webView.frameLoadDelegate = nil;
	[[_webView mainFrame] stopLoading];
}

- (void)showAboutWindow:(id)sender
{
	self.selectedPage = @"About";
	[self showWindow:self];
}

- (void)showChangesWindow:(id)sender
{
	self.selectedPage = @"Changes";
	[self showWindow:self];

	NSURL* url = [[NSBundle mainBundle] URLForResource:@"Changes" withExtension:@"html"];
	if(NSString* releaseNotes = [NSString stringWithContentsOfURL:url encoding:NSUTF8StringEncoding error:NULL])
		[[NSUserDefaults standardUserDefaults] setObject:Digest(releaseNotes) forKey:kUserDefaultsReleaseNotesDigestKey];
}

- (void)setSelectedPage:(NSString*)pageName
{
	if(_selectedPage == pageName || [_selectedPage isEqualToString:pageName])
		return;
	_selectedPage = pageName;

	NSDictionary* pages = @{
		@"About":         @"About",
		@"Changes":       @"Changes",
		@"Bundles":       @"Bundles",
		@"Registration":  @"Registration",
		@"Legal":         @"Legal",
		@"Contributions": @"Contributions"
	};

	if(NSString* file = pages[pageName])
	{
		if(NSURL* url = [[NSBundle mainBundle] URLForResource:file withExtension:@"html"])
			[self.webView loadRequest:[NSURLRequest requestWithURL:url cachePolicy:NSURLRequestReloadIgnoringCacheData timeoutInterval:60]];

		[self.window setTitle:pageName];
		[self.toolbar setSelectedItemIdentifier:pageName];
	}
}

- (void)selectPageAtRelativeOffset:(NSInteger)offset
{
	NSArray* allPages = [self toolbarSelectableItemIdentifiers:self.toolbar];
	NSUInteger index = [allPages indexOfObject:self.selectedPage];
	if(index != NSNotFound)
		self.selectedPage = allPages[(index + allPages.count + offset) % allPages.count];
}

- (IBAction)selectNextTab:(id)sender     { [self selectPageAtRelativeOffset:+1]; }
- (IBAction)selectPreviousTab:(id)sender { [self selectPageAtRelativeOffset:-1]; }

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

	if(identifier)
		self.selectedPage = identifier;
}

- (NSToolbarItem*)toolbar:(NSToolbar*)aToolbar itemForItemIdentifier:(NSString*)anIdentifier willBeInsertedIntoToolbar:(BOOL)flag
{
	NSToolbarItem* res = [[NSToolbarItem alloc] initWithItemIdentifier:anIdentifier];
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

- (void)updateShowTabMenu:(NSMenu*)aMenu
{
	if(![[self window] isKeyWindow])
	{
		[aMenu addItemWithTitle:@"No Tabs" action:@selector(nop:) keyEquivalent:@""];
		return;
	}

	char key = 0;
	for(NSString* label in [self toolbarSelectableItemIdentifiers:self.toolbar])
	{
		NSMenuItem* item = [aMenu addItemWithTitle:label action:@selector(didClickToolbarItem:) keyEquivalent:key < 9 ? [NSString stringWithFormat:@"%c", '0' + (++key % 10)] : @""];
		[item setRepresentedObject:label];
		[item setTarget:self];
		[item setState:[label isEqualToString:[self.toolbar selectedItemIdentifier]] ? NSOnState : NSOffState];
	}
}

// ====================

static NSDictionary* RemoveOldCommits (NSDictionary* src)
{
	NSMutableDictionary* res = [src mutableCopy];
	NSMutableArray* commits = [NSMutableArray array];

	NSInteger year = [[[[NSCalendar alloc] initWithCalendarIdentifier:NSCalendarIdentifierGregorian] components:NSCalendarUnitYear fromDate:[NSDate date]] year];
	NSMutableArray* years = [NSMutableArray array];
	for(size_t i = year-2; i <= year; ++i)
		[years addObject:[NSString stringWithFormat:@"%4zu-", i]];

	for(NSDictionary* commit in src[@"commits"])
	{
		NSString* dateString = commit[@"date"];
		for(NSString* prefix in years)
		{
			if([dateString hasPrefix:prefix]) // this is significantly faster than having to parse the date
				[commits addObject:commit];
		}
	}

	res[@"commits"] = commits;
	return res;
}

- (void)webView:(WebView*)aWebView didFinishLoadForFrame:(WebFrame*)aFrame
{
	if(![[self.toolbar selectedItemIdentifier] isEqualToString:@"Bundles"])
		return;

	bool first = true;
	NSMutableString* str = [NSMutableString stringWithString:@"{\"bundles\":["];
	for(Bundle* bundle in [BundlesManager sharedInstance].bundles)
	{
		if(!bundle.installed || !bundle.path)
			continue;

		NSError* err = NULL;
		if(NSString* content = [NSString stringWithContentsOfFile:[bundle.path stringByAppendingPathComponent:@"Changes.json"] encoding:NSUTF8StringEncoding error:&err])
		{
			if(NSDictionary* obj = [NSJSONSerialization JSONObjectWithData:[content dataUsingEncoding:NSUTF8StringEncoding] options:0 error:&err])
			{
				if(NSData* data = [NSJSONSerialization dataWithJSONObject:RemoveOldCommits(obj) options:0 error:&err])
				{
					if(!std::exchange(first, false))
						[str appendString:@","];

					[str appendString:[[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding]];
					continue;
				}
			}
		}
		NSLog(@"%@: %@", bundle.name, err.localizedDescription);
	}
	[str appendString:@"]}"];

	WebScriptObject* scriptObject = [aWebView windowScriptObject];
	[scriptObject callWebScriptMethod:@"setJSON" withArguments:@[ str ]];
}

- (void)webView:(WebView*)sender didClearWindowObject:(WebScriptObject*)windowScriptObject forFrame:(WebFrame*)frame
{
	AboutWindowJSBridge* bridge = [[AboutWindowJSBridge alloc] init];
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
