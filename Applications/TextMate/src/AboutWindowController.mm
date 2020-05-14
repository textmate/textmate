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

@interface AboutWindowController () <NSWindowDelegate, NSToolbarDelegate, WKNavigationDelegate, WKScriptMessageHandler>
@property (nonatomic) NSToolbar* toolbar;
@property (nonatomic) WKWebView* webView;
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
			NSData* lastDigest    = [NSUserDefaults.standardUserDefaults dataForKey:kUserDefaultsReleaseNotesDigestKey];
			NSData* currentDigest = Digest(releaseNotes);
			dispatch_async(dispatch_get_main_queue(), ^{
				if(lastDigest && ![lastDigest isEqualToData:currentDigest])
					[AboutWindowController.sharedInstance showChangesWindow:self];
				[NSUserDefaults.standardUserDefaults setObject:currentDigest forKey:kUserDefaultsReleaseNotesDigestKey];
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

	NSWindow* win = [[NSPanel alloc] initWithContentRect:rect styleMask:(NSWindowStyleMaskTitled|NSWindowStyleMaskClosable|NSWindowStyleMaskResizable|NSWindowStyleMaskMiniaturizable|NSWindowStyleMaskFullSizeContentView) backing:NSBackingStoreBuffered defer:NO];
	if((self = [super initWithWindow:win]))
	{
		self.toolbar = [[NSToolbar alloc] initWithIdentifier:@"About TextMate"];
		[self.toolbar setAllowsUserCustomization:NO];
		[self.toolbar setDisplayMode:NSToolbarDisplayModeLabelOnly];
		[self.toolbar setDelegate:self];
		[win setToolbar:self.toolbar];

		[win setTitle:@"About TextMate"];
		[win setFrameAutosaveName:@"BundlesReleaseNotes"];
		[win setDelegate:self];
		[win setAutorecalculatesKeyViewLoop:YES];
		[win setHidesOnDeactivate:NO];

		WKWebViewConfiguration* webConfig = [[WKWebViewConfiguration alloc] init];
		[webConfig.userContentController addScriptMessageHandler:self name:@"textmate"];

		self.webView = [[WKWebView alloc] initWithFrame:NSZeroRect configuration:webConfig];
		self.webView.navigationDelegate = self;
		[self.webView setValue:@NO forKey:@"drawsBackground"];

		if(NSURL* url = [NSBundle.mainBundle URLForResource:@"WKWebView" withExtension:@"js"])
		{
			NSError* error;
			if(NSMutableString* jsBridge = [NSMutableString stringWithContentsOfURL:url encoding:NSUTF8StringEncoding error:&error])
			{
				NSDictionary* variables = @{
					@"version":   [NSBundle.mainBundle objectForInfoDictionaryKey:@"CFBundleShortVersionString"],
					@"copyright": [NSBundle.mainBundle objectForInfoDictionaryKey:@"NSHumanReadableCopyright"],
					@"licensees": LicenseManager.sharedInstance.owner ?: [NSNull null],
				};

				[variables enumerateKeysAndObjectsUsingBlock:^(NSString* key, NSString* value, BOOL* stop){
					[jsBridge appendFormat:@"TextMate.%@ = %@;\n", key, [self javaScriptEscapedString:[value isEqual:[NSNull null]] ? @"" : value]];
				}];

				WKUserScript* script = [[WKUserScript alloc] initWithSource:jsBridge injectionTime:WKUserScriptInjectionTimeAtDocumentStart forMainFrameOnly:YES];
				[self.webView.configuration.userContentController addUserScript:script];
			}
			else if(error)
			{
				os_log_error(OS_LOG_DEFAULT, "Failed to load WKWebView.js: %{public}@", error.localizedDescription);
			}
		}
		else
		{
			os_log_error(OS_LOG_DEFAULT, "Failed to locate WKWebView.js in application bundle");
		}

		[self.webView.widthAnchor constraintGreaterThanOrEqualToConstant:200].active = YES;
		[self.webView.heightAnchor constraintGreaterThanOrEqualToConstant:200].active = YES;

		[win setContentView:self.webView];
	}
	return self;
}

- (void)dealloc
{
	[_webView.configuration.userContentController removeAllUserScripts];
	_webView.navigationDelegate = nil;
	[_webView stopLoading];
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
		[NSUserDefaults.standardUserDefaults setObject:Digest(releaseNotes) forKey:kUserDefaultsReleaseNotesDigestKey];
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
		[item setState:[label isEqualToString:[self.toolbar selectedItemIdentifier]] ? NSControlStateValueOn : NSControlStateValueOff];
	}
}

// =============
// = WKWebView =
// =============

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

- (void)webView:(WKWebView*)webView didFinishNavigation:(WKNavigation*)navigation
{
	if(![[self.toolbar selectedItemIdentifier] isEqualToString:@"Bundles"])
		return;

	bool first = true;
	NSMutableString* str = [NSMutableString stringWithString:@"{\"bundles\":["];
	for(Bundle* bundle in BundlesManager.sharedInstance.bundles)
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

	[self.webView evaluateJavaScript:[NSString stringWithFormat:@"setJSON(%@);", [self javaScriptEscapedString:str]] completionHandler:^(id res, NSError* error){ }];
}

- (NSString*)javaScriptEscapedString:(NSString*)src
{
	static NSRegularExpression* const regex = [NSRegularExpression regularExpressionWithPattern:@"['\"\\\\]" options:0 error:nil];
	NSString* escaped = src ? [regex stringByReplacingMatchesInString:src options:0 range:NSMakeRange(0, src.length) withTemplate:@"\\\\$0"] : @"";
	escaped = [escaped stringByReplacingOccurrencesOfString:@"\n" withString:@"\\n"];
	return [NSString stringWithFormat:@"'%@'", escaped];
}

- (void)webView:(WKWebView*)webView decidePolicyForNavigationAction:(WKNavigationAction*)navigationAction decisionHandler:(void(^)(WKNavigationActionPolicy))decisionHandler
{
	if(![navigationAction.request.URL.scheme isEqualToString:@"file"] && [NSWorkspace.sharedWorkspace openURL:navigationAction.request.URL])
			decisionHandler(WKNavigationActionPolicyCancel);
	else	decisionHandler(WKNavigationActionPolicyAllow);
}

- (void)userContentController:(WKUserContentController*)userContentController didReceiveScriptMessage:(WKScriptMessage*)message
{
	if(![message.name isEqualToString:@"textmate"])
	{
		os_log_error(OS_LOG_DEFAULT, "Message received for unknown message handler: %{public}@", message.name);
		return;
	}

	NSString* command     = message.body[@"command"];
	NSDictionary* payload = message.body[@"payload"];

	if([command isEqualToString:@"log"])
	{
		if([payload[@"level"] isEqualToString:@"error"])
		{
			static os_log_t log = os_log_create("com.macromates.JavaScript", "error");
			os_log_error(log, "%{public}@:%{public}@: %{public}@", payload[@"filename"], payload[@"lineno"], payload[@"message"]);
		}
		else
		{
			static os_log_t log = os_log_create("com.macromates.JavaScript", "log");
			os_log(log, "%{public}@: %{public}@", self.webView.title, payload[@"message"]);
		}
	}
	else if([command isEqualToString:@"addLicense"])
	{
		[LicenseManager.sharedInstance showAddLicenseWindow:self];
	}
}
@end
