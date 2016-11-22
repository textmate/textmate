#import "AppController.h"
#import <DocumentWindow/DocumentWindowController.h>
#import "ODBEditorSuite.h"
#import <Preferences/Keys.h>
#import <OakAppKit/NSSavePanel Additions.h>
#import <OakFoundation/NSString Additions.h>
#import <ns/ns.h>
#import <text/decode.h>
#import <document/OakDocument.h>
#import <document/OakDocumentController.h>

OAK_DEBUG_VAR(AppController_Documents);

@implementation AppController (Documents)
- (void)newDocument:(id)sender
{
	D(DBF_AppController_Documents, bug("\n"););
	[[DocumentWindowController new] showWindow:self];
}

- (void)newFileBrowser:(id)sender
{
	D(DBF_AppController_Documents, bug("\n"););
	NSString* urlString = [[NSUserDefaults standardUserDefaults] stringForKey:kUserDefaultsInitialFileBrowserURLKey];
	NSURL* url          = urlString ? [NSURL URLWithString:urlString] : nil;

	DocumentWindowController* controller = [DocumentWindowController new];
	controller.defaultProjectPath = [url isFileURL] ? [url path] : NSHomeDirectory();
	controller.fileBrowserVisible = YES;
	[controller showWindow:self];
}

- (void)openDocument:(id)sender
{
	D(DBF_AppController_Documents, bug("\n"););

	NSOpenPanel* openPanel = [NSOpenPanel openPanel];
	openPanel.allowsMultipleSelection         = YES;
	openPanel.canChooseDirectories            = YES;
	openPanel.canChooseFiles                  = YES;
	openPanel.treatsFilePackagesAsDirectories = YES;
	openPanel.title                           = [NSString stringWithFormat:@"%@: Open", [[[NSBundle mainBundle] localizedInfoDictionary] valueForKey: @"CFBundleName"] ?: [[NSProcessInfo processInfo] processName]];

	[openPanel setShowsHiddenFilesCheckBox:YES];
	if([openPanel runModal] == NSOKButton)
	{
		NSMutableArray* filenames = [NSMutableArray array];
		for(NSURL* url in [openPanel URLs])
			[filenames addObject:[[url filePathURL] path]];

		OakOpenDocuments(filenames);
	}
}

- (BOOL)application:(NSApplication*)theApplication openFile:(NSString*)aPath
{
	D(DBF_AppController_Documents, bug("%s\n", [aPath UTF8String]););
	if(!DidHandleODBEditorEvent([[[NSAppleEventManager sharedAppleEventManager] currentAppleEvent] aeDesc]))
		OakOpenDocuments(@[ aPath ]);
	return YES;
}

- (void)application:(NSApplication*)sender openFiles:(NSArray*)filenames
{
	D(DBF_AppController_Documents, bug("%s\n", [[filenames description] UTF8String]););
	if(!DidHandleODBEditorEvent([[[NSAppleEventManager sharedAppleEventManager] currentAppleEvent] aeDesc]))
		OakOpenDocuments(filenames);
	[sender replyToOpenOrPrint:NSApplicationDelegateReplySuccess];
}

- (BOOL)applicationOpenUntitledFile:(NSApplication*)theApplication
{
	D(DBF_AppController_Documents, bug("\n"););
	if([[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsShowFavoritesInsteadOfUntitledKey])
			[self openFavorites:self];
	else	[self newDocument:self];
	return YES;
}

- (void)handleTxMtURL:(NSURL*)aURL
{
	D(DBF_AppController_Documents, bug("%s\n", [[aURL absoluteString] UTF8String]););
	if([[aURL host] isEqualToString:@"open"])
	{
		std::map<std::string, std::string> parameters;
		for(NSString* part in [[aURL query] componentsSeparatedByString:@"&"])
		{
			NSArray* keyValue = [part componentsSeparatedByString:@"="];
			if([keyValue count] == 2)
			{
				std::string key = decode::url_part(to_s([keyValue firstObject]));
				parameters[key] = decode::url_part(to_s([keyValue lastObject]));
			}
		}

		std::map<std::string, std::string>::const_iterator const& url     = parameters.find("url");
		std::map<std::string, std::string>::const_iterator const& uuid    = parameters.find("uuid");
		std::map<std::string, std::string>::const_iterator const& line    = parameters.find("line");
		std::map<std::string, std::string>::const_iterator const& column  = parameters.find("column");
		std::map<std::string, std::string>::const_iterator const& project = parameters.find("project");

		text::range_t range = text::range_t::undefined;
		size_t col = column != parameters.end() ? atoi(column->second.c_str()) : 1;
		if(line != parameters.end())
			range = text::pos_t(atoi(line->second.c_str())-1, col-1);

		NSUUID* projectUUID = project != parameters.end() ? [[NSUUID alloc] initWithUUIDString:to_ns(project->second)] : nil;
		if(url != parameters.end())
		{
			static std::string const kTildeURLPrefixes[] = { "file://localhost/~/", "file:///~/", "file://~/" };
			static std::string const kRootURLPrefixes[]  = { "file://localhost/", "file:///" };

			std::string const& urlStr = url->second;
			std::string path = NULL_STR;

			for(auto root : kRootURLPrefixes)
			{
				if(urlStr.find(root) == 0)
					path = path::join("/", urlStr.substr(root.size()));
			}

			for(auto tilde : kTildeURLPrefixes)
			{
				if(urlStr.find(tilde) == 0)
					path = path::join(path::home(), urlStr.substr(tilde.size()));
			}

			if(path == NULL_STR && urlStr.find("file://") == 0)
				path = path::join(path::home(), urlStr.substr(std::string("file://").size()));

			if(path::is_directory(path))
			{
				[OakDocumentController.sharedInstance showFileBrowserAtPath:to_ns(path)];
			}
			else if(path::exists(path))
			{
				OakDocument* doc = [OakDocumentController.sharedInstance documentWithPath:to_ns(path)];
				doc.recentTrackingDisabled = YES;
				[OakDocumentController.sharedInstance showDocument:doc andSelect:range inProject:projectUUID bringToFront:YES];
			}
			else
			{
				NSAlert* alert        = [[NSAlert alloc] init];
				alert.messageText     = @"File Does not Exist";
				alert.informativeText = [NSString stringWithFormat:@"The item “%@” does not exist.", [NSString stringWithCxxString:path]];
				[alert addButtonWithTitle:@"Continue"];
				[alert runModal];
			}
		}
		else if(uuid != parameters.end())
		{
			if(OakDocument* doc = [OakDocumentController.sharedInstance findDocumentWithIdentifier:[[NSUUID alloc] initWithUUIDString:to_ns(uuid->second)]])
			{
				doc.recentTrackingDisabled = YES;
				[OakDocumentController.sharedInstance showDocument:doc andSelect:range inProject:projectUUID bringToFront:YES];
			}
			else
			{
				NSAlert* alert        = [[NSAlert alloc] init];
				alert.messageText     = @"File Does not Exist";
				alert.informativeText = [NSString stringWithFormat:@"No document found for UUID %@.", [NSString stringWithCxxString:uuid->second]];
				[alert addButtonWithTitle:@"Continue"];
				[alert runModal];
			}
		}
		else if(range != text::range_t::undefined)
		{
			for(NSWindow* win in [NSApp orderedWindows])
			{
				BOOL foundTextView = [[win firstResponder] tryToPerform:@selector(setSelectionString:) with:[NSString stringWithCxxString:range]];
				if(!foundTextView)
				{
					NSMutableArray* allViews = [[[win contentView] subviews] mutableCopy];
					for(NSUInteger i = 0; i < [allViews count]; ++i)
						[allViews addObjectsFromArray:[[allViews objectAtIndex:i] subviews]];

					for(NSView* view in allViews)
					{
						if([view respondsToSelector:@selector(setSelectionString:)])
						{
							[view performSelector:@selector(setSelectionString:) withObject:[NSString stringWithCxxString:range]];
							[win makeFirstResponder:view];
							foundTextView = YES;
							break;
						}
					}
				}

				if(foundTextView)
				{
					[win makeKeyAndOrderFront:self];
					break;
				}
			}
		}
		else
		{
			NSAlert* alert        = [[NSAlert alloc] init];
			alert.messageText     = @"Missing Parameter";
			alert.informativeText = [NSString stringWithFormat:@"You need to provide either a (file) url or line parameter. The URL given was: ‘%@’.", aURL];
			[alert addButtonWithTitle:@"Continue"];
			[alert runModal];
		}
	}
	else
	{
		NSAlert* alert        = [[NSAlert alloc] init];
		alert.messageText     = @"Unknown URL Scheme";
		alert.informativeText = [NSString stringWithFormat:@"This version of TextMate does not support “%@” in its URL scheme.", [aURL host]];
		[alert addButtonWithTitle:@"Continue"];
		[alert runModal];
	}
}

- (BOOL)applicationShouldHandleReopen:(NSApplication*)theApplication hasVisibleWindows:(BOOL)flag
{
	D(DBF_AppController_Documents, bug("%s\n", BSTR(flag)););
	BOOL disableUntitledAtReactivationPrefs = [[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsDisableNewDocumentAtReactivationKey];
	BOOL showFavoritesInsteadPrefs          = [[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsShowFavoritesInsteadOfUntitledKey];
	return flag || !disableUntitledAtReactivationPrefs || showFavoritesInsteadPrefs;
}

// ===========================
// = Application Termination =
// ===========================

- (NSApplicationTerminateReply)applicationShouldTerminate:(NSApplication*)sender
{
	D(DBF_AppController_Documents, bug("%s\n", [NSApp windows].description.UTF8String););
	return [DocumentWindowController applicationShouldTerminate:sender];
}
@end
