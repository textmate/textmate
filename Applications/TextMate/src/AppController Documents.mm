#import "AppController.h"
#import <DocumentWindow/DocumentController.h>
#import "ODBEditorSuite.h"
#import <Preferences/Keys.h>
#import <OakAppKit/NSSavePanel Additions.h>
#import <OakFoundation/NSArray Additions.h>
#import <OakFoundation/NSString Additions.h>
#import <OakFoundation/OakFoundation.h>
#import <ns/ns.h>
#import <oak/CocoaSTL.h>
#import <document/collection.h>
#import <document/session.h>

OAK_DEBUG_VAR(AppController_Documents);

static NSString* const OakGlobalSessionInfo = @"OakGlobalSessionInfo";

@implementation AppController (Documents)
- (void)newDocument:(id)sender
{
	D(DBF_AppController_Documents, bug("\n"););
	[[[DocumentController alloc] init] showWindow:nil];
}

- (void)newFileBrowser:(id)sender
{
	D(DBF_AppController_Documents, bug("\n"););
	DocumentController* controller = [[DocumentController alloc] init];
	[controller window];
	controller.fileBrowserHidden = NO;
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
	[[[DocumentController alloc] init] showWindow:nil];
	return YES;
}

- (void)handleTxMtURL:(NSURL*)aURL
{
	D(DBF_AppController_Documents, bug("%s\n", [[aURL absoluteString] UTF8String]););
	if([[aURL host] isEqualToString:@"open"])
	{
		std::map<std::string, std::string> parameters;

		BOOL hadURL = NO;
		NSArray* components = [[aURL query] componentsSeparatedByString:@"&"];
		for(NSString* part in components)
		{
			NSArray* keyValue = [part componentsSeparatedByString:@"="];
			if([keyValue count] == 2)
			{
				std::string key = to_s([[keyValue firstObject] stringByReplacingPercentEscapesUsingEncoding:NSUTF8StringEncoding]);
				NSURL* fileURL = key == "url" ? [NSURL URLWithString:[keyValue lastObject]] : nil;
				parameters[key] = to_s([fileURL isFileURL] ? [fileURL path] : [[keyValue lastObject] stringByReplacingPercentEscapesUsingEncoding:NSUTF8StringEncoding]);
				hadURL |= (key == "url");
			}
		}
		
		if(!hadURL && [[NSApp orderedWindows]count] > 0)
		{
			NSWindow* window=[[NSApp orderedWindows]objectAtIndex:0];
			DocumentController* controller = (DocumentController*)[window delegate];
			if([controller isKindOfClass:[DocumentController class]] && !controller->documentTabs.empty())
			{
				NSString* path=[controller documentFilePath];
				
				if(path && [path length] > 0) parameters["url"] = to_s(path);
			}
		}

		std::map<std::string, std::string>::const_iterator const& url     = parameters.find("url");
		std::map<std::string, std::string>::const_iterator const& line    = parameters.find("line");
		std::map<std::string, std::string>::const_iterator const& column  = parameters.find("column");
		std::map<std::string, std::string>::const_iterator const& project = parameters.find("project");

		if(url != parameters.end())
		{
			std::string const& path = url->second;
			if(path::is_directory(path))
			{
				document::show_browser(path);
			}
			else if(path::exists(path))
			{
				text::range_t range = text::range_t::undefined;
				size_t col = column != parameters.end() ? atoi(column->second.c_str()) : 1;
				if(line != parameters.end())
					range = text::pos_t(atoi(line->second.c_str())-1, col-1);

				document::document_ptr doc = document::create(path);
				doc->set_recent_tracking(false);
				document::show(doc, project != parameters.end() ? oak::uuid_t(project->second) : document::kCollectionCurrent, range);
			}
			else
			{
				NSRunAlertPanel(@"File Does not Exist", @"The item “%@” does not exist.", @"Continue", nil, nil, [NSString stringWithCxxString:path]);
			}
		}
		else
		{
			NSRunAlertPanel(@"Missing Parameter", @"You need to provide the URL parameter.", @"Continue", nil, nil);
		}
	}
	else
	{
		NSRunAlertPanel(@"Unknown URL Scheme", @"This version of TextMate does not support “%@” in its URL scheme.", @"Continue", nil, nil, [aURL host]);
	}
}

- (BOOL)applicationShouldHandleReopen:(NSApplication*)theApplication hasVisibleWindows:(BOOL)flag
{
	D(DBF_AppController_Documents, bug("%s\n", BSTR(flag)););
	BOOL disableUntitledAtReactivationPrefs = [[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsDisableNewDocumentAtReactivationKey];
	return !disableUntitledAtReactivationPrefs;
}

// =====================
// = Load/Save Session =
// =====================

- (void)windowNotificationActual:(id)sender
{
	document::schedule_session_backup();
}

- (void)windowNotification:(NSNotification*)aNotification
{
	D(DBF_AppController_Documents, bug("%s\n", [[aNotification description] UTF8String]););
	[self performSelector:@selector(windowNotificationActual:) withObject:nil afterDelay:0]; // A deadlock happens if we receive a notification while a sheet is closing and we save session (since session saving schedules a timer with the run loop, and the run loop is in a special state when a sheet is up, or something like that --Allan)
}

- (BOOL)loadSession:(id)sender
{
	BOOL res = document::load_session();

	static NSString* const WindowNotifications[] = { NSWindowDidBecomeKeyNotification, NSWindowDidDeminiaturizeNotification, NSWindowDidExposeNotification, NSWindowDidMiniaturizeNotification, NSWindowDidMoveNotification, NSWindowDidResizeNotification, NSWindowWillCloseNotification };
	iterate(notification, WindowNotifications)
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(windowNotification:) name:*notification object:nil];

	return res;
}

// ===========================
// = Application Termination =
// ===========================

- (void)closeAllWindows:(id)sender
{
	D(DBF_AppController_Documents, bug("\n"););
	NSAutoreleasePool* pool = [NSAutoreleasePool new];
	for(NSWindow* window in [NSApp windows])
	{
		if(window.isVisible)
			[window close];
	}
	[pool release];
}

- (NSApplicationTerminateReply)applicationShouldTerminate:(NSApplication*)sender
{
	D(DBF_AppController_Documents, bug("%s\n", [NSApp windows].description.UTF8String););
	for(NSWindow* window in [NSApp orderedWindows])
	{
		DocumentController* delegate = (DocumentController*)[window delegate];
		if([delegate isKindOfClass:[DocumentController class]])
			return [delegate applicationShouldTerminate:sender];
	}

	document::save_session(false);
	return NSTerminateNow;
}
@end
