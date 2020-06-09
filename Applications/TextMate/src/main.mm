#import <oak/debug.h>
#import <OakSystem/application.h>
#import <DocumentWindow/DocumentWindowController.h>
#import <io/path.h>
#import <text/format.h>
#import <crash/info.h>

static void sig_int_handler (void* unused)
{
	os_log(OS_LOG_DEFAULT, "Received SIGINT: Regular shutdown.");
	[NSApp terminate:nil];
}

static void sig_term_handler (void* unused)
{
	os_log(OS_LOG_DEFAULT, "Received SIGTERM: Quick shutdown.");
	[DocumentWindowController saveSessionIncludingUntitledDocuments:YES];
	[NSNotificationCenter.defaultCenter postNotificationName:NSApplicationWillTerminateNotification object:NSApp];
	[NSApp stop:nil];
	[NSApp postEvent:[NSEvent otherEventWithType:NSEventTypeApplicationDefined location:NSZeroPoint modifierFlags:0 timestamp:0 windowNumber:0 context:NULL subtype:0 data1:0 data2:0] atStart:NO];
	[NSUserDefaults.standardUserDefaults synchronize];
}

int main (int argc, char const* argv[])
{
	oak::application_t::set_support(path::join(path::home(), "Library/Application Support/TextMate"));
	oak::application_t app(argc, argv);

	signal(SIGINT,  SIG_IGN);
	signal(SIGTERM, SIG_IGN);
	signal(SIGPIPE, SIG_IGN);

	dispatch_source_t sigTermSrc = dispatch_source_create(DISPATCH_SOURCE_TYPE_SIGNAL, SIGTERM, 0, dispatch_get_main_queue());
	dispatch_source_set_event_handler_f(sigTermSrc, &sig_term_handler);
	dispatch_resume(sigTermSrc);

	dispatch_source_t sigIntSrc = dispatch_source_create(DISPATCH_SOURCE_TYPE_SIGNAL, SIGINT, 0, dispatch_get_main_queue());
	dispatch_source_set_event_handler_f(sigIntSrc, &sig_int_handler);
	dispatch_resume(sigIntSrc);

	@autoreleasepool {
		for(NSString* variable in [[[NSProcessInfo processInfo] environment] allKeys])
		{
			if([variable hasPrefix:@"TM_"])
				unsetenv([variable UTF8String]);
		}
	}

	try {
		return NSApplicationMain(argc, argv);
	}
	catch(std::exception const& e) {
		crash_reporter_info_t info("C++ Exception: %s", e.what());
		abort();
	}
}
