#import <cf/callback.h>
#import <oak/compat.h>
#import <oak/debug.h>
#import <OakSystem/application.h>
#import <DocumentWindow/DocumentController.h>
#import <io/path.h>
#import <text/format.h>
#import <crash/info.h>

static void sig_usr1_handler (void* unused)
{
	fprintf(stderr, "%s received SIGUSR1: Relaunch.\n", getprogname());
	oak::application_t::relaunch();
}

static void sig_int_handler (void* unused)
{
	fprintf(stderr, "%s received SIGINT: Regular shutdown.\n", getprogname());
	[NSApp terminate:nil];
}

static void sig_term_handler (void* unused)
{
	fprintf(stderr, "%s received SIGTERM: Quick shutdown.\n", getprogname());
	[DocumentController saveSessionIncludingUntitledDocuments:YES];
	[[NSNotificationCenter defaultCenter] postNotificationName:NSApplicationWillTerminateNotification object:NSApp];
	[NSApp stop:nil];
	[NSApp postEvent:[NSEvent otherEventWithType:NSApplicationDefined location:NSZeroPoint modifierFlags:0 timestamp:0 windowNumber:0 context:NULL subtype:0 data1:0 data2:0] atStart:NO];
	[[NSUserDefaults standardUserDefaults] synchronize];
}

int main (int argc, char const* argv[])
{
	curl_global_init(CURL_GLOBAL_ALL);

	oak::application_t::set_support(path::join(path::home(), "Library/Application Support/TextMate"));
	oak::application_t app(argc, argv, true);

	signal(SIGUSR1, SIG_IGN);
	signal(SIGINT,  SIG_IGN);
	signal(SIGTERM, SIG_IGN);
	signal(SIGPIPE, SIG_IGN);

	dispatch_source_t sigTermSrc = dispatch_source_create(DISPATCH_SOURCE_TYPE_SIGNAL, SIGTERM, 0, dispatch_get_main_queue());
	dispatch_source_set_event_handler_f(sigTermSrc, &sig_term_handler);
	dispatch_resume(sigTermSrc);

	dispatch_source_t sigIntSrc = dispatch_source_create(DISPATCH_SOURCE_TYPE_SIGNAL, SIGINT, 0, dispatch_get_main_queue());
	dispatch_source_set_event_handler_f(sigIntSrc, &sig_int_handler);
	dispatch_resume(sigIntSrc);

	dispatch_source_t sigUsr1Src = dispatch_source_create(DISPATCH_SOURCE_TYPE_SIGNAL, SIGUSR1, 0, dispatch_get_main_queue());
	dispatch_source_set_event_handler_f(sigUsr1Src, &sig_usr1_handler);
	dispatch_resume(sigUsr1Src);

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
		crash_reporter_info_t info(text::format("C++ Exception: %s", e.what()));
		abort();
	}
}
