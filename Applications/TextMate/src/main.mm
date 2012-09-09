#import <cf/callback.h>
#import <oak/compat.h>
#import <oak/debug.h>
#import <OakSystem/application.h>
#import <document/collection.h>
#import <io/path.h>

static void sig_int_handler ()
{
	fprintf(stderr, "%s received SIGINT: Regular shutdown.\n", getprogname());
	[NSApp terminate:nil];
}

static void sig_term_handler ()
{
	fprintf(stderr, "%s received SIGTERM: Quick shutdown.\n", getprogname());
	document::save_session(true);
	[[NSNotificationCenter defaultCenter] postNotificationName:NSApplicationWillTerminateNotification object:NSApp];
	[NSApp stop:nil];
	[NSApp postEvent:[NSEvent otherEventWithType:NSApplicationDefined location:NSZeroPoint modifierFlags:0 timestamp:0 windowNumber:0 context:NULL subtype:0 data1:0 data2:0] atStart:NO];
	[[NSUserDefaults standardUserDefaults] synchronize];
}

static cf::callback_ptr SigIntSource  = cf::create_callback(&sig_int_handler);
static cf::callback_ptr SigTermSource = cf::create_callback(&sig_term_handler);

void* signal_handler_thread (void* userdata)
{
	oak::set_thread_name("main::signal_handler");

	sigset_t sigs;
	sigemptyset(&sigs);
	sigaddset(&sigs, SIGINT);
	sigaddset(&sigs, SIGTERM);

	int receivedSignal;
	while(sigwait(&sigs, &receivedSignal) != -1)
	{
		switch(receivedSignal)
		{
			case SIGINT:  SigIntSource->signal();  break;
			case SIGTERM: SigTermSource->signal(); break;
		}
	}
	perror("sigwait()");
	return NULL;
}

int main (int argc, char const* argv[])
{
	curl_global_init(CURL_GLOBAL_ALL);

	oak::application_t::set_support(path::join(path::home(), "Library/Application Support/TextMate"));
	oak::application_t app(argc, argv);
	signal(SIGINT,  SIG_DFL);
	signal(SIGTERM, SIG_DFL);
	signal(SIGPIPE, SIG_IGN);

	sigset_t sigs;
	sigemptyset(&sigs);
	sigaddset(&sigs, SIGINT);
	sigaddset(&sigs, SIGTERM);
	if(pthread_sigmask(SIG_BLOCK, &sigs, NULL) == -1)
		perror("pthread_sigmask()");

	pthread_t thread;
	if(pthread_create(&thread, NULL, &signal_handler_thread, NULL) == 0)
			pthread_detach(thread);
	else	perror("pthread_create()");

	NSAutoreleasePool* pool = [NSAutoreleasePool new];
	for(NSString* variable in [[[NSProcessInfo processInfo] environment] allKeys])
	{
		if([variable hasPrefix:@"TM_"])
			unsetenv([variable UTF8String]);
	}
	[pool release];

	return NSApplicationMain(argc, argv);
}
