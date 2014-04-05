#include "run_loop.h"
#include <oak/duration.h>
#include <oak/debug.h>

namespace cf
{
	run_loop_t::run_loop_t (CFStringRef mode, double timeout, bool debug) : _should_stop(false), _timeout(timeout), _debug(debug)
	{
		struct helper_t {
			static void wake_up (void* arg) {
				if(((run_loop_t*)arg)->_debug)
					fprintf(stderr, "debug: main: run-loop source called (should stop)\n");
				((run_loop_t*)arg)->_should_stop = true;
			}
		};

		_mode = mode;
		CFRetain(_mode);
		_run_loop = CFRunLoopGetCurrent();
		CFRetain(_run_loop);

		CFRunLoopSourceContext context = { 0, this, NULL, NULL, NULL, NULL, NULL, NULL, NULL, &helper_t::wake_up };
		_source = CFRunLoopSourceCreate(kCFAllocatorDefault, 0, &context);
		CFRunLoopAddSource(_run_loop, _source, _mode);
	}

	run_loop_t::~run_loop_t ()
	{
		CFRunLoopRemoveSource(_run_loop, _source, _mode);
		CFRelease(_source);
		CFRelease(_run_loop);
		CFRelease(_mode);
	}

	bool run_loop_t::start () const
	{
		oak::duration_t timer;
		while(true)
		{
			if(_debug)
				fprintf(stderr, "debug: main: entering CFRunLoopRunInMode(timeout %.1f seconds) (should stop %s)\n", std::min(10.0, _timeout - timer.duration()), BSTR(_should_stop));
			SInt32 rc = CFRunLoopRunInMode(_mode, std::min(10.0, _timeout - timer.duration()), true);
			if(_debug)
				fprintf(stderr, "debug: main: CFRunLoopRunInMode() returned (timeout reached %s, should keep running %s)\n", BSTR(rc == kCFRunLoopRunTimedOut), BSTR(!_should_stop));
			if(_should_stop && rc != kCFRunLoopRunTimedOut)
				break;

			if(_should_stop)
				fprintf(stderr, "*** command completed but run-loop was not stopped.\n");

			if(_should_stop || _timeout <= timer.duration())
				return false;
		}
		return true;
	}

	void run_loop_t::stop () const
	{
		if(_debug)
			fprintf(stderr, "debug: stop: signal run loop source\n");
		CFRunLoopSourceSignal(_source);
		if(_debug)
			fprintf(stderr, "debug: stop: wake-up run loop\n");
		CFRunLoopWakeUp(_run_loop);
	}
	
} /* cf */
