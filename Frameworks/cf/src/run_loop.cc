#include "run_loop.h"
#include <oak/duration.h>

namespace cf
{
	run_loop_t::run_loop_t (CFStringRef mode, double timeout) : _should_stop(false), _timeout(timeout)
	{
		struct helper_t {
			static void wake_up (void* arg) { static_cast<run_loop_t*>(arg)->_should_stop = true; }
		};

		_mode = mode;
		CFRetain(_mode);
		_run_loop = CFRunLoopGetCurrent();
		CFRetain(_run_loop);

		CFRunLoopSourceContext context = { 0, this, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, &helper_t::wake_up };
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
			SInt32 rc = CFRunLoopRunInMode(_mode, std::min(10.0, _timeout - timer.duration()), true);
			if(_should_stop && rc != kCFRunLoopRunTimedOut)
				break;

			if(_should_stop)
				os_log_error(OS_LOG_DEFAULT, "Command completed but run-loop was not stopped.");

			if(_should_stop || _timeout <= timer.duration())
				return false;
		}
		return true;
	}

	void run_loop_t::stop () const
	{
		CFRunLoopSourceSignal(_source);
		CFRunLoopWakeUp(_run_loop);
	}

} /* cf */
