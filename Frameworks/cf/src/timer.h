#ifndef CF_TIMER_H_SCWA7DUD
#define CF_TIMER_H_SCWA7DUD

#include "callback.h"
#include <oak/debug.h>

namespace cf
{
	struct timer_t
	{
		WATCH_LEAKS(cf::timer_t);

		timer_t (CFAbsoluteTime fireAt, callback_ptr callback) : _callback(callback)
		{
			CFRunLoopTimerContext context = { 0, this, NULL, NULL, NULL };
			_timer = CFRunLoopTimerCreate(NULL, fireAt, 0, 0, 0, &timer_t::fire, &context);
			CFRunLoopAddTimer(CFRunLoopGetCurrent(), _timer, kCFRunLoopCommonModes);
		}

		~timer_t ()
		{
			CFRunLoopTimerInvalidate(_timer);
			CFRunLoopRemoveTimer(CFRunLoopGetCurrent(), _timer, kCFRunLoopCommonModes);
			CFRelease(_timer);
		}

	private:
		callback_ptr _callback;
		CFRunLoopTimerRef _timer;

		static void fire (CFRunLoopTimerRef timer, void* info)
		{
			((timer_t*)info)->_callback->signal();
		}
	};

	typedef std::shared_ptr<timer_t> timer_ptr;

	inline timer_ptr setup_timer (CFAbsoluteTime delay, callback_ptr callback)
	{
		return timer_ptr(new timer_t(CFAbsoluteTimeGetCurrent() + delay, callback));
	}

} /* cf */

#endif /* end of include guard: CF_TIMER_H_SCWA7DUD */
