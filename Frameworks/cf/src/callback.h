#ifndef CALLBACK_H_PCC555WY
#define CALLBACK_H_PCC555WY

#include <oak/debug.h>

namespace cf
{
	struct callback_t
	{
		WATCH_LEAKS(cf::callback_t);

		callback_t (callback_t const& rhs) = delete;
		callback_t& operator= (callback_t const& rhs) = delete;

		callback_t (std::function<void()> const& callback) : source(NULL), main_run_loop(NULL), callback(callback)
		{
			main_run_loop = CFRunLoopGetCurrent();
			CFRetain(main_run_loop);

			CFRunLoopSourceContext context = { 0, this, NULL, NULL, NULL, NULL, NULL, NULL, NULL, &callback_t::invoke_callback };
			source = CFRunLoopSourceCreate(kCFAllocatorDefault, 0, &context);
			CFRunLoopAddSource(CFRunLoopGetCurrent(), source, kCFRunLoopDefaultMode);
			CFRunLoopAddSource(CFRunLoopGetCurrent(), source, CFSTR("OakThreadSignalsRunLoopMode"));
		}

		~callback_t ()
		{
			if(main_run_loop && source)
			{
				CFRunLoopRemoveSource(CFRunLoopGetCurrent(), source, CFSTR("OakThreadSignalsRunLoopMode"));
				CFRunLoopRemoveSource(CFRunLoopGetCurrent(), source, kCFRunLoopDefaultMode);
				CFRelease(source);
				CFRelease(main_run_loop);
			}
		}

		void signal ()
		{
			CFRunLoopSourceSignal(source);
			CFRunLoopWakeUp(main_run_loop);
		}

	private:
		static void invoke_callback (void* cb)
		{
			((callback_t const*)cb)->callback();
		}

		CFRunLoopSourceRef source;
		CFRunLoopRef main_run_loop;
		std::function<void()> callback;
	};

	typedef std::shared_ptr<callback_t> callback_ptr;
	inline callback_ptr create_callback (std::function<void()> const& cb) { return std::make_shared<callback_t>(cb); }

} /* cf */

#endif /* end of include guard: CALLBACK_H_PCC555WY */
