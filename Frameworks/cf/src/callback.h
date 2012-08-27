#ifndef CALLBACK_H_PCC555WY
#define CALLBACK_H_PCC555WY

#include <oak/debug.h>

namespace cf
{
	struct callback_base_t
	{
		WATCH_LEAKS(cf::callback_base_t);
		callback_base_t () : source(NULL), main_run_loop(NULL) { }

		void setup (void(*f)(void*))
		{
			main_run_loop = CFRunLoopGetCurrent();
			CFRetain(main_run_loop);

			CFRunLoopSourceContext context = { 0, this, NULL, NULL, NULL, NULL, NULL, NULL, NULL, f };
			source = CFRunLoopSourceCreate(kCFAllocatorDefault, 0, &context);
			CFRunLoopAddSource(CFRunLoopGetCurrent(), source, kCFRunLoopDefaultMode);
			CFRunLoopAddSource(CFRunLoopGetCurrent(), source, CFSTR("OakThreadSignalsRunLoopMode"));
		}

		~callback_base_t ()
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
		callback_base_t (callback_base_t const& rhs);
		callback_base_t& operator= (callback_base_t const& rhs);

		CFRunLoopSourceRef source;
		CFRunLoopRef main_run_loop;
	};

	template <typename F, typename T>
	struct callback_t : callback_base_t
	{
		callback_t (F f, T udata) : f(f), user_data(udata)
		{
			struct helper_t {
				static void wake_up (void* arg) { ((callback_t*)arg)->f(((callback_t*)arg)->user_data); }
			};
			setup(&helper_t::wake_up);
		}

	private:
		F f;
		T user_data;
	};

	template <typename T>
	struct member_callback_t : callback_base_t
	{
		member_callback_t (void(T::*f)(), T* obj) : f(f), obj(obj)
		{
			struct helper_t {
				static void wake_up (void* arg)
				{
					member_callback_t const& proxy = *(member_callback_t const*)arg;
					(proxy.obj->*proxy.f)();
				}
			};
			setup(&helper_t::wake_up);
		}

	private:
		void(T::*f)();
		T* obj;
	};

	typedef std::shared_ptr<callback_base_t> callback_ptr;

	template <typename T>
	callback_ptr create_callback (void(T::*f)(), T* obj) { return callback_ptr(new member_callback_t<T>(f, obj)); }

	template <typename T>
	callback_ptr create_callback (void(T::*f)() const, T const* obj) { return callback_ptr(new member_callback_t<T const>(f, obj)); }

	template <typename T, typename F>
	callback_ptr create_callback (F f, T udata) { return callback_ptr(new callback_t<F, T>(f, udata)); }

} /* cf */

#endif /* end of include guard: CALLBACK_H_PCC555WY */
