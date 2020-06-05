#ifndef CF_RUN_LOOP_H_YH1AM5OH
#define CF_RUN_LOOP_H_YH1AM5OH

namespace cf
{
	struct run_loop_t
	{
		run_loop_t (CFStringRef mode = kCFRunLoopDefaultMode, double timeout = DBL_MAX);
		run_loop_t (run_loop_t const& rhs) = delete;
		run_loop_t& operator= (run_loop_t const& rhs) = delete;
		~run_loop_t ();

		bool start () const; // call from main thread
		void stop () const; // call from worker thread

		void set_timeout (double value) { _timeout = value; }

	private:
		CFStringRef _mode;
		CFRunLoopSourceRef _source;
		CFRunLoopRef _run_loop;
		bool _should_stop;
		double _timeout;
	};

} /* cf */

#endif /* end of include guard: CF_RUN_LOOP_H_YH1AM5OH */
