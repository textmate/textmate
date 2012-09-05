#ifndef COMPAT_H_RD1Z6YZA
#define COMPAT_H_RD1Z6YZA

namespace oak
{
	inline void set_thread_name (char const* threadName)
	{
		pthread_setname_np(threadName);
	}

} /* oak */

#endif /* end of include guard: COMPAT_H_RD1Z6YZA */
