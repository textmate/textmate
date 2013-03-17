#ifndef COMPAT_H_RD1Z6YZA
#define COMPAT_H_RD1Z6YZA

namespace oak
{
	inline void set_thread_name (char const* threadName)
	{
		pthread_setname_np(threadName);
	}

	inline size_t get_gestalt (OSType selector)
	{
		SInt32 res;
		Gestalt(selector, &res);
		return res;
	}

	inline size_t os_major () { return get_gestalt(gestaltSystemVersionMajor); }
	inline size_t os_minor () { return get_gestalt(gestaltSystemVersionMinor); }
	inline size_t os_patch () { return get_gestalt(gestaltSystemVersionBugFix); }

} /* oak */

#endif /* end of include guard: COMPAT_H_RD1Z6YZA */
