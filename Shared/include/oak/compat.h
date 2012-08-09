#ifndef COMPAT_H_RD1Z6YZA
#define COMPAT_H_RD1Z6YZA

#if MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5

#ifndef iconv_compat
#define iconv_compat(cd, inbuf, inbytesleft, outbuf, outbytesleft) iconv(cd, inbuf, inbytesleft, outbuf, outbytesleft)
#endif

#else /* MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5 */

#ifndef iconv_compat
#define iconv_compat(cd, inbuf, inbytesleft, outbuf, outbytesleft) iconv(cd, (const char**)inbuf, inbytesleft, outbuf, outbytesleft)
#endif

#endif /* MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5 */

#if MAC_OS_X_VERSION_MAX_ALLOWED == MAC_OS_X_VERSION_10_5
extern "C" int pthread_setname_np (const char*) WEAK_IMPORT_ATTRIBUTE;
#endif

namespace oak
{
	inline void set_thread_name (char const* threadName)
	{
#if MAC_OS_X_VERSION_MAX_ALLOWED > MAC_OS_X_VERSION_10_4
		if(pthread_setname_np != NULL)
			pthread_setname_np(threadName);
#endif
	}

} /* oak */

#endif /* end of include guard: COMPAT_H_RD1Z6YZA */
