#include "network.h"

namespace network
{
	bool can_reach_host (char const* host)
	{
#if !defined(MAC_OS_X_VERSION_10_6) || (MAC_OS_X_VERSION_MAX_ALLOWED < MAC_OS_X_VERSION_10_6)
		SCNetworkConnectionFlags flags;
		return SCNetworkCheckReachabilityByName(host, &flags) && (flags & kSCNetworkFlagsReachable);
#else
		bool res = false;
		if(SCNetworkReachabilityRef ref = SCNetworkReachabilityCreateWithName(kCFAllocatorDefault, host))
		{
			SCNetworkReachabilityFlags flags;
			if(SCNetworkReachabilityGetFlags(ref, &flags))
			{
				if(flags & kSCNetworkReachabilityFlagsReachable)
					res = true;
			}
			CFRelease(ref);
		}
		return res;
#endif
	}

} /* network */