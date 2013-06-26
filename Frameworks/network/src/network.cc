#include "network.h"

namespace network
{
	bool can_reach_host (char const* host)
	{
		if(char const* realHost = strstr(host, "://"))
			host = realHost + 3;

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
	}

} /* network */