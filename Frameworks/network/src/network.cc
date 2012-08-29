#include "network.h"

namespace network
{
	bool can_reach_host (char const* host)
	{
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