#include "network.h"

__attribute__((constructor)) static void setup_curl ()
{
	curl_global_init(CURL_GLOBAL_ALL);
}

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
