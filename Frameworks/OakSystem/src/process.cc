#include "process.h"
#include <oak/oak.h>

namespace oak
{
	void kill_process_group_in_background (pid_t groupId)
	{
		dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
			static int const signals[] = { SIGINT, SIGTERM, SIGKILL };
			for(int signal : signals)
			{
				// TODO if the process exits on SIGINT, it still might have offspring which didn’t
				if(killpg(groupId, signal) != 0 && errno == ESRCH)
					break;
				sleep(1);
			}
		});
	}

} /* oak */ 
