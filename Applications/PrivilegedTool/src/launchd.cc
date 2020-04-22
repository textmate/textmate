#include "launchd.h"
#include <launch.h>

int launchd_sockets ()
{
	int* fds = nullptr;
	size_t cnt = 0;
	int rc = launch_activate_socket("MasterSocket", &fds, &cnt);

	assert(rc == 0);
	assert(fds != nullptr);
	assert(cnt == 1);

	int res = rc == 0 && cnt == 1 ? fds[0] : -1;
	free(fds);

	return res;
}
