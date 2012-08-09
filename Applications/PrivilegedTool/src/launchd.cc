#include "launchd.h"
#include <launch.h> // There is no <x-man-page://3/launch> page <rdar://7072543>
#include <oak/debug.h>

OAK_DEBUG_VAR(AuthServer_Launchd);

int get_socket (launch_data_t dict, char const* name)
{
	launch_data_t array = launch_data_dict_lookup(dict, name);

	if(array == NULL || (launch_data_get_type(array) == LAUNCH_DATA_ARRAY && launch_data_array_get_count(array) == 0))
	{
		D(DBF_AuthServer_Launchd, bug("no activity for %s\n", name););
		return -1;
	}

	assert(array != NULL && launch_data_get_type(array) == LAUNCH_DATA_ARRAY);
	D(DBF_AuthServer_Launchd, bug("handle %zu sockets (%s)", launch_data_array_get_count(array), name););
	launch_data_t fdData = launch_data_array_get_index(array, 0);
	D(DBF_AuthServer_Launchd, bug("data %p", fdData););
	assert(fdData != NULL && launch_data_get_type(fdData) == LAUNCH_DATA_FD);
	int fd = launch_data_get_fd(fdData);
	D(DBF_AuthServer_Launchd, bug("fd %d", fd););
	assert(fd >= 0);

	return fd;
}

int launchd_sockets ()
{
	launch_data_t req = launch_data_new_string(LAUNCH_KEY_CHECKIN);
	assert(req != NULL);
	launch_data_t res = launch_msg(req);
	assert(res != NULL && launch_data_get_type(res) == LAUNCH_DATA_DICTIONARY); // LAUNCH_DATA_ERRNO
	launch_data_t allSockets = launch_data_dict_lookup(res, LAUNCH_JOBKEY_SOCKETS);
	assert(allSockets != NULL && launch_data_get_type(allSockets) == LAUNCH_DATA_DICTIONARY);

	return get_socket(allSockets, "MasterSocket");
}
