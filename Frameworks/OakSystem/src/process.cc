#include "process.h"
#include <oak/oak.h>
#include <oak/server.h>

namespace oak
{
	struct kill_process_group_in_background_t
	{
		kill_process_group_in_background_t (pid_t groupId)
		{
			_client_key = _server.register_client(this);
			_server.send_request(_client_key, groupId);
		}

		~kill_process_group_in_background_t ()
		{
			_server.unregister_client(_client_key);
		}

		static bool handle_request (int groupId)
		{
			static int const signals[] = { SIGINT, SIGTERM, SIGKILL };
			iterate(signal, signals)
			{
				// TODO if the process exits on SIGINT, it still might have offspring which didn’t
				if(killpg(groupId, *signal) != 0 && errno == ESRCH)
					return true;
				sleep(1);
			}
			return false;
		}

		void handle_reply (bool success)
		{
			delete this;
		}

	private:
		size_t _client_key;
		oak::server_t<kill_process_group_in_background_t, int> _server;
	};

	void kill_process_group_in_background (pid_t groupId)
	{
		new kill_process_group_in_background_t(groupId);
	}

} /* oak */ 
