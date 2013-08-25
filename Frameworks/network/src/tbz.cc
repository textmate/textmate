#include "tbz.h"
#include <text/format.h>
#include <text/trim.h>
#include <oak/compat.h>

namespace network
{
	tbz_t::tbz_t (std::string const& dest)
	{
		if(_group = dispatch_group_create())
		{
			if(_process = io::spawn(std::vector<std::string>{ "/usr/bin/tar", "-jxmkC", dest, "--strip-components", "1" }))
			{
				dispatch_group_async(_group, dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
					io::exhaust_fd(_process.out, &_output);
				});
				dispatch_group_async(_group, dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
					io::exhaust_fd(_process.err, &_error);
				});
				dispatch_group_async(_group, dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
					if(waitpid(_process.pid, &_status, 0) != _process.pid)
						perror("waitpid");
				});
			}
		}
	}

	tbz_t::~tbz_t ()
	{
		if(_group)
		{
			dispatch_group_wait(_group, DISPATCH_TIME_FOREVER);
			dispatch_release(_group);
		}
	}

	bool tbz_t::wait_for_tbz (std::string* output, std::string* error)
	{
		if(!_process)
			return false;

		close(_process.in);
		dispatch_group_wait(_group, DISPATCH_TIME_FOREVER);

		if(output)
			output->swap(_output);
		if(error)
			output->swap(_error);

		return WIFEXITED(_status) && WEXITSTATUS(_status) == 0;
	}

} /* network */