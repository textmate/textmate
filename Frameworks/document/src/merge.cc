#include "merge.h"
#include <io/src/io.h>

std::string merge (std::string const& oldContent, std::string const& myContent, std::string const& yourContent, bool* conflict)
{
	std::string oldFile  = path::temp("merge", oldContent);
	std::string myFile   = path::temp("merge", myContent);
	std::string yourFile = path::temp("merge", yourContent);

	ASSERT(oldFile != NULL_STR && myFile != NULL_STR && yourFile != NULL_STR);

	__block int status = 0;
	__block std::string output, error;

	if(io::process_t process = io::spawn(std::vector<std::string>{ "/usr/bin/diff3", "-Em", "-LLocal Changes", "-LOld File", "-LExternal Changes", myFile, oldFile, yourFile }, oak::basic_environment()))
	{
		close(process.in);

		dispatch_group_t group = dispatch_group_create();
		dispatch_group_async(group, dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
			io::exhaust_fd(process.out, &output);
		});
		dispatch_group_async(group, dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
			io::exhaust_fd(process.err, &error);
		});
		dispatch_group_async(group, dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
			if(waitpid(process.pid, &status, 0) != process.pid)
				perror("merge: waitpid");
		});
		dispatch_group_wait(group, DISPATCH_TIME_FOREVER);
		dispatch_release(group);

		if(conflict)
			*conflict = WIFEXITED(status) && WEXITSTATUS(status) != 0;
	}

	unlink(oldFile.c_str());
	unlink(myFile.c_str());
	unlink(yourFile.c_str());

	return WIFEXITED(status) ? output : NULL_STR;
}
