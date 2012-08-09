#include "merge.h"
#include <OakSystem/command.h>
#include <io/io.h>
#include <cf/run_loop.h>
#include <text/format.h>

static std::string save (std::string const& buf)
{
	std::string res = NULL_STR;

	std::string dst = path::join(path::temp(), "textmate_merge.XXXXXX");
	if(int fd = mkstemp(&dst[0]))
	{
		if(write(fd, buf.data(), buf.size()) == buf.size())
			res = dst;
		close(fd);
	}

	return res;
}

std::string merge (std::string const& oldContent, std::string const& myContent, std::string const& yourContent, bool* conflict)
{
	struct command_t : oak::command_t
	{
		int return_code;
		cf::run_loop_t run_loop;
		std::string result;

		void did_exit (int rc, std::string const& output, std::string const& error)
		{
			if(rc == 0 || rc == 1)
				result = output;
			return_code = rc;
			run_loop.stop();
		}
	};

	std::string oldFile  = save(oldContent);
	std::string myFile   = save(myContent);
	std::string yourFile = save(yourContent);

	ASSERT(oldFile != NULL_STR && myFile != NULL_STR && yourFile != NULL_STR);

	command_t cmd;
	cmd.environment = oak::basic_environment();
	cmd.command     = text::format("#!/bin/sh\n/usr/bin/diff3 -Em -L 'Local Changes' -L 'Old File' -L 'External Changes' '%s' '%s' '%s'", myFile.c_str(), oldFile.c_str(), yourFile.c_str());
	cmd.launch();
	close(cmd.input_fd);
	cmd.run_loop.start();

	unlink(oldFile.c_str());
	unlink(myFile.c_str());
	unlink(yourFile.c_str());

	if(conflict)
		*conflict = cmd.return_code != 0;

	return cmd.result;
}
