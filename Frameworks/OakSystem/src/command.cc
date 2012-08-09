#include "command.h"
#include <oak/oak.h>
#include <text/utf8.h>
#include <text/hexdump.h>
	
OAK_DEBUG_VAR(Command);

namespace oak
{
	command_t::command_t () : display_name("-"), output_reader(output_data, this), error_reader(error_data, this), open_count(3), return_code(-1)
	{
		D(DBF_Command, bug("%s\n", display_name.c_str()););
	}

	command_t::~command_t ()
	{
		D(DBF_Command, bug("%s\n", display_name.c_str()););
	}

	void command_t::launch ()
	{
		D(DBF_Command, bug("%s\n", display_name.c_str()););
		process_t::launch();
		output_reader.set_fd(output_fd);
		error_reader.set_fd(error_fd);
	}

	void command_t::sanitize_data (std::string& data)
	{
		std::string tmp;
		oak::replace_copy(data.begin(), data.end(), temp_path, temp_path + strlen(temp_path), beginof(display_name), endof(display_name), back_inserter(tmp));
		data.swap(tmp);

		if(!utf8::is_valid(data.begin(), data.end()))
		{
			tmp.clear();
			text::hex_dump(data.begin(), data.end(), back_inserter(tmp));
			data.swap(tmp);
		}
	}
	
	void command_t::decrease ()
	{
		D(DBF_Command, bug("open count: %zu\n", open_count););
		if(--open_count == 0)
		{
			sanitize_data(output_data);
			sanitize_data(error_data);
			did_exit(return_code, output_data, error_data);
		}
	}

	void command_t::did_exit (int rc)
	{
		D(DBF_Command, bug("%d\n", rc););
		oak::process_t::did_exit(rc);
		return_code = rc;
		decrease();
	}

} /* oak */ 
