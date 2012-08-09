#ifndef OAK_COMMAND_H_JOUWE2BO
#define OAK_COMMAND_H_JOUWE2BO

#include "process.h"
#include "reader.h"

namespace oak
{
	struct PUBLIC command_t : process_t
	{
		WATCH_LEAKS(oak::command_t);

		command_t ();
		virtual ~command_t ();
		void did_exit (int rc);

		void launch ();
		virtual void did_exit (int rc, std::string const& output, std::string const& error) { }
	
		std::string display_name;

	private:
		void decrease ();
		void sanitize_data (std::string& data);

		struct reader_t : io::reader_t
		{
			reader_t (std::string& data, command_t* command) : data(data), command(command) { }
			void receive_data (char const* bytes, size_t len)
			{
				data.insert(data.end(), bytes, bytes + len);
				if(len == 0)
					command->decrease();
			}

			std::string& data;
			command_t* command;

		} output_reader, error_reader;

		size_t open_count;
		std::string output_data, error_data;
		int return_code;
	};

} /* oak */ 

#endif /* end of include guard: OAK_COMMAND_H_JOUWE2BO */
