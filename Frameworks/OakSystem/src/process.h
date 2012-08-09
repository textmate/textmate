#ifndef OAK_PROCESS_H_3LZVHAMX
#define OAK_PROCESS_H_3LZVHAMX

#include <oak/debug.h>
#include <oak/misc.h>

namespace oak
{
	PUBLIC std::map<std::string, std::string> const& basic_environment ();
	PUBLIC void kill_process_group_in_background (pid_t groupId);

	struct PUBLIC c_array
	{
		c_array (std::map<std::string, std::string> const& map);
		~c_array ();
		operator char* const* () const { return _array; }
	private:
		char* const* _array;
	};

	struct PUBLIC process_t
	{
		WATCH_LEAKS(oak::process_t);

		process_t ();
		virtual ~process_t ();

		pid_t launch ();

		virtual void did_exit (int rc);

		std::string command;
		std::map<std::string, std::string> environment;

		pid_t process_id;
		int input_fd, output_fd, error_fd;
		bool is_running;

		char* temp_path; // unlink in destructor
	private:
		size_t client_key;
	};

} /* oak */ 

#endif /* end of include guard: OAK_PROCESS_H_3LZVHAMX */
