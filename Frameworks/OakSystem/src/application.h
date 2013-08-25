#ifndef APPLICATION_H_J6YAXEQE
#define APPLICATION_H_J6YAXEQE

#include <oak/misc.h>

namespace oak
{
	struct PUBLIC application_t
	{
		application_t (int argc, char const* argv[], bool redirectStdErr = false);

		static void relaunch ();
		static std::string name ();
		static std::string path (std::string const& relativePath = ".");
		static void set_name (std::string const& newName);
		static void set_path (std::string const& newPath);
		static void set_support (std::string const& newPath);
		static std::string support (std::string const& relativePath = ".");
		static std::string revision ();
	private:
		static void create_pid_file ();
		static void remove_pid_file ();
	};

} /* oak */

#endif /* end of include guard: APPLICATION_H_J6YAXEQE */
