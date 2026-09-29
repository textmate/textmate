#ifndef APPLICATION_H_J6YAXEQE
#define APPLICATION_H_J6YAXEQE

#include <string>

namespace oak
{
	struct application_t
	{
		application_t (int argc, char const* argv[]);

		static void relaunch (char const* args = "-disableSessionRestore NO");
		static std::string name ();
		static std::string path (std::string const& relativePath = ".");
		static void set_name (std::string const& newName);
		static void set_path (std::string const& newPath);
		static void set_support (std::string const& newPath);
		static std::string support (std::string const& relativePath = ".");
		static std::string version ();
	};

} /* oak */

#endif /* end of include guard: APPLICATION_H_J6YAXEQE */
