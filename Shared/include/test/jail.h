#ifndef JAIL_H_LUP8E04P
#define JAIL_H_LUP8E04P

#include <io/io.h>

namespace test
{
	struct jail_t
	{
		jail_t ()
		{
			root = path::resolve(path::temp("jail"));
			path::make_dir(root);
		}

		~jail_t ()
		{
			path::remove(root);
		}

		jail_t (jail_t const& rhs) = delete;
		jail_t& operator= (jail_t const& rhs) = delete;

		void mkdir (std::string const& relativeToRoot)
		{
			path::make_dir(path(relativeToRoot));
		}

		void touch (std::string const& relativeToRoot)
		{
			mkdir(path::parent(relativeToRoot));
			if(path::exists(path(relativeToRoot)))
					utimes(path(relativeToRoot).c_str(), NULL);
			else	set_content(relativeToRoot, "");
		}

		void set_content (std::string const& relativeToRoot, std::string const& content)
		{
			mkdir(path::parent(relativeToRoot));
			path::set_content(path(relativeToRoot), content);
		}

		void ln (std::string const& srcRelativeToRoot, std::string const& dstRelativeToRoot)
		{
			path::link(path(dstRelativeToRoot), path(srcRelativeToRoot));
		}

		void remove (std::string const& relativeToRoot)
		{
			path::remove(path(relativeToRoot));
		}

		std::string path (std::string const& path = "") const { return path::join(root, path); }

	private:
		std::string root;
	};
}

#endif /* end of include guard: JAIL_H_LUP8E04P */
