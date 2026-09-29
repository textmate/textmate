#ifndef JAIL_H_LUP8E04P
#define JAIL_H_LUP8E04P

#include <io/src/io.h>

namespace test
{
	struct jail_t
	{
		jail_t ()
		{
			helper = std::make_shared<helper_t>();
		}

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

		std::string path (std::string const& path = "") const { return path::join(helper->root, path); }

	private:
		struct helper_t
		{
			helper_t ()
			{
				root = path::resolve(path::temp("jail"));
				path::make_dir(root);
			}

			~helper_t ()
			{
				path::remove(root);
			}

			std::string root;
		};

		std::shared_ptr<helper_t> helper;
	};
}

#endif /* end of include guard: JAIL_H_LUP8E04P */
