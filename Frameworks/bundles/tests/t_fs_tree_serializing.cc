#include <bundles/fs_tree.h>
#include <test/jail.h>

class FsTreeSerializingTests : public CxxTest::TestSuite
{
	static void setup_test_folder (std::string const& cwd, size_t total = 4)
	{
		for(size_t i = 0; i < total; ++i)
		{
			std::string dir = path::join(cwd, text::format("dir_%zu.txt", i));
			path::make_dir(dir);
			setup_test_folder(dir, i);

			if(i > 0)
				path::link(text::format("dir_%zu/file_%zu.txt", i, i-1), path::join(cwd, text::format("link_%zu.txt", i-1)));

			path::set_content(path::join(cwd, text::format("file_%zu.txt", i)), "«content»");
		}
	}

public:
	void test_fs_tree_serializing ()
	{
		test::jail_t jail;
		std::string const path = jail.path();

		setup_test_folder(path);

		fs::node_t lhs(path);
		fs::node_t rhs(path);
		TS_ASSERT_EQUALS(lhs.rescan(path, "*", "*"), rhs.rescan(path, "*", "*"));

		std::string const& plistFile = jail.path("tree.plist");
		plist::save(plistFile, to_plist(lhs));
		TS_ASSERT_EQUALS(lhs, fs::from_plist(plist::load(plistFile)));
	}
};
