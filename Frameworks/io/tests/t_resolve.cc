#include <test/jail.h>

void test_resolve_symlinks ()
{
	using path::resolve;
	test::jail_t jail;

	jail.touch("file");
	jail.mkdir("dir");

	jail.ln("link_1",        "file"           );
	jail.ln("link_2",        "dir"            );
	jail.ln("link_3",        "dir/foo"        );
	jail.ln("link_4",        "link_2/bar"     );
	jail.ln("link_5",        "link_3/bar"     );
	jail.ln("dir/link_6",    "link_1"         );
	jail.ln("dir/link_7",    "link_2"         );
	jail.ln("dir/link_8",    "link_2/foo"     );
	jail.ln("dir/link_9",    "link_2/link_9"  );
	jail.ln("dir/link_10",   "dir/link_9"     );

	jail.ln("link_11",       "missing"        );
	jail.ln("link_12",       "dir/missing"    );
	jail.ln("link_13",       "link_2/missing" );

	OAK_ASSERT_EQ(resolve(jail.path("link_1")),      jail.path("file"));
	OAK_ASSERT_EQ(resolve(jail.path("link_2")),      jail.path("dir"));
	OAK_ASSERT_EQ(resolve(jail.path("link_3")),      jail.path("dir/foo"));
	OAK_ASSERT_EQ(resolve(jail.path("link_4")),      jail.path("dir/bar"));
	OAK_ASSERT_EQ(resolve(jail.path("link_5")),      jail.path("dir/foo/bar"));
	OAK_ASSERT_EQ(resolve(jail.path("dir/link_6")),  jail.path("file"));
	OAK_ASSERT_EQ(resolve(jail.path("dir/link_7")),  jail.path("dir"));
	OAK_ASSERT_EQ(resolve(jail.path("dir/link_8")),  jail.path("dir/foo"));
	OAK_ASSERT_EQ(resolve(jail.path("dir/link_9")),  jail.path("link_2/link_9"));
	OAK_ASSERT_EQ(resolve(jail.path("dir/link_10")), jail.path("link_2/link_9"));
	OAK_ASSERT_EQ(resolve(jail.path("link_11")),     jail.path("missing"));
	OAK_ASSERT_EQ(resolve(jail.path("link_12")),     jail.path("dir/missing"));
	OAK_ASSERT_EQ(resolve(jail.path("link_13")),     jail.path("dir/missing"));
}

void test_resolve_head ()
{
	using path::resolve_head;
	test::jail_t jail;

	jail.touch("file");
	jail.mkdir("dir");

	jail.ln("link_1",        "file"           );
	jail.ln("link_2",        "dir"            );
	jail.ln("link_3",        "dir/foo"        );
	jail.ln("link_4",        "link_2/bar"     );
	jail.ln("link_5",        "link_3/bar"     );
	jail.ln("dir/link_6",    "link_1"         );
	jail.ln("dir/link_7",    "link_2"         );
	jail.ln("dir/link_8",    "link_2/foo"     );
	jail.ln("dir/link_9",    "link_2/link_9"  );
	jail.ln("dir/link_10",   "dir/link_9"     );

	jail.ln("link_11",       "missing"        );
	jail.ln("link_12",       "dir/missing"    );
	jail.ln("link_13",       "link_2/missing" );

	OAK_ASSERT_EQ(resolve_head(jail.path("link_1")),      jail.path("file"));
	OAK_ASSERT_EQ(resolve_head(jail.path("link_2")),      jail.path("dir"));
	OAK_ASSERT_EQ(resolve_head(jail.path("link_3")),      jail.path("dir/foo"));
	OAK_ASSERT_EQ(resolve_head(jail.path("link_4")),      jail.path("link_2/bar"));
	OAK_ASSERT_EQ(resolve_head(jail.path("link_5")),      jail.path("link_3/bar"));
	OAK_ASSERT_EQ(resolve_head(jail.path("dir/link_6")),  jail.path("file"));
	OAK_ASSERT_EQ(resolve_head(jail.path("dir/link_7")),  jail.path("dir"));
	OAK_ASSERT_EQ(resolve_head(jail.path("dir/link_8")),  jail.path("link_2/foo"));
	OAK_ASSERT_EQ(resolve_head(jail.path("dir/link_9")),  jail.path("link_2/link_9"));
	OAK_ASSERT_EQ(resolve_head(jail.path("dir/link_10")), jail.path("link_2/link_9"));
	OAK_ASSERT_EQ(resolve_head(jail.path("link_11")),     jail.path("missing"));
	OAK_ASSERT_EQ(resolve_head(jail.path("link_12")),     jail.path("dir/missing"));
	OAK_ASSERT_EQ(resolve_head(jail.path("link_13")),     jail.path("link_2/missing"));
}
