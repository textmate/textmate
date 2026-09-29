#include <file/src/status.h>

void test_status ()
{
	// mkdir -p /tmp/x/{u,o}_{rw,ro}
	// touch /tmp/x/{u,o}_{rw,ro}/{rw,ro}.txt
	// chmod u-w /tmp/x/{u,o}_{rw,ro}/ro.txt
	// chmod u-w /tmp/x/{u,o}_ro
	// chmod a+w /tmp/x/o_rw
	// sudo chown -R root /tmp/x/o_{ro,rw}

	if(access("/tmp/x", F_OK) == 0)
	{
		OAK_ASSERT_EQ(file::status("/tmp/x/u_cr/cr.txt"), kFileTestNoParent);
		OAK_ASSERT_EQ(file::status("/tmp/x/u_rw/cr.txt"), kFileTestWritable);
		OAK_ASSERT_EQ(file::status("/tmp/x/u_rw/rw.txt"), kFileTestWritable);
		OAK_ASSERT_EQ(file::status("/tmp/x/u_rw/ro.txt"), kFileTestNotWritableButOwner);
		OAK_ASSERT_EQ(file::status("/tmp/x/u_ro/cr.txt"), kFileTestWritableByRoot); // ?
		OAK_ASSERT_EQ(file::status("/tmp/x/u_ro/rw.txt"), kFileTestWritable);
		OAK_ASSERT_EQ(file::status("/tmp/x/u_ro/ro.txt"), kFileTestNotWritableButOwner);
		OAK_ASSERT_EQ(file::status("/tmp/x/o_rw/cr.txt"), kFileTestWritable);
		OAK_ASSERT_EQ(file::status("/tmp/x/o_rw/rw.txt"), kFileTestWritableByRoot);
		OAK_ASSERT_EQ(file::status("/tmp/x/o_rw/ro.txt"), kFileTestNotWritable);
		OAK_ASSERT_EQ(file::status("/tmp/x/o_ro/cr.txt"), kFileTestWritableByRoot); // ?
		OAK_ASSERT_EQ(file::status("/tmp/x/o_ro/rw.txt"), kFileTestWritableByRoot);
		OAK_ASSERT_EQ(file::status("/tmp/x/o_ro/ro.txt"), kFileTestNotWritable);
	}
	else
	{
		OAK_WARN("Skipping file::status tests (no fixtures)");
	}

	if(access("/Volumes/ro", F_OK) == 0)
	{
		OAK_ASSERT_EQ(file::status("/Volumes/ro/cr.txt"), kFileTestReadOnly);
		OAK_ASSERT_EQ(file::status("/Volumes/ro/rw.txt"), kFileTestReadOnly);
	}
	else
	{
		OAK_WARN("Skipping file::status read-only tests (no fixtures)");
	}
}
