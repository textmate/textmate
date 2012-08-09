#include <file/status.h>

CXXTEST_ENUM_TRAITS(file_status_t,
	CXXTEST_ENUM_MEMBER(kFileTestWritable);
	CXXTEST_ENUM_MEMBER(kFileTestWritableByRoot);
	CXXTEST_ENUM_MEMBER(kFileTestNotWritable);
	CXXTEST_ENUM_MEMBER(kFileTestNotWritableButOwner);
	CXXTEST_ENUM_MEMBER(kFileTestReadOnly);
	CXXTEST_ENUM_MEMBER(kFileTestNoParent);
	CXXTEST_ENUM_MEMBER(kFileTestUnhandled);
);

class StatusTests : public CxxTest::TestSuite
{
public:
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
			TS_ASSERT_EQUALS(file::status("/tmp/x/u_cr/cr.txt"), kFileTestNoParent);
			TS_ASSERT_EQUALS(file::status("/tmp/x/u_rw/cr.txt"), kFileTestWritable);
			TS_ASSERT_EQUALS(file::status("/tmp/x/u_rw/rw.txt"), kFileTestWritable);
			TS_ASSERT_EQUALS(file::status("/tmp/x/u_rw/ro.txt"), kFileTestNotWritableButOwner);
			TS_ASSERT_EQUALS(file::status("/tmp/x/u_ro/cr.txt"), kFileTestWritableByRoot); // ?
			TS_ASSERT_EQUALS(file::status("/tmp/x/u_ro/rw.txt"), kFileTestWritable);
			TS_ASSERT_EQUALS(file::status("/tmp/x/u_ro/ro.txt"), kFileTestNotWritableButOwner);
			TS_ASSERT_EQUALS(file::status("/tmp/x/o_rw/cr.txt"), kFileTestWritable);
			TS_ASSERT_EQUALS(file::status("/tmp/x/o_rw/rw.txt"), kFileTestWritableByRoot);
			TS_ASSERT_EQUALS(file::status("/tmp/x/o_rw/ro.txt"), kFileTestNotWritable);
			TS_ASSERT_EQUALS(file::status("/tmp/x/o_ro/cr.txt"), kFileTestWritableByRoot); // ?
			TS_ASSERT_EQUALS(file::status("/tmp/x/o_ro/rw.txt"), kFileTestWritableByRoot);
			TS_ASSERT_EQUALS(file::status("/tmp/x/o_ro/ro.txt"), kFileTestNotWritable);
		}
		else
		{
			TS_WARN("Skipping file::status tests (no fixtures)");
		}

		if(access("/Volumes/ro", F_OK) == 0)
		{
			TS_ASSERT_EQUALS(file::status("/Volumes/ro/cr.txt"), kFileTestReadOnly);
			TS_ASSERT_EQUALS(file::status("/Volumes/ro/rw.txt"), kFileTestReadOnly);
		}
		else
		{
			TS_WARN("Skipping file::status read-only tests (no fixtures)");
		}
	}
};
