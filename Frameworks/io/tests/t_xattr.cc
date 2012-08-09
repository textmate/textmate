#include <io/path.h>

class XattrTests : public CxxTest::TestSuite
{
public:
	void test_xattr ()
	{
		// This isn’t a test per se, it’s more to keep the code around that tests volume capabilities (which is something we presently do anywhere in Avian).
		struct statfs buf;
		struct { u_int32_t length; vol_capabilities_attr_t attr; } attrBuf;
		TS_ASSERT_EQUALS(statfs(path::home().c_str(), &buf), 0);
		TS_ASSERT_EQUALS(getattrlist(buf.f_mntonname, &(attrlist){ ATTR_BIT_MAP_COUNT, 0, 0, ATTR_VOL_INFO|ATTR_VOL_CAPABILITIES, 0, 0, 0 }, &attrBuf, sizeof(attrBuf), 0), 0);
		TS_ASSERT_EQUALS(attrBuf.length, sizeof(attrBuf));
		TS_ASSERT_EQUALS(attrBuf.attr.capabilities[VOL_CAPABILITIES_INTERFACES] & VOL_CAP_INT_EXCHANGEDATA, VOL_CAP_INT_EXCHANGEDATA);
	}
};
