#include <io/path.h>

void test_xattr ()
{
	// This isn’t a test per se, it’s more to keep the code around that tests volume capabilities (which is something we presently do anywhere in Avian).
	struct statfs buf;
	struct { u_int32_t length; vol_capabilities_attr_t attr; } attrBuf;
	OAK_ASSERT_EQ(statfs(path::home().c_str(), &buf), 0);
	attrlist list = { ATTR_BIT_MAP_COUNT, 0, 0, ATTR_VOL_INFO|ATTR_VOL_CAPABILITIES, 0, 0, 0 };
	OAK_ASSERT_EQ(getattrlist(buf.f_mntonname, &list, &attrBuf, sizeof(attrBuf), 0), 0);
	OAK_ASSERT_EQ(attrBuf.length, sizeof(attrBuf));
	OAK_ASSERT_EQ(attrBuf.attr.capabilities[VOL_CAPABILITIES_INTERFACES] & VOL_CAP_INT_EXCHANGEDATA, VOL_CAP_INT_EXCHANGEDATA);
}
