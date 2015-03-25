#import "../src/OakFileManager.h"
#import <ns/ns.h>

void test_bump_date ()
{
	NSDate* date = [NSDate dateWithString:@"2015-03-25 00:00:00 +0000"];

	OAK_ASSERT_EQ(to_s(OakReplaceDateInString(@"2014-03-02 Invoice 37.tex", date)),   "2015-03-25 Invoice 37.tex");
	OAK_ASSERT_EQ(to_s(OakReplaceDateInString(@"2014-03-02_Invoice 37.tex", date)),   "2015-03-25_Invoice 37.tex");
	OAK_ASSERT_EQ(to_s(OakReplaceDateInString(@"Invoice 37 (2014-03-02).tex", date)), "Invoice 37 (2015-03-25).tex");
}
