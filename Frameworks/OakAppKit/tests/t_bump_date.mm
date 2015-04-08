#import "../src/OakFileManager.h"
#import <ns/ns.h>

void test_bump_date ()
{
    NSDate* date = [NSDate date];
    NSDateFormatter* dateFmt = [NSDateFormatter new];
    dateFmt.dateFormat = @"yyyy-MM-dd";
    NSString* formatted = [dateFmt stringFromDate:date];

    OAK_ASSERT_EQ(to_s(OakReplaceDateInString(@"2014-03-02 Invoice 37.tex", date)),
                  to_s([NSString stringWithFormat:@"%@ Invoice 37.tex", formatted]));
    OAK_ASSERT_EQ(to_s(OakReplaceDateInString(@"2014-03-02_Invoice 37.tex", date)),
                  to_s([NSString stringWithFormat:@"%@_Invoice 37.tex", formatted]));
    OAK_ASSERT_EQ(to_s(OakReplaceDateInString(@"Invoice 37 (2014-03-02).tex", date)),
                  to_s([NSString stringWithFormat:@"Invoice 37 (%@).tex", formatted]));
}
