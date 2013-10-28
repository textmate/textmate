#import <oak/misc.h>

PUBLIC @interface OakAbbreviations : NSObject
+ (OakAbbreviations*)abbreviationsForName:(NSString*)aName;

- (NSArray*)stringsForAbbreviation:(NSString*)anAbbreviation;
- (void)learnAbbreviation:(NSString*)anAbbreviation forString:(NSString*)aString;
@end
