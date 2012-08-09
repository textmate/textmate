@interface OakAbbreviations : NSObject
{
@private
	NSString* name;
	NSMutableArray* bindings;
}
+ (OakAbbreviations*)abbreviationsForName:(NSString*)aName;

- (NSArray*)stringsForAbbreviation:(NSString*)anAbbreviation;
- (void)learnAbbreviation:(NSString*)anAbbreviation forString:(NSString*)aString;
@end
