@interface OakStringListTransformer : NSValueTransformer
+ (void)createTransformerWithName:(NSString*)aName andObjectsArray:(NSArray*)aList;
+ (void)createTransformerWithName:(NSString*)aName andObjectsDictionary:(NSDictionary*)mapping;
@end
