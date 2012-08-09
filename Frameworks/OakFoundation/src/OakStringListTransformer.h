@interface OakStringListTransformer : NSValueTransformer
{
	NSArray* stringList;
}
+ (void)createTransformerWithName:(NSString*)aName andObjectsArray:(NSArray*)aList;
+ (void)createTransformerWithName:(NSString*)aName andObjects:(id)firstObj, ...;
@end
