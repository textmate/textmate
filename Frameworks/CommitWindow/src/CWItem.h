@interface CWItem : NSObject <NSCopying>
@property (nonatomic) NSString* path;
@property (nonatomic) BOOL commit;
@property (nonatomic) NSString* scmStatus;
+ (CWItem*)itemWithPath:(NSString*)aPath andSCMStatus:(NSString*)aStatus commit:(BOOL)state;
- (NSComparisonResult)compare:(CWItem*)item;
@end
