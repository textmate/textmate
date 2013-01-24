@interface NSString (Path)
+ (NSString*)stringWithUTF8String:(char const*)aString length:(unsigned)aLength;
+ (NSString*)stringWithCxxString:(std::string const&)aString;
- (BOOL)existsAsPath;
- (BOOL)isDirectory;
@end

@interface NSString (FinderSorting)
- (NSComparisonResult)displayNameCompare:(NSString*)otherString;
@end
