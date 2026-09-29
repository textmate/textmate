@interface NSString (Additions)
+ (NSString*)stringWithUTF8String:(char const*)aString length:(NSUInteger)aLength;
+ (NSString*)stringWithCxxString:(std::string const&)aString;
@end
