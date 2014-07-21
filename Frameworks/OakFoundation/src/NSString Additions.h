@interface NSString (Addition)
+ (NSString*)stringWithUTF8String:(char const*)aString length:(unsigned)aLength;
+ (NSString*)stringWithCxxString:(std::string const&)aString;
@end
