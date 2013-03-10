@interface NSColor (TMColorAdditions)
+ (NSColor*)colorWithString:(NSString*)aString;
+ (NSColor*)tmColorWithCGColor:(CGColorRef)aColor;
- (CGColorRef)tmCGColor;
@end
