@interface NSColor (Creation)
+ (NSColor*)colorWithString:(NSString*)aString;
+ (NSColor*)colorWithCGColor:(CGColorRef)aColor;
@end

@interface NSColor (OakColor)
- (BOOL)isDark;
@end
