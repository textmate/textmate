@interface NSColor (Creation)
+ (NSColor*)colorWithString:(NSString*)aString;
+ (NSColor*)tmColorWithCGColor:(CGColorRef)aColor;
@end

@interface NSColor (OakColor)
- (BOOL)isDark;
@end
