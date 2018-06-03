@interface StringImage : NSImage
@property (nonatomic) NSString* stringValue;
+ (instancetype)imageWithString:(NSString*)aString size:(NSSize)aSize;
@end
