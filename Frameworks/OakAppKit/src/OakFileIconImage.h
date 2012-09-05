#import <oak/misc.h>

PUBLIC @interface OakFileIconImage : NSImage
{
}
+ (id)fileIconImageWithPath:(NSString*)aPath isModified:(BOOL)flag size:(NSSize)aSize;
+ (id)fileIconImageWithPath:(NSString*)aPath isModified:(BOOL)flag;
+ (id)fileIconImageWithPath:(NSString*)aPath size:(NSSize)aSize;
@end
