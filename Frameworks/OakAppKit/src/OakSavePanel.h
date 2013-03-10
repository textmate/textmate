#import <file/encoding.h>
#import <oak/misc.h>

PUBLIC @interface OakSavePanel : NSObject
+ (void)showWithPath:(NSString*)aPathSuggestion directory:(NSString*)aDirectorySuggestion fowWindow:(NSWindow*)aWindow encoding:(encoding::type const&)encoding completionHandler:(void(^)(NSString* path, encoding::type const& encoding))aCompletionHandler;
@end
