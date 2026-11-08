#import <WebKit/WebKit.h>

@interface OakSystemCommandURLSchemeHandler : NSObject <WKURLSchemeHandler>
@property (nonatomic) std::map<std::string, std::string> environment;
@end
