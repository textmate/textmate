#import <Foundation/Foundation.h>

// Same base environment as bundle commands, so Preferences > Variables (PATH, locale) apply and the app's own launch environment does not leak,
// plus the .tm_properties variables that apply to `directory`.
NSDictionary<NSString*, NSString*>* TerminalEnvironmentForDirectory (NSString* directory);
