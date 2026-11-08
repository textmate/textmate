#import "TerminalEnvironment.h"
#import <ns/ns.h>
#import <settings/settings.h>
#import <io/environment.h>

NSDictionary<NSString*, NSString*>* TerminalEnvironmentForDirectory (NSString* directory)
{
	// The directory argument, not the path one: a path would search from its parent and skip the directory's own .tm_properties.
	NSMutableDictionary* res = [NSMutableDictionary dictionary];
	for(auto const& pair : variables_for_path(oak::basic_environment(), NULL_STR, "", to_s(directory)))
		res[to_ns(pair.first)] = to_ns(pair.second);
	return res;
}
