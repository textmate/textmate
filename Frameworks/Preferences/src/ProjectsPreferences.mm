#import "ProjectsPreferences.h"
#import "Keys.h"
#import <settings/settings.h>
#import <OakAppKit/NSImage Additions.h>
#import <OakFoundation/NSString Additions.h>

@implementation ProjectsPreferences
- (id)init
{
	if(self = [super initWithNibName:@"ProjectsPreferences" label:@"Projects" image:[NSImage imageNamed:@"Projects" inSameBundleAsClass:[self class]]])
	{
		self.defaultsProperties = @{
			@"foldersOnTop"       : kUserDefaultsFoldersOnTopKey,      
			@"showFileExtensions" : kUserDefaultsShowFileExtensionsKey,
		};

		self.tmProperties = @{
			@"excludePattern" : [NSString stringWithCxxString:kSettingsExcludeKey],
			@"includePattern" : [NSString stringWithCxxString:kSettingsIncludeKey],
			@"binaryPattern"  : [NSString stringWithCxxString:kSettingsBinaryKey],
		};
	}
	return self;
}
@end
