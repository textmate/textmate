#import "ProjectsPreferences.h"
#import "Keys.h"
#import <OakAppKit/NSImage Additions.h>

@implementation ProjectsPreferences
- (id)init
{
	if(self = [super initWithNibName:@"ProjectsPreferences" label:@"Projects" image:[NSImage imageNamed:@"Projects" inSameBundleAsClass:[self class]]])
	{
		self.defaultsProperties = [NSDictionary dictionaryWithObjectsAndKeys:
			kUserDefaultsFoldersOnTopKey,       @"foldersOnTop",
			kUserDefaultsShowFileExtensionsKey, @"showFileExtensions",
			kUserDefaultsExcludePatternKey,     @"excludePattern",
			kUserDefaultsIncludePatternKey,     @"includePattern",
			kUserDefaultsBinaryPatternKey,      @"binaryPattern",
		nil];
	}
	return self;
}
@end
