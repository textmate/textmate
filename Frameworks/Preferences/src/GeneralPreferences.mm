#import "GeneralPreferences.h"

@implementation GeneralPreferences
- (id)init                         { return [super initWithNibName:@"GeneralPreferences" bundle:[NSBundle bundleForClass:[self class]]]; }
- (NSString*)viewIdentifier        { return @"General"; }
- (NSImage*)toolbarItemImage       { return [NSImage imageNamed:NSImageNamePreferencesGeneral]; }
- (NSString*)toolbarItemLabel      { return @"General"; }
@end
