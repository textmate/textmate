#import "PreferencesPane.h"

@interface PreferencesPane ()
@property (nonatomic, readwrite) NSString* toolbarItemLabel;
@property (nonatomic, readwrite) NSImage*  toolbarItemImage;
@end

@implementation PreferencesPane
@synthesize toolbarItemLabel = label, toolbarItemImage = image, defaultsProperties;

- (NSString*)identifier { return label; }

- (id)initWithNibName:(NSString*)aNibName label:(NSString*)aLabel image:(NSImage*)anImage
{
	if(self = [super initWithNibName:aNibName bundle:[NSBundle bundleForClass:[self class]]])
	{
		self.toolbarItemLabel = aLabel;
		self.toolbarItemImage = anImage;
	}
	return self;
}

- (void)setValue:(id)newValue forUndefinedKey:(NSString*)aKey
{
	if(NSString* key = [defaultsProperties objectForKey:aKey])
		return [[NSUserDefaults standardUserDefaults] setObject:newValue forKey:key];
	[super setValue:newValue forUndefinedKey:aKey];
}

- (id)valueForUndefinedKey:(NSString*)aKey
{
	if(NSString* key = [defaultsProperties objectForKey:aKey])
		return [[NSUserDefaults standardUserDefaults] objectForKey:key];
	return [super valueForUndefinedKey:aKey];
}

- (IBAction)help:(id)sender
{
	NSString* anchor = [sender isKindOfClass:[NSButton class]] ? [sender alternateTitle] : nil;
	if(anchor)
		[[NSHelpManager sharedHelpManager] openHelpAnchor:anchor inBook:[[NSBundle mainBundle] objectForInfoDictionaryKey:@"CFBundleHelpBookName"]];
}

@end
