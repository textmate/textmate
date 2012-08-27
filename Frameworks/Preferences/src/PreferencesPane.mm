#import "PreferencesPane.h"
#import <OakFoundation/NSString Additions.h>
#import <ns/ns.h>
#import <settings/settings.h>

@interface PreferencesPane ()
@property (nonatomic, readwrite) NSString* toolbarItemLabel;
@property (nonatomic, readwrite) NSImage*  toolbarItemImage;
@end

@implementation PreferencesPane
@synthesize toolbarItemLabel = label, toolbarItemImage = image, defaultsProperties, tmProperties;

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
	{
		return [[NSUserDefaults standardUserDefaults] setObject:newValue forKey:key];
	}
	else if(NSString* key = [tmProperties objectForKey:aKey])
	{
		if([newValue isKindOfClass:[NSString class]])
			return settings_t::set(to_s(key), to_s((NSString*)newValue));
		NSLog(@"%s wrong type for %@: ‘%@’", sel_getName(_cmd), aKey, newValue);
	}
	[super setValue:newValue forUndefinedKey:aKey];
}

- (id)valueForUndefinedKey:(NSString*)aKey
{
	if(NSString* key = [defaultsProperties objectForKey:aKey])
		return [[NSUserDefaults standardUserDefaults] objectForKey:key];
	else if(NSString* key = [tmProperties objectForKey:aKey])
		return [NSString stringWithCxxString:settings_t::raw_get(to_s(key))];
	return [super valueForUndefinedKey:aKey];
}

- (IBAction)help:(id)sender
{
	NSString* anchor = [sender isKindOfClass:[NSButton class]] ? [sender alternateTitle] : nil;
	if(anchor)
		[[NSHelpManager sharedHelpManager] openHelpAnchor:anchor inBook:[[NSBundle mainBundle] objectForInfoDictionaryKey:@"CFBundleHelpBookName"]];
}

@end
