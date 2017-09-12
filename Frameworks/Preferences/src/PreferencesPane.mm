#import "PreferencesPane.h"
#import <OakFoundation/NSString Additions.h>
#import <ns/ns.h>
#import <settings/settings.h>

@interface PreferencesPane ()
@property (nonatomic, readwrite) NSString* toolbarItemLabel;
@property (nonatomic, readwrite) NSImage*  toolbarItemImage;
@end

@implementation PreferencesPane
- (NSString*)viewIdentifier { return _toolbarItemLabel; }

- (id)initWithNibName:(NSString*)aNibName label:(NSString*)aLabel image:(NSImage*)anImage
{
	if(self = [super initWithNibName:aNibName bundle:[NSBundle bundleForClass:[self class]]])
	{
		_toolbarItemLabel = aLabel;
		_toolbarItemImage = anImage;
	}
	return self;
}

- (void)setValue:(id)newValue forUndefinedKey:(NSString*)aKey
{
	if(NSString* key = [_defaultsProperties objectForKey:aKey])
	{
		return [[NSUserDefaults standardUserDefaults] setObject:newValue forKey:key];
	}
	else if(NSString* key = [_tmProperties objectForKey:aKey])
	{
		newValue = newValue ?: @"";
		if([newValue isKindOfClass:[NSString class]])
			return settings_t::set(to_s(key), to_s(newValue));
		NSLog(@"%s wrong type for %@: ‘%@’", sel_getName(_cmd), aKey, newValue);
	}
	[super setValue:newValue forUndefinedKey:aKey];
}

- (id)valueForUndefinedKey:(NSString*)aKey
{
	if(NSString* key = [_defaultsProperties objectForKey:aKey])
		return [[NSUserDefaults standardUserDefaults] objectForKey:key];
	else if(NSString* key = [_tmProperties objectForKey:aKey])
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
