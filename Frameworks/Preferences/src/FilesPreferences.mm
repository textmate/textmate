#import "FilesPreferences.h"
#import "Keys.h"
#import <OakAppKit/NSMenu Additions.h>
#import <OakFoundation/NSString Additions.h>
#import <OakFoundation/OakStringListTransformer.h>
#import <settings/settings.h>
#import <bundles/bundles.h>
#import <ns/ns.h>
#import <text/ctype.h>
#import <oak/oak.h>

@implementation FilesPreferences
- (id)init
{
	if(self = [super initWithNibName:@"FilesPreferences" label:@"Files" image:[NSImage imageNamed:NSImageNameMultipleDocuments]])
	{
		[OakStringListTransformer createTransformerWithName:@"OakLineEndingsTransformer" andObjectsArray:@[ @"\n", @"\r", @"\r\n" ]];

		self.defaultsProperties = @{
			@"disableSessionRestore"         : kUserDefaultsDisableSessionRestoreKey,
			@"disableDocumentAtStartup"      : kUserDefaultsDisableNewDocumentAtStartupKey,
			@"disableDocumentAtReactivation" : kUserDefaultsDisableNewDocumentAtReactivationKey,
		};

		self.tmProperties = @{
			@"encoding"       : [NSString stringWithCxxString:kSettingsEncodingKey],
			@"lineEndings"    : [NSString stringWithCxxString:kSettingsLineEndingsKey],
		};
	}
	return self;
}

- (void)selectFileType:(NSMenuItem*)sender
{
	[[NSUserDefaults standardUserDefaults] setObject:[sender representedObject] forKey:kUserDefaultsNewDocumentTypeKey];
}

- (void)loadView
{
	[super loadView];

	std::multimap<std::string, bundles::item_ptr, text::less_t> grammars;
	citerate(item, bundles::query(bundles::kFieldAny, NULL_STR, scope::wildcard, bundles::kItemTypeGrammar))
	{
		if(!(*item)->hidden_from_user())
			grammars.insert(std::make_pair((*item)->name(), *item));
	}

	if(!grammars.empty())
	{
		std::string const currentGrammar = to_s([[NSUserDefaults standardUserDefaults] stringForKey:kUserDefaultsNewDocumentTypeKey]);
		[documentTypesMenu removeAllItems];
		iterate(pair, grammars)
		{
			NSMenuItem* item = [documentTypesMenu addItemWithTitle:[NSString stringWithCxxString:pair->first] action:@selector(selectFileType:) keyEquivalent:@""];
			[item setRepresentedObject:[NSString stringWithCxxString:pair->second->uuid()]];
			[item setTarget:self];

			if(pair->second->uuid() == currentGrammar)
				[documentTypesPopUp selectItem:item];
		}
	}

	[self bind:@"encoding" toObject:encodingPopUp withKeyPath:@"encoding" options:nil];
}
@end
