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
		[OakStringListTransformer createTransformerWithName:@"OakLineEndingsSettingsTransformer" andObjectsArray:@[ @"\\n", @"\\r", @"\\r\\n" ]];

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
	settings_t::set(kSettingsFileTypeKey, to_s((NSString*)[sender representedObject]), "attr.untitled");
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
		std::string const defaultFileType = settings_t::raw_get(kSettingsFileTypeKey, "attr.untitled");
		[documentTypesMenu removeAllItems];
		iterate(pair, grammars)
		{
			std::string const& fileType = pair->second->value_for_field(bundles::kFieldGrammarScope);

			NSMenuItem* item = [documentTypesMenu addItemWithTitle:[NSString stringWithCxxString:pair->first] action:@selector(selectFileType:) keyEquivalent:@""];
			[item setRepresentedObject:[NSString stringWithCxxString:fileType]];
			[item setTarget:self];

			if(fileType == defaultFileType)
				[documentTypesPopUp selectItem:item];
		}
	}

	[self bind:@"encoding" toObject:encodingPopUp withKeyPath:@"encoding" options:nil];
}
@end
