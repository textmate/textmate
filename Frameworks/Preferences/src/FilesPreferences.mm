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

- (void)selectNewFileType:(NSMenuItem*)sender
{
	settings_t::set(kSettingsFileTypeKey, to_s((NSString*)[sender representedObject]), "attr.untitled");
}

- (void)selectUnknownFileType:(NSMenuItem*)sender
{
	settings_t::set(kSettingsFileTypeKey, to_s((NSString*)[sender representedObject]), "attr.file.unknown-type");
}

- (void)loadView
{
	[super loadView];

	[newDocumentTypesMenu removeAllItems];
	[unknownDocumentTypesMenu removeAllItems];

	NSMenuItem* item = [unknownDocumentTypesMenu addItemWithTitle:@"Prompt for type" action:@selector(selectUnknownFileType:) keyEquivalent:@""];
	[item setRepresentedObject:nil];
	[item setTarget:self];
	[unknownDocumentTypesMenu addItem:[NSMenuItem separatorItem]];

	std::multimap<std::string, bundles::item_ptr, text::less_t> grammars;
	for(auto const& item : bundles::query(bundles::kFieldAny, NULL_STR, scope::wildcard, bundles::kItemTypeGrammar))
	{
		if(!item->hidden_from_user())
			grammars.emplace(item->name(), item);
	}

	if(!grammars.empty())
	{
		std::string const defaultNewFileType     = settings_t::raw_get(kSettingsFileTypeKey, "attr.untitled");
		std::string const defaultUnknownFileType = settings_t::raw_get(kSettingsFileTypeKey, "attr.file.unknown-type");

		for(auto const& pair : grammars)
		{
			std::string const& fileType = pair.second->value_for_field(bundles::kFieldGrammarScope);
			if(fileType == NULL_STR)
				continue;

			NSMenuItem* item = [newDocumentTypesMenu addItemWithTitle:[NSString stringWithCxxString:pair.first] action:@selector(selectNewFileType:) keyEquivalent:@""];
			[item setRepresentedObject:[NSString stringWithCxxString:fileType]];
			[item setTarget:self];

			if(fileType == defaultNewFileType)
				[newDocumentTypesPopUp selectItem:item];

			item = [unknownDocumentTypesMenu addItemWithTitle:[NSString stringWithCxxString:pair.first] action:@selector(selectUnknownFileType:) keyEquivalent:@""];
			[item setRepresentedObject:[NSString stringWithCxxString:fileType]];
			[item setTarget:self];

			if(fileType == defaultUnknownFileType)
				[unknownDocumentTypesPopUp selectItem:item];
		}
	}

	[encodingPopUp bind:@"encoding" toObject:self withKeyPath:@"encoding" options:nil];
}
@end
