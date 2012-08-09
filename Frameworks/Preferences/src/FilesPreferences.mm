#import "FilesPreferences.h"
#import "Keys.h"
#import <OakAppKit/NSMenu Additions.h>
#import <OakFoundation/NSString Additions.h>
#import <OakFoundation/OakStringListTransformer.h>
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

		self.defaultsProperties = [NSDictionary dictionaryWithObjectsAndKeys:
			kUserDefaultsDisableSessionRestoreKey,            @"disableSessionRestore",
			kUserDefaultsDisableNewDocumentAtStartupKey,      @"disableDocumentAtStartup",
			kUserDefaultsDisableNewDocumentAtReactivationKey, @"disableDocumentAtReactivation",
			kUserDefaultsEncodingKey,                         @"encoding",
			kUserDefaultsLineEndingsKey,                      @"lineEndings",
		nil];
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

	encodingPopUp.encoding = [[NSUserDefaults standardUserDefaults] objectForKey:kUserDefaultsEncodingKey];
	[encodingPopUp addObserver:self forKeyPath:@"encoding" options:NSKeyValueObservingOptionInitial context:NULL];
}

- (void)observeValueForKeyPath:(NSString*)keyPath ofObject:(id)object change:(NSDictionary*)change context:(void*)context
{
	if([keyPath isEqualToString:@"encoding"] && object == encodingPopUp)
		[[NSUserDefaults standardUserDefaults] setObject:[object valueForKey:keyPath] forKey:kUserDefaultsEncodingKey];
}
@end
