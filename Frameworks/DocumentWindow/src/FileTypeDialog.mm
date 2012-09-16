#import "FileTypeDialog.h"
#import <text/ctype.h>
#import <io/path.h>
#import <regexp/format_string.h>
#import <updater/updater.h>
#import <bundles/bundles.h>
#import <file/type.h>
#import <ns/ns.h>
#import <network/network.h>
#import <OakFoundation/NSArray Additions.h>
#import <OakFoundation/NSString Additions.h>
#import <BundlesManager/BundlesManager.h>
#import <oak/CocoaSTL.h>

namespace
{
	struct grammar_info_t
	{
		grammar_info_t (std::string name, std::string scope, oak::uuid_t uuid, oak::uuid_t bundle_uuid) : name(name), scope(scope), uuid(uuid), bundle_uuid(bundle_uuid) { }

		std::string name;
		std::string scope;
		oak::uuid_t uuid;
		oak::uuid_t bundle_uuid;

		bool operator< (grammar_info_t const& rhs) const
		{
			return text::less_t()(name, rhs.name);
		}
	};
}

static NSArray* wrap (std::set<grammar_info_t> const& array)
{
	NSMutableArray* res = [NSMutableArray array];
	iterate(info, array)
	{
		NSMutableDictionary* dictionary = [NSMutableDictionary dictionaryWithObjectsAndKeys:
			[NSString stringWithCxxString:info->name],         @"name",
			[NSString stringWithCxxString:info->scope],        @"scope",
			[NSString stringWithCxxString:info->uuid],         @"uuid",
			[NSString stringWithCxxString:info->bundle_uuid],  @"bundleUUID",
			nil];
		[res addObject:dictionary];
	}
	return res;
}

@implementation FileTypeDialog
@synthesize path, enabledGrammars, persistentSetting, canOpenDocument;
@synthesize recommendedGrammars, installedGrammars, allGrammars;
@synthesize grammars, selectedGrammarIndexes;
@synthesize alertFormatString, infoFormatString, useForAllFormatString;

- (id)initWithPath:(NSString*)aPath first:(char const*)firstPointer last:(char const*)lastPointer
{
	if(self = [super initWithWindowNibName:@"FileTypeDialog"])
	{
		self.path = aPath;
		firstLine = std::string(firstPointer, std::find(firstPointer, lastPointer, '\n'));
	}
	return self;
}

- (void)dealloc
{
	self.path                   = nil;

	self.recommendedGrammars    = nil;
	self.installedGrammars      = nil;
	self.allGrammars            = nil;

	self.grammars               = nil;
	self.selectedGrammarIndexes = nil;

	self.alertFormatString      = nil;
	self.infoFormatString       = nil;
	self.useForAllFormatString  = nil;

	[super dealloc];
}

- (void)setupGrammars
{
	std::set<grammar_info_t> recommended, installed, all;

	citerate(item, bundles::query(bundles::kFieldAny, NULL_STR, scope::wildcard, bundles::kItemTypeGrammar))
		installed.insert(grammar_info_t((*item)->name(), (*item)->value_for_field(bundles::kFieldGrammarScope), (*item)->uuid(), (*item)->bundle_uuid()));

	all = installed;

	if(network::can_reach_host("updates.textmate.org"))
	{
		citerate(bundle, bundles_db::index())
		{
			citerate(grammar, (*bundle)->grammars())
			{
				grammar_info_t info((*grammar)->name(), (*grammar)->scope(), (*grammar)->uuid(), (*bundle)->uuid());
				all.insert(info);

				if((*grammar)->mode_line() != NULL_STR && regexp::search((*grammar)->mode_line(), firstLine.data(), firstLine.data() + firstLine.size()))
				{
					recommended.insert(info);
				}
				else
				{
					iterate(ext, (*grammar)->file_types())
					{
						if(path::rank(to_s(path), *ext))
							recommended.insert(info);
					}
				}
			}
		}
	}

	if(recommended.empty())
	{
		iterate(info, all)
		{
			if(info->scope == "text.plain")
				recommended.insert(*info);
		}
	}

	self.recommendedGrammars = wrap(recommended);
	self.installedGrammars   = wrap(installed);
	self.allGrammars         = wrap(all);
}

- (void)windowDidLoad
{
	self.alertFormatString     = [alertTextField stringValue];
	self.infoFormatString      = [infoTextField stringValue];
	self.useForAllFormatString = [useForAllCheckBox title];

	std::map<std::string, std::string> variables;
	variables["DisplayName"] = path::display_name(to_s(path));
	std::string const ext = path::extensions(to_s(path));;
	if(ext != "")
	{
		self.persistentSetting = YES;
		if(ext != path::name(to_s(path)))
			variables["X"] = ext;
	}

	std::string const alert  = format_string::expand(to_s(alertFormatString), variables);
	std::string const info   = format_string::expand(to_s(infoFormatString), variables);
	std::string const choice = format_string::expand(to_s(useForAllFormatString), variables);

	[alertTextField setStringValue:[NSString stringWithCxxString:alert]];
	[infoTextField setStringValue:[NSString stringWithCxxString:info]];
	[useForAllCheckBox setTitle:[NSString stringWithCxxString:choice]];
}

- (void)setEnabledGrammars:(NSInteger)newFilter
{
	enabledGrammars = newFilter;
	switch(enabledGrammars)
	{
		case kEnabledGrammarsRecommended: self.grammars = self.recommendedGrammars; break;
		case kEnabledGrammarsInstalled:   self.grammars = self.installedGrammars;   break;
		case kEnabledGrammarsAll:         self.grammars = self.allGrammars;         break;
	}
	[fileTypesTableView scrollRowToVisible:[fileTypesTableView selectedRow]];
}

- (NSDictionary*)grammar
{
	return [selectedGrammarIndexes count] == 0 ? nil : [grammars objectAtIndex:[selectedGrammarIndexes firstIndex]];
}

- (NSString*)fileType
{
	return [self.grammar objectForKey:@"scope"];
}

- (void)beginSheetModalForWindow:(NSWindow*)aWindow modalDelegate:(id <FileTypeDialogDelegate>)aDelegate contextInfo:(void*)info
{
	[self setupGrammars];
	self.grammars               = self.recommendedGrammars;
	self.selectedGrammarIndexes = [self.grammars count] == 0 ? [NSIndexSet indexSet] : [NSIndexSet indexSetWithIndex:0];

	self.canOpenDocument = YES;
	mainWindow  = aWindow;
	delegate    = aDelegate;
	contextInfo = info;
	[NSApp beginSheet:self.window modalForWindow:aWindow modalDelegate:self didEndSelector:@selector(sheetDidEnd:returnCode:contextInfo:) contextInfo:NULL];
}

static bool is_installed (oak::uuid_t const& uuid)
{
	return bundles::lookup(uuid) ? true : false;
}

- (void)checkIfBundleIsInstalled:(NSTimer*)aTimer
{
	NSString* uuid = [aTimer userInfo];
	if(is_installed(to_s(uuid)))
	{
		[aTimer invalidate];

		[installingBundleProgressIndicator stopAnimation:self];
		[installingBundleProgressIndicator unbind:@"value"];
		[installingBundleActivityTextField unbind:@"value"];
		[NSApp endSheet:installingBundleWindow];
		[installingBundleWindow orderOut:self];
		[delegate fileTypeDialog:self didSelectFileType:self.fileType contextInfo:contextInfo];
	}
}

- (void)sheetDidEnd:(NSWindow*)aSheet returnCode:(NSInteger)returnCode contextInfo:(void*)unused;
{
	self.canOpenDocument = NO;
	if(returnCode == NSRunAbortedResponse)
		return [delegate fileTypeDialog:self didSelectFileType:nil contextInfo:contextInfo];

	NSDictionary* grammar = self.grammar;
	std::string scope      = to_s((NSString*)[grammar objectForKey:@"scope"]);
	oak::uuid_t uuid       = to_s((NSString*)[grammar objectForKey:@"uuid"]);
	oak::uuid_t bundleUUID = to_s((NSString*)[grammar objectForKey:@"bundleUUID"]);

	if(is_installed(uuid))
	{
		if(self.persistentSetting)
			file::set_type(to_s(path), scope);
		return [delegate fileTypeDialog:self didSelectFileType:self.fileType contextInfo:contextInfo];
	}

	citerate(bundle, bundles_db::index())
	{
		if(bundleUUID == (*bundle)->uuid())
		{
			[NSTimer scheduledTimerWithTimeInterval:0.1 target:self selector:@selector(checkIfBundleIsInstalled:) userInfo:[grammar objectForKey:@"uuid"] repeats:YES];
			[[BundlesManager sharedInstance] installBundle:*bundle];
			[installingBundleActivityTextField bind:@"value" toObject:[BundlesManager sharedInstance] withKeyPath:@"activityText" options:nil];
			[installingBundleProgressIndicator bind:@"value" toObject:[BundlesManager sharedInstance] withKeyPath:@"progress" options:nil];
			[installingBundleProgressIndicator startAnimation:self];
			[NSApp beginSheet:installingBundleWindow modalForWindow:mainWindow modalDelegate:nil didEndSelector:NULL contextInfo:NULL];
			return;
		}
	}

	return [delegate fileTypeDialog:self didSelectFileType:nil contextInfo:contextInfo];
}

- (IBAction)performOpenDocument:(id)sender
{
	[self.window orderOut:self];
	[NSApp endSheet:self.window returnCode:NSRunStoppedResponse];
}

- (IBAction)performCancelOperation:(id)sender
{
	[self.window orderOut:self];
	[NSApp endSheet:self.window returnCode:NSRunAbortedResponse];
}
@end
