#import "FileTypeDialog.h"
#import <text/ctype.h>
#import <io/path.h>
#import <regexp/format_string.h>
#import <updater/updater.h>
#import <bundles/bundles.h>
#import <file/type.h>
#import <ns/ns.h>
#import <network/network.h>
#import <OakAppKit/OakAppKit.h>
#import <OakFoundation/NSArray Additions.h>
#import <OakFoundation/NSString Additions.h>
#import <BundlesManager/BundlesManager.h>

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
	for(auto const& info : array)
	{
		NSMutableDictionary* dictionary = [NSMutableDictionary dictionaryWithObjectsAndKeys:
			[NSString stringWithCxxString:info.name],         @"name",
			[NSString stringWithCxxString:info.scope],        @"scope",
			[NSString stringWithCxxString:info.uuid],         @"uuid",
			[NSString stringWithCxxString:info.bundle_uuid],  @"bundleUUID",
			nil];
		[res addObject:dictionary];
	}
	return res;
}

static bool is_installed (oak::uuid_t const& uuid)
{
	return bundles::lookup(uuid) ? true : false;
}

@interface FileTypeDialog ()
@property (nonatomic, retain) NSString* path;

@property (nonatomic, retain) NSArray* recommendedGrammars;
@property (nonatomic, retain) NSArray* installedGrammars;
@property (nonatomic, retain) NSArray* allGrammars;

@property (nonatomic, retain) NSString* alertFormatString;
@property (nonatomic, retain) NSString* infoFormatString;
@property (nonatomic, retain) NSString* useForAllFormatString;

@property (nonatomic, readonly) NSDictionary* grammar;
@property (nonatomic, readonly) NSString* fileType;
@end

@implementation FileTypeDialog
{
	std::string firstLine;
}

- (id)initWithPath:(NSString*)aPath first:(char const*)firstPointer last:(char const*)lastPointer
{
	if(self = [super initWithWindowNibName:@"FileTypeDialog"])
	{
		self.path = aPath;
		firstLine = std::string(firstPointer, std::find(firstPointer, lastPointer, '\n'));
	}
	return self;
}

- (void)setupGrammars
{
	std::set<grammar_info_t> recommended, installed, all;

	for(auto const& item : bundles::query(bundles::kFieldAny, NULL_STR, scope::wildcard, bundles::kItemTypeGrammar))
	{
		if(item->value_for_field(bundles::kFieldGrammarScope) != NULL_STR)
			installed.insert(grammar_info_t(item->name(), item->value_for_field(bundles::kFieldGrammarScope), item->uuid(), item->bundle_uuid()));
	}

	all = installed;

	// Exclude uninstalled grammars when offline
	if(network::can_reach_host(REST_API))
	{
		for(auto const& bundle : bundles_db::index())
		{
			for(auto const& grammar : bundle->grammars())
			{
				grammar_info_t info(grammar->name(), grammar->scope(), grammar->uuid(), bundle->uuid());
				all.insert(info);

				if(grammar->mode_line() != NULL_STR && regexp::search(grammar->mode_line(), firstLine))
				{
					recommended.insert(info);
				}
				else
				{
					for(auto const& ext : grammar->file_types())
					{
						if(path::rank(to_s(self.path), ext))
							recommended.insert(info);
					}
				}
			}
		}
	}

	for(auto const& info : all)
	{
		if(info.scope == "text.plain")
			recommended.insert(info);
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
	variables["DisplayName"] = path::display_name(to_s(self.path));
	std::string const ext = path::extensions(to_s(self.path));;
	if(ext != "")
	{
		self.persistentSetting = YES;
		if(ext != path::name(to_s(self.path)))
			variables["X"] = ext;
	}

	std::string const alert  = format_string::expand(to_s(self.alertFormatString), variables);
	std::string const info   = format_string::expand(to_s(self.infoFormatString), variables);
	std::string const choice = format_string::expand(to_s(self.useForAllFormatString), variables);

	[alertTextField setStringValue:[NSString stringWithCxxString:alert]];
	[infoTextField setStringValue:[NSString stringWithCxxString:info]];
	[useForAllCheckBox setTitle:[NSString stringWithCxxString:choice]];
}

- (void)setEnabledGrammars:(NSInteger)newFilter
{
	_enabledGrammars = newFilter;
	switch(_enabledGrammars)
	{
		case kEnabledGrammarsRecommended: self.grammars = self.recommendedGrammars; break;
		case kEnabledGrammarsInstalled:   self.grammars = self.installedGrammars;   break;
		case kEnabledGrammarsAll:         self.grammars = self.allGrammars;         break;
	}
	[fileTypesTableView scrollRowToVisible:[fileTypesTableView selectedRow]];
}

- (NSDictionary*)grammar
{
	return [_selectedGrammarIndexes count] == 0 ? nil : [_grammars objectAtIndex:[_selectedGrammarIndexes firstIndex]];
}

- (NSString*)fileType
{
	return [self.grammar objectForKey:@"scope"];
}

- (void)beginSheetModalForWindow:(NSWindow*)aWindow completionHandler:(void(^)(NSString* fileType))aCompletionHandler
{
	[self setupGrammars];
	self.grammars               = self.recommendedGrammars;
	self.selectedGrammarIndexes = [self.grammars count] == 0 ? [NSIndexSet indexSet] : [NSIndexSet indexSetWithIndex:0];

	self.canOpenDocument = YES;

	OakShowSheetForWindow(self.window, aWindow, ^(NSInteger returnCode){
		self.canOpenDocument = NO;
		if(returnCode == NSRunAbortedResponse)
			return aCompletionHandler(nil);

		NSDictionary* grammar = self.grammar;
		std::string scope      = to_s((NSString*)[grammar objectForKey:@"scope"]);
		oak::uuid_t uuid       = to_s((NSString*)[grammar objectForKey:@"uuid"]);
		oak::uuid_t bundleUUID = to_s((NSString*)[grammar objectForKey:@"bundleUUID"]);

		if(is_installed(uuid))
		{
			if(self.persistentSetting)
				file::set_type(to_s(self.path), scope);
			return aCompletionHandler(self.fileType);
		}

		for(auto const& bundle : bundles_db::index())
		{
			if(bundleUUID == bundle->uuid())
			{
				[installingBundleActivityTextField bind:NSValueBinding toObject:[BundlesManager sharedInstance] withKeyPath:@"activityText" options:nil];
				[installingBundleProgressIndicator bind:NSValueBinding toObject:[BundlesManager sharedInstance] withKeyPath:@"progress" options:nil];
				[installingBundleProgressIndicator bind:NSIsIndeterminateBinding toObject:[BundlesManager sharedInstance] withKeyPath:@"determinateProgress" options:@{ NSValueTransformerNameBindingOption: @"NSNegateBoolean" }];
				[installingBundleProgressIndicator startAnimation:self];

				OakShowSheetForWindow(installingBundleWindow, aWindow, ^(NSInteger returnCode){ });

				[[BundlesManager sharedInstance] installBundle:bundle completionHandler:^(BOOL success){
					[installingBundleProgressIndicator stopAnimation:self];
					[installingBundleProgressIndicator unbind:NSValueBinding];
					[installingBundleActivityTextField unbind:NSValueBinding];
					[NSApp endSheet:installingBundleWindow];
					[installingBundleWindow orderOut:self];

					aCompletionHandler(is_installed(uuid) ? self.fileType : @"text.plain");
				}];
				return;
			}
		}

		aCompletionHandler(nil);
	});
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
