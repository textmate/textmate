#import "../../Dialog2.h"
#import "../../TMDCommand.h"
#import "TMDNibController.h"

// ==========
// = Window =
// ==========

@interface TMDWindowCommand : TMDCommand
@end

// ===================
// = Command handler =
// ===================

std::string find_nib (std::string nibName, std::string currentDirectory, NSDictionary* env)
{
	std::vector<std::string> candidates;

	if(nibName.find(".nib") == std::string::npos)
		nibName += ".nib";

	if(nibName.size() && nibName[0] != '/') // relative path
	{
		candidates.push_back(currentDirectory + "/" + nibName);

		if(char const* bundleSupport = [[env objectForKey:@"TM_BUNDLE_SUPPORT"] UTF8String])
			candidates.push_back(bundleSupport + std::string("/nibs/") + nibName);

		if(char const* supportPath = [[env objectForKey:@"TM_SUPPORT_PATH"] UTF8String])
			candidates.push_back(supportPath + std::string("/nibs/") + nibName);
	}
	else
	{
		candidates.push_back(nibName);
	}

	for(std::string const& path : candidates)
	{
		struct stat sb;
		if(stat(path.c_str(), &sb) == 0)
			return path;
	}

	fprintf(stderr, "nib could not be loaded: %s (does not exist)\n", nibName.c_str());
	return "";
}

@implementation TMDWindowCommand
+ (void)load
{
	[super registerObject:[self new] forCommand:@"nib"];
}

/*
env|egrep 'DIALOG|TM_SUPPORT'|grep -v DIALOG_1|perl -pe 's/(.*?)=(.*)/export $1="$2"/'|pbcopy

"$DIALOG" nib --load "$TM_SUPPORT_PATH/../Bundles/Latex.tmbundle/Support/nibs/tex_prefs.nib" --defaults '{ latexEngineOptions = "bar"; }'

"$DIALOG" nib --load RequestString --center --model '{title = "Name?"; prompt = "Please enter your name:"; }'
"$DIALOG" nib --update 1 --model '{title = "updated title"; prompt = "updated prompt"; }'
"$DIALOG" nib --dispose 1
"$DIALOG" nib --list

"$DIALOG" nib --load "$TM_SUPPORT_PATH/../Bundles/SQL.tmbundle/Support/nibs/connections.nib" --defaults "{'SQL Connections' = ( { title = untitled; serverType = MySQL; hostName = localhost; userName = '$LOGNAME'; } ); }"

"$DIALOG" help nib
*/

- (void)handleCommand:(CLIProxy*)proxy
{
	NSDictionary* args = [proxy parameters];

	NSDictionary* model = [args objectForKey:@"model"];
	BOOL shouldCenter   = [args objectForKey:@"center"] ? YES : NO;
	BOOL shouldRunModal = [args objectForKey:@"modal"]  ? YES : NO;

	// FIXME this is needed only because we presently can’t express argument constraints (CLIProxy would otherwise correctly validate/convert CLI arguments)
	if([model isKindOfClass:[NSString class]])
		model = [NSPropertyListSerialization propertyListWithData:[(NSString*)model dataUsingEncoding:NSUTF8StringEncoding] options:NSPropertyListImmutable format:NULL error:NULL];

	if(NSString* updateToken = [args objectForKey:@"update"])
	{
		if(TMDNibController* nibController = [TMDNibController controllerForToken:updateToken])
				[nibController updateParametersWith:model];
		else	[proxy writeStringToError:[NSString stringWithFormat:@"No nib found for token: %@\n", updateToken]];
	}

	if(NSString* waitToken = [args objectForKey:@"wait"])
	{
		if(TMDNibController* nibController = [TMDNibController controllerForToken:waitToken])
				[nibController addClientFileHandle:[proxy outputHandle] modal:shouldRunModal];
		else	[proxy writeStringToError:[NSString stringWithFormat:@"No nib found for token: %@\n", waitToken]];
	}

	if(NSString* disposeToken = [args objectForKey:@"dispose"])
	{
		if(TMDNibController* nibController = [TMDNibController controllerForToken:disposeToken])
				[nibController tearDown];
		else	[proxy writeStringToError:[NSString stringWithFormat:@"No nib found for token: %@\n", disposeToken]];
	}

	if([args objectForKey:@"list"])
	{
		[proxy writeStringToOutput:@"Loaded nibs:\n"];
		for(TMDNibController* nibController in [TMDNibController controllers])
			[proxy writeStringToOutput:[NSString stringWithFormat:@"%@ (%@)\n", nibController.token, [[nibController window] title]]];
	}

	if(NSString* nibName = [args objectForKey:@"load"])
	{
		// TODO we should let an option type be ‘filename’ and have CLIProxy resolve these (and error when file does not exist)
		NSString* nib = @(find_nib([nibName UTF8String], [[proxy workingDirectory] UTF8String] ?: "", [proxy environment]).c_str());
		if(nib == nil || [nib length] == 0)
		{
			[proxy writeStringToError:[NSString stringWithFormat:@"No nib found for name: ‘%@’\n", nibName]];
		}
		else
		{
			if(TMDNibController* nibController = [[TMDNibController alloc] initWithNibPath:nib])
			{
				[nibController updateParametersWith:model];
				[nibController showWindowAndCenter:shouldCenter];

				[proxy writeStringToOutput:nibController.token];
			}
		}
	}
}

- (NSString*)commandDescription
{
	return @"Displays custom dialogs from NIBs.";
}

- (NSString*)usageForInvocation:(NSString*)invocation;
{
	return [NSString stringWithFormat:
		@"%1$@ --load «nib file» [«options»]\n"
		@"%1$@ --update «token» [«options»]\n"
		@"%1$@ --wait «token»\n"
		@"%1$@ --dispose «token»\n"
		@"%1$@ --list\n"
		@"\nThe nib will be disposed after user closes its window unless --wait is being used.\n"
		@"\nOptions:\n"
		@"\t--center\n"
		@"\t--modal\n"
		@"\t--model «plist»\n",
		invocation];
}
@end
