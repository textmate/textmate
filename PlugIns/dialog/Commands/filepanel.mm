#import "../Dialog2.h"
#import "../TMDCommand.h"

/*
"$DIALOG" help filepanel
*/

// =======================
// = TMDFilePanelCommand =
// =======================

@interface TMDFilePanelCommand : TMDCommand
@end

@implementation TMDFilePanelCommand
+ (void)load
{
	[super registerObject:[self new] forCommand:@"filepanel"];
}

- (NSSavePanel*)setupSavePanel:(NSSavePanel*)savePanel usingParameters:(NSDictionary*)args
{
	if(NSString* title = args[@"title"])
		[savePanel setTitle:title];
	if(NSString* prompt = args[@"prompt"])
		[savePanel setPrompt:prompt];
	if(NSString* message = args[@"message"])
		[savePanel setMessage:message];
	if(NSString* label = args[@"label"])
		[savePanel setNameFieldLabel:label];
	if(NSString* filename = args[@"filename"])
		[savePanel setNameFieldStringValue:filename];

	if(args[@"canCreateDirectories"])
		[savePanel setCanCreateDirectories:[args[@"canCreateDirectories"] boolValue]];
	if(args[@"treatsFilePackagesAsDirectories"])
		[savePanel setTreatsFilePackagesAsDirectories:[args[@"treatsFilePackagesAsDirectories"] boolValue]];
	if(args[@"showsHiddenFiles"])
		[savePanel setShowsHiddenFiles:[args[@"showsHiddenFiles"] boolValue]];

	if(NSString* path = args[@"defaultDirectory"])
	{
		if(NSURL* url = [NSURL fileURLWithPath:[path stringByResolvingSymlinksInPath] isDirectory:YES])
			[savePanel setDirectoryURL:url];
	}

	if(NSString* typesStr = args[@"allowedFileTypes"])
	{
		id rawTypes = [NSPropertyListSerialization propertyListWithData:[typesStr dataUsingEncoding:NSUTF8StringEncoding] options:NSPropertyListImmutable format:NULL error:NULL];
		NSArray* types = nil;
		if([rawTypes isKindOfClass:[NSString class]])
			types = [NSArray arrayWithObject:rawTypes];
		else if([rawTypes isKindOfClass:[NSArray class]])
			types = rawTypes;
		else
			fprintf(stderr, "no single string or plist array passed as value for option '--allowedFileTypes'\n");

		if(types)
			[savePanel setAllowedFileTypes:types];
	}

	if(args[@"allowsOtherFileTypes"])
		[savePanel setAllowsOtherFileTypes:[args[@"allowsOtherFileTypes"] boolValue]];

	return savePanel;
}

- (NSOpenPanel*)setupOpenPanel:(NSOpenPanel*)openPanel usingParameters:(NSDictionary*)args
{
	[self setupSavePanel:openPanel usingParameters:args];

	if(args[@"canChooseFiles"])
		[openPanel setCanChooseFiles:[args[@"canChooseFiles"] boolValue]];
	if(args[@"canChooseDirectories"])
		[openPanel setCanChooseDirectories:[args[@"canChooseDirectories"] boolValue]];
	if(args[@"allowsMultipleSelection"])
		[openPanel setAllowsMultipleSelection:[args[@"allowsMultipleSelection"] boolValue]];

	return openPanel;
}

- (void)handleCommand:(CLIProxy*)proxy
{
	NSDictionary* args = [proxy parameters];
	NSMutableDictionary* resultDict = [NSMutableDictionary dictionary];

	if(args[@"isSavePanel"])
	{
		NSSavePanel* panel = [self setupSavePanel:[NSSavePanel savePanel] usingParameters:args];
		if([panel runModal] == NSModalResponseOK)
			resultDict[@"path"] = [[panel URL] path];
	}
	else
	{
		NSOpenPanel* panel = [self setupOpenPanel:[NSOpenPanel openPanel] usingParameters:args];
		if([panel runModal] == NSModalResponseOK)
		{
			NSMutableArray* paths = [NSMutableArray arrayWithCapacity:[[panel URLs] count]];
			for(NSURL* url in [panel URLs])
				[paths addObject:[url path]];
			resultDict[@"paths"] = paths;

			if([[panel URLs] count] == 1)
				resultDict[@"path"] = [[panel URL] path];
		}
	}

	[TMDCommand writePropertyList:resultDict toFileHandle:[proxy outputHandle]];
}

- (NSString*)commandDescription
{
	return @"Shows an open file/folder or save file panel.";
}

- (NSString*)usageForInvocation:(NSString*)invocation;
{
	return [NSString stringWithFormat:
		@"\t%1$@ --title Title --prompt Prompt --message Message --defaultDirectory '~/Desktop' showsHiddenFiles 1\n"
		@"\t%1$@ --isSavePanel --title 'Save Me' --label 'Label:' --filename 'test.txt' --allowedFileTypes '(txt,tab)'\n"
		@"\nOptions:\n"
		@"\t--allowedFileTypes «plist array of allowed file types or a single string»\n"
		@"\t\te.g. --allowedFileTypes pdf\n"
		@"\t\t     --allowedFileTypes '(txt,tab)'\n"
		@"\t--allowsMultipleSelection {1,0} [not in 'isSavePanel' mode]\n"
		@"\t--allowsOtherFileTypes {1,0}\n"
		@"\t--canChooseDirectories {1,0}\n"
		@"\t--canChooseFiles {1,0}\n"
		@"\t--canCreateDirectories {1,0}\n"
		@"\t--defaultDirectory «valid directory path»\n"
		@"\t\tdefault directory for panel, if not passed the last visited one will be used\n"
		@"\t--filename «default file name» [only in 'isSavePanel' mode]\n"
		@"\t--isSavePanel\n"
		@"\t\tif passed shows a save file panel otherwise an open file panel\n"
		@"\t--label «a label» [only in 'isSavePanel' mode]\n"
		@"\t\tdefault 'Save As:'\n"
		@"\t--message «a message»\n"
		@"\t--prompt «a prompt»\n"
		@"\t\taction button title - default 'Open' or 'Save' for isSavePanel mode\n"
		@"\t--showsHiddenFiles {1,0}\n"
		@"\t--title «a title»\n"
		@"\t\twindow title - default 'Open' or 'Save' for isSavePanel mode\n"
		@"\t--treatsFilePackagesAsDirectories {1,0}\n"
			, invocation];
}
@end
