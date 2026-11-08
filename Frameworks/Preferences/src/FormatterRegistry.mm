#import "FormatterRegistry.h"
#import "Keys.h"
#import <OakFoundation/NSString Additions.h>
#import <ns/ns.h>
#import <io/environment.h>
#import <io/path.h>
#import <text/tokenize.h>

@implementation FormatterEntry
@end

static NSArray<FormatterEntry*>* DefaultFormatterTable ()
{
	struct { NSString* glob; NSString* name; NSString* cmd; NSString* exe; } const defaults[] = {
		{ @"*.swift",                    @"swiftformat",   @"swiftformat --stdinpath \"$TM_FILEPATH\"",           @"swiftformat"   },
		{ @"*.{js,ts,jsx,tsx,css,json}", @"prettier",      @"prettier --ignore-path \"\" --stdin-filepath \"$TM_FILEPATH\"",         @"prettier"      },
		{ @"*.{html,vue,svelte}",        @"prettier",      @"prettier --ignore-path \"\" --stdin-filepath \"$TM_FILEPATH\"",         @"prettier"      },
		{ @"*.php",                      @"prettier",      @"prettier --ignore-path \"\" --parser=php --stdin-filepath \"$TM_FILEPATH\"", @"prettier" },
		{ @"*.py",                       @"black",          @"black -q -",                                         @"black"         },
		{ @"*.go",                       @"gofmt",          @"gofmt",                                              @"gofmt"         },
		{ @"*.rs",                       @"rustfmt",        @"rustfmt --edition 2021",                             @"rustfmt"       },
		{ @"*.{c,cc,cpp,h,hpp,m,mm}",   @"clang-format",   @"clang-format",                                       @"clang-format"  },
		{ @"*.rb",                       @"rubocop",        @"rubocop -A --stderr --stdin \"$TM_FILEPATH\"",       @"rubocop"       },
	};

	NSMutableArray* entries = [NSMutableArray array];
	for(auto const& d : defaults)
	{
		FormatterEntry* e = [[FormatterEntry alloc] init];
		e.fileTypeGlob    = d.glob;
		e.formatterName   = d.name;
		e.command          = d.cmd;
		e.executableName   = d.exe;
		e.enabled          = YES;
		[entries addObject:e];
	}
	return entries;
}

@interface FormatterRegistry ()
@property (nonatomic) NSMutableArray<FormatterEntry*>* entries;
@end

@implementation FormatterRegistry
+ (instancetype)sharedInstance
{
	static FormatterRegistry* instance = [self new];
	return instance;
}

- (instancetype)init
{
	if(self = [super init])
	{
		[self loadFromDefaults];
		[self detectAll];
	}
	return self;
}

- (void)loadFromDefaults
{
	NSArray* saved = [NSUserDefaults.standardUserDefaults arrayForKey:kUserDefaultsFormatterConfigurationsKey];
	if(saved.count)
	{
		NSMutableArray* entries = [NSMutableArray array];
		for(NSDictionary* dict in saved)
		{
			FormatterEntry* e = [[FormatterEntry alloc] init];
			e.fileTypeGlob    = dict[@"glob"] ?: @"";
			e.formatterName   = dict[@"name"] ?: @"";
			e.command          = dict[@"command"] ?: @"";
			e.executableName   = dict[@"executable"] ?: @"";
			e.enabled          = [dict[@"enabled"] boolValue];
			[entries addObject:e];
		}
		_entries = entries;
	}
	else
	{
		_entries = [DefaultFormatterTable() mutableCopy];
	}
}

- (void)saveToDefaults
{
	NSMutableArray* array = [NSMutableArray array];
	for(FormatterEntry* e in _entries)
	{
		[array addObject:@{
			@"glob":        e.fileTypeGlob ?: @"",
			@"name":        e.formatterName ?: @"",
			@"command":     e.command ?: @"",
			@"executable":  e.executableName ?: @"",
			@"enabled":     @(e.enabled),
		}];
	}
	[NSUserDefaults.standardUserDefaults setObject:array forKey:kUserDefaultsFormatterConfigurationsKey];
}

- (void)detectAll
{
	std::string const& envPath = oak::basic_environment().at("PATH");
	std::vector<std::string> searchPaths;
	for(auto const& p : text::tokenize(envPath.begin(), envPath.end(), ':'))
	{
		if(!p.empty())
			searchPaths.push_back(p);
	}

	NSLog(@"[Formatters] Detecting formatters in %zu search paths", searchPaths.size());

	for(FormatterEntry* entry in _entries)
	{
		std::string exe = to_s(entry.executableName);
		entry.detectedPath = nil;

		for(auto const& dir : searchPaths)
		{
			std::string candidate = path::join(dir, exe);
			if(path::is_executable(candidate))
			{
				entry.detectedPath = [NSString stringWithCxxString:candidate];
				NSLog(@"[Formatters] Found %s at %s", exe.c_str(), candidate.c_str());
				break;
			}
		}

		if(!entry.detectedPath)
			NSLog(@"[Formatters] %s not found in any search path", exe.c_str());
	}
}

- (NSString*)formatCommandForPath:(NSString*)filePath
{
	if(!filePath)
		return nil;

	NSString* fileName = filePath.lastPathComponent;
	for(FormatterEntry* entry in _entries)
	{
		if(!entry.enabled || !entry.detectedPath)
			continue;

		if([self fileName:fileName matchesGlob:entry.fileTypeGlob])
			return entry.command;
	}
	return nil;
}

- (BOOL)fileName:(NSString*)fileName matchesGlob:(NSString*)glob
{
	if([glob containsString:@"{"])
	{
		NSRange braceOpen = [glob rangeOfString:@"{"];
		NSRange braceClose = [glob rangeOfString:@"}" options:0 range:NSMakeRange(braceOpen.location, glob.length - braceOpen.location)];
		if(braceClose.location != NSNotFound)
		{
			NSString* prefix = [glob substringToIndex:braceOpen.location];
			NSString* suffix = [glob substringFromIndex:braceClose.location + 1];
			NSString* alternatives = [glob substringWithRange:NSMakeRange(braceOpen.location + 1, braceClose.location - braceOpen.location - 1)];
			for(NSString* alt in [alternatives componentsSeparatedByString:@","])
			{
				NSString* expandedGlob = [NSString stringWithFormat:@"%@%@%@", prefix, alt, suffix];
				if([self fileName:fileName matchesGlob:expandedGlob])
					return YES;
			}
			return NO;
		}
	}

	if([glob hasPrefix:@"*."])
	{
		NSString* ext = [glob substringFromIndex:2];
		return [[fileName pathExtension] caseInsensitiveCompare:ext] == NSOrderedSame;
	}

	return NO;
}

- (NSArray<FormatterEntry*>*)allEntries
{
	return [_entries copy];
}

- (void)setEnabled:(BOOL)enabled forEntryAtIndex:(NSUInteger)index
{
	if(index < _entries.count)
	{
		_entries[index].enabled = enabled;
		[self saveToDefaults];
	}
}

- (void)setCommand:(NSString*)command forEntryAtIndex:(NSUInteger)index
{
	if(index < _entries.count)
	{
		_entries[index].command = command;
		[self saveToDefaults];
	}
}
@end
