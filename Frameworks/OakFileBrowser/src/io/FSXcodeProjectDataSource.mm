#import "FSXcodeProjectDataSource.h"
#import "FSItem.h"
#import <XcodeEditor/XCProject.h>
#import <XcodeEditor/XCGroup.h>
#import <XcodeEditor/XCSourceFile.h>
#import <XcodeEditor/XCTarget.h>
#import <XcodeEditor/XCBuildConfigurationList.h>
#import <XcodeEditor/XcodeGroupMember.h>
#import <XcodeEditor/XcodeSourceFileType.h>
#import <OakFoundation/NSArray Additions.h>
#import <OakFoundation/NSString Additions.h>
#import <OakAppKit/OakFileIconImage.h>
#import <io/path.h>
#import <io/exec.h>

static BOOL isFaultyProductGroup(XCGroup* group)
{
	return ([group groupMemberType] == PBXGroup && [[group alias] isEqualToString:@"Products"]);
}

static NSURL* pathURLWithBaseAndRelativePath(NSString* basePath, NSString* relativePath)
{
	return [NSURL fileURLWithPath:[[basePath stringByAppendingPathComponent:relativePath] stringByResolvingSymlinksInPath]];
}

static NSString *caseSensitiveMatchInArrayForString(NSArray *array, NSString *stringToMatch)
{
	for (NSString *possibleString in array)
	{
		if (![stringToMatch caseInsensitiveCompare:possibleString])
			return [possibleString copy];
	}
	return nil;
}

#pragma mark -

@interface FSXcodeProjectDataSource (Private)
- (FSItem*)itemForFrameworkAtPath:(NSString*)path;
- (NSArray*)itemsForDirectoryAtPath:(NSString*)path;

- (FSItem*)itemForProject:(XCProject*)project atURL:(NSURL*)anURL;
- (NSArray*)itemsForGroup:(XCGroup*)group withBasePath:(NSString*)basePath inProject:(XCProject*)project;

- (NSString*)frameworkPathForSDKRoot:(NSString*)SDKRoot;
@end

#pragma mark -

@implementation FSXcodeProjectDataSource
- (id)initWithURL:(NSURL*)anURL options:(NSUInteger)someOptions
{
	if((self = [super init])) {
		_projects = [[NSMutableDictionary alloc] init];

		NSString* xcodeprojPath = [anURL path];
		if ([xcodeprojPath existsAsPath])
		{
			XCProject* project = [XCProject projectWithFilePath:xcodeprojPath];

			self.rootItem = [self itemForProject:project atURL:[NSURL fileURLWithPath:xcodeprojPath]];

			[_projects setObject:project forKey:[NSURL fileURLWithPath:xcodeprojPath]];
		}
		else
			self.rootItem = [FSItem itemWithURL:anURL];
	}
	return self;
}

#pragma mark -

- (FSItem*)itemForFrameworkAtPath:(NSString*)path
{
	FSItem* item = [FSItem itemWithURL:[NSURL fileURLWithPath:path]];

	NSMutableArray* results = [NSMutableArray array];
	for (NSString* file in [[NSFileManager defaultManager] contentsOfDirectoryAtPath:path error:nil])
	{
		if ([file hasPrefix:@"_CodeSignature"])
			continue;

		NSString* workingPath = [path stringByAppendingPathComponent:file];
		if (![workingPath isDirectory])
			continue;

		FSItem* item = [FSItem itemWithURL:[NSURL fileURLWithPath:workingPath]];
		item.children = [self itemsForDirectoryAtPath:[item path]];
		[results addObject:item];
	}
	item.children = results;

	return item;
}

- (NSArray*)itemsForDirectoryAtPath:(NSString*)path
{
	NSMutableArray* results = [NSMutableArray array];
	for (NSString* name in [[NSFileManager defaultManager] contentsOfDirectoryAtPath:path error:nil])
	{
		NSString* file = [path stringByAppendingPathComponent:name];

		FSItem* item = [FSItem itemWithURL:[NSURL fileURLWithPath:file]];
		if ([file isDirectory])
			item.children = [self itemsForDirectoryAtPath:file];
		[results addObject:item];
	}
	return [results copy];
}

#pragma mark -

- (FSItem*)itemForProject:(XCProject*)project atURL:(NSURL*)anURL
{
	FSItem* item = [FSItem itemWithURL:anURL];
	item.name = [[[project filePath] lastPathComponent] stringByDeletingPathExtension];
	item.url = [anURL URLByDeletingLastPathComponent];

	NSMutableArray* results = [NSMutableArray array];
	NSString* basePath = [[project filePath] stringByDeletingLastPathComponent];
	for (XCGroup* group in [project rootGroups])
	{
		if (isFaultyProductGroup(group))
			continue;

		FSItem* item = [FSItem itemWithURL:pathURLWithBaseAndRelativePath(basePath, [group pathRelativeToProjectRoot])];
		item.name = [anURL lastPathComponent];
		item.children = [self itemsForGroup:group withBasePath:basePath inProject:project];
		[results addObject:item];
	}
	item.children = results;
	return item;
}

- (NSArray*)itemsForGroup:(XCGroup*)group withBasePath:(NSString*)basePath inProject:(XCProject*)project
{
	NSMutableArray* results = [NSMutableArray array];
	for (id<XcodeGroupMember> member in [group members])
	{
		NSURL* itemURL = pathURLWithBaseAndRelativePath(basePath, [member pathRelativeToProjectRoot]);
		if (![[itemURL path] existsAsPath])
			itemURL = pathURLWithBaseAndRelativePath(basePath, [[group pathRelativeToProjectRoot] stringByAppendingPathComponent:[member pathRelativeToProjectRoot]]);
		if (![[itemURL path] existsAsPath])
			itemURL = pathURLWithBaseAndRelativePath(basePath, [[group pathRelativeToProjectRoot] stringByAppendingPathComponent:[member displayName]]);
		if (![[itemURL path] existsAsPath])
			itemURL = [NSURL fileURLWithPath:basePath];

		if ([[[member pathRelativeToProjectRoot] pathExtension] isEqualToString:@"xcodeproj"])
		{
			if ([[itemURL path] existsAsPath])
			{
				XCProject* project = [XCProject projectWithFilePath:[itemURL path]];
				[results addObject:[self itemForProject:project atURL:itemURL]];

				[_projects setObject:project forKey:itemURL];
			}
			else
				[results addObject:[FSItem itemWithURL:itemURL]];
		}
		else
		{
			FSItem* item = [FSItem itemWithURL:itemURL];
			item.name = member.displayName;

			if ([member groupMemberType] == PBXGroup)
			{
				if (isFaultyProductGroup(member))
					continue;
				item.children = [self itemsForGroup:member withBasePath:basePath inProject:project];
			}
			else
			{
				if (([(XCSourceFile* )member type] == Framework || [[member pathRelativeToProjectRoot] hasSuffix:@"dylib"]))
				{
					NSArray* targets = [project targets];
					if (![targets count])
						continue;

					if (![_developerDirectoryPath length])
					{
						std::string xcodePath = io::exec("/usr/bin/xcode-select", "--print-path", NULL);
						xcodePath = xcodePath.substr(0, (xcodePath.length() - 1));

						_developerDirectoryPath = [[NSString stringWithCxxString:xcodePath] copy];
					}

					XCTarget* target = [targets objectAtIndex:0];
					XCBuildConfigurationList* targetBuildConfiguration = [target defaultConfiguration];
					XCBuildConfigurationList* projectBuildConfiguration = [project defaultConfiguration];

					NSString* SDKRoot = [targetBuildConfiguration valueForKey:@"SDKROOT"];
					if (!SDKRoot.length)
						SDKRoot = [projectBuildConfiguration valueForKey:@"SDKROOT"];

					if (!SDKRoot.length)
						SDKRoot = @"macosx";

					NSString* frameworkPath = [self frameworkPathForSDKRoot:SDKRoot];

					frameworkPath = [frameworkPath stringByAppendingPathComponent:[member pathRelativeToProjectRoot]];
					if ([frameworkPath hasSuffix:@"framework"])
						item = [self itemForFrameworkAtPath:frameworkPath];
					else
						item.url = [NSURL fileURLWithPath:frameworkPath];
				}
			}
			item.icon = [OakFileIconImage fileIconImageWithPath:[[item url] path] size:NSMakeSize(16, 16)];
			[results addObject:item];
		}
	}
	return [results copy];
}

#pragma mark -

- (NSString*)frameworkPathForSDKRoot:(NSString*)SDKRoot
{
	NSString *frameworkPath = nil;
	if ([SDKRoot rangeOfString:@"iphoneos" options:(NSCaseInsensitiveSearch | NSAnchoredSearch) range:NSMakeRange(0, SDKRoot.length)].location != NSNotFound)
		frameworkPath = [_developerDirectoryPath stringByAppendingPathComponent:@"/Platforms/iPhoneOS.platform/Developer/SDKs/"];
	else if ([SDKRoot rangeOfString:@"mac" options:(NSCaseInsensitiveSearch | NSAnchoredSearch) range:NSMakeRange(0, SDKRoot.length)].location != NSNotFound)
		frameworkPath = [_developerDirectoryPath stringByAppendingPathComponent:@"/Platforms/MacOSX.platform/Developer/SDKs/"];

	NSArray *files = [[NSFileManager defaultManager] contentsOfDirectoryAtPath:frameworkPath error:nil];
	NSString *workingSDKRoot = caseSensitiveMatchInArrayForString(files, [SDKRoot stringByAppendingString:@".sdk"]);

	// if an SDK version was specified directly, use it
	if (workingSDKRoot)
		return [frameworkPath stringByAppendingPathComponent:workingSDKRoot];

	NSInteger index = NSNotFound;
	if ([SDKRoot isEqualToString:@"iphoneos"])
		index = @"iphoneos".length;
	else if ([SDKRoot isEqualToString:@"macosx"])
		index = @"macosx".length;

	if (index == NSNotFound)
		return nil;

	files = [files sortedArrayUsingComparator:^(id one, id two) {
		CGFloat oneFloat = [[one substringFromIndex:index] floatValue];
		CGFloat twoFloat = [[two substringFromIndex:index] floatValue];
		if (oneFloat > twoFloat)
			return (NSComparisonResult)NSOrderedDescending;
		if (oneFloat < twoFloat)
			return (NSComparisonResult)NSOrderedAscending;
		return (NSComparisonResult)NSOrderedSame;
	}];

	return [frameworkPath stringByAppendingPathComponent:[files lastObject]];
}

#pragma mark -

- (BOOL)outlineView:(NSOutlineView*)anOutlineView acceptDrop:(id <NSDraggingInfo>)info item:(FSItem*)item childIndex:(NSInteger)childIndex
{
	return NO;
}
@end
