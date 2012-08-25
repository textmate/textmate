#import "FSXcodeProjectDataSource.h"
#import "FSItem.h"
#import <XcodeprojEditor/XCProject.h>
#import <XcodeprojEditor/XCGroup.h>
#import <XcodeprojEditor/XCSourceFile.h>
#import <XcodeprojEditor/XCTarget.h>
#import <XcodeprojEditor/XCBuildConfigurationList.h>
#import <XcodeprojEditor/XcodeGroupMember.h>
#import <XcodeprojEditor/XcodeSourceFileType.h>
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

#pragma mark -

@interface FSXcodeProjectDataSource (Private)
- (FSItem*)itemForFrameworkAtPath:(NSString*)path;
- (NSArray*)itemsForDirectoryAtPath:(NSString*)path;

- (FSItem*)itemForProject:(XCProject*)project atURL:(NSURL*)anURL;
- (NSArray*)itemsForGroup:(XCGroup*)group withBasePath:(NSString*)basePath inProject:(XCProject*)project;

- (NSString*)frameworkPathForSDKRoot:(NSString*)SDKRoot withBasePath:(NSString*)basePath;
@end

#pragma mark -

@implementation FSXcodeProjectDataSource
- (id)initWithURL:(NSURL*)anURL options:(NSUInteger)someOptions
{
	if((self = [super init])) {
		_projects = [[NSMutableDictionary alloc] init];

		if ([[NSFileManager defaultManager] fileExistsAtPath:[anURL path]])
		{
			XCProject* project = [XCProject projectWithFilePath:[anURL path]];

			self.rootItem = [self itemForProject:project atURL:anURL];

			[_projects setObject:project forKey:anURL];
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
	for (NSString* file in [[NSFileManager defaultManager] contentsOfDirectoryAtPath:path error:nil])
	{
		file = [path stringByAppendingPathComponent:file];

		FSItem* item = [FSItem itemWithURL:[NSURL fileURLWithPath:file]];
		NSDictionary* attributes = [[NSFileManager defaultManager] attributesOfItemAtPath:file error:nil];
		if ([attributes objectForKey:NSFileType] == NSFileTypeDirectory)
			item.children = [self itemsForDirectoryAtPath:file];
		[results addObject:item];
	}
	return results;
}

#pragma mark -

- (FSItem*)itemForProject:(XCProject*)project atURL:(NSURL*)anURL
{
	FSItem* item = [FSItem itemWithURL:anURL];
	item.name = [[project filePath] lastPathComponent];
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
		if (item.name.length || item.children.count)
			[results addObject:item];
	}
	item.children = results;
	return item;
}

- (NSArray*)itemsForGroup:(XCGroup*)group withBasePath:(NSString*)basePath inProject:(XCProject*)project
{
	NSMutableArray* results = [NSMutableArray new];
	for (id<XcodeGroupMember> member in [group members])
	{
		NSURL* itemURL = pathURLWithBaseAndRelativePath(basePath, [member pathRelativeToProjectRoot]);
		if (![[NSFileManager defaultManager] fileExistsAtPath:[itemURL path]])
			itemURL = pathURLWithBaseAndRelativePath(basePath, [[group pathRelativeToProjectRoot] stringByAppendingPathComponent:[member pathRelativeToProjectRoot]]);
		if ([[[member pathRelativeToProjectRoot] pathExtension] isEqualToString:@"xcodeproj"])
		{
			if ([[NSFileManager defaultManager] fileExistsAtPath:[itemURL path]])
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
				if (isFaultyProductGroup(group))
					continue;
				item.children = [self itemsForGroup:member withBasePath:basePath inProject:project];
			}
			else
			{
				if (![[NSFileManager defaultManager] fileExistsAtPath:[itemURL path]] && ([(XCSourceFile* )member type] == Framework || [[member pathRelativeToProjectRoot] hasSuffix:@"dylib"]))
				{
					NSArray* targets = [project targets];
					if (![targets count])
						continue;

					if (![_developerDirectoryPath length])
					{
						std::string xcodePath = io::exec("/usr/bin/xcode-select", "--print-path", NULL);
						xcodePath = xcodePath.substr(0, (xcodePath.length() - 1));

						_developerDirectoryPath = [NSString stringWithCxxString:xcodePath];
					}

					XCTarget* target = [targets objectAtIndex:0];
					XCBuildConfigurationList* targetBuildConfiguration = [target defaultConfiguration];
					XCBuildConfigurationList* projectBuildConfiguration = [project defaultConfiguration];

					NSString* SDKRoot = [targetBuildConfiguration valueForKey:@"SDKROOT"];
					if (!SDKRoot.length)
						SDKRoot = [projectBuildConfiguration valueForKey:@"SDKROOT"];

					NSString* frameworkPath = [self frameworkPathForSDKRoot:SDKRoot withBasePath:_developerDirectoryPath];

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

- (NSString*)frameworkPathForSDKRoot:(NSString*)SDKRoot withBasePath:(NSString*)basePath
{
	NSString *frameworkPath = nil;
	if ([SDKRoot rangeOfString:@"iphoneos" options:(NSCaseInsensitiveSearch | NSAnchoredSearch) range:NSMakeRange(0, SDKRoot.length)].location != NSNotFound)
		frameworkPath = [basePath stringByAppendingPathComponent:@"/Platforms/iPhoneOS.platform/Developer/SDKs/"];
	else if ([SDKRoot rangeOfString:@"mac" options:(NSCaseInsensitiveSearch | NSAnchoredSearch) range:NSMakeRange(0, SDKRoot.length)].location != NSNotFound)
		frameworkPath = [basePath stringByAppendingPathComponent:@"/Platforms/MacOSX.platform/Developer/SDKs/"];

	NSArray *files = [[NSFileManager defaultManager] contentsOfDirectoryAtPath:frameworkPath error:nil];
	NSString *workingSDKRoot = [files caseSensitiveMatchForString:SDKRoot];

	// if an SDK version was specified directly, use it
	NSInteger index = NSNotFound;

	if (workingSDKRoot)
		return [frameworkPath stringByAppendingPathComponent:workingSDKRoot];

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
