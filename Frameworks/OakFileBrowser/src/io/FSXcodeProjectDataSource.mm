#import "FSXcodeProjectDataSource.h"
#import "FSItem.h"
#import <XcodeprojEditor/XCProject.h>
#import <XcodeprojEditor/XCGroup.h>
#import <XcodeprojEditor/XCSourceFile.h>
#import <XcodeprojEditor/XCTarget.h>
#import <XcodeprojEditor/XCBuildConfigurationList.h>
#import <XcodeprojEditor/XcodeGroupMember.h>
#import <XcodeprojEditor/XcodeSourceFileType.h>
#import <OakFoundation/NSString Additions.h>
#import <OakFoundation/NSArray Additions.h>
#import <OakAppKit/OakFileIconImage.h>
#import <plist/plist.h>
#import <io/path.h>
#import <cf/cf.h>
#import <ns/ns.h>
#import <oak/oak.h>
#import <oak/debug.h>

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
		{
			self.rootItem = [FSItem itemWithURL:anURL];
		}
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
		if ([file hasPrefix:@"_"])
			continue;

		NSString* workingPath = [path stringByAppendingPathComponent:file];
		BOOL isDirectory = NO;
		[[NSFileManager defaultManager] fileExistsAtPath:workingPath isDirectory:&isDirectory];

		if (!isDirectory)
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
		file = [[path stringByAppendingPathComponent:file] stringByResolvingSymlinksInPath];

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

	NSMutableArray* results = [NSMutableArray array];
	NSString* basePath = [[project filePath] stringByDeletingLastPathComponent];
	for (XCGroup* group in [project rootGroups])
	{
		if (isFaultyProductGroup(group))
			continue;

		FSItem* item = [FSItem itemWithURL:pathURLWithBaseAndRelativePath(basePath, [group pathRelativeToProjectRoot])];
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
			{
				[results addObject:[FSItem itemWithURL:itemURL]];
			}
		}
		else
		{
			FSItem* item = [FSItem itemWithURL:itemURL];
			if (member.displayName.length)
				item.name = member.displayName;
			if ([member groupMemberType] == PBXGroup)
			{
				if (isFaultyProductGroup(group))
					continue;
				item.children = [self itemsForGroup:member withBasePath:basePath inProject:project];
			}
			else
			{
				if ([(XCSourceFile* )member type] == Framework || [[member pathRelativeToProjectRoot] hasSuffix:@"dylib"])
				{
					NSArray* targets = [project targets];
					if (![targets count])
						continue;

					if (![[NSFileManager defaultManager] fileExistsAtPath:@"/usr/bin/xcode-select"])
						continue;

					NSTask* findXcodeTask = [[NSTask alloc] init];
					[findXcodeTask setLaunchPath:@"/usr/bin/xcode-select"];
					[findXcodeTask setArguments:[NSArray arrayWithObject:@"--print-path"]];

					NSPipe* outputPipe = [NSPipe pipe];
					[findXcodeTask setStandardOutput:outputPipe];

					[findXcodeTask launch];
					[findXcodeTask waitUntilExit];

					NSData* results = [[outputPipe fileHandleForReading] readDataToEndOfFile];
					[findXcodeTask release];

					// Strip out null terminator from the results
					NSString* xcodePath = [[[NSString alloc] initWithData:[results subdataWithRange:NSMakeRange(0, [results length] - 1)] encoding:NSUTF8StringEncoding] autorelease];
					NSString* frameworkPath = nil;

					XCTarget* target = [targets objectAtIndex:0];
					XCBuildConfigurationList* targetBuildConfiguration = [target defaultConfiguration];
					XCBuildConfigurationList* projectBuildConfiguration = [project defaultConfiguration];

					NSString* SDKRoot = [targetBuildConfiguration valueForKey:@"SDKROOT"];
					if (!SDKRoot.length)
						SDKRoot = [projectBuildConfiguration valueForKey:@"SDKROOT"];

					if ([SDKRoot rangeOfString:@"iphone" options:(NSCaseInsensitiveSearch | NSAnchoredSearch) range:NSMakeRange(0, SDKRoot.length)].location != NSNotFound)
					{
						// TODO: iphoneos6.0
						frameworkPath = [xcodePath stringByAppendingPathComponent:@"/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS6.0.sdk/"];
					}
					else if ([SDKRoot rangeOfString:@"mac" options:(NSCaseInsensitiveSearch | NSAnchoredSearch) range:NSMakeRange(0, SDKRoot.length)].location != NSNotFound)
					{
						// TODO: macosx10.8
						frameworkPath = [xcodePath stringByAppendingPathComponent:@"/Platforms/MacOSX.platform/Developer/SDKs/iPhoneOS6.0.sdk/"];
					}

					frameworkPath = [frameworkPath stringByAppendingPathComponent:[member pathRelativeToProjectRoot]];
					if ([frameworkPath hasSuffix:@"framework"])
						item = [self itemForFrameworkAtPath:frameworkPath];
					else
					{
						item.url = [NSURL fileURLWithPath:frameworkPath];
					}
				}
			}
			item.icon = [OakFileIconImage fileIconImageWithPath:[[item url] path] size:NSMakeSize(16, 16)];
			[results addObject:item];
		}
	}
	return [results copy];
}
@end
