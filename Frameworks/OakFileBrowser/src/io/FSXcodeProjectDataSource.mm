#import "FSXcodeProjectDataSource.h"
#import "FSItem.h"
#import <XcodeprojEditor/XCProject.h>
#import <XcodeprojEditor/XCGroup.h>
#import <XcodeprojEditor/XCSourceFile.h>
#import <XcodeprojEditor/XcodeGroupMember.h>
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
	NSString* path = [basePath stringByAppendingPathComponent:[relativePath stringByReplacingPercentEscapesUsingEncoding:NSUTF8StringEncoding]];
	return [NSURL fileURLWithPath:[path stringByResolvingSymlinksInPath]];
}

#pragma mark -

@interface FSXcodeProjectDataSource (Private)
- (FSItem*)itemForProject:(XCProject*)project atURL:(NSURL*)anURL;
- (NSArray*)itemsForGroup:(XCGroup*)group withBasePath:(NSString*)basePath;
@end

#pragma mark -

@implementation FSXcodeProjectDataSource
- (id)initWithURL:(NSURL*)anURL options:(NSUInteger)someOptions
{
	if((self = [super init])) {
		_projects = [[NSMutableDictionary alloc] init];

		XCProject* project = [XCProject projectWithFilePath:[anURL path]];

		self.rootItem = [self itemForProject:project atURL:anURL];

		[_projects setObject:project forKey:anURL];
	}
	return self;
}

#pragma mark -

- (FSItem*)itemForProject:(XCProject*)project atURL:(NSURL*)anURL
{
	FSItem* item = [FSItem itemWithURL:anURL];

	NSMutableArray* results = [NSMutableArray array];
	NSString* basePath = [[project filePath] stringByDeletingLastPathComponent];
	for (XCGroup* group in [project rootGroups])
	{
		if (isFaultyProductGroup(group))
		{
			continue;
		}
		FSItem* item = [FSItem itemWithURL:pathURLWithBaseAndRelativePath(basePath, [group pathRelativeToProjectRoot])];
		item.children = [self itemsForGroup:group withBasePath:basePath];
		if (group.displayName.length)
		{
			item.name = group.displayName;
		}
		if (item.name.length || item.children.count)
		{
			[results addObject:item];
		}
	}
	item.children = results;
	return item;
}

- (NSArray*)itemsForGroup:(XCGroup*)group withBasePath:(NSString*)basePath
{
	NSMutableArray* results = [NSMutableArray new];
	for (id<XcodeGroupMember> member in [group members])
	{
		NSURL* itemURL = pathURLWithBaseAndRelativePath(basePath, [member pathRelativeToProjectRoot]);
		if (![[NSFileManager defaultManager] fileExistsAtPath:[itemURL path]])
		{
			itemURL = pathURLWithBaseAndRelativePath(basePath, [[group pathRelativeToProjectRoot] stringByAppendingPathComponent:[member pathRelativeToProjectRoot]]);
		}
		if ([[[member pathRelativeToProjectRoot] pathExtension] isEqualToString:@"xcodeproj"])
		{
			XCProject* project = [XCProject projectWithFilePath:[itemURL path]];
			[results addObject:[self itemForProject:project atURL:itemURL]];

			[_projects setObject:project forKey:itemURL];
		}
		else
		{
			FSItem* item = [FSItem itemWithURL:itemURL];
			item.icon = [OakFileIconImage fileIconImageWithPath:[[item url] path] size:NSMakeSize(16, 16)];
			if (member.displayName.length)
			{
				item.name = member.displayName;
			}
			if ([member groupMemberType] == PBXGroup)
			{
				if (isFaultyProductGroup(group))
				{
					continue;
				}
				item.children = [self itemsForGroup:member withBasePath:basePath];
			}
			[results addObject:item];
		}
	}
	return [results copy];
}
@end
