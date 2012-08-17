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

@interface FSXcodeProjectDataSource (Private)
- (FSItem*)itemForProject:(XCProject*)project atURL:(NSURL*)anURL;
- (NSArray*)itemsForGroup:(XCGroup*)group withBasePath:(NSString*)basePath;
@end

@implementation FSXcodeProjectDataSource
- (id)initWithURL:(NSURL*)anURL options:(NSUInteger)someOptions
{
	if((self = [super init])) {
		_projects = [[NSMutableDictionary alloc] init];

		XCProject *project = [XCProject projectWithFilePath:[anURL path]];

		[_projects setObject:project forKey:anURL];

		self.rootItem = [self itemForProject:project atURL:anURL];
	}
	return self;
}

#pragma mark -

- (FSItem*)itemForProject:(XCProject*)project atURL:(NSURL*)anURL
{
	FSItem *item = [FSItem itemWithURL:anURL];
	item.icon     = [OakFileIconImage fileIconImageWithPath:[anURL path] size:NSMakeSize(16, 16)];
	item.name     = [NSString stringWithCxxString:path::display_name([[anURL path] fileSystemRepresentation])];

	NSMutableArray *results = [NSMutableArray array];
	NSString *basePath = [[project filePath] stringByDeletingLastPathComponent];
	for (XCGroup *group in [project rootGroups])
	{
		FSItem *item = [FSItem itemWithURL:[NSURL fileURLWithPath:[basePath stringByAppendingPathComponent:[group pathRelativeToProjectRoot]]]];
		if (group.displayName)
		{
			item.name = group.displayName;
		}
		item.children = [self itemsForGroup:group withBasePath:basePath];

		[results addObject:item];
	}
	item.children = results;
	return item;
}

- (NSArray*)itemsForGroup:(XCGroup*)group withBasePath:(NSString*)basePath
{
	NSMutableArray* results = [NSMutableArray new];
	for (id<XcodeGroupMember> member in [[group members] arrayByReversingOrder])
	{
		NSURL *itemURL = [NSURL fileURLWithPath:[basePath stringByAppendingPathComponent:[member pathRelativeToProjectRoot]]];
		if ([[[member pathRelativeToProjectRoot] pathExtension] isEqualToString:@"xcodeproj"])
		{
			XCProject *project = [XCProject projectWithFilePath:[itemURL path]];
			[results addObject:[self itemForProject:project atURL:itemURL]];
		}
		else
		{
			FSItem* item = [FSItem itemWithURL:itemURL];
			item.icon = [OakFileIconImage fileIconImageWithPath:[[item url] path] size:NSMakeSize(16, 16)];
			if (member.displayName)
			{
				item.name = member.displayName;
			}
			if ([member groupMemberType] == PBXGroup)
			{
				item.children = [[self itemsForGroup:member withBasePath:basePath] arrayByReversingOrder];
			}
			[results addObject:item];
		}
	}
	return [results copy];
}
@end
