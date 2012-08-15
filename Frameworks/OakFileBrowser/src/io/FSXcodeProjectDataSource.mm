#import "FSXcodeProjectDataSource.h"
#import "FSItem.h"
#import <XcodeprojEditor/XCProject.h>
#import <XcodeprojEditor/XCGroup.h>
#import <XcodeprojEditor/XCSourceFile.h>
#import <XcodeprojEditor/XcodeGroupMember.h>
#import <OakFoundation/NSString Additions.h>
#import <OakAppKit/OakFileIconImage.h>
#import <plist/plist.h>
#import <io/path.h>
#import <cf/cf.h>
#import <ns/ns.h>
#import <oak/oak.h>
#import <oak/debug.h>

@interface FSXcodeProjectDataSource (Private)
- (NSArray*)itemsForGroup:(XCGroup*)group withBasePath:(NSString*)basePath;
@end

@implementation FSXcodeProjectDataSource
- (id)initWithURL:(NSURL*)anURL options:(NSUInteger)someOptions {
	if((self = [super init])) {
		_project = [[XCProject alloc] initWithFilePath:[anURL path]];

		self.rootItem = [FSItem itemWithURL:anURL];
		self.rootItem.icon     = [OakFileIconImage fileIconImageWithPath:[anURL path] size:NSMakeSize(16, 16)];
		self.rootItem.name     = [NSString stringWithCxxString:path::display_name([[anURL path] fileSystemRepresentation])];

		NSMutableArray *results = [NSMutableArray array];
		NSString *basePath = [[_project filePath] stringByDeletingLastPathComponent];
		for (XCGroup *group in [_project rootGroups])
		{
			FSItem *item = [FSItem itemWithURL:[NSURL fileURLWithPath:[basePath stringByAppendingPathComponent:[group pathRelativeToProjectRoot]]]];
			item.children = [self itemsForGroup:group withBasePath:basePath];

			[results addObject:item];
		}
		self.rootItem.children = results;
	}
	return self;
}

#pragma mark -

- (NSArray*)itemsForGroup:(XCGroup*)group withBasePath:(NSString*)basePath {
	NSMutableArray* results = [NSMutableArray new];
	for (id<XcodeGroupMember> member in [group members])
	{
		FSItem* item = [FSItem itemWithURL:[NSURL fileURLWithPath:[basePath stringByAppendingPathComponent:[member pathRelativeToProjectRoot]]]];
		item.icon = [OakFileIconImage fileIconImageWithPath:[[item url] path] size:NSMakeSize(16, 16)];
		if ([member groupMemberType] == PBXGroup)
		{
			item.children = [self itemsForGroup:member withBasePath:basePath];
		}
		[results addObject:item];
	}
	return [results copy];
}
@end
