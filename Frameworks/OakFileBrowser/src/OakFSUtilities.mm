#import "OakFSUtilities.h"
#import <io/io.h>
#import <OakSystem/application.h>
#import <OakFoundation/NSString Additions.h>
#import <OakAppKit/OakFileIconImage.h>

NSURL* kURLLocationComputer;
NSURL* kURLLocationHome;
NSURL* kURLLocationDesktop;
NSURL* kURLLocationFavorites;
NSURL* kURLLocationBundles;

__attribute__((constructor)) // executed after +loads and initializers in linked frameworks
static void initializeConstants ()
{
	@autoreleasepool {
		kURLLocationComputer  = [[NSURL alloc] initWithString:@"computer:///"];
		kURLLocationHome      = [[NSURL alloc] initFileURLWithPath:NSHomeDirectory() isDirectory:YES];
		kURLLocationDesktop   = [[NSURL alloc] initFileURLWithPath:[NSString stringWithCxxString:path::desktop()] isDirectory:YES];
		kURLLocationFavorites = [[NSURL alloc] initFileURLWithPath:[NSString stringWithCxxString:oak::application_t::support("Favorites")] isDirectory:YES];
		kURLLocationBundles   = [[NSURL alloc] initWithString:@"bundles:///"];
	}
}

NSString* DisplayName (NSURL* url, size_t numberOfParents)
{
	NSString* res = nil;
	if([[url scheme] isEqualToString:[kURLLocationComputer scheme]])
		res = CFBridgingRelease(SCDynamicStoreCopyComputerName(NULL, NULL));
	else if([[url scheme] isEqualToString:[kURLLocationBundles scheme]])
		res = @"Bundles";
	else // if([url isFileURL])
		res = [NSString stringWithCxxString:path::display_name([[url path] fileSystemRepresentation], numberOfParents)];
	return res ?: [url absoluteString] ?: @"«nil»";
}

NSImage* IconImage (NSURL* url, NSSize size)
{
	NSImage* iconImage = nil;
	if([[url scheme] isEqualToString:[kURLLocationComputer scheme]])
		iconImage = [NSImage imageNamed:NSImageNameComputer];
	else if([[url scheme] isEqualToString:[kURLLocationBundles scheme]])
		iconImage = [NSImage imageNamed:NSImageNameFolderSmart];
	else if([[url scheme] isEqualToString:@"scm"])
		iconImage = [NSImage imageNamed:NSImageNameFolderSmart];
	else // if([url isFileURL])
		iconImage = [OakFileIconImage fileIconImageWithPath:[url path] size:size];

	[iconImage setSize:size];
	return iconImage;
}

NSURL* ParentForURL (NSURL* url)
{
	struct statfs buf;
	NSString* currentPath = [url path];
	NSString* parentPath  = [currentPath stringByDeletingLastPathComponent];

	if([[url scheme] isEqualToString:[kURLLocationComputer scheme]])
		return nil;
	else if([currentPath isEqualToString:parentPath] || [url isFileURL] && statfs([currentPath fileSystemRepresentation], &buf) == 0 && path::normalize(buf.f_mntonname) == path::normalize([currentPath fileSystemRepresentation]))
		return kURLLocationComputer;
	else if([url isFileURL])
		return [NSURL fileURLWithPath:parentPath isDirectory:YES];
	else if([[url scheme] isEqualToString:@"scm"])
		return [NSURL fileURLWithPath:[url path] isDirectory:YES];
	else if([@[ @"xcodeproj", @"search" ] containsObject:[url scheme]])
		return [NSURL fileURLWithPath:parentPath isDirectory:YES];
	else
		return [[NSURL alloc] initWithScheme:[url scheme] host:[url host] path:parentPath];
}
