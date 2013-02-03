#import "FSSCMDataSource.h"
#import "FSItem.h"
#import <scm/scm.h>
#import <OakFoundation/NSString Additions.h>
#import <OakAppKit/OakFileIconImage.h>
#import <io/path.h>
#import <text/encode.h>
#import <text/ctype.h>
#import <text/format.h>
#import <oak/oak.h>

static NSImage* SCMFolderIcon ()
{
	NSImage* res = [[NSWorkspace sharedWorkspace] iconForFileType:NSFileTypeForHFSTypeCode(kGenericFolderIcon)];
	[res setSize:NSMakeSize(16, 16)];
	return res;
}

static NSURL* URLAppend (NSURL* base, NSString* relativePath)
{
	return [NSURL URLWithString:[[NSURL URLWithString:relativePath relativeToURL:base] absoluteString]];
}

static NSArray* convert (std::vector<std::string> const& paths, std::string const& wcPath, NSUInteger options)
{
	NSMutableArray* res = [NSMutableArray array];
	iterate(path, paths)
	{
		FSItem* item    = [FSItem itemWithURL:[NSURL fileURLWithPath:[NSString stringWithCxxString:*path]]];
		item.target     = [NSURL fileURLWithPath:[NSString stringWithCxxString:*path]];
		item.labelIndex = path::label_index(*path);
		item.toolTip    = [NSString stringWithCxxString:path::relative_to(*path, wcPath)];
		item.leaf       = YES;
		[res addObject:item];
	}
	return [FSDataSource sortArray:res usingOptions:options];
}

template <typename _Iter>
_Iter prune_path_parents (_Iter it, _Iter last)
{
	_Iter out = it;
	std::sort(it, last);
	std::reverse(it, last);
	for(std::string child = NULL_STR; it != last; child = *it++)
	{
		if(child.size() <= it->size() || child.at(it->size()) != '/' || child.find(*it) != 0)
			*out++ = *it;
	}
	return out;
}

template <typename _Iter>
_Iter prune_path_children (_Iter it, _Iter last)
{
	_Iter out = it;
	std::sort(it, last);
	for(std::string parent = NULL_STR; it != last; ++it)
	{
		if(it->size() <= parent.size() || it->at(parent.size()) != '/' || it->find(parent) != 0)
			*out++ = parent = *it;
	}
	return out;
}

@implementation FSSCMDataSource
{
	OBJC_WATCH_LEAKS(FSSCMDataSource);
	NSUInteger options;
	scm::ng::info_ptr scmInfo;
}

+ (NSURL*)scmURLWithPath:(NSString*)aPath
{
	std::string root = scm::ng::root_for_path([aPath fileSystemRepresentation]);
	if(root != NULL_STR)
		return [NSURL URLWithString:[NSString stringWithCxxString:"scm://localhost" + encode::url_part(root, "/") + "/"]];
	return [NSURL fileURLWithPath:aPath];
}

- (NSArray*)repositoryStatus
{
	std::vector<std::string> unstagedPaths, untrackedPaths;

	for(auto pair : scmInfo->status())
	{
		if(pair.second & (scm::status::modified|scm::status::added|scm::status::deleted|scm::status::conflicted|scm::status::unversioned))
		{
			if(pair.second & scm::status::unversioned)
					untrackedPaths.push_back(pair.first);
			else	unstagedPaths.push_back(pair.first);
		}
	}

	if(!scmInfo->tracks_directories())
	{
		unstagedPaths.erase(prune_path_parents(unstagedPaths.begin(), unstagedPaths.end()), unstagedPaths.end());
		untrackedPaths.erase(prune_path_children(untrackedPaths.begin(), untrackedPaths.end()), untrackedPaths.end());
	}

	std::sort(unstagedPaths.begin(), unstagedPaths.end(), text::less_t());
	std::sort(untrackedPaths.begin(), untrackedPaths.end(), text::less_t());

	FSItem* unstagedItem  = [FSItem itemWithURL:URLAppend(self.rootItem.url, @".unstaged/")];
	unstagedItem.icon     = SCMFolderIcon();
	unstagedItem.name     = @"Uncommitted Changes";
	unstagedItem.group    = YES;
	unstagedItem.children = convert(unstagedPaths, scmInfo->root_path(), options);

	FSItem* untrackedItem  = [FSItem itemWithURL:URLAppend(self.rootItem.url, @".untracked/")];
	untrackedItem.icon     = SCMFolderIcon();
	untrackedItem.name     = @"Untracked Items";
	untrackedItem.group    = YES;
	untrackedItem.children = convert(untrackedPaths, scmInfo->root_path(), options);

	NSMutableArray* children = [NSMutableArray array];
	[children addObject:unstagedItem];
	[children addObject:untrackedItem];
	return children;
}

- (void)postReloadNotification
{
	[[NSNotificationCenter defaultCenter] postNotificationName:FSItemDidReloadNotification object:self userInfo:@{ @"item" : self.rootItem, @"children" : [FSDataSource sortArray:[self repositoryStatus] usingOptions:options], @"recursive" : @YES }];
}

- (id)initWithURL:(NSURL*)anURL options:(NSUInteger)someOptions
{
	if((self = [super init]))
	{
		options = someOptions;

		std::string const rootPath = [[anURL path] fileSystemRepresentation];
		if(scmInfo = scm::ng::info(rootPath))
		{
			std::string name = path::display_name(rootPath);
			if(!scmInfo->dry())
			{
				auto const& vars = scmInfo->variables();
				auto const branch = vars.find("TM_SCM_BRANCH");
				if(branch != vars.end())
					name = text::format("%s (%s)", path::display_name(scmInfo->root_path()).c_str(), branch->second.c_str());
			}

			self.rootItem          = [FSItem itemWithURL:anURL];
			self.rootItem.icon     = [NSImage imageNamed:NSImageNameFolderSmart];
			self.rootItem.name     = [NSString stringWithCxxString:name];
			self.rootItem.children = [self repositoryStatus];

			__weak FSSCMDataSource* weakSelf = self;
			scmInfo->add_callback(^(scm::ng::info_t const&){
				[weakSelf postReloadNotification];
			});
		}
	}
	return self;
}

- (NSArray*)expandedURLs
{
	return @[ URLAppend(self.rootItem.url, @".unstaged/"), URLAppend(self.rootItem.url, @".untracked/") ];
}
@end
