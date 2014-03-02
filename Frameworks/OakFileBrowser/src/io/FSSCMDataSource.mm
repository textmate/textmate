#import "FSSCMDataSource.h"
#import "FSItem.h"
#import <scm/scm.h>
#import <OakFoundation/NSString Additions.h>
#import <io/path.h>
#import <text/encode.h>
#import <text/ctype.h>
#import <text/format.h>
#import <oak/oak.h>

static OakFileIconImage* SCMFolderIcon ()
{
	OakFileIconImage* res = [[OakFileIconImage alloc] initWithSize:NSMakeSize(16, 16)];
	res.directory = YES;
	return res;
}

static NSURL* URLAppend (NSURL* base, NSString* relativePath)
{
	return [NSURL URLWithString:[[NSURL URLWithString:relativePath relativeToURL:base] absoluteString]];
}

static NSArray* convert (std::vector<std::string> const& paths, std::string const& wcPath, NSUInteger options, bool hideSCMBadge = false)
{
	NSMutableArray* res = [NSMutableArray array];
	for(auto const& path : paths)
	{
		FSItem* item    = [FSItem itemWithURL:[NSURL fileURLWithPath:[NSString stringWithCxxString:path]]];
		item.target     = [NSURL fileURLWithPath:[NSString stringWithCxxString:path]];
		item.labelIndex = path::label_index(path);
		item.toolTip    = [NSString stringWithCxxString:path::relative_to(path, wcPath)];
		item.leaf       = YES;

		if(hideSCMBadge)
			item.icon.scmStatus = scm::status::none;

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
		if(!path::is_child(child, *it))
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
		if(!path::is_child(*it, parent))
			*out++ = parent = *it;
	}
	return out;
}

@implementation FSSCMDataSource
{
	OBJC_WATCH_LEAKS(FSSCMDataSource);
	NSUInteger options;
	scm::info_ptr scmInfo;
}

+ (NSURL*)scmURLWithPath:(NSString*)aPath
{
	std::string root = scm::root_for_path([aPath fileSystemRepresentation]);
	if(root != NULL_STR)
		return [NSURL URLWithString:[NSString stringWithCxxString:"scm://localhost" + encode::url_part(root, "/") + "/"]];
	return [NSURL URLWithString:[@"scm://locahost" stringByAppendingString:[aPath stringByAppendingString:@"?status=unversioned"]]];
}

+ (NSString*)parseSCMURLStatusQuery:(NSURL*)anURL
{
	NSArray* query = [[anURL query] componentsSeparatedByString:@"="];
	return [query lastObject] ?: @"all";
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

	FSItem* unstagedItem  = [FSItem itemWithURL:URLAppend(self.rootItem.url, @"?status=unstaged")];
	unstagedItem.icon     = SCMFolderIcon();
	unstagedItem.name     = @"Uncommitted Changes";
	unstagedItem.group    = YES;
	unstagedItem.children = convert(unstagedPaths, scmInfo->root_path(), options);

	FSItem* untrackedItem  = [FSItem itemWithURL:URLAppend(self.rootItem.url, @"?status=untracked")];
	untrackedItem.icon     = SCMFolderIcon();
	untrackedItem.name     = @"Untracked Items";
	untrackedItem.group    = YES;
	untrackedItem.children = convert(untrackedPaths, scmInfo->root_path(), options, true);

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
		std::string name = path::display_name(rootPath);

		NSString* status = [FSSCMDataSource parseSCMURLStatusQuery:anURL];
		if([status isEqualToString:@"unversioned"])
		{
			self.rootItem          = [FSItem itemWithURL:nil];
			self.rootItem.icon     = [NSImage imageNamed:NSImageNameFolderSmart];
			self.rootItem.name     = [NSString stringWithCxxString:text::format("%s (%s)", name.c_str(), "Unversioned")];
			self.rootItem.children = nil;

			return self;
		}

		if(scmInfo = scm::info(rootPath))
		{
			if(!scmInfo->dry())
			{
				auto const& vars = scmInfo->scm_variables();
				auto const branch = vars.find("TM_SCM_BRANCH");
				if(branch != vars.end())
					name = text::format("%s (%s)", path::display_name(scmInfo->root_path()).c_str(), branch->second.c_str());
			}

			if([status isEqualToString:@"all"])
			{
				self.rootItem          = [FSItem itemWithURL:anURL];
				self.rootItem.icon     = [NSImage imageNamed:NSImageNameFolderSmart];
				self.rootItem.name     = [NSString stringWithCxxString:name];
				self.rootItem.children = [self repositoryStatus];
			}
			else
			{
				name = path::display_name(rootPath);
				for(FSItem* item in [self repositoryStatus])
				{
					if([[FSSCMDataSource parseSCMURLStatusQuery:item.url] isEqualToString:status])
					{
						self.rootItem          = [FSItem itemWithURL:anURL];
						self.rootItem.icon     = [NSImage imageNamed:NSImageNameFolderSmart];
						self.rootItem.name     = [NSString stringWithFormat:@"%@ (%@)", [NSString stringWithCxxString:name], item.name];
						self.rootItem.children = item.children;
					}
				}
			}

			__weak FSSCMDataSource* weakSelf = self;
			scmInfo->add_callback(^(scm::info_t const&){
				[weakSelf postReloadNotification];
			});
		}
	}
	return self;
}

- (NSArray*)expandedURLs
{
	return @[ URLAppend(self.rootItem.url, @"?status=unstaged"), URLAppend(self.rootItem.url, @"?status=untracked") ];
}
@end
