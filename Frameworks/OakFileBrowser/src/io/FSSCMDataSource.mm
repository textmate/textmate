#import "FSSCMDataSource.h"
#import "FSItem.h"
#import <OakFoundation/NSString Additions.h>
#import <OakAppKit/OakFileIconImage.h>
#import <io/path.h>
#import <text/encode.h>
#import <text/ctype.h>
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

@implementation FSSCMDataSource { OBJC_WATCH_LEAKS(FSSCMDataSource); }
+ (NSURL*)scmURLWithPath:(NSString*)aPath
{
	if(scm::info_ptr info = scm::info([aPath fileSystemRepresentation]))
		return [NSURL URLWithString:[NSString stringWithCxxString:"scm://localhost" + encode::url_part(info->path(), "/") + "/"]];
	return [NSURL fileURLWithPath:aPath];
}

- (NSArray*)repositoryStatus
{
	std::vector<std::string> unstagedPaths, untrackedPaths;
	citerate(pair, scm::tracked_files(scmInfo->path(), scm::status::modified|scm::status::added|scm::status::deleted|scm::status::conflicted|scm::status::unversioned))
	{
		if(pair->second & scm::status::unversioned)
				untrackedPaths.push_back(pair->first);
		else	unstagedPaths.push_back(pair->first);
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
	unstagedItem.children = convert(unstagedPaths, scmInfo->path(), options);

	FSItem* untrackedItem  = [FSItem itemWithURL:URLAppend(self.rootItem.url, @".untracked/")];
	untrackedItem.icon     = SCMFolderIcon();
	untrackedItem.name     = @"Untracked Items";
	untrackedItem.group    = YES;
	untrackedItem.children = convert(untrackedPaths, scmInfo->path(), options);

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

		if(scmInfo = scm::info([[anURL path] fileSystemRepresentation]))
		{
			struct scm_callback_t : scm::callback_t
			{
				scm_callback_t (FSSCMDataSource* self) : _self(self) { }

				void status_changed (scm::info_t const& info, std::set<std::string> const& changedPaths)
				{
					[_self postReloadNotification];
				}

			private:
				__weak FSSCMDataSource* _self;
			};

			std::string name = path::display_name(scmInfo->path());
			if(scmInfo->branch() != NULL_STR)
				name += " (" + scmInfo->branch() + ")";

			self.rootItem          = [FSItem itemWithURL:anURL];
			self.rootItem.icon     = [NSImage imageNamed:NSImageNameFolderSmart];
			self.rootItem.name     = [NSString stringWithCxxString:name];
			self.rootItem.children = [self repositoryStatus];

			scmCallback = new scm_callback_t(self);
			scmInfo->add_callback(scmCallback);
		}
	}
	return self;
}

- (void)dealloc
{
	if(scmInfo && scmCallback)
	{
		scmInfo->remove_callback(scmCallback);
		delete scmCallback;
	}
}
@end
