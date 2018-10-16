#import "FileItem.h"
#import "SCMManager.h"
#import <OakAppKit/NSImage Additions.h>
#import <io/path.h>
#import <ns/ns.h>
#import <scm/scm.h>

// ================
// = SCM Observer =
// ================

@interface SCMStatusObserver : NSObject
{
	id _scmObserver;
}
@end

@implementation SCMStatusObserver
- (instancetype)initWithURL:(NSURL*)url usingBlock:(void(^)(NSArray<NSURL*>*))handler
{
	if(self = [super init])
	{
		NSURL* repositoryURL = [NSURL fileURLWithPath:url.path isDirectory:YES];
		if([url.query hasSuffix:@"unstaged"])
		{
			_scmObserver = [SCMManager.sharedInstance addObserverForStatus:(scm::status::type)(scm::status::modified|scm::status::added|scm::status::deleted|scm::status::conflicted) inDirectoryAtURL:repositoryURL usingBlock:^(std::map<std::string, scm::status::type> const& map){
				handler([SCMStatusObserver unstagedURLsInDirectoryAtURL:url]);
			}];
		}
		else if([url.query hasSuffix:@"untracked"])
		{
			_scmObserver = [SCMManager.sharedInstance addObserverForStatus:scm::status::unversioned inDirectoryAtURL:repositoryURL usingBlock:^(std::map<std::string, scm::status::type> const& map){
				handler([SCMStatusObserver untrackedURLsInDirectoryAtURL:url]);
			}];
		}
		else if(scm::scm_enabled_for_path(repositoryURL.fileSystemRepresentation))
		{
			auto scmInfo = scm::info(repositoryURL.fileSystemRepresentation);
			if(scmInfo && scmInfo->root_path() != NULL_STR)
			{
				handler(@[
					[NSURL URLWithString:[NSString stringWithFormat:@"scm://localhost%@/?show=unstaged", to_ns(scmInfo->root_path())]],
					[NSURL URLWithString:[NSString stringWithFormat:@"scm://localhost%@/?show=untracked", to_ns(scmInfo->root_path())]],
				]);
			}
		}
	}
	return self;
}

- (void)dealloc
{
	if(_scmObserver)
		[SCMManager.sharedInstance removeObserver:_scmObserver];
}

+ (NSArray<NSURL*>*)unstagedURLsInDirectoryAtURL:(NSURL*)url
{
	std::map<std::string, scm::status::type> unstagedPaths;
	if(auto scmInfo = scm::info(url.fileSystemRepresentation))
	{
		for(auto const& pair : scmInfo->status())
		{
			if(pair.second & (scm::status::modified|scm::status::added|scm::status::deleted|scm::status::conflicted|scm::status::unversioned))
			{
				if(!(pair.second & scm::status::unversioned))
					unstagedPaths.insert(pair);
			}
		}

		if(!scmInfo->tracks_directories())
		{
			std::vector<std::string> parents;

			std::string child = NULL_STR;
			for(auto it = unstagedPaths.rbegin(); it != unstagedPaths.rend(); ++it)
			{
				if(path::is_child(child, it->first))
						parents.push_back(it->first);
				else	child = it->first;
			}

			for(auto const& path : parents)
				unstagedPaths.erase(path);
		}
	}

	NSMutableArray<NSURL*>* res = [NSMutableArray array];
	for(auto const& pair : unstagedPaths)
		[res addObject:[NSURL fileURLWithPath:to_ns(pair.first)]];
	return res;
}

+ (NSArray<NSURL*>*)untrackedURLsInDirectoryAtURL:(NSURL*)url
{
	std::map<std::string, scm::status::type> untrackedPaths;
	if(auto scmInfo = scm::info(url.fileSystemRepresentation))
	{
		for(auto pair : scmInfo->status())
		{
			if(pair.second & (scm::status::modified|scm::status::added|scm::status::deleted|scm::status::conflicted|scm::status::unversioned))
			{
				if(pair.second & scm::status::unversioned)
					untrackedPaths.insert(pair);
			}
		}

		if(!scmInfo->tracks_directories())
		{
			std::vector<std::string> children;

			std::string parent = NULL_STR;
			for(auto const& pair : untrackedPaths)
			{
				if(path::is_child(pair.first, parent))
					children.push_back(pair.first);
				else	parent = pair.first;
			}

			for(auto const& path : children)
				untrackedPaths.erase(path);
		}
	}

	NSMutableArray<NSURL*>* res = [NSMutableArray array];
	for(auto const& pair : untrackedPaths)
	{
		NSURL* url = [NSURL fileURLWithPath:to_ns(pair.first)];
		[url setTemporaryResourceValue:@YES forKey:@"org.textmate.disable-scm-status"];
		[res addObject:url];
	}
	return res;
}
@end

// ===================
// = SCM Data Source =
// ===================

@interface SCMStatusFileItem : FileItem
{
	scm::info_ptr _scmInfo;
}
@end

@implementation SCMStatusFileItem
+ (void)load
{
	[self registerClass:self forURLScheme:@"scm"];
}

+ (id)makeObserverForURL:(NSURL*)url usingBlock:(void(^)(NSArray<NSURL*>*))handler
{
	return [[SCMStatusObserver alloc] initWithURL:url usingBlock:handler];
}

- (instancetype)initWithURL:(NSURL*)url
{
	if(self = [super initWithURL:url])
	{
		if(!scm::scm_enabled_for_path(url.fileSystemRepresentation))
		{
			self.disambiguationSuffix = @" (disabled)";
		}
		else if(![self.URL.query hasSuffix:@"unstaged"] && ![self.URL.query hasSuffix:@"untracked"])
		{
			_scmInfo = scm::info(url.fileSystemRepresentation);
			if(_scmInfo && _scmInfo->root_path() != NULL_STR)
			{
				[self updateBranchName];

				__weak SCMStatusFileItem* weakSelf = self;
				_scmInfo->push_callback(^(scm::info_t const& info){
					[weakSelf updateBranchName];
				});
			}
			else
			{
				self.disambiguationSuffix = @" (no status)";
				_scmInfo.reset();
			}
		}
	}
	return self;
}

- (void)dealloc
{
	if(_scmInfo)
		_scmInfo->pop_callback();
}

- (void)updateBranchName
{
	if(!_scmInfo->dry())
	{
		auto const vars   = _scmInfo->scm_variables();
		auto const branch = vars.find("TM_SCM_BRANCH");
		self.disambiguationSuffix = branch != vars.end() ? [NSString stringWithFormat:@" (%@)", to_ns(branch->second)] : @"";
	}
	else
	{
		self.disambiguationSuffix = @" (fetching…)";
	}
}

- (NSString*)localizedName
{
	if([self.URL.query hasSuffix:@"unstaged"])
		return @"Uncommitted Changes";
	else if([self.URL.query hasSuffix:@"untracked"])
		return @"Untracked Items";
	else if(_scmInfo && _scmInfo->root_path() != NULL_STR)
		return [NSFileManager.defaultManager displayNameAtPath:to_ns(_scmInfo->root_path())];

	return super.localizedName;
}

- (NSImage*)image
{
	if([self.URL.query hasSuffix:@"unstaged"] || [self.URL.query hasSuffix:@"untracked"])
		return super.image;
	return [NSImage imageNamed:@"SCMTemplate" inSameBundleAsClass:NSClassFromString(@"OakFileBrowser")];
}

- (NSURL*)parentURL
{
	if([self.URL.query hasSuffix:@"unstaged"] || [self.URL.query hasSuffix:@"untracked"])
		return [NSURL URLWithString:[NSString stringWithFormat:@"scm://localhost%@/", self.URL.path]];
	return [NSURL fileURLWithPath:self.URL.path];
}
@end
