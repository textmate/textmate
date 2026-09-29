#import "FileItem.h"
#import "SCMManager.h"
#import <OakAppKit/src/NSImage Additions.h>
#import <io/src/path.h>
#import <ns/src/ns.h>

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
			_scmObserver = [SCMManager.sharedInstance addObserverToRepositoryAtURL:repositoryURL usingBlock:^(SCMRepository* repository){
				handler([SCMStatusObserver unstagedURLsInRepository:repository]);
			}];
		}
		else if([url.query hasSuffix:@"untracked"])
		{
			_scmObserver = [SCMManager.sharedInstance addObserverToRepositoryAtURL:repositoryURL usingBlock:^(SCMRepository* repository){
				handler([SCMStatusObserver untrackedURLsInRepository:repository]);
			}];
		}
		else if(SCMRepository* repository = [SCMManager.sharedInstance repositoryAtURL:repositoryURL])
		{
			if(repository.enabled)
			{
				handler(@[
					[NSURL URLWithString:[NSString stringWithFormat:@"scm://localhost%@/?show=unstaged", [repository.URL.path stringByAddingPercentEncodingWithAllowedCharacters:NSCharacterSet.URLPathAllowedCharacterSet]]],
					[NSURL URLWithString:[NSString stringWithFormat:@"scm://localhost%@/?show=untracked", [repository.URL.path stringByAddingPercentEncodingWithAllowedCharacters:NSCharacterSet.URLPathAllowedCharacterSet]]],
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

+ (NSArray<NSURL*>*)unstagedURLsInRepository:(SCMRepository*)repository
{
	std::map<std::string, scm::status::type> unstagedPaths;
	for(auto const& pair : repository.status)
	{
		if(pair.second & (scm::status::modified|scm::status::added|scm::status::deleted|scm::status::conflicted|scm::status::unversioned))
		{
			if(!(pair.second & scm::status::unversioned))
				unstagedPaths.insert(pair);
		}
	}

	if(!repository.tracksDirectories)
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

	NSMutableArray<NSURL*>* res = [NSMutableArray array];
	for(auto const& pair : unstagedPaths)
		[res addObject:[NSURL fileURLWithPath:to_ns(pair.first)]];
	return res;
}

+ (NSArray<NSURL*>*)untrackedURLsInRepository:(SCMRepository*)repository
{
	std::map<std::string, scm::status::type> untrackedPaths;
	for(auto pair : repository.status)
	{
		if(pair.second & (scm::status::modified|scm::status::added|scm::status::deleted|scm::status::conflicted|scm::status::unversioned))
		{
			if(pair.second & scm::status::unversioned)
				untrackedPaths.insert(pair);
		}
	}

	if(!repository.tracksDirectories)
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
	SCMRepository* _repository;
	id _observer;
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
		_repository = [SCMManager.sharedInstance repositoryAtURL:[NSURL fileURLWithPath:url.path isDirectory:YES]];
		if(_repository && _repository.enabled == NO)
		{
			self.disambiguationSuffix = @" (disabled)";
		}
		else if(![self.URL.query hasSuffix:@"unstaged"] && ![self.URL.query hasSuffix:@"untracked"])
		{
			if(_repository)
			{
				__weak SCMStatusFileItem* weakSelf = self;
				_observer = [SCMManager.sharedInstance addObserverToRepositoryAtURL:_repository.URL usingBlock:^(SCMRepository* repository){
					[weakSelf updateBranchName];
				}];
			}
			else
			{
				self.disambiguationSuffix = @" (no status)";
			}
		}
	}
	return self;
}

- (void)dealloc
{
	[SCMManager.sharedInstance removeObserver:_observer];
}

- (void)updateBranchName
{
	if(_repository)
	{
		NSString* branch = _repository.variables[@"TM_SCM_BRANCH"];
		self.disambiguationSuffix = branch ? [NSString stringWithFormat:@" (%@)", branch] : @"";
	}
}

- (NSString*)localizedName
{
	if([self.URL.query hasSuffix:@"unstaged"])
		return @"Uncommitted Changes";
	else if([self.URL.query hasSuffix:@"untracked"])
		return @"Untracked Items";
	else if(_repository)
		return [NSFileManager.defaultManager displayNameAtPath:_repository.URL.path];

	return super.localizedName;
}

- (NSURL*)parentURL
{
	if([self.URL.query hasSuffix:@"unstaged"] || [self.URL.query hasSuffix:@"untracked"])
		return [NSURL URLWithString:[NSString stringWithFormat:@"scm://localhost%@/", [self.URL.path stringByAddingPercentEncodingWithAllowedCharacters:NSCharacterSet.URLPathAllowedCharacterSet]]];
	return [NSURL fileURLWithPath:self.URL.path];
}
@end
