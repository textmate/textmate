#import "FSSearchDataSource.h"
#import "FSItem.h"
#import <OakFoundation/NSString Additions.h>
#import <OakAppKit/OakFileIconImage.h>
#import <plist/plist.h>
#import <io/path.h>
#import <cf/cf.h>
#import <ns/ns.h>
#import <oak/oak.h>
#import <oak/debug.h>

OAK_DEBUG_VAR(FileBrowser_Spotlight);

namespace
{
	typedef std::shared_ptr<__MDQuery> MDQueryPtr;

	struct result_t
	{
		struct iterator
		{
			iterator (MDQueryPtr query, CFIndex index) : _query(query), _index(index) { }

			iterator operator++ ()                      { ++_index; return *this; }
			iterator operator-- ()                      { --_index; return *this; }
			bool operator== (iterator const& rhs) const { return _query == rhs._query && _index == rhs._index; }
			bool operator!= (iterator const& rhs) const { return !(*this == rhs); }

			MDItemRef operator* () const;

		private:
			MDQueryPtr _query;
			CFIndex _index;
		};

		result_t (MDQueryPtr _query) : _query(_query) { }
		iterator begin () const                       { return iterator(_query, 0); }
		iterator end () const                         { return iterator(_query, _query ? MDQueryGetResultCount(_query.get()) : 0); }

	private:
		MDQueryPtr _query;
	};

	MDItemRef result_t::iterator::operator* () const
	{
		return (MDItemRef)MDQueryGetResultAtIndex(_query.get(), _index);
	}

	result_t execute_saved_search (std::string const& path)
	{
		MDQueryPtr query;

		D(DBF_FileBrowser_Spotlight, bug("file ‘%s’\n", path.c_str()););
		plist::dictionary_t savedSearch = plist::load(path::resolve_head(path));

		std::string rawQuery;
		if(plist::get_key_path(savedSearch, "RawQueryDict.RawQuery", rawQuery))
		{
			D(DBF_FileBrowser_Spotlight, bug("query ‘%s’\n", rawQuery.c_str()););
			query.reset(MDQueryCreate(kCFAllocatorDefault, cf::wrap(rawQuery), NULL, NULL), CFRelease);

			plist::array_t scopes;
			if(plist::get_key_path(savedSearch, "RawQueryDict.SearchScopes", scopes))
			{
				std::vector<std::string> searchScopes;
				for(auto const& it : scopes)
				{
					if(std::string const* str = boost::get<std::string>(&it))
					{
						D(DBF_FileBrowser_Spotlight, bug("scope ‘%s’\n", str->c_str()););
						searchScopes.push_back(*str);
					}
				}
				MDQuerySetSearchScope(query.get(), cf::wrap(searchScopes), 0);
			}

			MDQueryExecute(query.get(), kMDQuerySynchronous);
			MDQueryDisableUpdates(query.get());
		}

		// TODO Handle ‘FinderFilesOnly’ and ‘UserFilesOnly’
		return result_t(query);
	}
}

@implementation FSSearchDataSource { OBJC_WATCH_LEAKS(FSSearchDataSource); }
- (id)initWithURL:(NSURL*)anURL options:(NSUInteger)someOptions
{
	if((self = [super init]))
	{
		NSMutableArray* results = [NSMutableArray new];
		for(auto const& item : execute_saved_search([[anURL path] fileSystemRepresentation]))
			[results addObject:[FSItem itemWithURL:[NSURL fileURLWithPath:CFBridgingRelease(MDItemCopyAttribute(item, kMDItemPath)) isDirectory:NO]]];

		self.rootItem = [FSItem itemWithURL:anURL];
		self.rootItem.icon     = [OakFileIconImage fileIconImageWithPath:[anURL path] size:NSMakeSize(16, 16)];
		self.rootItem.name     = [NSString stringWithCxxString:path::display_name([[anURL path] fileSystemRepresentation])];
		self.rootItem.children = results;
	}
	return self;
}
@end
