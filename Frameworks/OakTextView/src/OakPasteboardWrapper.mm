#import "OakPasteboardWrapper.h"
#import <OakAppKit/OakPasteboard.h>
#import <OakFoundation/NSString Additions.h>
#import <plist/plist.h>
#import <cf/cf.h>
#import <ns/ns.h>

struct my_entry_t : clipboard_t::entry_t
{
	my_entry_t (std::string const& content, std::map<std::string, std::string> const& options) : clipboard_t::entry_t(content), _options(options) { }
	virtual std::map<std::string, std::string> const& options () const { return _options; }
private:
	std::map<std::string, std::string> _options;
};

static clipboard_t::entry_ptr to_entry (OakPasteboardEntry* src, BOOL includeFindOptions)
{
	if(!src)
		return clipboard_t::entry_ptr();

	std::map<std::string, std::string> map;
	plist::dictionary_t const& plist = plist::convert((__bridge CFDictionaryRef)src.options);
	for(auto const& pair : plist)
		plist::get_key_path(plist, pair.first, map[pair.first]);

	if(includeFindOptions)
	{
		map["wrapAround"] = [[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsFindWrapAround] ? "1" : "0";
		map["ignoreCase"] = [[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsFindIgnoreCase] ? "1" : "0";
	}

	return std::make_shared<my_entry_t>(to_s(src.string), map);
}

struct oak_pasteboard_t : clipboard_t
{
	oak_pasteboard_t (NSString* pboardName)
	{
		pasteboard         = [OakPasteboard pasteboardWithName:pboardName];
		includeFindOptions = [pboardName isEqualToString:NSFindPboard];
	}

	bool empty () const                     { return false; }

	entry_ptr previous ()                   { return to_entry([pasteboard previous], includeFindOptions); }
	entry_ptr current () const              { return to_entry([pasteboard current], includeFindOptions); }
	entry_ptr next ()                       { return to_entry([pasteboard next], includeFindOptions); }

	void push_back (entry_ptr entry)        { [pasteboard addEntryWithString:[NSString stringWithCxxString:entry->content()] andOptions:(__bridge NSDictionary*)((CFDictionaryRef)cf::wrap(entry->options()))]; }
private:
	OakPasteboard* pasteboard;
	BOOL includeFindOptions;
};

clipboard_ptr get_clipboard (NSString* pboardName)
{
	return std::make_shared<oak_pasteboard_t>(pboardName);
}
