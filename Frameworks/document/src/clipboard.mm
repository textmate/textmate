#import "clipboard.h"
#import <OakAppKit/src/OakPasteboard.h>
#import <OakFoundation/src/NSString Additions.h>
#import <plist/src/plist.h>
#import <cf/src/cf.h>
#import <ns/src/ns.h>

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
		map["wrapAround"] = [NSUserDefaults.standardUserDefaults boolForKey:kUserDefaultsFindWrapAround] ? "1" : "0";
		map["ignoreCase"] = [NSUserDefaults.standardUserDefaults boolForKey:kUserDefaultsFindIgnoreCase] ? "1" : "0";
	}

	std::vector<std::string> contents;
	for(NSString* str in src.strings)
		contents.push_back(to_s(str));

	return std::make_shared<clipboard_t::entry_t>(contents, map);
}

struct oak_pasteboard_t : clipboard_t
{
	oak_pasteboard_t (OakPasteboard* pboard)
	{
		pasteboard         = pboard;
		includeFindOptions = [pboard isEqual:OakPasteboard.findPasteboard];
	}

	bool empty () const                     { return false; }

	entry_ptr previous ()                   { return to_entry([pasteboard previous], includeFindOptions); }
	entry_ptr current () const              { return to_entry([pasteboard current], includeFindOptions); }
	entry_ptr next ()                       { return to_entry([pasteboard next], includeFindOptions); }

	void push_back (entry_ptr entry)
	{
		if(OakPasteboardEntry* res = [pasteboard addEntryWithStrings:(__bridge NSArray*)((CFArrayRef)cf::wrap(entry->contents())) options:(__bridge NSDictionary*)((CFDictionaryRef)cf::wrap(entry->options()))])
			[pasteboard updatePasteboardWithEntry:res];
	}

private:
	OakPasteboard* pasteboard;
	BOOL includeFindOptions;
};

clipboard_ptr get_clipboard (OakPasteboard* pboard)
{
	return std::make_shared<oak_pasteboard_t>(pboard);
}
