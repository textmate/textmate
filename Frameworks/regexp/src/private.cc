#include "private.h"
#include <oak/oak.h>
#include <text/format.h>

struct udata_t
{
	UChar const* buffer;
	OnigRegion const* match;
	std::map<std::string, std::string>& res;
};

static int copy_matches_for_name (UChar const* name, UChar const* name_end, int len, int* list, regex_t* pattern, void* udata)
{
	udata_t const& data     = *(udata_t const*)udata;
	UChar const* buffer     = data.buffer;
	OnigRegion const* match = data.match;

	std::string value = "";
	bool has_value = false;
	foreach(it, list, list + len)
	{
		if(match->beg[*it] == -1)
			continue;
		value.insert(value.end(), buffer + match->beg[*it], buffer + match->end[*it]);
		has_value = true;
	}

	if(has_value)
		data.res.insert(std::make_pair(std::string(name, name_end), value));

	return 0;
}

std::map<std::string, std::string> extract_captures (UChar const* buffer, OnigRegion const* match, regex_t* regexp)
{
	std::map<std::string, std::string> res;
	for(size_t i = 0; i < match->num_regs; ++i)
	{
		if(match->beg[i] != -1)
			res.insert(std::make_pair(text::format("%zu", i), std::string(buffer + match->beg[i], buffer + match->end[i])));
	}
	udata_t udata = { buffer, match, res };
	onig_foreach_name(regexp, &copy_matches_for_name, (void*)&udata);
	return res;
}
