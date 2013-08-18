#include <Onigmo/oniguruma.h>

OnigRegex pattern (char const* ptrn, OnigOptionType options = ONIG_OPTION_NONE)
{
	OnigErrorInfo einfo;
	OnigRegex regex = nullptr;
	OAK_ASSERT_EQ(ONIG_NORMAL, onig_new(&regex, (OnigUChar const*)ptrn, (OnigUChar const*)ptrn + strlen(ptrn), options, ONIG_ENCODING_UTF8, ONIG_SYNTAX_DEFAULT, &einfo));
	return regex;
}

void disabled_test_match_range ()
{
	std::string res = "none";

	char const* buf = "abcdef";
	size_t from = 3, to = 0;

	OnigRegion* region = onig_region_new();
	if(ONIG_MISMATCH != onig_search(pattern("\\h+"), (OnigUChar const*)buf, (OnigUChar const*)buf + strlen(buf), (OnigUChar const*)buf + from, (OnigUChar const*)buf + to, region, ONIG_OPTION_NONE))
	{
		char* tmp = nullptr;
		asprintf(&tmp, "%zu-%zu", region->beg[0], region->end[0]);
		res = tmp;
		free(tmp);
	}
	onig_region_free(region, 1);

	OAK_ASSERT_EQ("2-3", res);
}
