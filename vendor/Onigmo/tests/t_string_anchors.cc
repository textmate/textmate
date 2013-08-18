#include <Onigmo/oniguruma.h>

#define ONIG_OPTION_BACKWARD (ONIG_OPTION_MAXBIT << 1)

OnigRegex pattern (char const* ptrn, OnigOptionType options = ONIG_OPTION_NONE)
{
	OnigErrorInfo einfo;
	OnigRegex regex = nullptr;
	OAK_ASSERT_EQ(ONIG_NORMAL, onig_new(&regex, (OnigUChar const*)ptrn, (OnigUChar const*)ptrn + strlen(ptrn), options, ONIG_ENCODING_UTF8, ONIG_SYNTAX_DEFAULT, &einfo));
	return regex;
}

std::string match (OnigRegex regex, char const* buf, OnigOptionType options = ONIG_OPTION_NONE)
{
	std::string res = "none";

	size_t from = 0, to = strlen(buf);
	if(options & ONIG_OPTION_BACKWARD)
		std::swap(from, to);

	OnigRegion* region = onig_region_new();
	if(ONIG_MISMATCH != onig_search(regex, (OnigUChar const*)buf, (OnigUChar const*)buf + strlen(buf), (OnigUChar const*)buf + from, (OnigUChar const*)buf + to, region, options))
	{
		char* tmp = nullptr;
		asprintf(&tmp, "%zu-%zu", region->beg[0], region->end[0]);
		res = tmp;
		free(tmp);
	}
	onig_region_free(region, 1);
	return res;
}

void test_bol ()
{
	char const* buf = "foo bar\nfoo bar";

	OAK_ASSERT_EQ( "0-3", match(pattern("^foo"), buf));
	OAK_ASSERT_EQ("8-11", match(pattern("^foo"), buf, ONIG_OPTION_NOTBOL));
	OAK_ASSERT_EQ( "0-3", match(pattern("^foo"), buf, ONIG_OPTION_NOTBOS));

	OAK_ASSERT_EQ( "0-3", match(pattern("\\Afoo"), buf));
	OAK_ASSERT_EQ( "0-3", match(pattern("\\Afoo"), buf, ONIG_OPTION_NOTBOL));
	OAK_ASSERT_EQ("none", match(pattern("\\Afoo"), buf, ONIG_OPTION_NOTBOS));

	OAK_ASSERT_EQ( "0-3", match(pattern("^foo", ONIG_OPTION_SINGLELINE), buf));
	OAK_ASSERT_EQ( "0-3", match(pattern("^foo", ONIG_OPTION_SINGLELINE), buf, ONIG_OPTION_NOTBOL));
	OAK_ASSERT_EQ("none", match(pattern("^foo", ONIG_OPTION_SINGLELINE), buf, ONIG_OPTION_NOTBOS));

	OAK_ASSERT_EQ( "0-3", match(pattern("\\Afoo", ONIG_OPTION_SINGLELINE), buf));
	OAK_ASSERT_EQ( "0-3", match(pattern("\\Afoo", ONIG_OPTION_SINGLELINE), buf, ONIG_OPTION_NOTBOL));
	OAK_ASSERT_EQ("none", match(pattern("\\Afoo", ONIG_OPTION_SINGLELINE), buf, ONIG_OPTION_NOTBOS));
}

void test_eol ()
{
	char const* buf = "foo bar\nfoo bar";

	OAK_ASSERT_EQ("12-15", match(pattern("bar$"), buf, ONIG_OPTION_BACKWARD));
	OAK_ASSERT_EQ(  "4-7", match(pattern("bar$"), buf, ONIG_OPTION_BACKWARD|ONIG_OPTION_NOTEOL));
	OAK_ASSERT_EQ("12-15", match(pattern("bar$"), buf, ONIG_OPTION_BACKWARD|ONIG_OPTION_NOTEOS));

	OAK_ASSERT_EQ("12-15", match(pattern("bar\\z"), buf));
	OAK_ASSERT_EQ("12-15", match(pattern("bar\\z"), buf, ONIG_OPTION_NOTEOL));
	OAK_ASSERT_EQ( "none", match(pattern("bar\\z"), buf, ONIG_OPTION_NOTEOS));
}
