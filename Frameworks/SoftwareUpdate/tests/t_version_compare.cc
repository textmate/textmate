#include "../src/version_compare.h"

static bool less (std::string const& lhs, std::string const& rhs)
{
	return version_less(lhs, rhs) && !version_less(rhs, lhs);
}

static bool equal (std::string const& lhs, std::string const& rhs)
{
	return version_equal(lhs, rhs);
}

static bool greator (std::string const& lhs, std::string const& rhs)
{
	return !less(lhs, rhs);
}

void test_trailing_zero ()
{
	OAK_ASSERT(   less("2-beta",        "2.0"));
	OAK_ASSERT(   less("2.0-beta",      "2.0"));
	OAK_ASSERT(   less("2.0.0-beta",    "2.0"));
	OAK_ASSERT(  equal("2",             "2.0"));
	OAK_ASSERT(  equal("2",             "2.0+git.hash"));
	OAK_ASSERT(  equal("2+git.hash",    "2.0"));
	OAK_ASSERT(greator("2.0.1-beta",    "2.0"));
	OAK_ASSERT(greator("2.1-beta",      "2.0"));
}

void test_null_string ()
{
	OAK_ASSERT(   less(NULL_STR, "2.0"));
	OAK_ASSERT( !equal(NULL_STR, "2.0"));
	OAK_ASSERT(greator("2.0", NULL_STR));
}

void test_exhaustive ()
{
	std::string const numbers[] = { "1", "1.01", "1.1.1", "1.1.2", "1.2", "1.2.1", "1.10", "2", "2.1", "2.1.1", "2.2" };
	std::vector<std::string> versions;

	for(auto number : numbers)
	{
		versions.push_back(number + "-alpha");
		versions.push_back(number + "-alpha.1");
		versions.push_back(number + "-alpha.2");
		versions.push_back(number + "-alpha.3+debug");
		versions.push_back(number + "-beta");
		versions.push_back(number + "-beta.1");
		versions.push_back(number + "-beta.2");
		versions.push_back(number + "-rc.1");
		versions.push_back(number);
	}

	for(size_t i = 0; i < versions.size(); ++i)
	{
		for(size_t j = i; j < versions.size(); ++j)
		{
			for(auto lhs : { versions[i], versions[i] + "+git.c0de" })
			{
				for(auto rhs : { versions[j], versions[j] + "+git.b337" })
				{
					auto msg = text::format("%s %c %s", lhs.c_str(), i < j ? '<' : (i == j ? '=' : '>'), rhs.c_str());
					if(i < j)  { OAK_MASSERT(msg, less(lhs, rhs));    }
					if(i == j) { OAK_MASSERT(msg, equal(lhs, rhs));   }
					if(i > j)  { OAK_MASSERT(msg, greator(lhs, rhs)); }
				}
			}
		}
	}
}
