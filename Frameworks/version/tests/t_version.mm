#include <version/version.h>
#include <text/format.h>

using version::less;
using version::less_or_equal;
using version::equal;
using version::greater;

void test_trailing_zero ()
{
	OAK_ASSERT(   less("2-beta",        "2.0"));
	OAK_ASSERT(   less("2.0-beta",      "2.0"));
	OAK_ASSERT(   less("2.0.0-beta",    "2.0"));
	OAK_ASSERT(  equal("2",             "2.0"));
	OAK_ASSERT(  equal("2",             "2.0+git.hash"));
	OAK_ASSERT(  equal("2+git.hash",    "2.0"));
	OAK_ASSERT(greater("2.0.1-beta",    "2.0"));
	OAK_ASSERT(greater("2.1-beta",      "2.0"));
}

void test_null_string ()
{
	OAK_ASSERT(   less(NULL_STR, "2.0"));
	OAK_ASSERT( !equal(NULL_STR, "2.0"));
	OAK_ASSERT(greater("2.0", NULL_STR));
}

void test_exhaustive ()
{
	std::vector<std::string> versions;
	for(std::string number : { "1", "1.01", "1.1.1", "1.1.2", "1.2", "1.2.1", "1.10", "2", "2.1", "2.1.1", "2.2" })
	{
		for(std::string suffix : { "-alpha", "-alpha.1", "-alpha.2", "-alpha.3+debug", "-beta", "-beta.1", "-beta.2", "-rc.1", "" })
			versions.push_back(number + suffix);
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
					if(i < j)  { OAK_MASSERT(msg, less(lhs, rhs));          }
					if(i < j)  { OAK_MASSERT(msg, less_or_equal(lhs, rhs)); }
					if(i == j) { OAK_MASSERT(msg, less_or_equal(lhs, rhs)); }
					if(i == j) { OAK_MASSERT(msg, equal(lhs, rhs));         }
					if(i > j)  { OAK_MASSERT(msg, greater(lhs, rhs));       }
				}
			}
		}
	}
}
