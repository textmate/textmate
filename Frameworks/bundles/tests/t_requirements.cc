#include <bundles/src/wrappers.h>

static bundles::item_ptr find (std::string const& name)
{
	auto vec = bundles::query(bundles::kFieldName, name);
	OAK_ASSERT_EQ(vec.size(), 1);
	return vec.front();
}

void test_requirements ()
{
	auto location               = find("TrueWithLocation");
	auto variable               = find("TrueWithVariable");
	auto locationAndVariable    = find("TrueWithLocationAndVariable");
	auto badLocation            = find("TrueWithBadLocation");
	auto badLocationAndVariable = find("TrueWithBadLocationAndVariable");

	std::map<std::string, std::string> env;
	bundles::required_command_t failedRequirement;

	env = { { "PATH", "/usr" } };
	OAK_ASSERT(!missing_requirement(location, env, &failedRequirement));
	OAK_ASSERT_EQ(env["PATH"], "/usr:/usr/bin");

	env = { { "PATH", "/usr" } };
	OAK_ASSERT(missing_requirement(badLocation, env, &failedRequirement));

	env = { { "PATH", "/usr/bin" } };
	OAK_ASSERT(!missing_requirement(badLocation, env, &failedRequirement));
	OAK_ASSERT_EQ(env["PATH"], "/usr/bin");

	env = { { "PATH", "/usr" } };
	OAK_ASSERT(missing_requirement(variable, env, &failedRequirement));

	env = { { "PATH", "/usr/bin" } };
	OAK_ASSERT(!missing_requirement(variable, env, &failedRequirement));
	OAK_ASSERT_EQ(env["TM_TRUE"], "/usr/bin/true");
	OAK_ASSERT_EQ(env["PATH"], "/usr/bin");

	env = { { "PATH", "/usr" }, { "TM_TRUE", "/usr/bin/true" } };
	OAK_ASSERT(!missing_requirement(variable, env, &failedRequirement));
	OAK_ASSERT_EQ(env["TM_TRUE"], "/usr/bin/true");
	OAK_ASSERT_EQ(env["PATH"], "/usr");

	env = { { "PATH", "/usr" }, { "TM_TRUE", "/usr/bin/true --help" } };
	OAK_ASSERT(!missing_requirement(variable, env, &failedRequirement));
	OAK_ASSERT_EQ(env["TM_TRUE"], "/usr/bin/true --help");
	OAK_ASSERT_EQ(env["PATH"], "/usr");

	env = { { "PATH", "/usr" }, { "TM_TRUE", "/foo/bar/true" } };
	OAK_ASSERT(!missing_requirement(locationAndVariable, env, &failedRequirement));
	OAK_ASSERT_EQ(env["TM_TRUE"], "/usr/bin/true");
	OAK_ASSERT_EQ(env["PATH"], "/usr");

	env = { { "PATH", "/usr" }, { "TM_TRUE", "/usr/bin/true --help" } };
	OAK_ASSERT(!missing_requirement(locationAndVariable, env, &failedRequirement));
	OAK_ASSERT_EQ(env["TM_TRUE"], "/usr/bin/true --help");
	OAK_ASSERT_EQ(env["PATH"], "/usr");

	env = { { "PATH", "/usr" }, { "TM_TRUE", "/foo/bar/true" } };
	OAK_ASSERT(missing_requirement(badLocationAndVariable, env, &failedRequirement));

	env = { { "PATH", "/usr" }, { "TM_TRUE", "/usr/bin/true --help" } };
	OAK_ASSERT(!missing_requirement(badLocationAndVariable, env, &failedRequirement));
	OAK_ASSERT_EQ(env["TM_TRUE"], "/usr/bin/true --help");
	OAK_ASSERT_EQ(env["PATH"], "/usr");

	env = { { "PATH", "/usr/bin" } };
	OAK_ASSERT(!missing_requirement(badLocationAndVariable, env, &failedRequirement));
	OAK_ASSERT_EQ(env["TM_TRUE"], "/usr/bin/true");
	OAK_ASSERT_EQ(env["PATH"], "/usr/bin");
}
