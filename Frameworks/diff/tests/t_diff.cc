#include <diff/diff.h>

static std::vector<std::string> file1 = {
"#include <stdio.h>",
"",
"// Frobs foo heartily",
"int frobnitz(int foo)",
"{",
"    int i;",
"    for(i = 0; i < 10; i++)",
"    {",
"        printf(\"Your answer is: \");",
"        printf(\"%d\n\", foo);",
"    }",
"}",
"",
"int fact(int n)",
"{",
"    if(n > 1)",
"    {",
"        return fact(n-1) * n;",
"    }",
"    return 1;",
"}",
"",
"int main(int argc, char **argv)",
"{",
"    frobnitz(fact(10));",
"}",
};

static std::vector<std::string> file2 = {
"#include <stdio.h>",
"",
"int fib(int n)",
"{",
"    if(n > 2)",
"    {",
"        return fib(n-1) + fib(n-2);",
"    }",
"    return 1;",
"}",
"",
"// Frobs foo heartily",
"int frobnitz(int foo)",
"{",
"    int i;",
"    for(i = 0; i < 10; i++)",
"    {",
"        printf(\"%d\n\", foo);",
"    }",
"}",
"",
"int main(int argc, char **argv)",
"{",
"    frobnitz(fib(10));",
"}",
};

bool operator== (diff::position_t const& lhs, diff::position_t const& rhs) { return true; }
void test_file_type ()
{
	auto cache = diff::updateable_diff(file1, file2);
	auto result = diff::update(cache);
	std::vector<diff::position_t> expected = {{0,0},{1,1}};
	//OAK_ASSERT_EQ(result, expected);
}