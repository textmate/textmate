#include <text/trim.h>

class trim_tests : public CxxTest::TestSuite
{
public:
	void test_trim ()
	{
		TS_ASSERT_EQUALS(text::trim("Hello",             " \t\n"), "Hello");
		TS_ASSERT_EQUALS(text::trim(" Hello",            " \t\n"), "Hello");
		TS_ASSERT_EQUALS(text::trim("Hello ",            " \t\n"), "Hello");
		TS_ASSERT_EQUALS(text::trim(" Hello ",           " \t\n"), "Hello");
		TS_ASSERT_EQUALS(text::trim("  Hello  ",         " \t\n"), "Hello");
		TS_ASSERT_EQUALS(text::trim("\tHello",           " \t\n"), "Hello");
		TS_ASSERT_EQUALS(text::trim("Hello\t",           " \t\n"), "Hello");
		TS_ASSERT_EQUALS(text::trim("\tHello\t",         " \t\n"), "Hello");
		TS_ASSERT_EQUALS(text::trim(" \tHello\t ",       " \t\n"), "Hello");
		TS_ASSERT_EQUALS(text::trim("\nHello",           " \t\n"), "Hello");
		TS_ASSERT_EQUALS(text::trim("Hello\n",           " \t\n"), "Hello");
		TS_ASSERT_EQUALS(text::trim("\nHello\n",         " \t\n"), "Hello");
		TS_ASSERT_EQUALS(text::trim(" \t\nHello\n\t ",   " \t\n"), "Hello");
		TS_ASSERT_EQUALS(text::trim("  \t\nHello\n\t  ", " \t\n"), "Hello");
	}
};
