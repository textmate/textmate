#include <text/trim.h>

void test_trim ()
{
	OAK_ASSERT_EQ(text::trim("Hello",             " \t\n"), "Hello");
	OAK_ASSERT_EQ(text::trim(" Hello",            " \t\n"), "Hello");
	OAK_ASSERT_EQ(text::trim("Hello ",            " \t\n"), "Hello");
	OAK_ASSERT_EQ(text::trim(" Hello ",           " \t\n"), "Hello");
	OAK_ASSERT_EQ(text::trim("  Hello  ",         " \t\n"), "Hello");
	OAK_ASSERT_EQ(text::trim("\tHello",           " \t\n"), "Hello");
	OAK_ASSERT_EQ(text::trim("Hello\t",           " \t\n"), "Hello");
	OAK_ASSERT_EQ(text::trim("\tHello\t",         " \t\n"), "Hello");
	OAK_ASSERT_EQ(text::trim(" \tHello\t ",       " \t\n"), "Hello");
	OAK_ASSERT_EQ(text::trim("\nHello",           " \t\n"), "Hello");
	OAK_ASSERT_EQ(text::trim("Hello\n",           " \t\n"), "Hello");
	OAK_ASSERT_EQ(text::trim("\nHello\n",         " \t\n"), "Hello");
	OAK_ASSERT_EQ(text::trim(" \t\nHello\n\t ",   " \t\n"), "Hello");
	OAK_ASSERT_EQ(text::trim("  \t\nHello\n\t  ", " \t\n"), "Hello");
}
