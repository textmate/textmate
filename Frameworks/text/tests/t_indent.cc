#include <text/indent.h>

void test_tab_follows_indent ()
{
	text::indent_t indent(4);
	OAK_ASSERT_EQ(indent.tab_size(),    4);
	OAK_ASSERT_EQ(indent.indent_size(), 4);

	indent.set_tab_size(8);
	OAK_ASSERT_EQ(indent.tab_size(),    8);
	OAK_ASSERT_EQ(indent.indent_size(), 8);

	indent.set_indent_size(3);
	OAK_ASSERT_EQ(indent.tab_size(),    3);
	OAK_ASSERT_EQ(indent.indent_size(), 3);

	indent.set_tab_follows_indent(false);

	indent.set_tab_size(5);
	OAK_ASSERT_EQ(indent.tab_size(),    5);
	OAK_ASSERT_EQ(indent.indent_size(), 3);

	indent.set_indent_size(2);
	OAK_ASSERT_EQ(indent.tab_size(),    5);
	OAK_ASSERT_EQ(indent.indent_size(), 2);
}

void test_indent_soft_tabs ()
{
	OAK_ASSERT_EQ(text::indent_t(4, 4, true).create(0, 1), "    ");
	OAK_ASSERT_EQ(text::indent_t(4, 4, true).create(1, 1),  "   ");
	OAK_ASSERT_EQ(text::indent_t(4, 4, true).create(2, 1),   "  ");
	OAK_ASSERT_EQ(text::indent_t(4, 4, true).create(3, 1),    " ");
	OAK_ASSERT_EQ(text::indent_t(4, 4, true).create(4, 1), "    ");
	OAK_ASSERT_EQ(text::indent_t(4, 4, true).create(5, 1),  "   ");
	OAK_ASSERT_EQ(text::indent_t(4, 4, true).create(6, 1),   "  ");
	OAK_ASSERT_EQ(text::indent_t(4, 4, true).create(7, 1),    " ");
	OAK_ASSERT_EQ(text::indent_t(4, 4, true).create(8, 1), "    ");

	OAK_ASSERT_EQ(text::indent_t(4, 4, true).create(0, 2), "        ");
	OAK_ASSERT_EQ(text::indent_t(4, 4, true).create(1, 2),  "       ");
	OAK_ASSERT_EQ(text::indent_t(4, 4, true).create(2, 2),   "      ");
	OAK_ASSERT_EQ(text::indent_t(4, 4, true).create(3, 2),    "     ");
	OAK_ASSERT_EQ(text::indent_t(4, 4, true).create(4, 2), "        ");
}

void test_indent_tab_larger_than_indent ()
{
	OAK_ASSERT_EQ(text::indent_t(8, 4, false).create(0, 1), "    ");
	OAK_ASSERT_EQ(text::indent_t(8, 4, false).create(1, 1),  "   ");
	OAK_ASSERT_EQ(text::indent_t(8, 4, false).create(2, 1),   "  ");
	OAK_ASSERT_EQ(text::indent_t(8, 4, false).create(3, 1),    " ");
	OAK_ASSERT_EQ(text::indent_t(8, 4, false).create(4, 1),   "\t");
	OAK_ASSERT_EQ(text::indent_t(8, 4, false).create(5, 1),   "\t");
	OAK_ASSERT_EQ(text::indent_t(8, 4, false).create(6, 1),   "\t");
	OAK_ASSERT_EQ(text::indent_t(8, 4, false).create(7, 1),   "\t");
	OAK_ASSERT_EQ(text::indent_t(8, 4, false).create(8, 1), "    ");

	OAK_ASSERT_EQ(text::indent_t(8, 4, false).create(0, 2), "\t");
	OAK_ASSERT_EQ(text::indent_t(8, 4, false).create(1, 2), "\t");
	OAK_ASSERT_EQ(text::indent_t(8, 4, false).create(2, 2), "\t");
	OAK_ASSERT_EQ(text::indent_t(8, 4, false).create(3, 2), "\t");
	OAK_ASSERT_EQ(text::indent_t(8, 4, false).create(4, 2), "\t    ");
	OAK_ASSERT_EQ(text::indent_t(8, 4, false).create(5, 2), "\t    ");
	OAK_ASSERT_EQ(text::indent_t(8, 4, false).create(6, 2), "\t    ");
	OAK_ASSERT_EQ(text::indent_t(8, 4, false).create(7, 2), "\t    ");
	OAK_ASSERT_EQ(text::indent_t(8, 4, false).create(8, 2), "\t");

	OAK_ASSERT_EQ(text::indent_t(8, 4, false).create(0, 3), "\t    ");
	OAK_ASSERT_EQ(text::indent_t(8, 4, false).create(1, 3), "\t    ");
	OAK_ASSERT_EQ(text::indent_t(8, 4, false).create(2, 3), "\t    ");
	OAK_ASSERT_EQ(text::indent_t(8, 4, false).create(3, 3), "\t    ");
	OAK_ASSERT_EQ(text::indent_t(8, 4, false).create(4, 3), "\t\t");
	OAK_ASSERT_EQ(text::indent_t(8, 4, false).create(5, 3), "\t\t");
	OAK_ASSERT_EQ(text::indent_t(8, 4, false).create(6, 3), "\t\t");
	OAK_ASSERT_EQ(text::indent_t(8, 4, false).create(7, 3), "\t\t");
	OAK_ASSERT_EQ(text::indent_t(8, 4, false).create(8, 3), "\t    ");
}

void test_indent_indent_larger_than_tab ()
{
	OAK_ASSERT_EQ(text::indent_t(4, 8, false).create(0, 1), "\t\t");
	OAK_ASSERT_EQ(text::indent_t(4, 8, false).create(1, 1), "\t\t");
	OAK_ASSERT_EQ(text::indent_t(4, 8, false).create(2, 1), "\t\t");
	OAK_ASSERT_EQ(text::indent_t(4, 8, false).create(3, 1), "\t\t");
	OAK_ASSERT_EQ(text::indent_t(4, 8, false).create(4, 1), "\t\t");
	OAK_ASSERT_EQ(text::indent_t(4, 8, false).create(5, 1), "\t\t");
	OAK_ASSERT_EQ(text::indent_t(4, 8, false).create(6, 1), "\t\t");
	OAK_ASSERT_EQ(text::indent_t(4, 8, false).create(7, 1), "\t\t");
	OAK_ASSERT_EQ(text::indent_t(4, 8, false).create(8, 1), "\t\t");

	OAK_ASSERT_EQ(text::indent_t(4, 8, false).create(0, 2), "\t\t\t\t");
	OAK_ASSERT_EQ(text::indent_t(4, 8, false).create(1, 2), "\t\t\t\t");
	OAK_ASSERT_EQ(text::indent_t(4, 8, false).create(2, 2), "\t\t\t\t");
	OAK_ASSERT_EQ(text::indent_t(4, 8, false).create(3, 2), "\t\t\t\t");
	OAK_ASSERT_EQ(text::indent_t(4, 8, false).create(4, 2), "\t\t\t\t");
	OAK_ASSERT_EQ(text::indent_t(4, 8, false).create(5, 2), "\t\t\t\t");
	OAK_ASSERT_EQ(text::indent_t(4, 8, false).create(6, 2), "\t\t\t\t");
	OAK_ASSERT_EQ(text::indent_t(4, 8, false).create(7, 2), "\t\t\t\t");
	OAK_ASSERT_EQ(text::indent_t(4, 8, false).create(8, 2), "\t\t\t\t");

	OAK_ASSERT_EQ(text::indent_t(4, 8, false).create(0, 3), "\t\t\t\t\t\t");
	OAK_ASSERT_EQ(text::indent_t(4, 8, false).create(1, 3), "\t\t\t\t\t\t");
	OAK_ASSERT_EQ(text::indent_t(4, 8, false).create(2, 3), "\t\t\t\t\t\t");
	OAK_ASSERT_EQ(text::indent_t(4, 8, false).create(3, 3), "\t\t\t\t\t\t");
	OAK_ASSERT_EQ(text::indent_t(4, 8, false).create(4, 3), "\t\t\t\t\t\t");
	OAK_ASSERT_EQ(text::indent_t(4, 8, false).create(5, 3), "\t\t\t\t\t\t");
	OAK_ASSERT_EQ(text::indent_t(4, 8, false).create(6, 3), "\t\t\t\t\t\t");
	OAK_ASSERT_EQ(text::indent_t(4, 8, false).create(7, 3), "\t\t\t\t\t\t");
	OAK_ASSERT_EQ(text::indent_t(4, 8, false).create(8, 3), "\t\t\t\t\t\t");
}
