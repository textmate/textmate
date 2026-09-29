#include <buffer/src/buffer.h>
#include <selection/src/selection.h>

static std::string move (ng::buffer_t const& buffer, size_t caret, move_unit_type unit)
{
	return to_s(buffer, ng::move(buffer, ng::range_t(caret), unit));
}

static std::string extend (ng::buffer_t const& buffer, size_t caret, select_unit_type unit)
{
	return to_s(buffer, ng::extend(buffer, ng::range_t(caret), unit).last().sorted());
}

void test_indent_begin_movement ()
{
	ng::buffer_t buf;
	buf.insert(0, "    indented line\n");

	OAK_ASSERT_EQ(move(buf, 0, kSelectionMoveToBeginOfIndentedLine), "1");
	OAK_ASSERT_EQ(move(buf, 2, kSelectionMoveToBeginOfIndentedLine), "1");
	OAK_ASSERT_EQ(move(buf, 4, kSelectionMoveToBeginOfIndentedLine), "1");
	OAK_ASSERT_EQ(move(buf, 6, kSelectionMoveToBeginOfIndentedLine), "1:5");
	OAK_ASSERT_EQ(move(buf, 8, kSelectionMoveToBeginOfIndentedLine), "1:5");
}

void test_indent_end_movement ()
{
	ng::buffer_t buf;
	buf.insert(0, "    indented line\n");

	OAK_ASSERT_EQ(move(buf,  0, kSelectionMoveToEndOfIndentedLine), "1:5");
	OAK_ASSERT_EQ(move(buf,  2, kSelectionMoveToEndOfIndentedLine), "1:5");
	OAK_ASSERT_EQ(move(buf,  4, kSelectionMoveToEndOfIndentedLine), "1:18");
	OAK_ASSERT_EQ(move(buf,  6, kSelectionMoveToEndOfIndentedLine), "1:18");
	OAK_ASSERT_EQ(move(buf,  8, kSelectionMoveToEndOfIndentedLine), "1:18");
	OAK_ASSERT_EQ(move(buf, 17, kSelectionMoveToEndOfIndentedLine), "1:18");
}

void test_indent_begin_selection ()
{
	ng::buffer_t buf;
	buf.insert(0, "    indented line\n");

	OAK_ASSERT_EQ(extend(buf, 0, kSelectionExtendToBeginOfIndentedLine), "1");
	OAK_ASSERT_EQ(extend(buf, 2, kSelectionExtendToBeginOfIndentedLine), "1-1:3");
	OAK_ASSERT_EQ(extend(buf, 4, kSelectionExtendToBeginOfIndentedLine), "1-1:5");
	OAK_ASSERT_EQ(extend(buf, 6, kSelectionExtendToBeginOfIndentedLine), "1:5-1:7");
	OAK_ASSERT_EQ(extend(buf, 8, kSelectionExtendToBeginOfIndentedLine), "1:5-1:9");
}

void test_indent_end_selection ()
{
	ng::buffer_t buf;
	buf.insert(0, "    indented line\n");

	OAK_ASSERT_EQ(extend(buf,  0, kSelectionExtendToEndOfIndentedLine), "1-1:5");
	OAK_ASSERT_EQ(extend(buf,  2, kSelectionExtendToEndOfIndentedLine), "1:3-1:5");
	OAK_ASSERT_EQ(extend(buf,  4, kSelectionExtendToEndOfIndentedLine), "1:5-1:18");
	OAK_ASSERT_EQ(extend(buf,  6, kSelectionExtendToEndOfIndentedLine), "1:7-1:18");
	OAK_ASSERT_EQ(extend(buf,  8, kSelectionExtendToEndOfIndentedLine), "1:9-1:18");
	OAK_ASSERT_EQ(extend(buf, 17, kSelectionExtendToEndOfIndentedLine), "1:18");
}
