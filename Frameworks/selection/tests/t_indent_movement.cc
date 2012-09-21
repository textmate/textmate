#include <buffer/buffer.h>
#include <selection/selection.h>

class IndentMovementTests : public CxxTest::TestSuite
{
	ng::buffer_t _buffer;

	std::string move (size_t caret, move_unit_type unit)
	{
		return to_s(_buffer, ng::move(_buffer, ng::range_t(caret), unit));
	}

	std::string extend (size_t caret, select_unit_type unit)
	{
		return to_s(_buffer, ng::extend(_buffer, ng::range_t(caret), unit).last().sorted());
	}

public:
	void setUp ()
	{
		if(_buffer.empty())
			_buffer.insert(0, "    indented line\n");
	}

	void test_indent_begin_movement ()
	{
		TS_ASSERT_EQUALS(move(0, kSelectionMoveToBeginOfIndentedLine), "1");
		TS_ASSERT_EQUALS(move(2, kSelectionMoveToBeginOfIndentedLine), "1");
		TS_ASSERT_EQUALS(move(4, kSelectionMoveToBeginOfIndentedLine), "1");
		TS_ASSERT_EQUALS(move(6, kSelectionMoveToBeginOfIndentedLine), "1:5");
		TS_ASSERT_EQUALS(move(8, kSelectionMoveToBeginOfIndentedLine), "1:5");
	}

	void test_indent_end_movement ()
	{
		TS_ASSERT_EQUALS(move( 0, kSelectionMoveToEndOfIndentedLine), "1:5");
		TS_ASSERT_EQUALS(move( 2, kSelectionMoveToEndOfIndentedLine), "1:5");
		TS_ASSERT_EQUALS(move( 4, kSelectionMoveToEndOfIndentedLine), "1:18");
		TS_ASSERT_EQUALS(move( 6, kSelectionMoveToEndOfIndentedLine), "1:18");
		TS_ASSERT_EQUALS(move( 8, kSelectionMoveToEndOfIndentedLine), "1:18");
		TS_ASSERT_EQUALS(move(17, kSelectionMoveToEndOfIndentedLine), "1:18");
	}

	void test_indent_begin_selection ()
	{
		TS_ASSERT_EQUALS(extend(0, kSelectionExtendToBeginOfIndentedLine), "1");
		TS_ASSERT_EQUALS(extend(2, kSelectionExtendToBeginOfIndentedLine), "1-1:3");
		TS_ASSERT_EQUALS(extend(4, kSelectionExtendToBeginOfIndentedLine), "1-1:5");
		TS_ASSERT_EQUALS(extend(6, kSelectionExtendToBeginOfIndentedLine), "1:5-1:7");
		TS_ASSERT_EQUALS(extend(8, kSelectionExtendToBeginOfIndentedLine), "1:5-1:9");
	}

	void test_indent_end_selection ()
	{
		TS_ASSERT_EQUALS(extend( 0, kSelectionExtendToEndOfIndentedLine), "1-1:5");
		TS_ASSERT_EQUALS(extend( 2, kSelectionExtendToEndOfIndentedLine), "1:3-1:5");
		TS_ASSERT_EQUALS(extend( 4, kSelectionExtendToEndOfIndentedLine), "1:5-1:18");
		TS_ASSERT_EQUALS(extend( 6, kSelectionExtendToEndOfIndentedLine), "1:7-1:18");
		TS_ASSERT_EQUALS(extend( 8, kSelectionExtendToEndOfIndentedLine), "1:9-1:18");
		TS_ASSERT_EQUALS(extend(17, kSelectionExtendToEndOfIndentedLine), "1:18");
	}
};
