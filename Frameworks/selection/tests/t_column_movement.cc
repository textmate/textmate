#include <buffer/src/buffer.h>
#include <selection/src/selection.h>

void test_column_movement ()
{
	ng::buffer_t buf(
		"  lorem\n"  // 1:  0-08
		"  sit\n"    // 2:  8-14
		"  ipsum\n"  // 3: 14-22
		"\n"         // 4: 22-23
		"  dollar\n" // 5: 23-32
		"  it\n"     // 6: 32-37
	);

	OAK_ASSERT_EQ(ng::to_s(buf, ng::move(buf, ng::ranges_t( 2), kSelectionMoveToEndOfColumn)), "3:3");
	OAK_ASSERT_EQ(ng::to_s(buf, ng::move(buf, ng::ranges_t( 5), kSelectionMoveToEndOfColumn)), "3:6");
	OAK_ASSERT_EQ(ng::to_s(buf, ng::move(buf, ng::ranges_t( 7), kSelectionMoveToEndOfColumn)), "3:8");
	OAK_ASSERT_EQ(ng::to_s(buf, ng::move(buf, ng::ranges_t(21), kSelectionMoveToEndOfColumn)), "5:8");
}
