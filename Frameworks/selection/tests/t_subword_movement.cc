#include <buffer/buffer.h>
#include <selection/selection.h>

static std::string move (std::string input, move_unit_type action)
{
	static std::string const kMarker = "‸";

	size_t pos = input.find(kMarker);
	OAK_ASSERT_NE(pos, std::string::npos);
	input.replace(pos, kMarker.size(), "");

	ng::buffer_t buf;
	buf.insert(0, input);
	for(auto range : ng::move(buf, ng::range_t(pos), action))
		buf.replace(range.min().index, range.max().index, "‸");
	return buf.substr(0, buf.size());
}

void test_subword_movement ()
{
	OAK_ASSERT_EQ(move(" ‸NSNotFound ", kSelectionMoveToEndOfSubWord),   " NS‸NotFound ");
	OAK_ASSERT_EQ(move(" NS‸NotFound ", kSelectionMoveToEndOfSubWord),   " NSNot‸Found ");
	OAK_ASSERT_EQ(move(" NSNot‸Found ", kSelectionMoveToEndOfSubWord),   " NSNotFound‸ ");
	OAK_ASSERT_EQ(move(" NSNotFound‸ ", kSelectionMoveToBeginOfSubWord), " NSNot‸Found ");
	OAK_ASSERT_EQ(move(" NSNot‸Found ", kSelectionMoveToBeginOfSubWord), " NS‸NotFound ");
	OAK_ASSERT_EQ(move(" NS‸NotFound ", kSelectionMoveToBeginOfSubWord), " ‸NSNotFound ");

	OAK_ASSERT_EQ(move(" ‸camelCase ", kSelectionMoveToEndOfSubWord),   " camel‸Case ");
	OAK_ASSERT_EQ(move(" camel‸Case ", kSelectionMoveToEndOfSubWord),   " camelCase‸ ");
	OAK_ASSERT_EQ(move(" camelCase‸ ", kSelectionMoveToBeginOfSubWord), " camel‸Case ");
	OAK_ASSERT_EQ(move(" camel‸Case ", kSelectionMoveToBeginOfSubWord), " ‸camelCase ");

	OAK_ASSERT_EQ(move(" ‸CamelCase ", kSelectionMoveToEndOfSubWord),   " Camel‸Case ");
	OAK_ASSERT_EQ(move(" Camel‸Case ", kSelectionMoveToEndOfSubWord),   " CamelCase‸ ");
	OAK_ASSERT_EQ(move(" CamelCase‸ ", kSelectionMoveToBeginOfSubWord), " Camel‸Case ");
	OAK_ASSERT_EQ(move(" Camel‸Case ", kSelectionMoveToBeginOfSubWord), " ‸CamelCase ");

	OAK_ASSERT_EQ(move(" ‸snake_Case ", kSelectionMoveToEndOfSubWord),   " snake‸_Case ");
	OAK_ASSERT_EQ(move(" snake‸_Case ", kSelectionMoveToEndOfSubWord),   " snake_Case‸ ");
	OAK_ASSERT_EQ(move(" snake_Case‸ ", kSelectionMoveToBeginOfSubWord), " snake_‸Case ");
	OAK_ASSERT_EQ(move(" snake_‸Case ", kSelectionMoveToBeginOfSubWord), " ‸snake_Case ");

	OAK_ASSERT_EQ(move(" ‸NDEBUG ", kSelectionMoveToEndOfSubWord),   " NDEBUG‸ ");
	OAK_ASSERT_EQ(move(" NDEBUG‸ ", kSelectionMoveToBeginOfSubWord), " ‸NDEBUG ");

	OAK_ASSERT_EQ(move(" ‸ space ", kSelectionMoveToEndOfSubWord),   "  space‸ ");
	OAK_ASSERT_EQ(move(" space ‸ ", kSelectionMoveToBeginOfSubWord), " ‸space  ");

	OAK_ASSERT_EQ(move(" ‸  space ", kSelectionMoveToEndOfSubWord),   "   ‸space ");
	OAK_ASSERT_EQ(move(" space  ‸ ", kSelectionMoveToBeginOfSubWord), " space‸   ");

	OAK_ASSERT_EQ(move("‸0b0000'0000", kSelectionMoveToEndOfSubWord),   "0b‸0000'0000");
	OAK_ASSERT_EQ(move("0b‸0000'0000", kSelectionMoveToEndOfSubWord),   "0b0000‸'0000");
	OAK_ASSERT_EQ(move("0b0000‸'0000", kSelectionMoveToEndOfSubWord),   "0b0000'‸0000");
	OAK_ASSERT_EQ(move("0b0000'‸0000", kSelectionMoveToEndOfSubWord),   "0b0000'0000‸");
	OAK_ASSERT_EQ(move("0b0000'0000‸", kSelectionMoveToBeginOfSubWord), "0b0000'‸0000");
	OAK_ASSERT_EQ(move("0b0000'‸0000", kSelectionMoveToBeginOfSubWord), "0b0000‸'0000");
	OAK_ASSERT_EQ(move("0b0000‸'0000", kSelectionMoveToBeginOfSubWord), "0b‸0000'0000");
	OAK_ASSERT_EQ(move("0b‸0000'0000", kSelectionMoveToBeginOfSubWord), "0‸b0000'0000");
}
