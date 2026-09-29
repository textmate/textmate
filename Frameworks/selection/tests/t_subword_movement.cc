#include <buffer/src/buffer.h>
#include <selection/src/selection.h>

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

static std::string left  (std::string const& input) { return move(input, kSelectionMoveToBeginOfSubWord); }
static std::string right (std::string const& input) { return move(input, kSelectionMoveToEndOfSubWord);   }

void test_subword_movement ()
{
	OAK_ASSERT_EQ(right(" ‸NSNotFound "), " NS‸NotFound ");
	OAK_ASSERT_EQ(right(" NS‸NotFound "), " NSNot‸Found ");
	OAK_ASSERT_EQ(right(" NSNot‸Found "), " NSNotFound‸ ");
	OAK_ASSERT_EQ( left(" NSNotFound‸ "), " NSNot‸Found ");
	OAK_ASSERT_EQ( left(" NSNot‸Found "), " NS‸NotFound ");
	OAK_ASSERT_EQ( left(" NS‸NotFound "), " ‸NSNotFound ");

	OAK_ASSERT_EQ(right(" ‸camelCase "), " camel‸Case ");
	OAK_ASSERT_EQ(right(" camel‸Case "), " camelCase‸ ");
	OAK_ASSERT_EQ( left(" camelCase‸ "), " camel‸Case ");
	OAK_ASSERT_EQ( left(" camel‸Case "), " ‸camelCase ");

	OAK_ASSERT_EQ(right(" ‸CamelCase "), " Camel‸Case ");
	OAK_ASSERT_EQ(right(" Camel‸Case "), " CamelCase‸ ");
	OAK_ASSERT_EQ( left(" CamelCase‸ "), " Camel‸Case ");
	OAK_ASSERT_EQ( left(" Camel‸Case "), " ‸CamelCase ");

	OAK_ASSERT_EQ(right(" ‸snake_Case "), " snake‸_Case ");
	OAK_ASSERT_EQ(right(" snake‸_Case "), " snake_Case‸ ");
	OAK_ASSERT_EQ( left(" snake_Case‸ "), " snake_‸Case ");
	OAK_ASSERT_EQ( left(" snake_‸Case "), " ‸snake_Case ");

	OAK_ASSERT_EQ(right(" ‸NDEBUG "), " NDEBUG‸ ");
	OAK_ASSERT_EQ( left(" NDEBUG‸ "), " ‸NDEBUG ");

	OAK_ASSERT_EQ(right(" ‸ space "), "  space‸ ");
	OAK_ASSERT_EQ( left(" space ‸ "), " ‸space  ");

	OAK_ASSERT_EQ(right(" ‸  space "), "   ‸space ");
	OAK_ASSERT_EQ( left(" space  ‸ "), " space‸   ");

	OAK_ASSERT_EQ(right("‸0b0000'0000"), "0b‸0000'0000");
	OAK_ASSERT_EQ(right("0b‸0000'0000"), "0b0000‸'0000");
	OAK_ASSERT_EQ(right("0b0000‸'0000"), "0b0000'‸0000");
	OAK_ASSERT_EQ(right("0b0000'‸0000"), "0b0000'0000‸");
	OAK_ASSERT_EQ( left("0b0000'0000‸"), "0b0000'‸0000");
	OAK_ASSERT_EQ( left("0b0000'‸0000"), "0b0000‸'0000");
	OAK_ASSERT_EQ( left("0b0000‸'0000"), "0b‸0000'0000");
	OAK_ASSERT_EQ( left("0b‸0000'0000"), "0‸b0000'0000");
}
