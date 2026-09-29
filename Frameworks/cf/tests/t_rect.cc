#include <cf/cgrect.h>
#include <text/src/format.h>

static void set (std::vector<std::string>& canvas, CGRect const& r, char m = 'x')
{
	for(size_t y = (size_t)CGRectGetMinY(r); y < (size_t)CGRectGetMaxY(r); ++y)
	{
		for(size_t x = (size_t)CGRectGetMinX(r); x < (size_t)CGRectGetMaxX(r); ++x)
			canvas[y][x] = m;
	}
}

static std::string to_str (std::vector<CGRect> const& rList)
{
	std::vector<std::string> canvas(3, "...");
	for(auto const& r : rList)
		set(canvas, r);
	return text::join(canvas, "");
}

static std::string to_str (CGRect const& r)
{
	return to_str(std::vector<CGRect>(1, r));
}

static CGRect from_str (std::string const& s)
{
	size_t x0 = 3, x1 = 0;
	size_t y0 = 3, y1 = 0;
	for(size_t y = 0; y < 3; ++y)
	{
		for(size_t x = 0; x < 3; ++x)
		{
			if(s[3*y + x] == 'x')
			{
				x0 = std::min(x0, x);
				x1 = std::max(x1, x+1);
				y0 = std::min(y0, y);
				y1 = std::max(y1, y+1);
			}
		}
	}
	return CGRectMake(x0, y0, x1 - x0, y1 - y0);
}

static std::string xor_rects (std::string const& lhs, std::string const& rhs)
{
	std::vector<CGRect> res;
	OakRectDifference(from_str(lhs), from_str(rhs), back_inserter(res));
	return to_str(res);
}

void test_string_rects ()
{
	OAK_ASSERT_EQ(to_str(from_str("xxxxxxxxx")), "xxxxxxxxx");
	OAK_ASSERT_EQ(to_str(from_str(".........")), ".........");

	OAK_ASSERT_EQ(to_str(from_str("x........")), "x........");
	OAK_ASSERT_EQ(to_str(from_str(".x.......")), ".x.......");
	OAK_ASSERT_EQ(to_str(from_str("..x......")), "..x......");
	OAK_ASSERT_EQ(to_str(from_str("...x.....")), "...x.....");
	OAK_ASSERT_EQ(to_str(from_str("....x....")), "....x....");
	OAK_ASSERT_EQ(to_str(from_str(".....x...")), ".....x...");
	OAK_ASSERT_EQ(to_str(from_str("......x..")), "......x..");
	OAK_ASSERT_EQ(to_str(from_str(".......x.")), ".......x.");
	OAK_ASSERT_EQ(to_str(from_str("........x")), "........x");

	OAK_ASSERT_EQ(to_str(from_str("xx.......")), "xx.......");
	OAK_ASSERT_EQ(to_str(from_str(".xx......")), ".xx......");
	OAK_ASSERT_EQ(to_str(from_str("...xx....")), "...xx....");
	OAK_ASSERT_EQ(to_str(from_str("....xx...")), "....xx...");
	OAK_ASSERT_EQ(to_str(from_str("......xx.")), "......xx.");
	OAK_ASSERT_EQ(to_str(from_str(".......xx")), ".......xx");

	OAK_ASSERT_EQ(to_str(from_str("x..x.....")), "x..x.....");
	OAK_ASSERT_EQ(to_str(from_str(".x..x....")), ".x..x....");
	OAK_ASSERT_EQ(to_str(from_str("..x..x...")), "..x..x...");
	OAK_ASSERT_EQ(to_str(from_str("...x..x..")), "...x..x..");
	OAK_ASSERT_EQ(to_str(from_str("....x..x.")), "....x..x.");
	OAK_ASSERT_EQ(to_str(from_str(".....x..x")), ".....x..x");

	OAK_ASSERT_EQ(to_str(from_str("xxx......")), "xxx......");
	OAK_ASSERT_EQ(to_str(from_str("...xxx...")), "...xxx...");
	OAK_ASSERT_EQ(to_str(from_str("......xxx")), "......xxx");

	OAK_ASSERT_EQ(to_str(from_str("x..x..x..")), "x..x..x..");
	OAK_ASSERT_EQ(to_str(from_str(".x..x..x.")), ".x..x..x.");
	OAK_ASSERT_EQ(to_str(from_str("..x..x..x")), "..x..x..x");
}

void test_rect_diff ()
{
	OAK_ASSERT_EQ(xor_rects("xxxxxxxxx", "........."), "xxxxxxxxx");
	OAK_ASSERT_EQ(xor_rects("xxxxxxxxx", "xxxxxxxxx"), ".........");

	OAK_ASSERT_EQ(xor_rects("xxxxxxxxx", "x........"), ".xxxxxxxx");
	OAK_ASSERT_EQ(xor_rects("xxxxxxxxx", ".x......."), "x.xxxxxxx");
	OAK_ASSERT_EQ(xor_rects("xxxxxxxxx", "..x......"), "xx.xxxxxx");
	OAK_ASSERT_EQ(xor_rects("xxxxxxxxx", "...x....."), "xxx.xxxxx");
	OAK_ASSERT_EQ(xor_rects("xxxxxxxxx", "....x...."), "xxxx.xxxx");
	OAK_ASSERT_EQ(xor_rects("xxxxxxxxx", ".....x..."), "xxxxx.xxx");
	OAK_ASSERT_EQ(xor_rects("xxxxxxxxx", "......x.."), "xxxxxx.xx");
	OAK_ASSERT_EQ(xor_rects("xxxxxxxxx", ".......x."), "xxxxxxx.x");
	OAK_ASSERT_EQ(xor_rects("xxxxxxxxx", "........x"), "xxxxxxxx.");

	OAK_ASSERT_EQ(xor_rects("xxxxxxxxx", "xx......."), "..xxxxxxx");
	OAK_ASSERT_EQ(xor_rects("xxxxxxxxx", ".xx......"), "x..xxxxxx");
	OAK_ASSERT_EQ(xor_rects("xxxxxxxxx", "...xx...."), "xxx..xxxx");
	OAK_ASSERT_EQ(xor_rects("xxxxxxxxx", "....xx..."), "xxxx..xxx");
	OAK_ASSERT_EQ(xor_rects("xxxxxxxxx", "......xx."), "xxxxxx..x");
	OAK_ASSERT_EQ(xor_rects("xxxxxxxxx", ".......xx"), "xxxxxxx..");

	OAK_ASSERT_EQ(xor_rects("xxxxxxxxx", "x..x....."), ".xx.xxxxx");
	OAK_ASSERT_EQ(xor_rects("xxxxxxxxx", ".x..x...."), "x.xx.xxxx");
	OAK_ASSERT_EQ(xor_rects("xxxxxxxxx", "..x..x..."), "xx.xx.xxx");
	OAK_ASSERT_EQ(xor_rects("xxxxxxxxx", "...x..x.."), "xxx.xx.xx");
	OAK_ASSERT_EQ(xor_rects("xxxxxxxxx", "....x..x."), "xxxx.xx.x");
	OAK_ASSERT_EQ(xor_rects("xxxxxxxxx", ".....x..x"), "xxxxx.xx.");

	OAK_ASSERT_EQ(xor_rects("xxxxxxxxx", "xxx......"), "...xxxxxx");
	OAK_ASSERT_EQ(xor_rects("xxxxxxxxx", "...xxx..."), "xxx...xxx");
	OAK_ASSERT_EQ(xor_rects("xxxxxxxxx", "......xxx"), "xxxxxx...");

	OAK_ASSERT_EQ(xor_rects("xxxxxxxxx", "x..x..x.."), ".xx.xx.xx");
	OAK_ASSERT_EQ(xor_rects("xxxxxxxxx", ".x..x..x."), "x.xx.xx.x");
	OAK_ASSERT_EQ(xor_rects("xxxxxxxxx", "..x..x..x"), "xx.xx.xx.");
}
