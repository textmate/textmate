#include <cf/cgrect.h>
#include <text/format.h>

class RectTests : public CxxTest::TestSuite
{
	static void set (std::vector<std::string>& canvas, CGRect const& r, char m = 'x')
	{
		for(size_t y = (size_t)CGRectGetMinY(r); y < (size_t)CGRectGetMaxY(r); ++y)
		{
			for(size_t x = (size_t)CGRectGetMinX(r); x < (size_t)CGRectGetMaxX(r); ++x)
				canvas[y][x] = m;
		}
	}

	static std::string to_s (std::vector<CGRect> const& rList)
	{
		std::vector<std::string> canvas(3, "...");
		iterate(r, rList)
			set(canvas, *r);
		return text::join(canvas, "");
	}

	static std::string to_s (CGRect const& r)
	{
		return to_s(std::vector<CGRect>(1, r));
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
		return to_s(res);
	}

public:
	void test_string_rects ()
	{
		TS_ASSERT_EQUALS(to_s(from_str("xxxxxxxxx")), "xxxxxxxxx");
		TS_ASSERT_EQUALS(to_s(from_str(".........")), ".........");

		TS_ASSERT_EQUALS(to_s(from_str("x........")), "x........");
		TS_ASSERT_EQUALS(to_s(from_str(".x.......")), ".x.......");
		TS_ASSERT_EQUALS(to_s(from_str("..x......")), "..x......");
		TS_ASSERT_EQUALS(to_s(from_str("...x.....")), "...x.....");
		TS_ASSERT_EQUALS(to_s(from_str("....x....")), "....x....");
		TS_ASSERT_EQUALS(to_s(from_str(".....x...")), ".....x...");
		TS_ASSERT_EQUALS(to_s(from_str("......x..")), "......x..");
		TS_ASSERT_EQUALS(to_s(from_str(".......x.")), ".......x.");
		TS_ASSERT_EQUALS(to_s(from_str("........x")), "........x");

		TS_ASSERT_EQUALS(to_s(from_str("xx.......")), "xx.......");
		TS_ASSERT_EQUALS(to_s(from_str(".xx......")), ".xx......");
		TS_ASSERT_EQUALS(to_s(from_str("...xx....")), "...xx....");
		TS_ASSERT_EQUALS(to_s(from_str("....xx...")), "....xx...");
		TS_ASSERT_EQUALS(to_s(from_str("......xx.")), "......xx.");
		TS_ASSERT_EQUALS(to_s(from_str(".......xx")), ".......xx");

		TS_ASSERT_EQUALS(to_s(from_str("x..x.....")), "x..x.....");
		TS_ASSERT_EQUALS(to_s(from_str(".x..x....")), ".x..x....");
		TS_ASSERT_EQUALS(to_s(from_str("..x..x...")), "..x..x...");
		TS_ASSERT_EQUALS(to_s(from_str("...x..x..")), "...x..x..");
		TS_ASSERT_EQUALS(to_s(from_str("....x..x.")), "....x..x.");
		TS_ASSERT_EQUALS(to_s(from_str(".....x..x")), ".....x..x");

		TS_ASSERT_EQUALS(to_s(from_str("xxx......")), "xxx......");
		TS_ASSERT_EQUALS(to_s(from_str("...xxx...")), "...xxx...");
		TS_ASSERT_EQUALS(to_s(from_str("......xxx")), "......xxx");

		TS_ASSERT_EQUALS(to_s(from_str("x..x..x..")), "x..x..x..");
		TS_ASSERT_EQUALS(to_s(from_str(".x..x..x.")), ".x..x..x.");
		TS_ASSERT_EQUALS(to_s(from_str("..x..x..x")), "..x..x..x");
	}

	void test_rect_diff ()
	{
		TS_ASSERT_EQUALS(xor_rects("xxxxxxxxx", "........."), "xxxxxxxxx");
		TS_ASSERT_EQUALS(xor_rects("xxxxxxxxx", "xxxxxxxxx"), ".........");

		TS_ASSERT_EQUALS(xor_rects("xxxxxxxxx", "x........"), ".xxxxxxxx");
		TS_ASSERT_EQUALS(xor_rects("xxxxxxxxx", ".x......."), "x.xxxxxxx");
		TS_ASSERT_EQUALS(xor_rects("xxxxxxxxx", "..x......"), "xx.xxxxxx");
		TS_ASSERT_EQUALS(xor_rects("xxxxxxxxx", "...x....."), "xxx.xxxxx");
		TS_ASSERT_EQUALS(xor_rects("xxxxxxxxx", "....x...."), "xxxx.xxxx");
		TS_ASSERT_EQUALS(xor_rects("xxxxxxxxx", ".....x..."), "xxxxx.xxx");
		TS_ASSERT_EQUALS(xor_rects("xxxxxxxxx", "......x.."), "xxxxxx.xx");
		TS_ASSERT_EQUALS(xor_rects("xxxxxxxxx", ".......x."), "xxxxxxx.x");
		TS_ASSERT_EQUALS(xor_rects("xxxxxxxxx", "........x"), "xxxxxxxx.");

		TS_ASSERT_EQUALS(xor_rects("xxxxxxxxx", "xx......."), "..xxxxxxx");
		TS_ASSERT_EQUALS(xor_rects("xxxxxxxxx", ".xx......"), "x..xxxxxx");
		TS_ASSERT_EQUALS(xor_rects("xxxxxxxxx", "...xx...."), "xxx..xxxx");
		TS_ASSERT_EQUALS(xor_rects("xxxxxxxxx", "....xx..."), "xxxx..xxx");
		TS_ASSERT_EQUALS(xor_rects("xxxxxxxxx", "......xx."), "xxxxxx..x");
		TS_ASSERT_EQUALS(xor_rects("xxxxxxxxx", ".......xx"), "xxxxxxx..");

		TS_ASSERT_EQUALS(xor_rects("xxxxxxxxx", "x..x....."), ".xx.xxxxx");
		TS_ASSERT_EQUALS(xor_rects("xxxxxxxxx", ".x..x...."), "x.xx.xxxx");
		TS_ASSERT_EQUALS(xor_rects("xxxxxxxxx", "..x..x..."), "xx.xx.xxx");
		TS_ASSERT_EQUALS(xor_rects("xxxxxxxxx", "...x..x.."), "xxx.xx.xx");
		TS_ASSERT_EQUALS(xor_rects("xxxxxxxxx", "....x..x."), "xxxx.xx.x");
		TS_ASSERT_EQUALS(xor_rects("xxxxxxxxx", ".....x..x"), "xxxxx.xx.");

		TS_ASSERT_EQUALS(xor_rects("xxxxxxxxx", "xxx......"), "...xxxxxx");
		TS_ASSERT_EQUALS(xor_rects("xxxxxxxxx", "...xxx..."), "xxx...xxx");
		TS_ASSERT_EQUALS(xor_rects("xxxxxxxxx", "......xxx"), "xxxxxx...");

		TS_ASSERT_EQUALS(xor_rects("xxxxxxxxx", "x..x..x.."), ".xx.xx.xx");
		TS_ASSERT_EQUALS(xor_rects("xxxxxxxxx", ".x..x..x."), "x.xx.xx.x");
		TS_ASSERT_EQUALS(xor_rects("xxxxxxxxx", "..x..x..x"), "xx.xx.xx.");
	}
};
