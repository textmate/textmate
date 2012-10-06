#include <theme/theme.h>

class ThemeTests : public CxxTest::TestSuite
{
public:
	void test_missing_theme_item_still_sets_gutter_colors ()
	{
		theme_t theme(NULL);
		auto gutter = theme.gutter_styles();
		TS_ASSERT_EQUALS(to_s(gutter.divider),               "#666666FF");
		TS_ASSERT_EQUALS(to_s(gutter.selectionBorder),       "#666666FF");
		TS_ASSERT_EQUALS(to_s(gutter.foreground),            "#808080FF");
		TS_ASSERT_EQUALS(to_s(gutter.background),            "#212121FF");
		TS_ASSERT_EQUALS(to_s(gutter.icons),                 "#808080FF");
		TS_ASSERT_EQUALS(to_s(gutter.iconsHover),            "#808080FF");
		TS_ASSERT_EQUALS(to_s(gutter.iconsPressed),          "#808080FF");
		TS_ASSERT_EQUALS(to_s(gutter.selectionForeground),   "#F2F2F2FF");
		TS_ASSERT_EQUALS(to_s(gutter.selectionBackground),   "#0D0D0DFF");
		TS_ASSERT_EQUALS(to_s(gutter.selectionIcons),        "#F2F2F2FF");
		TS_ASSERT_EQUALS(to_s(gutter.selectionIconsHover),   "#F2F2F2FF");
		TS_ASSERT_EQUALS(to_s(gutter.selectionIconsPressed), "#F2F2F2FF");
	}
};
