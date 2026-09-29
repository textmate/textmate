#include "CGColorRefToString.h"
#include <theme/src/theme.h>

void test_missing_theme_item_still_sets_gutter_colors ()
{
	bundles::item_ptr themeItem;
	theme_t theme(themeItem);
	auto const& gutter = theme.gutter_styles();
	OAK_ASSERT_EQ(to_s(gutter.divider),               "#666666FF");
	OAK_ASSERT_EQ(to_s(gutter.selectionBorder),       "#666666FF");
	OAK_ASSERT_EQ(to_s(gutter.foreground),            "#808080FF");
	OAK_ASSERT_EQ(to_s(gutter.background),            "#212121FF");
	OAK_ASSERT_EQ(to_s(gutter.icons),                 "#808080FF");
	OAK_ASSERT_EQ(to_s(gutter.iconsHover),            "#808080FF");
	OAK_ASSERT_EQ(to_s(gutter.iconsPressed),          "#808080FF");
	OAK_ASSERT_EQ(to_s(gutter.selectionForeground),   "#F2F2F2FF");
	OAK_ASSERT_EQ(to_s(gutter.selectionBackground),   "#0D0D0DFF");
	OAK_ASSERT_EQ(to_s(gutter.selectionIcons),        "#F2F2F2FF");
	OAK_ASSERT_EQ(to_s(gutter.selectionIconsHover),   "#F2F2F2FF");
	OAK_ASSERT_EQ(to_s(gutter.selectionIconsPressed), "#F2F2F2FF");
}
