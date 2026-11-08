#include "CGColorRefToString.h"
#include <theme/theme.h>
#include <theme/font_size.h>
#include <cmath>

void test_missing_theme_item_still_sets_gutter_colors ()
{
	bundles::item_ptr themeItem;
	theme_t theme(themeItem);
	auto const& gutter = theme.gutter_styles();
	OAK_ASSERT_EQ(to_s(gutter.divider),               "#666666FF");
	OAK_ASSERT_EQ(to_s(gutter.selectionBorder),       "#666666FF");
	OAK_ASSERT_EQ(to_s(gutter.foreground),            "#808080FF");
	OAK_ASSERT_EQ(to_s(gutter.background),            "#171717FF");
	OAK_ASSERT_EQ(to_s(gutter.icons),                 "#808080FF");
	OAK_ASSERT_EQ(to_s(gutter.iconsHover),            "#808080FF");
	OAK_ASSERT_EQ(to_s(gutter.iconsPressed),          "#808080FF");
	OAK_ASSERT_EQ(to_s(gutter.selectionForeground),   "#F2F2F2FF");
	OAK_ASSERT_EQ(to_s(gutter.selectionBackground),   "#0D0D0DFF");
	OAK_ASSERT_EQ(to_s(gutter.selectionIcons),        "#F2F2F2FF");
	OAK_ASSERT_EQ(to_s(gutter.selectionIconsHover),   "#F2F2F2FF");
	OAK_ASSERT_EQ(to_s(gutter.selectionIconsPressed), "#F2F2F2FF");
}

void test_font_sizes_share_units_and_relative_semantics ()
{
	for(auto const& entry : std::vector<std::pair<std::string, double>>{ { "120%", -1.2 }, { "1.2em", -1.2 }, { "14pt", 14 }, { "14 pt", 14 }, { "14", 14 }, { ".5em", -0.5 } })
	{
		auto size = theme::parse_font_size(entry.first);
		OAK_ASSERT(size.has_value());
		OAK_ASSERT(std::abs(*size - entry.second) < 0.00001);
		oak::uuid_t uuid;
		uuid.generate();
		auto item = std::make_shared<bundles::item_t>(uuid, bundles::item_ptr(), bundles::kItemTypeTheme);
		item->set_plist({ { "settings", plist::array_t{ plist::dictionary_t{ { "settings", plist::dictionary_t{ { "fontSize", entry.first } } } } } } });
		theme_t renderer(item, "Menlo-Regular", 12, false);
		double expected = entry.second > 0 ? entry.second : 12 * -entry.second;
		OAK_ASSERT(std::abs(CTFontGetSize(renderer.styles_for_scope(scope::scope_t("source.js")).font()) - expected) < 0.00001);
	}
	for(auto const& invalid : { "", "garbage", "12px", "12ptjunk", "0", "-2", "nan", "inf", "1.2.3", "12em junk" })
		OAK_ASSERT(!theme::parse_font_size(invalid));
}

void test_draft_styles_refresh_without_replacing_cached_theme ()
{
	oak::uuid_t uuid;
	uuid.generate();
	auto item = std::make_shared<bundles::item_t>(uuid, bundles::item_ptr(), bundles::kItemTypeTheme);
	plist::dictionary_t value = {
		{ "uuid", to_s(uuid) },
		{ "settings", plist::array_t{ plist::dictionary_t{ { "settings", plist::dictionary_t{ { "foreground", "#123456" }, { "background", "#FFFFFF" } } } } } }
	};
	item->set_plist(value);
	theme_t installed(item);
	OAK_ASSERT_EQ(to_s(installed.foreground()), "#123456FF");

	value["settings"] = plist::array_t{ plist::dictionary_t{ { "settings", plist::dictionary_t{ { "foreground", "#ABCDEF" }, { "background", "#000000" } } } } };
	item->set_plist(value);
	theme_t draft(item, "Menlo-Regular", 13, false);
	OAK_ASSERT_EQ(to_s(draft.foreground()), "#ABCDEFFF");
	OAK_ASSERT_EQ(to_s(draft.background()), "#000000FF");
	OAK_ASSERT_EQ(to_s(installed.foreground()), "#123456FF");

	// The layout copies a theme to the current editor font when installing it.
	auto resized = draft.copy_with_font_name_and_size("Menlo-Regular", 18);
	OAK_ASSERT_EQ(to_s(resized->foreground()), "#ABCDEFFF");
	OAK_ASSERT_EQ(resized->font_size(), 18);
	OAK_ASSERT_EQ(to_s(theme_t(item).foreground()), "#123456FF");
}

void test_font_copy_drops_previously_resolved_scope_fonts ()
{
	theme_t theme(bundles::item_ptr(), "Menlo-Regular", 12, false);
	OAK_ASSERT_EQ(CTFontGetSize(theme.styles_for_scope(scope::scope_t("source.c++")).font()), 12);
	auto resized = theme.copy_with_font_name_and_size("Menlo-Regular", 19);
	OAK_ASSERT_EQ(CTFontGetSize(resized->styles_for_scope(scope::scope_t("source.c++")).font()), 19);
	OAK_ASSERT_EQ(CTFontGetSize(theme.styles_for_scope(scope::scope_t("source.c++")).font()), 12);
}
