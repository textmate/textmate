#include <bundles/load.h>
#include <bundles/locations.h>
#include <test/jail.h>
#include "../src/appearance.h"
#include "../src/bundle_index.h"
#include "../src/samples.h"
#include "../src/rule_matching.h"

void test_save_new_theme_and_edit_managed_theme_as_local_override ()
{
	test::jail_t jail;
	struct restore_locations_t
	{
		std::vector<std::string> locations = bundles::locations();
		~restore_locations_t () { bundles::set_locations(locations); }
	} restore;
	bundles::set_locations({ jail.path("Local"), jail.path("Managed") });

	oak::uuid_t bundleUUID;
	bundleUUID.generate();
	auto bundle = std::make_shared<bundles::item_t>(bundleUUID, bundles::item_ptr(), bundles::kItemTypeBundle);
	bundle->set_plist({ { "name", "Theme Editor" }, { "uuid", to_s(bundleUUID) } });
	oak::uuid_t uuid;
	uuid.generate();
	auto theme = std::make_shared<bundles::item_t>(uuid, bundle, bundles::kItemTypeTheme);
	plist::dictionary_t value = {
		{ "name", "Test Theme" }, { "uuid", to_s(uuid) }, { "author", "Preserve me" },
		{ "semanticClass", "theme.light.test-theme" },
		{ "settings", plist::array_t{
			plist::dictionary_t{ { "settings", plist::dictionary_t{ { "background", "#FFFFFF" }, { "fontName", "Menlo-Regular" }, { "fontSize", "14" } } } },
			plist::dictionary_t{ { "scope", "comment" }, { "settings", plist::dictionary_t{ { "foreground", "#008800" }, { "fontStyle", "italic" } } } }
		} }
	};
	theme->set_plist(value);
	OAK_ASSERT(theme->save());
	OAK_ASSERT(bundle->local());
	OAK_ASSERT(theme->local());
	OAK_ASSERT_EQ(plist::load(theme->paths().front()), value);

	// Simulate an installed managed bundle, then save a local delta over it.
	jail.mkdir("Managed");
	OAK_ASSERT_EQ(rename(jail.path("Local/Bundles").c_str(), jail.path("Managed/Bundles").c_str()), 0);
	plist::cache_t cache;
	auto loaded = create_bundle_index({ jail.path("Local/Bundles"), jail.path("Managed/Bundles") }, cache);
	bundles::item_ptr installed;
	for(auto const& item : loaded.first)
		if(item->uuid() == uuid)
			installed = item;
	OAK_ASSERT(installed);
	OAK_ASSERT(!installed->local());
	// Leave a persisted index from before the local override was created.
	std::string const cachePath = jail.path("BundlesIndex.plist");
	cache.save(cachePath);
	std::string managedPath = installed->paths().front();
	value["name"] = "Edited Theme";
	theme_editor::set_appearance(value, theme_editor::dark);
	installed->set_plist(value);
	OAK_ASSERT(installed->save());
	OAK_ASSERT_EQ(plist::load(managedPath)["name"], plist::any_t("Test Theme"));

	auto reloaded = theme_editor::load_bundle_index(cachePath);
	bool found = false;
	for(auto const& item : reloaded.first)
	{
		if(item->uuid() == uuid)
		{
			OAK_ASSERT_EQ(item->name(), "Edited Theme");
			OAK_ASSERT_EQ(item->plist(), value);
			found = true;
		}
	}
	OAK_ASSERT(found);

	// A missing disk cache must still discover all installed themes.
	auto uncached = theme_editor::load_bundle_index(jail.path("MissingIndex.plist"));
	OAK_ASSERT_EQ(uncached.first.size(), reloaded.first.size());
}

void test_appearance_matches_textmate_theme_menu ()
{
	OAK_ASSERT(theme_editor::appearance("theme.light.example") == theme_editor::light);
	OAK_ASSERT(theme_editor::appearance("theme.dark.example") == theme_editor::dark);
	OAK_ASSERT(theme_editor::appearance("theme.dark.example, other.metadata") == theme_editor::dark);
	OAK_ASSERT(theme_editor::appearance("theme.dark") == theme_editor::unspecified);
	OAK_ASSERT(theme_editor::appearance("theme.unspecified.example") == theme_editor::unspecified);
	OAK_ASSERT(theme_editor::appearance("") == theme_editor::unspecified);
}

void test_changing_appearance_keeps_semantic_metadata ()
{
	plist::dictionary_t theme = { { "semanticClass", "theme.light.example.variant, other.metadata" } };
	theme_editor::set_appearance(theme, theme_editor::dark);
	OAK_ASSERT_EQ(theme["semanticClass"], plist::any_t("theme.dark.example.variant, other.metadata"));
	theme_editor::set_appearance(theme, theme_editor::unspecified);
	OAK_ASSERT_EQ(theme["semanticClass"], plist::any_t("theme.unspecified.example.variant, other.metadata"));
	OAK_ASSERT(theme_editor::appearance(theme) == theme_editor::unspecified);

	theme = { { "semanticClass", "other.metadata" } };
	theme_editor::set_appearance(theme, theme_editor::light);
	OAK_ASSERT_EQ(theme["semanticClass"], plist::any_t("theme.light.custom, other.metadata"));

	theme.clear();
	theme_editor::set_appearance(theme, theme_editor::unspecified);
	OAK_ASSERT(theme.empty());
	theme_editor::set_appearance(theme, theme_editor::light);
	OAK_ASSERT(theme_editor::appearance(theme) == theme_editor::light);
}

void test_language_samples_have_distinct_complete_resources ()
{
	std::set<std::string> names, files;
	for(auto const& sample : theme_editor::samples())
	{
		OAK_ASSERT(names.insert(sample.name).second);
		OAK_ASSERT(files.insert(sample.file).second);
		OAK_ASSERT(!sample.scopes.empty());
		std::string content = path::content(path::join(THEME_EDITOR_SAMPLE_DIR, sample.file));
		OAK_ASSERT(content.size() > 1000);
		OAK_ASSERT(std::count(content.begin(), content.end(), '\n') >= 45);
		OAK_ASSERT(content.back() == '\n');
	}
}

void test_scope_inspection_uses_specificity_and_later_rule_ties ()
{
	plist::array_t rules = {
		plist::dictionary_t{ { "settings", plist::dictionary_t{ { "foreground", "#FFFFFF" } } } },
		plist::dictionary_t{ { "scope", "comment" } },
		plist::dictionary_t{ { "scope", "source.js comment.block" } },
		plist::dictionary_t{ { "scope", "string, constant.numeric" } },
		plist::dictionary_t{ { "scope", "source.js comment.block" } },
		plist::dictionary_t{ { "scope", "comment - comment.block" } },
	};
	OAK_ASSERT_EQ(*theme_editor::matching_rule(rules, scope::scope_t("source.js comment.block.documentation")), 4UL);
	OAK_ASSERT_EQ(*theme_editor::matching_rule(rules, scope::scope_t("source.js comment.line.double-slash")), 5UL);
	OAK_ASSERT_EQ(*theme_editor::matching_rule(rules, scope::scope_t("source.js string.quoted.double")), 3UL);
	OAK_ASSERT_EQ(*theme_editor::matching_rule(rules, scope::scope_t("source.js constant.numeric")), 3UL);
	OAK_ASSERT_EQ(*theme_editor::matching_rule(rules, scope::scope_t("source.js variable.other")), 0UL);
	OAK_ASSERT(!theme_editor::matching_rule({}, scope::scope_t("source.js")));
	OAK_ASSERT(!theme_editor::matching_rule(rules, scope::scope_t()));
}
