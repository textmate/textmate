#import "../src/colors.h"
#import <theme/theme.h>
#import <ns/ns.h>

void test_color_controls_match_renderer_for_legacy_and_srgb_themes ()
{
	for(NSDictionary* metadata in @[ @{}, @{ @"colorSpaceName": @"sRGB" } ])
	{
		NSColorSpace* space = theme_editor::color_space(metadata);
		NSMutableDictionary* value = [metadata mutableCopy];
		value[@"settings"] = @[ @{ @"settings": @{ @"background": @"#516B6B", @"foreground": @"#9B59B6", @"selection": @"#73597E80" } } ];
		oak::uuid_t uuid;
		uuid.generate();
		auto item = std::make_shared<bundles::item_t>(uuid, bundles::item_ptr(), bundles::kItemTypeTheme);
		item->set_plist(plist::convert((__bridge CFPropertyListRef)value));
		theme_t renderer(item, "Menlo-Regular", 12, false);
		auto const& style = renderer.styles_for_scope(scope::scope_t("source.js"));
		for(NSArray* pair in @[ @[ @"#516B6B", [NSColor colorWithCGColor:renderer.background()] ], @[ @"#9B59B6", [NSColor colorWithCGColor:renderer.foreground()] ], @[ @"#73597E80", [NSColor colorWithCGColor:style.selection()] ] ])
		{
			NSColor* control = theme_editor::parse_color(pair[0], space);
			NSColor* actual = pair[1];
			OAK_ASSERT(CFEqual(control.colorSpace.CGColorSpace, actual.colorSpace.CGColorSpace));
			NSColor* displayedControl = [control colorUsingColorSpace:NSColorSpace.sRGBColorSpace];
			NSColor* displayedActual = [actual colorUsingColorSpace:NSColorSpace.sRGBColorSpace];
			OAK_ASSERT(fabs(displayedControl.redComponent - displayedActual.redComponent) < 0.00001);
			OAK_ASSERT(fabs(displayedControl.greenComponent - displayedActual.greenComponent) < 0.00001);
			OAK_ASSERT(fabs(displayedControl.blueComponent - displayedActual.blueComponent) < 0.00001);
			OAK_ASSERT(fabs(displayedControl.alphaComponent - displayedActual.alphaComponent) < 0.00001);
		}
	}
}

void test_picker_converts_back_to_theme_color_space_without_changing_alpha ()
{
	NSColor* picked = theme_editor::parse_color(@"#516B6B80", NSColorSpace.sRGBColorSpace);
	OAK_ASSERT([theme_editor::color_string(picked, NSColorSpace.sRGBColorSpace) isEqualToString:@"#516B6B80"]);
	NSString* legacyHex = theme_editor::color_string(picked, NSColorSpace.genericRGBColorSpace);
	OAK_ASSERT(![legacyHex isEqualToString:@"#516B6B80"]);
	OAK_ASSERT([legacyHex hasSuffix:@"80"]);
	NSColor* stored = [picked colorUsingColorSpace:NSColorSpace.genericRGBColorSpace];
	NSColor* reloaded = theme_editor::parse_color(legacyHex, NSColorSpace.genericRGBColorSpace);
	OAK_ASSERT(fabs(stored.redComponent - reloaded.redComponent) <= 0.5/255);
	OAK_ASSERT(fabs(stored.greenComponent - reloaded.greenComponent) <= 0.5/255);
	OAK_ASSERT(fabs(stored.blueComponent - reloaded.blueComponent) <= 0.5/255);
	OAK_ASSERT([theme_editor::color_string(reloaded, NSColorSpace.genericRGBColorSpace) isEqualToString:legacyHex]);
}

void test_wide_gamut_picker_colors_produce_valid_theme_hex ()
{
	NSColor* picked = [NSColor colorWithDisplayP3Red:1 green:0 blue:0 alpha:0.5];
	for(NSColorSpace* space in @[ NSColorSpace.genericRGBColorSpace, NSColorSpace.sRGBColorSpace ])
	{
		NSString* value = theme_editor::color_string(picked, space);
		OAK_ASSERT_EQ(value.length, 9UL);
		OAK_ASSERT([value hasSuffix:@"80"]);
		OAK_ASSERT(theme_editor::parse_color(value, space) != nil);
	}
}

void test_shorthand_colors_match_expanded_values ()
{
	for(NSColorSpace* space in @[ NSColorSpace.genericRGBColorSpace, NSColorSpace.sRGBColorSpace ])
	{
		for(NSArray<NSString*>* pair in @[ @[ @"#123", @"#112233" ], @[ @"#aBc", @"#AABBCC" ], @[ @"#1234", @"#11223344" ], @[ @"#0000", @"#00000000" ], @[ @"#ffff", @"#FFFFFF" ] ])
		{
			NSColor* shorthand = theme_editor::parse_color(pair[0], space);
			NSColor* expanded = theme_editor::parse_color(pair[1], space);
			OAK_ASSERT(shorthand != nil);
			OAK_ASSERT(CFEqual(shorthand.CGColor, expanded.CGColor));
			OAK_ASSERT([theme_editor::color_string(shorthand, space) isEqualToString:pair[1]]);
		}
		OAK_ASSERT([theme_editor::color_string(theme_editor::parse_color(@"#123456", space), space) isEqualToString:@"#123456"]);
		OAK_ASSERT([theme_editor::color_string(theme_editor::parse_color(@"#123456FF", space), space) isEqualToString:@"#123456"]);
		for(NSString* invalid in @[ @"#12", @"#12345", @"#1234567", @"#12G", @"#123Z", @"123", @"#123456789" ])
			OAK_ASSERT(theme_editor::parse_color(invalid, space) == nil);
	}
}
