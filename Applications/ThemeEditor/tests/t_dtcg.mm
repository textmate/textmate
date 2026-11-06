#import "../src/dtcg.h"
#import "../src/ase.h"

static NSData* TokenData (id value)
{
	return [NSJSONSerialization dataWithJSONObject:value options:0 error:nil];
}

static bool RejectsTokens (id value)
{
	try { theme_editor::read_dtcg(TokenData(value)); }
	catch(std::runtime_error const&) { return true; }
	return false;
}

void test_dtcg_project_color_sets_and_theme_mapping ()
{
	NSData* data = [NSData dataWithContentsOfFile:@THEME_EDITOR_FIXTURE_DIR "/project.tokens.json"];
	OAK_ASSERT(data != nil);
	for(bool dark : { false, true })
	{
		NSDictionary* palette = theme_editor::read_dtcg(data, dark);
		OAK_ASSERT([palette[@"colors"] count] == 15);
		OAK_ASSERT([palette[@"syntaxColors"] count] == 5);
		NSDictionary* theme = theme_editor::theme_from_palette(palette, @"project", dark);
		OAK_ASSERT([theme[@"name"] isEqual:@"project"]);
		OAK_ASSERT([theme[@"settings"][0][@"settings"][@"background"] isEqual:dark ? @"#000000" : @"#FFFFFF"]);
		for(NSUInteger index = 0; index < 5; ++index)
		{
			NSDictionary* swatch = palette[@"syntaxColors"][index];
			OAK_ASSERT([swatch[@"name"] hasPrefix:dark ? @"Dark_tokens.json." : @"Light_tokens.json."]);
			OAK_ASSERT([theme[@"settings"][index + 1][@"settings"][@"foreground"] isEqual:swatch[@"color"]]);
		}
		OAK_ASSERT([theme[@"themeEditorPalette"] isEqual:palette[@"colors"]]);
	}
}

void test_dtcg_nested_types_aliases_pointers_and_alpha ()
{
	NSDictionary* document = @{
		@"base": @{ @"$type": @"color", @"red": @{ @"$value": @{ @"colorSpace": @"srgb", @"components": @[ @1, @0, @0 ], @"alpha": @0.5, @"hex": @"#000000" } } },
		@"rootGroup": @{ @"$type": @"color", @"$root": @{ @"$value": @"#ABCDEF" } },
		@"rootAlias": @{ @"$value": @"{rootGroup}" },
		@"alias": @{ @"$value": @"{base.red}" },
		@"pointer": @{ @"$type": @"color", @"$value": @{ @"$ref": @"#/base/red/$value" } },
		@"hex": @{ @"$type": @"color", @"$value": @"#1234" },
		@"linear": @{ @"$type": @"color", @"$value": @{ @"colorSpace": @"srgb-linear", @"components": @[ @0.5, @0.5, @0.5 ] } },
		@"p3": @{ @"$type": @"color", @"$value": @{ @"colorSpace": @"display-p3", @"components": @[ @0, @0, @0 ] } },
		@"size": @{ @"$type": @"number", @"$value": @16 },
		@"$extensions": @{ @"ignored": @{ @"$type": @"color", @"$value": @"#FFFFFF" } }
	};
	NSDictionary* palette = theme_editor::read_dtcg(TokenData(document));
	OAK_ASSERT([palette[@"colors"] count] == 8);
	NSMutableDictionary* byName = [NSMutableDictionary dictionary];
	for(NSDictionary* swatch in palette[@"colors"]) byName[swatch[@"name"]] = swatch[@"color"];
	OAK_ASSERT([byName[@"base.red"] isEqual:@"#FF000080"]);
	OAK_ASSERT([byName[@"alias"] isEqual:byName[@"base.red"]]);
	OAK_ASSERT([byName[@"pointer"] isEqual:byName[@"base.red"]]);
	OAK_ASSERT([byName[@"hex"] isEqual:@"#11223344"]);
	OAK_ASSERT([byName[@"linear"] isEqual:@"#BCBCBC"]);
	OAK_ASSERT([byName[@"p3"] isEqual:@"#000000"]);
	OAK_ASSERT([byName[@"rootAlias"] isEqual:@"#ABCDEF"]);
	NSDictionary* theme = theme_editor::theme_from_palette(palette, @"Alpha");
	OAK_ASSERT([theme[@"settings"][0][@"settings"][@"selection"] isEqual:@"#FF000060"]);
}

void test_dtcg_rejects_invalid_colors_and_references ()
{
	for(id document in @[
		@[], @{}, @{ @"size": @{ @"$type": @"number", @"$value": @16 } },
		@{ @"bad": @{ @"$type": @"color", @"$value": @"{missing}" } },
		@{ @"loop": @{ @"$type": @"color", @"$value": @"{loop}" } },
		@{ @"a": @{ @"$value": @"{b}" }, @"b": @{ @"$value": @"{a}" } },
		@{ @"bad": @{ @"$type": @"color", @"$value": @{ @"$ref": @"other.json#/color" } } },
		@{ @"bad": @{ @"$type": @"color", @"$value": @{ @"$ref": @"#/missing" } } },
		@{ @"bad": @{ @"$type": @"color", @"$value": @"blue" } },
		@{ @"bad": @{ @"$type": @"color", @"$value": @{ @"colorSpace": @"srgb", @"components": @[ @0, @0 ] } } },
		@{ @"bad": @{ @"$type": @"color", @"$value": @{ @"colorSpace": @"srgb", @"components": @[ @0, @YES, @0 ] } } },
		@{ @"bad": @{ @"$type": @"color", @"$value": @{ @"colorSpace": @"srgb", @"components": @[ @0, @0, @0 ], @"alpha": @2 } } }
	]) OAK_ASSERT(RejectsTokens(document));
	bool rejected = false;
	try { theme_editor::read_dtcg([NSData dataWithBytes:"{" length:1]); }
	catch(std::runtime_error const&) { rejected = true; }
	OAK_ASSERT(rejected);
}
