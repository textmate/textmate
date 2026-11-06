#import "../src/ase.h"

static NSData* AdobeColorFixture ()
{
	return [NSData dataWithContentsOfFile:@THEME_EDITOR_FIXTURE_DIR "/adobe-color.ase"];
}

static bool RejectsASE (NSData* data)
{
	try { theme_editor::read_ase(data); }
	catch(std::runtime_error const&) { return true; }
	return false;
}

void test_adobe_color_palette_and_generated_theme ()
{
	NSDictionary* palette = theme_editor::read_ase(AdobeColorFixture());
	OAK_ASSERT([palette[@"name"] isEqual:@"My Color Theme"]);
	NSArray* expected = @[ @"#33AEDE", @"#D46B2C", @"#03718B", @"#D4E1C8", @"#4DAE73" ];
	OAK_ASSERT([[palette[@"colors"] valueForKey:@"color"] isEqual:expected]);
	OAK_ASSERT([palette[@"colors"][0][@"name"] isEqual:@"Color 1"]);
	NSDictionary* theme = theme_editor::theme_from_palette(palette, @"Fallback");
	OAK_ASSERT([theme[@"name"] isEqual:@"My Color Theme"]);
	OAK_ASSERT([theme[@"themeEditorPalette"] isEqual:palette[@"colors"]]);
	OAK_ASSERT([theme[@"colorSpaceName"] isEqual:@"sRGB"]);
	OAK_ASSERT([theme[@"settings"] count] == 6);
	NSArray* scopes = @[ @"string", @"keyword, storage", @"constant", @"entity.name, support", @"comment" ];
	for(bool dark : { false, true })
	{
		NSDictionary* mapped = theme_editor::theme_from_palette(palette, @"Fallback", dark);
		NSDictionary* global = mapped[@"settings"][0][@"settings"];
		OAK_ASSERT([global[@"background"] isEqual:dark ? @"#000000" : @"#FFFFFF"]);
		OAK_ASSERT([global[@"foreground"] isEqual:dark ? @"#FFFFFF" : @"#000000"]);
		OAK_ASSERT([mapped[@"semanticClass"] isEqual:dark ? @"theme.dark.custom" : @"theme.light.custom"]);
		for(NSUInteger index = 0; index < expected.count; ++index)
		{
			NSDictionary* rule = mapped[@"settings"][index + 1];
			OAK_ASSERT([rule[@"scope"] isEqual:scopes[index]]);
			OAK_ASSERT([rule[@"settings"][@"foreground"] isEqual:expected[index]]);
		}
	}

}

void test_ase_rejects_all_truncations_and_bad_lengths ()
{
	NSData* fixture = AdobeColorFixture();
	OAK_ASSERT(fixture != nil);
	for(NSUInteger size = 0; size < fixture.length; ++size)
		OAK_ASSERT(RejectsASE([fixture subdataWithRange:NSMakeRange(0, size)]));
	for(NSUInteger offset : { 0UL, 5UL, 11UL, 14UL, 17UL, 49UL, 63UL, 64UL, 77UL, 195UL })
	{
		NSMutableData* bad = [fixture mutableCopy];
		static_cast<uint8_t*>(bad.mutableBytes)[offset] = 0xFF;
		OAK_ASSERT(RejectsASE(bad));
	}
	NSMutableData* trailing = [fixture mutableCopy];
	[trailing appendData:[NSData dataWithBytes:"x" length:1]];
	OAK_ASSERT(RejectsASE(trailing));
}

// Build independent binary fixtures for the less common color models.
static NSData* ASEColor (char const* model, std::initializer_list<float> components)
{
	NSMutableData* data = [NSMutableData data];
	auto integer = [&](uint32_t value, size_t count) {
		for(size_t i = count; i > 0; --i) { uint8_t byte = value >> ((i - 1) * 8); [data appendBytes:&byte length:1]; }
	};
	integer(0x41534546, 4); integer(1, 2); integer(0, 2); integer(1, 4);
	integer(1, 2); integer(10 + components.size() * 4, 4);
	integer(1, 2); integer(0, 2); // Empty UTF-16 name.
	[data appendBytes:model length:4];
	for(float component : components) { uint32_t bits; memcpy(&bits, &component, 4); integer(bits, 4); }
	integer(2, 2);
	return data;
}

void test_ase_color_models_and_component_validation ()
{
	for(NSData* data in @[ ASEColor("RGB ", {1, 0, 0}), ASEColor("CMYK", {0, 1, 1, 0}), ASEColor("LAB ", {0.5, 0, 0}), ASEColor("Gray", {0.5}) ])
	{
		NSDictionary* palette = theme_editor::read_ase(data);
		OAK_ASSERT([palette[@"colors"] count] == 1);
		OAK_ASSERT(theme_editor::parse_color(palette[@"colors"][0][@"color"], NSColorSpace.sRGBColorSpace));
		OAK_ASSERT([theme_editor::theme_from_palette(palette, @"Filename")[@"name"] isEqual:@"Filename"]);
	}
	OAK_ASSERT([theme_editor::read_ase(ASEColor("Gray", {0.5}))[@"colors"][0][@"color"] isEqual:@"#808080"]);
	OAK_ASSERT([theme_editor::read_ase(ASEColor("RGB ", {1, 0, 0}))[@"colors"][0][@"color"] isEqual:@"#FF0000"]);
	for(NSData* data in @[ ASEColor("RGB ", {NAN, 0, 0}), ASEColor("RGB ", {INFINITY, 0, 0}), ASEColor("RGB ", {-1, 0, 0}), ASEColor("CMYK", {0, 0, 0, 2}), ASEColor("LAB ", {0.5, 200, 0}), ASEColor("????", {0}) ])
		OAK_ASSERT(RejectsASE(data));
	uint8_t empty[] = { 'A', 'S', 'E', 'F', 0, 1, 0, 0, 0, 0, 0, 0 };
	OAK_ASSERT(RejectsASE([NSData dataWithBytes:empty length:sizeof(empty)]));
}
