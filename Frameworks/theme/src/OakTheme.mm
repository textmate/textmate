#import "OakTheme.h"
#import "theme.h" // kSystemUIThemeUUID
#import <ns/src/ns.h>

typedef NS_OPTIONS(NSUInteger, OakThemeStyleOptions) {
	OakThemeStyleBold          = 0x0001,
	OakThemeStyleItalic        = 0x0002,
	OakThemeStyleUnderline     = 0x0004,
	OakThemeStyleStrikethrough = 0x0008,
	OakThemeStyleMisspelled    = 0x0010,
};

@interface OakThemeRawStyle : NSObject
@property (nonatomic, readonly) NSString* name;
@property (nonatomic, readonly) scope::selector_t scopeSelector;
@property (nonatomic, readonly) NSString* fontName;
@property (nonatomic, readonly) CGFloat fontSize; // Negative size means it is relative to font size of parent style
@property (nonatomic, readonly) NSColorSpace* colorSpace;
@property (nonatomic, readonly) NSString* foregroundColorString;
@property (nonatomic, readonly) NSString* backgroundColorString;
@property (nonatomic, readonly) NSString* caretColorString;
@property (nonatomic, readonly) NSString* selectionColorString;
@property (nonatomic, readonly) NSString* invisiblesColorString;
@property (nonatomic, readonly) OakThemeStyleOptions options;
@property (nonatomic, readonly) OakThemeStyleOptions optionsMask;
@end

static double my_strtod (char const* str, char const** last) // The problem with strtod() is that it uses LC_NUMERIC for point separator.
{
	double res = atof(str);
	if(last)
	{
		while(*str && (isdigit(*str) || *str == '.'))
			++str;
		*last = str;
	}
	return res;
}

static CGFloat ParseFontSize (NSString* fontSizeString)
{
	if(fontSizeString)
	{
		char const* first = fontSizeString.UTF8String;
		char const* last;
		double size = my_strtod(first, &last);
		if(first != last)
		{
			while(*last == ' ')
				++last;

			if(strcmp(last, "pt") == 0 || *last == '\0')
				return size;
			else if(strcmp(last, "em") == 0)
				return -size;
			else if(strcmp(last, "%") == 0)
				return -size / 100;
			else
				NSLog(@"*** unsupported font size unit: %s (%@)", last, fontSizeString);
		}
		else
		{
			NSLog(@"*** unsupported font size format: %@", fontSizeString);
		}
	}
	return 0;
}

@implementation OakThemeRawStyle
- (instancetype)initWithDefaultStyles
{
	if(self = [super init])
	{
		_fontName = [NSFont userFixedPitchFontOfSize:0].fontName;
		_fontSize = [NSFont userFixedPitchFontOfSize:0].pointSize;

		_foregroundColorString = @"textColor";
		_backgroundColorString = @"textBackgroundColor";
		_caretColorString      = @"textColor";
		_selectionColorString  = @"selectedTextBackgroundColor";
		_invisiblesColorString = @"quaternaryLabelColor";
	}
	return self;
}

- (instancetype)initWithGlobalStylesForScope:(scope::scope_t const&)scope
{
	if(self = [super init])
	{
		_fontName = [self stringForSetting:@"fontName" inScope:scope];
		_fontSize = ParseFontSize([self stringForSetting:@"fontSize" inScope:scope]);

		_foregroundColorString = [self stringForSetting:@"foreground" inScope:scope];
		_backgroundColorString = [self stringForSetting:@"background" inScope:scope];
		_caretColorString      = [self stringForSetting:@"caret" inScope:scope];
		_selectionColorString  = [self stringForSetting:@"selection" inScope:scope];
		_invisiblesColorString = [self stringForSetting:@"invisibles" inScope:scope];

		static struct { NSString* key; OakThemeStyleOptions flag; } const options[] = {
			{ @"bold",          OakThemeStyleBold          },
			{ @"italic",        OakThemeStyleItalic        },
			{ @"underline",     OakThemeStyleUnderline     },
			{ @"strikethrough", OakThemeStyleStrikethrough },
			{ @"misspelled",    OakThemeStyleMisspelled    },
		};

		for(auto option : options)
		{
			bundles::item_ptr item;
			plist::any_t const value = bundles::value_for_setting(to_s(option.key), scope, &item);
			if(item)
			{
				_options     |= plist::is_true(value) ? option.flag : 0;
				_optionsMask |= option.flag;
			}
		}
	}
	return self;
}

- (instancetype)initWithPropertyList:(NSDictionary*)plist colorSpace:(NSColorSpace*)colorSpace
{
	if(self = [super init])
	{
		_colorSpace = colorSpace;

		_name = plist[@"name"];
		if(NSString* scopeSelector = plist[@"scope"])
			_scopeSelector = to_s(scopeSelector);

		if(NSDictionary* settings = plist[@"settings"])
		{
			_fontName = settings[@"fontName"];
			_fontSize = ParseFontSize(settings[@"fontSize"]);

			_foregroundColorString = settings[@"foreground"];
			_backgroundColorString = settings[@"background"];
			_caretColorString      = settings[@"caret"];
			_selectionColorString  = settings[@"selection"];
			_invisiblesColorString = settings[@"invisibles"];

			if(NSNumber* misspelled = settings[@"misspelled"])
			{
				_options     |= misspelled.boolValue ? OakThemeStyleMisspelled : 0;
				_optionsMask |= OakThemeStyleMisspelled;
			}

			if(NSString* fontStyle = settings[@"fontStyle"])
			{
				if([fontStyle containsString:@"plain"])
					_optionsMask |= (OakThemeStyleBold|OakThemeStyleItalic|OakThemeStyleUnderline|OakThemeStyleStrikethrough);

				static struct { NSString* keyword; OakThemeStyleOptions flag; } const options[] = {
					{ @"bold",          OakThemeStyleBold          },
					{ @"italic",        OakThemeStyleItalic        },
					{ @"underline",     OakThemeStyleUnderline     },
					{ @"strikethrough", OakThemeStyleStrikethrough },
				};

				for(auto option : options)
				{
					if([fontStyle containsString:option.keyword])
					{
						_options     |= option.flag;
						_optionsMask |= option.flag;
					}
				}
			}
		}
	}
	return self;
}

- (NSString*)stringForSetting:(NSString*)key inScope:(scope::scope_t const&)scope
{
	bundles::item_ptr item;
	plist::any_t const value = bundles::value_for_setting(to_s(key), scope, &item);
	return item ? to_ns(plist::get<std::string>(value)) : nil;
}

- (NSString*)description
{
	NSMutableDictionary* info = [NSMutableDictionary dictionary];

	info[@"name"]            = _name;
	info[@"scopeSelector"]   = to_ns(to_s(_scopeSelector));
	info[@"fontName"]        = _fontName;
	info[@"fontSize"]        = _fontSize != 0 ? @(_fontSize) : nil;
	info[@"foregroundColor"] = _foregroundColorString;
	info[@"backgroundColor"] = _backgroundColorString;
	info[@"caretColor"]      = _caretColorString;
	info[@"selectionColor"]  = _selectionColorString;
	info[@"invisiblesColor"] = _invisiblesColorString;
	info[@"options"]         = [NSString stringWithFormat:@"%02lx", _options];
	info[@"optionsMask"]     = [NSString stringWithFormat:@"%02lx", _optionsMask];

	return [NSString stringWithFormat:@"<%@: %@>", self.class, info];
}
@end

// ==================
// = OakThemeStyles =
// ==================

@interface OakThemeStyles ()
{
	NSColor* _foregroundColor;
	NSColor* _backgroundColor;
	NSColor* _caretColor;
	NSColor* _selectionColor;
	NSColor* _invisiblesColor;
}
@property (nonatomic, readonly) NSArray<OakThemeRawStyle*>* rawStyles;
@end

@implementation OakThemeStyles
- (instancetype)initWithRawStyles:(NSArray<OakThemeRawStyle*>*)styles
{
	if(self = [super init])
	{
		_rawStyles = styles;

		NSString* fontName = nil;
		CGFloat fontSize   = 0;
		BOOL boldEnabled   = NO;
		BOOL italicEnabled = NO;

		for(OakThemeRawStyle* style in _rawStyles)
		{
			fontName = style.fontName ?: fontName;
			fontSize = style.fontSize < 0 ? (-style.fontSize * fontSize) : (style.fontSize > 0 ? style.fontSize : fontSize);

			if(style.optionsMask & OakThemeStyleBold)
				boldEnabled = (style.options & OakThemeStyleBold) ? YES : NO;
			if(style.optionsMask & OakThemeStyleItalic)
				italicEnabled = (style.options & OakThemeStyleItalic) ? YES : NO;

			if(style.optionsMask & OakThemeStyleUnderline)
				_underlined = (style.options & OakThemeStyleUnderline) ? YES : NO;
			if(style.optionsMask & OakThemeStyleStrikethrough)
				_strikethrough = (style.options & OakThemeStyleStrikethrough) ? YES : NO;
			if(style.optionsMask & OakThemeStyleMisspelled)
				_misspelled = (style.options & OakThemeStyleMisspelled) ? YES : NO;
		}

		_font       = [NSFont fontWithName:fontName size:fontSize];
		_fontTraits = (boldEnabled ? NSBoldFontMask : 0) | (italicEnabled ? NSItalicFontMask : 0);

		if(_fontTraits)
			_font = [NSFontManager.sharedFontManager convertFont:_font toHaveTrait:_fontTraits];
	}
	return self;
}

- (NSString*)description
{
	NSMutableDictionary* info = [NSMutableDictionary dictionary];
	info[@"foregroundColor"] = self.foregroundColor;
	info[@"backgroundColor"] = self.backgroundColor;
	info[@"caretColor"]      = self.caretColor;
	info[@"selectionColor"]  = self.selectionColor;
	info[@"invisiblesColor"] = self.invisiblesColor;
	info[@"font"]            = self.font;
	info[@"underlined"]      = self.underlined    ? @"YES" : nil;
	info[@"strikethrough"]   = self.strikethrough ? @"YES" : nil;
	info[@"misspelled"]      = self.misspelled    ? @"YES" : nil;
	return [NSString stringWithFormat:@"<%@: %@>", self.class, info];
}

- (NSColor*)makeColorWithBlending:(BOOL)blendingEnabled block:(NSString*(^)(OakThemeRawStyle*))getColor
{
	NSColor* res;
	for(OakThemeRawStyle* style in _rawStyles)
	{
		if(NSString* colorString = getColor(style))
		{
			if([colorString hasPrefix:@"#"])
			{
				unsigned int red = 0, green = 0, blue = 0, alpha = 255;
				if(sscanf(colorString.UTF8String, "#%02x%02x%02x%02x", &red, &green, &blue, &alpha) >= 3)
				{
					if(blendingEnabled && alpha < 255 && res)
					{
						CGFloat components[] = { red/255.0, green/255.0, blue/255.0, 1.0 };
						NSColor* color = [NSColor colorWithColorSpace:style.colorSpace components:components count:sizeofA(components)];
						res = [res blendedColorWithFraction:alpha/255.0 ofColor:color];
					}
					else
					{
						CGFloat components[] = { red/255.0, green/255.0, blue/255.0, alpha/255.0 };
						res = [NSColor colorWithColorSpace:style.colorSpace components:components count:sizeofA(components)];
					}
				}
			}
			else if([NSColor respondsToSelector:NSSelectorFromString(colorString)])
			{
				res = [NSColor performSelector:NSSelectorFromString(colorString)];
			}
		}
	}
	return res;
}

- (NSColor*)foregroundColor
{
	if(!_foregroundColor)
		_foregroundColor = [self makeColorWithBlending:NO block:^(OakThemeRawStyle* style){ return style.foregroundColorString; }];
	return _foregroundColor;
}

- (NSColor*)backgroundColor
{
	if(!_backgroundColor)
		_backgroundColor = [self makeColorWithBlending:YES block:^(OakThemeRawStyle* style){ return style.backgroundColorString; }];
	return _backgroundColor;
}

- (NSColor*)caretColor
{
	if(!_caretColor)
		_caretColor = [self makeColorWithBlending:NO block:^(OakThemeRawStyle* style){ return style.caretColorString; }];
	return _caretColor;
}

- (NSColor*)selectionColor
{
	if(!_selectionColor)
		_selectionColor = [self makeColorWithBlending:NO block:^(OakThemeRawStyle* style){ return style.selectionColorString; }];
	return _selectionColor;
}

- (NSColor*)invisiblesColor
{
	if(!_invisiblesColor)
		_invisiblesColor = [self makeColorWithBlending:NO block:^(OakThemeRawStyle* style){ return style.invisiblesColorString; }];
	return _invisiblesColor;
}
@end

// ============
// = OakTheme =
// ============

@interface OakTheme ()
@property (nonatomic) NSArray<OakThemeRawStyle*>* globalStyles;
@property (nonatomic) NSArray<OakThemeRawStyle*>* themeStyles;
@end

@implementation OakTheme
+ (instancetype)theme
{
	if(bundles::item_ptr themeItem = bundles::lookup(kSystemUIThemeUUID))
		return [[OakTheme alloc] initWithBundleItem:themeItem];
	return [[OakTheme alloc] initWithPropertyList:@{ }];
}

- (instancetype)initWithPropertyList:(NSDictionary*)plist
{
	if(self = [super init])
	{
		NSColorSpace* colorSpace = [plist[@"colorSpaceName"] isEqualToString:@"sRGB"] ? NSColorSpace.sRGBColorSpace : NSColorSpace.genericRGBColorSpace;

		NSMutableArray* styles = [NSMutableArray array];
		for(NSDictionary* item in [plist objectForKey:@"settings"])
		{
			if(OakThemeRawStyle* style = [[OakThemeRawStyle alloc] initWithPropertyList:item colorSpace:colorSpace])
				[styles addObject:style];
		}
		_themeStyles = [styles copy];
	}
	return self;
}

- (instancetype)initWithBundleItem:(bundles::item_ptr const&)themeItem
{
	if(self = [self initWithPropertyList:themeItem ? ns::to_dictionary(themeItem->plist()) : nil])
		_identifier = [[NSUUID alloc] initWithUUIDString:to_ns(themeItem->uuid())];
	return self;
}

- (NSFont*)font
{
	return [self stylesForScope:scope::scope_t()].font;
}

- (NSColor*)foregroundColor
{
	return [self stylesForScope:scope::scope_t()].foregroundColor;
}

- (NSColor*)backgroundColor
{
	return [self stylesForScope:scope::scope_t()].backgroundColor;
}

- (OakThemeStyles*)stylesForScope:(scope::scope_t const&)scope
{
	NSMutableArray<OakThemeRawStyle*>* styles = [NSMutableArray array];
	[styles addObject:[[OakThemeRawStyle alloc] initWithDefaultStyles]];
	[styles addObject:[[OakThemeRawStyle alloc] initWithGlobalStylesForScope:scope]];

	std::multimap<double, OakThemeRawStyle*> ordering;
	for(OakThemeRawStyle* style in _globalStyles)
	{
		if(auto rank = style.scopeSelector.does_match(scope))
			ordering.emplace(*rank, style);
	}

	for(OakThemeRawStyle* style in _themeStyles)
	{
		if(auto rank = style.scopeSelector.does_match(scope))
			ordering.emplace(*rank, style);
	}

	for(auto pair : ordering)
		[styles addObject:pair.second];

	return [[OakThemeStyles alloc] initWithRawStyles:styles];
}
@end
