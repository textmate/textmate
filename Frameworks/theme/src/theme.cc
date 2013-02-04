#include "theme.h"
#include <cf/cf.h>

static theme_t::color_info_t read_color (std::string const& str_color);
static cf::color_t soften (cf::color_t color, CGFloat factor = 0.5);
static CGFloat read_font_size (std::string const& str_font_size);

static void get_key_path (plist::dictionary_t const& plist, std::string const& setting, theme_t::color_info_t& color)
{
	std::string temp_str;
	plist::get_key_path(plist, setting, temp_str);
	color = read_color(temp_str);
}

static void get_key_path (plist::dictionary_t const& plist, std::string const& setting, CGFloat& font_size)
{
	std::string temp_str = NULL_STR;
	plist::get_key_path(plist, setting, temp_str);
	font_size = read_font_size(temp_str);
}

static void get_key_path (plist::dictionary_t const& plist, std::string const& setting, cf::color_t& color)
{
	std::string temp_str;
	if(plist::get_key_path(plist, setting, temp_str))
		color = read_color(temp_str);
}

theme_t::decomposed_style_t theme_t::shared_styles_t::parse_styles (plist::dictionary_t const& plist)
{
	decomposed_style_t res;

	std::string scopeSelector;
	if(plist::get_key_path(plist, "scope", scopeSelector))
		res.scope_selector = scopeSelector;

	plist::get_key_path(plist, "settings.fontName", res.font_name);
	get_key_path(plist, "settings.fontSize",        res.font_size);
	get_key_path(plist, "settings.foreground",      res.foreground);
	get_key_path(plist, "settings.background",      res.background);
	get_key_path(plist, "settings.caret",           res.caret);
	get_key_path(plist, "settings.selection",       res.selection);
	get_key_path(plist, "settings.invisibles",      res.invisibles);

	bool flag;
	res.misspelled = plist::get_key_path(plist, "settings.misspelled", flag) ? (flag ? bool_true : bool_false) : bool_unset;

	std::string fontStyle;
	if(plist::get_key_path(plist, "settings.fontStyle", fontStyle))
	{
		if(fontStyle.find("plain") != std::string::npos)
		{
			res.bold       = bool_false;
			res.italic     = bool_false;
			res.underlined = bool_false;
		}
		else
		{
			res.bold       = fontStyle.find("bold")      != std::string::npos ? bool_true : bool_unset;
			res.italic     = fontStyle.find("italic")    != std::string::npos ? bool_true : bool_unset;
			res.underlined = fontStyle.find("underline") != std::string::npos ? bool_true : bool_unset;
		}
	}
	return res;
}

std::vector<theme_t::decomposed_style_t> theme_t::global_styles (scope::scope_t const& scope)
{
	static struct { std::string name; theme_t::color_info_t decomposed_style_t::*field; } const colorKeys[] =
	{
		{ "foreground", &decomposed_style_t::foreground },
		{ "background", &decomposed_style_t::background },
		{ "caret",      &decomposed_style_t::caret      },
		{ "selection",  &decomposed_style_t::selection  },
		{ "invisibles", &decomposed_style_t::invisibles },
	};

	static struct { std::string name; bool_t decomposed_style_t::*field; } const booleanKeys[] =
	{
		{ "misspelled", &decomposed_style_t::misspelled },
		{ "bold",       &decomposed_style_t::bold       },
		{ "italic",     &decomposed_style_t::italic     },
		{ "underline",  &decomposed_style_t::underlined },
	};

	std::vector<decomposed_style_t> res;

	for(size_t i = 0; i < sizeofA(colorKeys); ++i)
	{
		bundles::item_ptr item;
		plist::any_t const& value = bundles::value_for_setting(colorKeys[i].name, scope, &item);
		if(item)
		{
			res.push_back(decomposed_style_t(item->scope_selector()));
			res.back().*(colorKeys[i].field) = read_color(plist::get<std::string>(value));
		}
	}

	for(size_t i = 0; i < sizeofA(booleanKeys); ++i)
	{
		bundles::item_ptr item;
		plist::any_t const& value = bundles::value_for_setting(booleanKeys[i].name, scope, &item);
		if(item)
		{
			res.push_back(decomposed_style_t(item->scope_selector()));
			res.back().*(booleanKeys[i].field) = plist::is_true(value) ? bool_true : bool_false;
		}
	}

	bundles::item_ptr fontNameItem;
	plist::any_t const& fontNameValue = bundles::value_for_setting("fontName", scope, &fontNameItem);
	if(fontNameItem)
	{
		res.push_back(decomposed_style_t(fontNameItem->scope_selector()));
		res.back().font_name = plist::get<std::string>(fontNameValue);
	}

	bundles::item_ptr fontSizeItem;
	plist::any_t const& fontSizeValue = bundles::value_for_setting("fontSize", scope, &fontSizeItem);
	if(fontSizeItem)
	{
		res.push_back(decomposed_style_t(fontSizeItem->scope_selector()));
		res.back().font_size = read_font_size(plist::get<std::string>(fontSizeValue));
	}

	return res;
}

// ===========
// = theme_t =
// ===========

theme_ptr theme_t::copy_with_font_name_and_size (std::string const& fontName, CGFloat fontSize)
{
	if(_font_name == fontName && _font_size == fontSize)
		return theme_ptr(new theme_t::theme_t(*this));
	return theme_ptr(new theme_t::theme_t(this->_item, fontName, fontSize));
}

theme_t::theme_t (bundles::item_ptr const& themeItem, std::string const& fontName, CGFloat fontSize) :_item(themeItem), _font_name(fontName), _font_size(fontSize)
{
	_styles = find_shared_styles(themeItem);
}

static cf::color_t soften (cf::color_t color, CGFloat factor)
{
	CGFloat r = color.red(), g = color.green(), b = color.blue(), a = color.alpha();
	
	if(color_is_dark(color))
	{
		r = 1 - factor*(1 - r);
		g = 1 - factor*(1 - g);
		b = 1 - factor*(1 - b);
	}
	else
	{
		r *= factor;
		g *= factor;
		b *= factor;
	}
	
	return cf::color_t(r, g, b, a);
}

theme_t::shared_styles_t::shared_styles_t (bundles::item_ptr const& themeItem): _item(themeItem), _callback(*this)
{
	setup_styles();
	bundles::add_callback(&_callback);
}

theme_t::shared_styles_t::~shared_styles_t ()
{
	bundles::remove_callback(&_callback);
}
void theme_t::shared_styles_t::setup_styles ()
{
	_styles.clear();

	if(_item)
	{
		if(bundles::item_ptr newItem = bundles::lookup(_item->uuid()))
			_item = newItem;

		plist::array_t items;
		if(plist::get_key_path(_item->plist(), "settings", items))
		{
			iterate(it, items)
			{
				if(plist::dictionary_t const* styles = boost::get<plist::dictionary_t>(&*it))
				{
					_styles.push_back(parse_styles(*styles));
					if(!_styles.back().invisibles.is_blank())
					{
						decomposed_style_t invisibleStyle("deco.invisible");
						invisibleStyle.foreground = _styles.back().invisibles;
						_styles.push_back(invisibleStyle);
					}
				}
			}
		}
	}

	// =======================================
	// = Find “global” foreground/background =
	// =======================================

	// We assume that the first style is the unscoped root style
	_foreground     = _styles.empty() ? cf::color_t("#FFFFFF") : _styles[0].foreground;
	_background     = _styles.empty() ? cf::color_t("#000000") : _styles[0].background;
	_is_dark        = color_is_dark(_background);
	_is_transparent = _background.alpha() < 1;

	// =========================
	// = Default Gutter Styles =
	// =========================

	_gutter_styles.divider             = soften(_foreground, 0.4);
	_gutter_styles.foreground          = soften(_foreground, 0.5);
	_gutter_styles.background          = soften(_background, 0.87);
	_gutter_styles.selectionForeground = soften(_foreground, 0.95);
	_gutter_styles.selectionBackground = soften(_background, 0.95);

	plist::dictionary_t gutterSettings;
	if(_item && plist::get_key_path(_item->plist(), "gutterSettings", gutterSettings))
	{
		static struct { std::string const key; cf::color_t gutter_styles_t::*field; } const gutterKeys[] =
		{
			{ "divider",               &gutter_styles_t::divider               },
			{ "selectionBorder",       &gutter_styles_t::selectionBorder       },
			{ "foreground",            &gutter_styles_t::foreground            },
			{ "background",            &gutter_styles_t::background            },
			{ "icons",                 &gutter_styles_t::icons                 },
			{ "iconsHover",            &gutter_styles_t::iconsHover            },
			{ "iconsPressed",          &gutter_styles_t::iconsPressed          },
			{ "selectionForeground",   &gutter_styles_t::selectionForeground   },
			{ "selectionBackground",   &gutter_styles_t::selectionBackground   },
			{ "selectionIcons",        &gutter_styles_t::selectionIcons        },
			{ "selectionIconsHover",   &gutter_styles_t::selectionIconsHover   },
			{ "selectionIconsPressed", &gutter_styles_t::selectionIconsPressed },
		};

		iterate(gutterKey, gutterKeys)
			get_key_path(gutterSettings, gutterKey->key, _gutter_styles.*(gutterKey->field));
	}

	_gutter_styles.selectionBorder       = _gutter_styles.selectionBorder       ? _gutter_styles.selectionBorder       : _gutter_styles.divider;
	_gutter_styles.icons                 = _gutter_styles.icons                 ? _gutter_styles.icons                 : _gutter_styles.foreground;
	_gutter_styles.iconsHover            = _gutter_styles.iconsHover            ? _gutter_styles.iconsHover            : _gutter_styles.icons;
	_gutter_styles.iconsPressed          = _gutter_styles.iconsPressed          ? _gutter_styles.iconsPressed          : _gutter_styles.icons;
	_gutter_styles.selectionIcons        = _gutter_styles.selectionIcons        ? _gutter_styles.selectionIcons        : _gutter_styles.selectionForeground;
	_gutter_styles.selectionIconsHover   = _gutter_styles.selectionIconsHover   ? _gutter_styles.selectionIconsHover   : _gutter_styles.selectionIcons;
	_gutter_styles.selectionIconsPressed = _gutter_styles.selectionIconsPressed ? _gutter_styles.selectionIconsPressed : _gutter_styles.selectionIcons;
}

oak::uuid_t const& theme_t::uuid () const
{
	static oak::uuid_t const FallbackThemeUUID = oak::uuid_t().generate();
	return _item ? _item->uuid() : FallbackThemeUUID;
}

std::string const& theme_t::font_name () const
{
	return _font_name;
}

CGFloat theme_t::font_size () const
{
	return _font_size;
}

CGColorRef theme_t::foreground () const
{
	return _styles->_foreground;
}

CGColorRef theme_t::background (std::string const& fileType) const
{
	if(fileType != NULL_STR)
		return styles_for_scope(fileType).background();
	return _styles->_background;
}

bool theme_t::is_dark () const
{
	return _styles->_is_dark;
}

bool theme_t::is_transparent () const
{
	return _styles->_is_transparent;
}

gutter_styles_t const& theme_t::gutter_styles () const
{
	return _styles->_gutter_styles;
}

styles_t const& theme_t::styles_for_scope (scope::scope_t const& scope) const
{
	ASSERT(scope);

	std::map<scope::scope_t, styles_t>::iterator styles = _cache.find(scope);
	if(styles == _cache.end())
	{
		std::multimap<double, decomposed_style_t> ordering;
		citerate(it, global_styles(scope))
		{
			double rank = 0;
			if(it->scope_selector.does_match(scope, &rank))
				ordering.insert(std::make_pair(rank, *it));
		}

		iterate(it, _styles->_styles)
		{
			double rank = 0;
			if(it->scope_selector.does_match(scope, &rank))
				ordering.insert(std::make_pair(rank, *it));
		}

		decomposed_style_t base(scope::selector_t(), _font_name, _font_size);
		iterate(it, ordering)
			base += it->second;

		CTFontPtr font(CTFontCreateWithName(cf::wrap(base.font_name), round(base.font_size), NULL), CFRelease);
		if(CTFontSymbolicTraits traits = (base.bold == bool_true ? kCTFontBoldTrait : 0) + (base.italic == bool_true ? kCTFontItalicTrait : 0))
		{
			if(CTFontRef newFont = CTFontCreateCopyWithSymbolicTraits(font.get(), round(base.font_size), NULL, traits, kCTFontBoldTrait | kCTFontItalicTrait))
				font.reset(newFont, CFRelease);
		}

		cf::color_t foreground = base.foreground.is_blank()                  ? cf::color_t("#000000")   : base.foreground;
		cf::color_t background = base.background.is_blank()                  ? cf::color_t("#FFFFFF")   : base.background;
		cf::color_t caret      = base.caret.is_blank()                       ? cf::color_t("#000000")   : base.caret;
		cf::color_t selection  = base.selection.is_blank()                   ? cf::color_t("#4D97FF54") : base.selection;

		styles_t res(foreground, background, caret, selection, font, base.underlined == bool_true, base.misspelled == bool_true);
		styles = _cache.insert(std::make_pair(scope, res)).first;
	}
	return styles->second;
}

static theme_t::color_info_t read_color (std::string const& str_color ) 
{
	enum { R, G, B, A };
	unsigned int col[4] = { 0x00, 0x00, 0x00, 0xFF } ;
	
	int res = sscanf(str_color.c_str(), "#%02x%02x%02x%02x", &col[R], &col[G], &col[B], &col[A]);
	if(res < 3) // R G B was not parsed, or color is 100% transparent
		return theme_t::color_info_t::color_info_t(); // color is not set

	return theme_t::color_info_t::color_info_t(col[R]/255.0, col[G]/255.0, col[B]/255.0, col[A]/255.0);
}

static theme_t::color_info_t blend (theme_t::color_info_t const& lhs, theme_t::color_info_t const& rhs)
{
	double a = rhs.alpha, ia = 1.0 - rhs.alpha;
	return lhs.is_blank() ? rhs : theme_t::color_info_t(ia * lhs.red + a * rhs.red, ia * lhs.green + a * rhs.green, ia * lhs.blue + a * rhs.blue, lhs.alpha);
}

static double my_strtod (char const* str, char const** last) // problem with strtod() is that it uses LC_NUMERIC for point separator.
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

static CGFloat read_font_size (std::string const& str_font_size)
{
	// Treat positive values as absolute font
	// and negative as relative, that way we don't have to use a bool as a flag :)
	if(str_font_size != NULL_STR)
	{
		char const* first = str_font_size.c_str();
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
				fprintf(stderr, "*** unsupported font size unit: %s (%s)\n", last, str_font_size.c_str());
		}
		else
		{
			fprintf(stderr, "*** unsupported font size format: %s\n", str_font_size.c_str());
		}
	}
	return -1;
}

theme_t::decomposed_style_t& theme_t::decomposed_style_t::operator+= (theme_t::decomposed_style_t const& rhs)
{
	font_name  = rhs.font_name != NULL_STR    ? rhs.font_name : font_name;
	font_size  = rhs.font_size > 0            ? rhs.font_size : font_size * fabs(rhs.font_size);

	foreground = rhs.foreground.is_blank()    ? foreground : rhs.foreground;
	background = rhs.background.is_blank()    ? background : blend(background, rhs.background);
	caret      = rhs.caret.is_blank()         ? caret      : rhs.caret;
	selection  = rhs.selection.is_blank()     ? selection  : rhs.selection;
	invisibles = rhs.invisibles.is_blank()    ? invisibles : rhs.invisibles;

	bold       = rhs.bold       == bool_unset ? bold       : rhs.bold;
	italic     = rhs.italic     == bool_unset ? italic     : rhs.italic;
	underlined = rhs.underlined == bool_unset ? underlined : rhs.underlined;
	misspelled = rhs.misspelled == bool_unset ? misspelled : rhs.misspelled;

	return *this;
}

theme_t::shared_styles_ptr theme_t::find_shared_styles (bundles::item_ptr const& themeItem)
{
	static oak::uuid_t const kEmptyThemeUUID = oak::uuid_t().generate();
	static std::map<oak::uuid_t, theme_t::shared_styles_ptr> Cache;

	oak::uuid_t const& uuid = themeItem ? themeItem->uuid() : kEmptyThemeUUID;
	auto theme = Cache.find(uuid);
	if(theme == Cache.end())
		theme = Cache.insert(std::make_pair(uuid, shared_styles_ptr(new shared_styles_t(themeItem)))).first;
	return theme->second;
}

// ==============
// = Public API =
// ==============

theme_ptr parse_theme (bundles::item_ptr const& themeItem)
{
	static oak::uuid_t const kEmptyThemeUUID = oak::uuid_t().generate();
	static std::map<oak::uuid_t, theme_ptr> Cache;

	oak::uuid_t const& uuid = themeItem ? themeItem->uuid() : kEmptyThemeUUID;
	auto theme = Cache.find(uuid);
	if(theme == Cache.end())
		theme = Cache.insert(std::make_pair(uuid, theme_ptr(new theme_t(themeItem)))).first;
	return theme->second;
}
