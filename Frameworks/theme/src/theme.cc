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

theme_t::decomposed_style_t theme_t::parse_styles (plist::dictionary_t const& plist)
{
	decomposed_style_t res;

	std::string scopeSelector;
	if(plist::get_key_path(plist, "scope", scopeSelector))
		res.scope_selector = scopeSelector;

	plist::get_key_path(plist, "settings.fontName",           res.font_name);
	get_key_path(plist, "settings.fontSize",                  res.font_size);
	get_key_path(plist, "settings.foreground",                res.foreground);
	get_key_path(plist, "settings.background",                res.background);
	get_key_path(plist, "settings.gutterForeground",          res.gutterForeground);
	get_key_path(plist, "settings.gutterBackground",          res.gutterBackground);
	get_key_path(plist, "settings.gutterDivider",             res.gutterDivider);
	get_key_path(plist, "settings.gutterSelectionForeground", res.gutterSelectionForeground);
	get_key_path(plist, "settings.gutterSelectionBackground", res.gutterSelectionBackground);
	get_key_path(plist, "settings.gutterSelectionBorder",     res.gutterSelectionBorder);
	get_key_path(plist, "settings.gutterIcons",               res.gutterIcons);
	get_key_path(plist, "settings.caret",                     res.caret);
	get_key_path(plist, "settings.selection",                 res.selection);
	get_key_path(plist, "settings.invisibles",                res.invisibles);

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

std::vector<theme_t::decomposed_style_t> theme_t::global_styles (scope::context_t const& scope)
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

theme_t::theme_t (bundles::item_ptr const& themeItem) : _item(themeItem), _callback(*this)
{
	setup_styles();
	bundles::add_callback(&_callback);
}

theme_t::~theme_t ()
{
	bundles::remove_callback(&_callback);
}

void theme_t::setup_styles ()
{
	_styles.clear();
	_cache.clear();

	if(!_item)
		return;

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
					decomposed_style_t invisbleStyle("deco.invisible");
					invisbleStyle.foreground = _styles.back().invisibles;
					_styles.push_back(invisbleStyle);
				}
			}
		}
	}
}

oak::uuid_t const& theme_t::uuid () const
{
	static oak::uuid_t const FallbackThemeUUID = oak::uuid_t().generate();
	return _item ? _item->uuid() : FallbackThemeUUID;
}

styles_t const& theme_t::styles_for_scope (scope::context_t const& scope, std::string fontName, CGFloat fontSize) const
{
	ASSERT(scope.left && scope.right);

	std::map<key_t, styles_t>::iterator styles = _cache.find(key_t(scope, fontName, fontSize));
	if(styles == _cache.end())
	{
		std::multimap<double, decomposed_style_t> ordering;
		citerate(it, global_styles(scope))
		{
			double rank = 0;
			if(it->scope_selector.does_match(scope, &rank))
				ordering.insert(std::make_pair(rank, *it));
		}

		iterate(it, _styles)
		{
			double rank = 0;
			if(it->scope_selector.does_match(scope, &rank))
				ordering.insert(std::make_pair(rank, *it));
		}

		decomposed_style_t base(scope::selector_t(), fontName, fontSize);
		iterate(it, ordering)
			base += it->second;

		CTFontPtr font(CTFontCreateWithName(cf::wrap(base.font_name), round(base.font_size), NULL), CFRelease);
		if(CTFontSymbolicTraits traits = (base.bold == bool_true ? kCTFontBoldTrait : 0) + (base.italic == bool_true ? kCTFontItalicTrait : 0))
		{
			if(CTFontRef newFont = CTFontCreateCopyWithSymbolicTraits(font.get(), round(base.font_size), NULL, traits, kCTFontBoldTrait | kCTFontItalicTrait))
				font.reset(newFont, CFRelease);
		}

		cf::color_t foreground                = base.foreground.is_blank()                ? cf::color_t("#000000")   : base.foreground;
		cf::color_t background                = base.background.is_blank()                ? cf::color_t("#FFFFFF")   : base.background;
		cf::color_t gutterForeground          = base.gutterForeground.is_blank()          ? soften(foreground, 0.5)  : base.gutterForeground;
		cf::color_t gutterBackground          = base.gutterBackground.is_blank()          ? soften(background, 0.87) : base.gutterBackground;
		cf::color_t gutterDivider             = base.gutterDivider.is_blank()             ? soften(foreground, 0.4)  : base.gutterDivider;
		cf::color_t gutterSelectionForeground = base.gutterSelectionForeground.is_blank() ? soften(foreground, 0.95) : base.gutterSelectionForeground;
		cf::color_t gutterSelectionBackground = base.gutterSelectionBackground.is_blank() ? soften(background, 0.95) : base.gutterSelectionBackground;
		cf::color_t gutterSelectionBorder     = base.gutterSelectionBorder.is_blank()     ? soften(background, 0.95) : base.gutterSelectionBorder;
		cf::color_t gutterIcons               = base.gutterIcons.is_blank()               ? cf::color_t("#000000")   : base.gutterIcons;
		cf::color_t selection                 = base.selection.is_blank()                 ? cf::color_t("#4D97FF54") : base.selection;
		cf::color_t caret                     = base.caret.is_blank()                     ? cf::color_t("#000000")   : base.caret;

		styles_t res(foreground, background, gutterForeground, gutterBackground, gutterDivider, gutterSelectionForeground, gutterSelectionBackground, gutterSelectionBorder, gutterIcons, selection, caret, font, base.underlined == bool_true, base.misspelled == bool_true);
		styles = _cache.insert(std::make_pair(key_t(scope, fontName, fontSize), res)).first;
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

static void alpha_blend (theme_t::color_info_t& lhs, theme_t::color_info_t const& rhs)
{
	if(rhs.is_blank())
	{
		return;
	}
	else if(rhs.is_opaque() || lhs.is_blank())
	{
		lhs = rhs;
	}
	else
	{
		double alpha = rhs.alpha;
		lhs.red   = (1.0 - alpha) * lhs.red + alpha * rhs.red;
		lhs.green = (1.0 - alpha) * lhs.green + alpha * rhs.green;
		lhs.blue  = (1.0 - alpha) * lhs.blue + alpha * rhs.blue;
	}
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
	font_name = rhs.font_name == NULL_STR ? font_name : rhs.font_name;
	font_size = rhs.font_size > 0 ? rhs.font_size : font_size * fabs(rhs.font_size);

	alpha_blend(foreground,                rhs.foreground);
	alpha_blend(background,                rhs.background);
	alpha_blend(gutterForeground,          rhs.gutterForeground);
	alpha_blend(gutterBackground,          rhs.gutterBackground);
	alpha_blend(gutterDivider,             rhs.gutterDivider);
	alpha_blend(gutterSelectionForeground, rhs.gutterSelectionForeground);
	alpha_blend(gutterSelectionBackground, rhs.gutterSelectionBackground);
	alpha_blend(gutterSelectionBorder,     rhs.gutterSelectionBorder);
	alpha_blend(gutterIcons,               rhs.gutterIcons);
	alpha_blend(caret,                     rhs.caret);
	alpha_blend(selection,                 rhs.selection);
	alpha_blend(invisibles,                rhs.invisibles);

	bold       = rhs.bold       == bool_unset ? bold       : rhs.bold;
	italic     = rhs.italic     == bool_unset ? italic     : rhs.italic;
	underlined = rhs.underlined == bool_unset ? underlined : rhs.underlined;
	misspelled = rhs.misspelled == bool_unset ? misspelled : rhs.misspelled;

	return *this;
}

// ==============
// = Public API =
// ==============

theme_ptr parse_theme (bundles::item_ptr const& themeItem)
{
	static oak::uuid_t const kEmptyThemeUUID = oak::uuid_t().generate();
	static std::map<oak::uuid_t, theme_ptr> Cache;

	oak::uuid_t const& uuid = themeItem ? themeItem->uuid() : kEmptyThemeUUID;
	std::map<oak::uuid_t, theme_ptr>::iterator theme = Cache.find(uuid);
	if(theme == Cache.end())
		theme = Cache.insert(std::make_pair(uuid, theme_ptr(new theme_t(themeItem)))).first;
	return theme->second;
}
