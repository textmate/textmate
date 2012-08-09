#include "theme.h"
#include <cf/cf.h>

theme_t::decomposed_style_t theme_t::parse_styles (plist::dictionary_t const& plist)
{
	decomposed_style_t res;

	std::string scopeSelector;
	if(plist::get_key_path(plist, "scope", scopeSelector))
		res.scope_selector = scopeSelector;

	plist::get_key_path(plist, "settings.fontName",   res.font_name);
	plist::get_key_path(plist, "settings.fontSize",   res.font_size);
	plist::get_key_path(plist, "settings.foreground", res.foreground);
	plist::get_key_path(plist, "settings.background", res.background);
	plist::get_key_path(plist, "settings.caret",      res.caret);
	plist::get_key_path(plist, "settings.selection",  res.selection);
	plist::get_key_path(plist, "settings.invisibles", res.invisibles);

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
	static struct { std::string name; std::string decomposed_style_t::*field; } const stringKeys[] =
	{
		{ "foreground", &decomposed_style_t::foreground },
		{ "background", &decomposed_style_t::background },
		{ "caret",      &decomposed_style_t::caret      },
		{ "selection",  &decomposed_style_t::selection  },
		{ "invisibles", &decomposed_style_t::invisibles },
		{ "fontName",   &decomposed_style_t::font_name  },
		{ "fontSize",   &decomposed_style_t::font_size  },
	};

	static struct { std::string name; bool_t decomposed_style_t::*field; } const booleanKeys[] =
	{
		{ "misspelled", &decomposed_style_t::misspelled },
		{ "bold",       &decomposed_style_t::bold       },
		{ "italic",     &decomposed_style_t::italic     },
		{ "underline",  &decomposed_style_t::underlined },
	};

	std::vector<decomposed_style_t> res;

	for(size_t i = 0; i < sizeofA(stringKeys); ++i)
	{
		bundles::item_ptr item;
		plist::any_t const& value = bundles::value_for_setting(stringKeys[i].name, scope, &item);
		if(item)
		{
			res.push_back(decomposed_style_t(item->scope_selector()));
			res.back().*(stringKeys[i].field) = plist::get<std::string>(value);
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
				if(_styles.back().invisibles != NULL_STR)
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

		CTFontPtr font(CTFontCreateWithName(cf::wrap(base.font_name), round(base.absolute_font_size), NULL), CFRelease);
		if(CTFontSymbolicTraits traits = (base.bold == bool_true ? kCTFontBoldTrait : 0) + (base.italic == bool_true ? kCTFontItalicTrait : 0))
		{
			if(CTFontRef newFont = CTFontCreateCopyWithSymbolicTraits(font.get(), round(base.absolute_font_size), NULL, traits, kCTFontBoldTrait | kCTFontItalicTrait))
				font.reset(newFont, CFRelease);
		}

		base.foreground = base.foreground == NULL_STR ? "#000000"   : base.foreground;
		base.background = base.background == NULL_STR ? "#FFFFFF"   : base.background;
		base.caret      = base.caret      == NULL_STR ? "#000000"   : base.caret;
		base.selection  = base.selection  == NULL_STR ? "#4D97FF54" : base.selection;
		base.invisibles = base.invisibles == NULL_STR ? "#BFBFBF"   : base.invisibles;

		styles_t res(base.foreground, base.background, base.selection, base.caret, font, base.underlined == bool_true, base.misspelled == bool_true);
		styles = _cache.insert(std::make_pair(key_t(scope, fontName, fontSize), res)).first;
	}
	return styles->second;
}

static std::string alpha_blend (std::string const& lhs, std::string const& rhs)
{
	if(lhs == NULL_STR || rhs.size() != 9 || lhs == rhs)
		return rhs == NULL_STR ? lhs : rhs;

	enum { R, G, B, A };
	unsigned int col[2][4] = { { 0x00, 0x00, 0x00, 0xFF }, { 0x00, 0x00, 0x00, 0xFF } };

	if(sscanf(lhs.c_str(), "#%02x%02x%02x%02x", &col[0][R], &col[0][G], &col[0][B], &col[0][A]) < 3)
		return rhs == NULL_STR ? lhs : rhs;
	if(sscanf(rhs.c_str(), "#%02x%02x%02x%02x", &col[1][R], &col[1][G], &col[1][B], &col[1][A]) < 4 || col[1][A] == 0xFF)
		return rhs == NULL_STR ? lhs : rhs;

	double alpha = col[1][A]/255.0;
	double red   = (1.0 - alpha) * col[0][R]/255.0 + alpha * col[1][R]/255.0;
	double green = (1.0 - alpha) * col[0][G]/255.0 + alpha * col[1][G]/255.0;
	double blue  = (1.0 - alpha) * col[0][B]/255.0 + alpha * col[1][B]/255.0;
	if(alpha != 1.0)
		alpha = col[0][A]/255.0;

	return text::format("#%02lX%02lX%02lX%02lX", lround(255 * red), lround(255 * green), lround(255 * blue), lround(255 * alpha));
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

theme_t::decomposed_style_t& theme_t::decomposed_style_t::operator+= (theme_t::decomposed_style_t const& rhs)
{
	font_name = rhs.font_name == NULL_STR ? font_name : rhs.font_name;

	if(rhs.font_size != NULL_STR)
	{
		char const* first = rhs.font_size.c_str();
		char const* last;
		double size = my_strtod(first, &last);
		if(first != last)
		{
			while(*last == ' ')
				++last;

			if(strcmp(last, "pt") == 0 || *last == '\0')
				absolute_font_size = size;
			else if(strcmp(last, "em") == 0)
				absolute_font_size = absolute_font_size * size;
			else if(strcmp(last, "%") == 0)
				absolute_font_size = absolute_font_size * size / 100;
			else
				fprintf(stderr, "*** unsupported font size unit: %s (%s)\n", last, rhs.font_size.c_str());
		}
		else
		{
			fprintf(stderr, "*** unsupported font size format: %s\n", rhs.font_size.c_str());
		}
	}

	foreground = alpha_blend(foreground, rhs.foreground);
	background = alpha_blend(background, rhs.background);
	caret      = alpha_blend(caret,      rhs.caret);
	selection  = alpha_blend(selection,  rhs.selection);
	invisibles = alpha_blend(invisibles, rhs.invisibles);

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
