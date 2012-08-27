#ifndef THEME_H_T0VVCP8F
#define THEME_H_T0VVCP8F

#include <bundles/bundles.h>
#include <scope/scope.h>
#include <cf/color.h>

typedef std::shared_ptr<struct __CTFont const> CTFontPtr;

struct PUBLIC styles_t
{
	styles_t (cf::color_t const& foreground, cf::color_t const& background, cf::color_t const& gutterForeground, cf::color_t const& gutterBackground, cf::color_t const& gutterIcons, cf::color_t const& gutterDivider, cf::color_t const& gutterSelectionForeground, cf::color_t const& gutterSelectionBackground, cf::color_t const& gutterSelectionIcons, cf::color_t const& gutterSelectionBorder, cf::color_t const& selection, cf::color_t const& caret, CTFontPtr font, bool underlined, bool misspelled) : _foreground(foreground), _background(background), _gutterForeground(gutterForeground), _gutterBackground(gutterBackground), _gutterIcons(gutterIcons), _gutterDivider(gutterDivider), _gutterSelectionForeground(gutterSelectionForeground), _gutterSelectionBackground(gutterSelectionBackground), _gutterSelectionIcons(gutterSelectionIcons), _gutterSelectionBorder(gutterSelectionBorder), _selection(selection), _caret(caret), _font(font), _underlined(underlined), _misspelled(misspelled) { }

	CGColorRef foreground () const               { return _foreground; }
	CGColorRef background () const               { return _background; }
	CGColorRef gutterForeground () const         { return _gutterForeground; }
	CGColorRef gutterBackground () const         { return _gutterBackground; }
	CGColorRef gutterIcons() const               { return _gutterIcons; }
	CGColorRef gutterDivider () const            { return _gutterDivider; }
	CGColorRef gutterSelectionForeground() const { return _gutterSelectionForeground; }
	CGColorRef gutterSelectionBackground() const { return _gutterSelectionBackground; }
	CGColorRef gutterSelectionIcons() const      { return _gutterSelectionIcons; }
	CGColorRef gutterSelectionBorder() const     { return _gutterSelectionBorder; }
	CGColorRef caret () const                    { return _caret; }
	CGColorRef selection () const                { return _selection; }
	CTFontRef font () const                      { return _font.get(); }
	bool underlined () const                     { return _underlined; }
	bool misspelled () const                     { return _misspelled; }

private:
	cf::color_t _foreground;
	cf::color_t _background;
	cf::color_t _gutterForeground;
	cf::color_t _gutterBackground;
	cf::color_t _gutterIcons;
	cf::color_t _gutterDivider;
	cf::color_t _gutterSelectionForeground;
	cf::color_t _gutterSelectionBackground;
	cf::color_t _gutterSelectionIcons;
	cf::color_t _gutterSelectionBorder;
	cf::color_t _selection;
	cf::color_t _caret;
	CTFontPtr _font;
	bool _underlined;
	bool _misspelled;
};

struct PUBLIC theme_t
{
	theme_t (bundles::item_ptr const& themeItem);
	~theme_t ();

	oak::uuid_t const& uuid () const;
	styles_t const& styles_for_scope (scope::context_t const& scope, std::string fontName, CGFloat fontSize) const;

	struct color_info_t
	{
		color_info_t () : red(-1), green(0), blue(0), alpha(1) { }
		color_info_t (double red, double green, double blue, double alpha = 1.0) : red(red), green(green), blue(blue), alpha(alpha) { } 

		bool is_blank () const  { return red < 0; }
		bool is_opaque () const { return alpha == 1; };

		operator cf::color_t () { ASSERT(!is_blank()); return cf::color_t(red, green, blue, alpha); }

		double red, green, blue, alpha;
	};

private:
	enum bool_t { bool_true, bool_false, bool_unset };

	struct decomposed_style_t
	{
		decomposed_style_t (scope::selector_t const& scopeSelector = scope::selector_t(), std::string const& fontName = NULL_STR, CGFloat fontSize = -1) : scope_selector(scopeSelector), font_name(fontName), font_size(fontSize), bold(bool_unset), italic(bool_unset), underlined(bool_unset), misspelled(bool_unset) { }
		decomposed_style_t& operator+= (decomposed_style_t const& rhs);

		scope::selector_t scope_selector;

		std::string font_name;
		CGFloat font_size;
		color_info_t foreground;
		color_info_t background;
		color_info_t gutterForeground;
		color_info_t gutterBackground;
		color_info_t gutterIcons;
		color_info_t gutterDivider;
		color_info_t gutterSelectionForeground;
		color_info_t gutterSelectionBackground;
		color_info_t gutterSelectionIcons;
		color_info_t gutterSelectionBorder;
		color_info_t caret;
		color_info_t selection;
		color_info_t invisibles;
		bool_t bold;
		bool_t italic;
		bool_t underlined;
		bool_t misspelled;
	};

	struct callback_t : bundles::callback_t
	{
		callback_t (theme_t& theme) : _theme(theme) { }
		void bundles_did_change ()                  { _theme.setup_styles(); }
	private:
		theme_t& _theme;
	};

	void setup_styles ();

	static decomposed_style_t parse_styles (plist::dictionary_t const& plist);
	static std::vector<decomposed_style_t> global_styles (scope::context_t const& scope);

	bundles::item_ptr _item;
	std::vector<decomposed_style_t> _styles;
	callback_t _callback;

	typedef boost::tuple<scope::context_t, std::string, CGFloat> key_t; // scope, font name, font size
	mutable std::map<key_t, styles_t> _cache;
};

typedef std::shared_ptr<theme_t> theme_ptr;
PUBLIC theme_ptr parse_theme (bundles::item_ptr const& themeItem);

#endif /* end of include guard: THEME_H_T0VVCP8F */
