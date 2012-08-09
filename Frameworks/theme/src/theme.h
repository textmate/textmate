#ifndef THEME_H_T0VVCP8F
#define THEME_H_T0VVCP8F

#include <bundles/bundles.h>
#include <scope/scope.h>
#include <cf/color.h>

typedef std::tr1::shared_ptr<struct __CTFont const> CTFontPtr;

struct PUBLIC styles_t
{
	styles_t (cf::color_t const& foreground, cf::color_t const& background, cf::color_t const& selection, cf::color_t const& caret, CTFontPtr font, bool underlined, bool misspelled) : _foreground(foreground), _background(background), _selection(selection), _caret(caret), _font(font), _underlined(underlined), _misspelled(misspelled) { }

	CGColorRef foreground () const { return _foreground; }
	CGColorRef background () const { return _background; }
	CGColorRef caret () const      { return _caret; }
	CGColorRef selection () const  { return _selection; }
	CTFontRef font () const        { return _font.get(); }
	bool underlined () const       { return _underlined; }
	bool misspelled () const       { return _misspelled; }

private:
	cf::color_t _foreground;
	cf::color_t _background;
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

private:
	enum bool_t { bool_true, bool_false, bool_unset };

	struct decomposed_style_t
	{
		decomposed_style_t (scope::selector_t const& scopeSelector = scope::selector_t(), std::string const& fontName = NULL_STR, CGFloat fontSize = 0) : scope_selector(scopeSelector), font_name(fontName), font_size(NULL_STR), foreground(NULL_STR), background(NULL_STR), caret(NULL_STR), selection(NULL_STR), invisibles(NULL_STR), bold(bool_unset), italic(bool_unset), underlined(bool_unset), misspelled(bool_unset), absolute_font_size(fontSize) { }
		decomposed_style_t& operator+= (decomposed_style_t const& rhs);

		scope::selector_t scope_selector;

		std::string font_name;
		std::string font_size;
		std::string foreground;
		std::string background;
		std::string caret;
		std::string selection;
		std::string invisibles;
		bool_t bold;
		bool_t italic;
		bool_t underlined;
		bool_t misspelled;

		CGFloat absolute_font_size;
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

typedef std::tr1::shared_ptr<theme_t> theme_ptr;
PUBLIC theme_ptr parse_theme (bundles::item_ptr const& themeItem);

#endif /* end of include guard: THEME_H_T0VVCP8F */
