#ifndef THEME_EDITOR_APPEARANCE_H
#define THEME_EDITOR_APPEARANCE_H

#include <bundles/item.h>
#include <text/parse.h>
#include <text/trim.h>

namespace theme_editor
{
	enum appearance_t { light, dark, unspecified };

	inline appearance_t appearance (std::string const& semanticClass)
	{
		// Match TextMate's theme menu, including its treatment of unclassified themes.
		auto classes = text::split(semanticClass, ",");
		auto parts = text::split(classes.empty() ? "" : text::trim(classes.front()), ".");
		if(parts.size() > 2 && parts.front() == "theme")
		{
			if(parts[1] == "light") return light;
			if(parts[1] == "dark")  return dark;
		}
		return unspecified;
	}

	inline appearance_t appearance (plist::dictionary_t const& theme)
	{
		std::string semanticClass;
		plist::get_key_path(theme, "semanticClass", semanticClass);
		return appearance(semanticClass);
	}

	inline void set_appearance (plist::dictionary_t& theme, appearance_t value)
	{
		std::string semanticClass;
		plist::get_key_path(theme, "semanticClass", semanticClass);
		auto comma = semanticClass.find(',');
		std::string first = text::trim(semanticClass.substr(0, comma));
		auto parts = text::split(first, ".");
		std::string category = value == light ? "light" : value == dark ? "dark" : "unspecified";
		if(parts.size() > 2 && parts.front() == "theme")
		{
			// Keep the theme's identifier suffix and any additional semantic classes.
			auto suffix = first.find('.', first.find('.') + 1);
			theme["semanticClass"] = "theme." + category + first.substr(suffix) + (comma == std::string::npos ? "" : semanticClass.substr(comma));
		}
		else if(value != unspecified)
		{
			theme["semanticClass"] = "theme." + category + ".custom" + (semanticClass.empty() ? "" : ", " + semanticClass);
		}
	}
}

#endif
