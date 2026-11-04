#ifndef THEME_FONT_SIZE_H
#define THEME_FONT_SIZE_H

#include <optional>
#include <string>

namespace theme
{
	// Positive values are points; negative values are multipliers of the base size.
	std::optional<double> parse_font_size (std::string const& value);
}

#endif
