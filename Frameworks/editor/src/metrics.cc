#include "editor.h"
#include <bundles/src/bundles.h>
#include <text/src/classification.h>
#include <text/src/utf8.h>
#include <text/src/my_ctype.h>

namespace ng
{
	// ====================
	// = Visual Distances =
	// ====================

	size_t editor_t::visual_distance (ng::buffer_t const& buffer, index_t first, index_t last, bool eastAsianWidth)
	{
		ASSERT_LE(first.index, last.index);
		ASSERT_EQ(buffer.convert(first.index).line, buffer.convert(last.index).line);
		size_t len = 0, tabSize = buffer.indent().tab_size();
		std::string const& str = buffer.substr(first.index, last.index);
		citerate(ch, diacritics::make_range(str.data(), str.data() + str.size()))
			len += *ch == '\t' ? tabSize - (len % tabSize) : (eastAsianWidth && text::is_east_asian_width(*ch) ? 2 : 1);
		return len + (first.index == last.index ? last.carry - first.carry : last.carry);
	}

	index_t editor_t::visual_advance (ng::buffer_t const& buffer, index_t caret, size_t distance, bool eastAsianWidth)
	{
		size_t len = 0, tabSize = buffer.indent().tab_size();
		std::string const& str = buffer.substr(caret.index, buffer.eol(buffer.convert(caret.index).line));
		citerate(ch, diacritics::make_range(str.data(), str.data() + str.size()))
		{
			if(len == distance)
				return index_t(caret.index + (&ch - str.data()));

			size_t chWidth = *ch == '\t' ? tabSize - (len % tabSize) : (eastAsianWidth && text::is_east_asian_width(*ch) ? 2 : 1);
			if(len + chWidth > distance || *ch == '\n')
				return index_t(caret.index + (&ch - str.data()), distance - len);

			len += chWidth;
		}
		return index_t(caret.index + str.size(), distance - len);
	}

} /* ng */
