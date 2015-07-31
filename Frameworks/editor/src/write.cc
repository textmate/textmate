#include "write.h"
#include <text/utf8.h>
#include <text/ctype.h>

namespace ng
{
	static size_t count_columns (buffer_t const& buffer, index_t caret, size_t tabSize)
	{
		std::string const str = buffer.substr(buffer.begin(buffer.convert(caret.index).line), caret.index);
		size_t len = 0;
		citerate(ch, diacritics::make_range(str.data(), str.data() + str.size()))
			len += *ch == '\t' ? tabSize - (len % tabSize) : (text::is_east_asian_width(*ch) ? 2 : 1);
		return len + caret.carry;
	}

	ng::ranges_t write_unit_to_fd (buffer_t const& buffer, ranges_t const& ranges, size_t tabSize, int fd, input::type unit, input::type fallbackUnit, input_format::type format, scope::selector_t const& scopeSelector, std::map<std::string, std::string>& variables, bool* inputWasSelection) // TODO Move write_unit_to_fd to command framework.
	{
		bool noSelection = true;
		for(auto const& range : ranges)
			noSelection = noSelection && range.empty();

		input::type actualUnit = unit == input::selection && noSelection ? fallbackUnit : unit;
		*inputWasSelection = actualUnit == input::selection;

		ng::ranges_t res = ranges;
		if(ranges.size() == 1 || unit != input::selection)
		{
			range_t r, range = ranges.last();
			switch(actualUnit)
			{
				case input::character:        r = extend_if_empty(buffer, range, kSelectionExtendRight).last();        break;
				case input::word:             r = word_at(buffer, range);                                          break;
				case input::line:             r = extend_if_empty(buffer, range, kSelectionExtendToLineExclLF).last(); break;
				case input::scope:            r = select_scope(buffer, range, scopeSelector).last();                   break;
				case input::selection:        r = range;                                                               break;
				case input::entire_document:  r = extend(buffer, range, kSelectionExtendToAll).last();                 break;
			};
			res = r;
		}

		if(res.size() != 1 && unit == input::selection || !res.last().empty())
		{
			std::string str = "";
			bool first = true;
			for(auto const& range : dissect_columnar(buffer, res))
			{
				if(!std::exchange(first, false))
					str += "\n";
				str += format == input_format::xml ? to_xml(buffer, range.min().index, range.max().index) : buffer.substr(range.min().index, range.max().index);
			}

			dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
				if(write(fd, str.data(), str.size()) == -1)
					perror("write");
				close(fd);
			});
		}
		else
		{
			close(fd);
		}

		if(res.size() == 1 && !res.last().empty() && actualUnit != input::entire_document)
		{
			text::pos_t const& pos = buffer.convert(res.last().min().index);
			variables.emplace("TM_INPUT_START_LINE",       std::to_string(pos.line + 1));
			variables.emplace("TM_INPUT_START_LINE_INDEX", std::to_string(pos.column));
			variables.emplace("TM_INPUT_START_COLUMN",     std::to_string(count_columns(buffer, res.last().min(), tabSize) + 1));
		}

		return res;
	}

} /* ng */
