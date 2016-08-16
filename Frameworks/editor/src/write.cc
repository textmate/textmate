#include "write.h"
#include <text/utf8.h>
#include <text/ctype.h>

namespace ng
{
	static size_t count_columns (buffer_api_t const& buffer, index_t caret, size_t tabSize)
	{
		std::string const str = buffer.substr(buffer.begin(buffer.convert(caret.index).line), caret.index);
		size_t len = 0;
		citerate(ch, diacritics::make_range(str.data(), str.data() + str.size()))
			len += *ch == '\t' ? tabSize - (len % tabSize) : (text::is_east_asian_width(*ch) ? 2 : 1);
		return len + caret.carry;
	}

	ng::ranges_t write_unit_to_fd (buffer_api_t const& buffer, ranges_t const& ranges, size_t tabSize, int fd, input::type unit, input::type fallbackUnit, input_format::type format, scope::selector_t const& scopeSelector, std::map<std::string, std::string>& variables, bool* inputWasSelection) // TODO Move write_unit_to_fd to command framework.
	{
		bool noSelection = true;
		for(auto const& range : ranges)
			noSelection = noSelection && range.empty();

		input::type actualUnit = unit == input::selection && noSelection && (ranges.size() == 1 || fallbackUnit != input::entire_document) ? fallbackUnit : unit;
		*inputWasSelection = actualUnit == input::selection;

		ng::ranges_t res;
		for(auto range : ranges)
		{
			switch(actualUnit)
			{
				case input::character:        range = extend_if_empty(buffer, range, kSelectionExtendRight).last();        break;
				case input::word:             range = word_at(buffer, range);                                              break;
				case input::line:             range = extend_if_empty(buffer, range, kSelectionExtendToLineExclLF).last(); break;
				case input::scope:            range = select_scope(buffer, range, scopeSelector).last();                   break;
				case input::entire_document:  range = extend(buffer, range, kSelectionExtendToAll).last();                 break;
			};
			res.push_back(range);
		}

		if(res.size() != 1)
			res = sanitize(buffer, res);

		if(res.size() != 1 || !res.last().empty())
		{
			std::string str = "";
			bool first = true;
			for(auto const& range : dissect_columnar(buffer, res))
			{
				if(!std::exchange(first, false))
					str += "\n";
				str += format == input_format::xml ? buffer.xml_substr(range.min().index, range.max().index) : buffer.substr(range.min().index, range.max().index);
			}

			dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
				if(write(fd, str.data(), str.size()) == -1)
					perror("ng::write_unit_to_fd: write");
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
