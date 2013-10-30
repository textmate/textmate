#include "selection.h"
#include <buffer/buffer.h>
#include <bundles/bundles.h>
#include <regexp/find.h>
#include <regexp/regexp.h>
#include <text/classification.h>
#include <text/utf8.h>
#include <text/ctype.h>
#include <text/types.h>
#include <text/tokenize.h>

namespace ng
{
	static size_t count_columns (buffer_t const& buffer, index_t caret)
	{
		size_t const tabSize = buffer.indent().tab_size();
		std::string const str = buffer.substr(buffer.begin(buffer.convert(caret.index).line), caret.index);
		size_t len = 0;
		citerate(ch, diacritics::make_range(str.data(), str.data() + str.size()))
			len += *ch == '\t' ? tabSize - (len % tabSize) : (text::is_east_asian_width(*ch) ? 2 : 1);
		return len + caret.carry;
	}

	static index_t at_column (buffer_t const& buffer, size_t line, size_t column)
	{
		size_t const tabSize = buffer.indent().tab_size();
		size_t caret = buffer.begin(line), len = 0;
		std::string const str = buffer.substr(caret, buffer.eol(line));
		citerate(ch, diacritics::make_range(str.data(), str.data() + str.size()))
		{
			if(len == column)
				return caret + (&ch - str.data());

			size_t chWidth = *ch == '\t' ? tabSize - (len % tabSize) : (text::is_east_asian_width(*ch) ? 2 : 1);
			if(len + chWidth > column || *ch == '\n')
				return index_t(caret + (&ch - str.data()), column - len);

			len += chWidth;
		}
		return index_t(caret + str.size(), column - len);
	}

	std::string const kCharacterClassWord    = "word";
	std::string const kCharacterClassSpace   = "space";
	std::string const kCharacterClassOther   = "other";
	std::string const kCharacterClassUnknown = "unknown";

	std::string character_class (buffer_t const& buffer, size_t index)
	{
		bundles::item_ptr match;
		plist::any_t value = bundles::value_for_setting("characterClass", buffer.scope(index), &match);
		if(match)
			return boost::get<std::string>(value);
		else if(text::is_word_char(buffer[index]))
			return kCharacterClassWord;
		else
		{
			value = bundles::value_for_setting("wordCharacters", buffer.scope(index), &match);
			if(match && boost::get<std::string>(value).find(buffer[index]) != std::string::npos)
				return kCharacterClassWord;
			else if(text::is_whitespace(buffer[index]))
				return kCharacterClassSpace;
		}
		return kCharacterClassOther;
	}

	static bool is_part_of_word (buffer_t const& buffer, size_t index)
	{
		return character_class(buffer, index) != kCharacterClassSpace && character_class(buffer, index) != kCharacterClassOther;
	}

	static size_t extend_scope_left (buffer_t const& buffer, size_t caret, scope::scope_t const& scope)
	{
		while(caret && buffer.scope(caret).left.has_prefix(scope))
			caret -= buffer[caret-1].size();
		return caret;
	}

	static size_t extend_scope_right (buffer_t const& buffer, size_t caret, scope::scope_t const& scope)
	{
		while(caret < buffer.size() && buffer.scope(caret).right.has_prefix(scope))
			caret += buffer[caret].size();
		return caret;
	}

	static ranges_t sanitize (buffer_t const& buffer, ranges_t const& selection)
	{
		/* This function will transform the selection so that
		   all indexes are on proper multi-byte boundaries and
		   ensure that no ranges overlap. The latter should
		   probably not be done here, but instead prior to
		   edits (and drawing should take overlap into account).

		   The main reason is that the user may temporarily
		   create overlapping ranges, and if we fix them,
		   successive selection adjustment commands will not
		   work as expected.

		   It should also be noted that for column selections
		   we are not able to ensure that there is no overlap.
		*/

		struct indexed_range_t
		{
			range_t range;
			size_t index;

			indexed_range_t (range_t const& range, size_t const& index) : range(range), index(index) { }

			bool operator< (indexed_range_t const& rhs) const
			{
				return as_tuple() < rhs.as_tuple();
			}

		private:
			std::tuple<size_t, size_t, size_t, size_t, size_t> as_tuple () const
			{
				size_t col = range.columnar ? 1 : 0;
				size_t min = range.min().index;
				size_t off = range.freehanded ? range.min().carry : 0;
				size_t max = SIZE_T_MAX - range.max().index;
				size_t mOff = range.freehanded ? range.max().carry : 0;
				return std::make_tuple(col, min, off, max, mOff);
			}
		};

		size_t index = 0;
		std::set<indexed_range_t> set;
		for(auto range : selection)
			set.emplace(range_t(index_t(buffer.sanitize_index(range.first.index), range.first.carry), index_t(buffer.sanitize_index(range.last.index), range.last.carry), range.columnar, range.freehanded, range.unanchored), index++);

		index_t last;
		std::map<size_t, range_t> map;
		for(auto record : set)
		{
			range_t range = record.range;
			auto max = range.normalized().max();

			if(!last || range.columnar)
			{
				map.emplace(record.index, range);
				last = max;
			}
			else if(last < max || last == max && range.empty())
			{
				auto min = range.normalized().min();
				if(min < last)
					range.min() = last;
				map.emplace(record.index, range);
				last = max;
			}
		}

		ranges_t res;
		for(auto pair : map)
			res.push_back(pair.second);

		return res;
	}

	static index_t cap (buffer_t const& buf, text::pos_t const& pos)
	{
		size_t line = oak::cap<size_t>(0, pos.line,   buf.lines()-1);
		size_t col  = oak::cap<size_t>(0, pos.column, buf.eol(line) - buf.begin(line));
		index_t res = buf.sanitize_index(buf.convert(text::pos_t(line, col)));
		if(pos.offset && res.index < buf.size() && buf[res.index] == "\n")
			res.carry = pos.offset;
		return res;
	}

	static range_t cap (buffer_t const& buf, text::range_t const& range)
	{
		return range_t(cap(buf, range.from), cap(buf, range.to), range.columnar, false, true);
	}

	ranges_t convert (buffer_t const& buf, text::selection_t const& sel)
	{
		ranges_t res;
		iterate(range, sel)
			res.push_back(cap(buf, *range));
		return sanitize(buf, res);
	}

	// =======================
	// = Typing Pair Support =
	// =======================

	namespace
	{
		struct pattern_t
		{
			pattern_t () { }

			pattern_t (std::string const& plain) : plain(plain)
			{
				if(plain.size() > 2 && plain[0] == '/' && plain[plain.size()-1] == '/')
				{
					left_anchored_regexp = "\\G" + plain.substr(1, plain.size()-2);
					right_anchored_regexp = plain.substr(1, plain.size()-2) + "\\G";
					is_regexp = true;
				}
			}

			std::string plain;
			regexp::pattern_t left_anchored_regexp;
			regexp::pattern_t right_anchored_regexp;
			bool is_regexp = false;
		};

		struct match_t
		{
			match_t (std::string const& match = NULL_STR, pattern_t const& counterpart_ptrn = pattern_t(), bool matched_opener = false) : match(match), counterpart_ptrn(counterpart_ptrn), matched_opener(matched_opener) { }
			operator bool () const { return match != NULL_STR; }
			std::string match;
			pattern_t counterpart_ptrn;
			bool matched_opener;
		};

		struct enclosed_range_t
		{
			enclosed_range_t () { }
			enclosed_range_t (std::pair<pattern_t, pattern_t> const& pair) : opener_ptrn(pair.first), closer_ptrn(pair.second) { }

			operator bool () const { return open_index != SIZE_T_MAX && close_index != 0; }

			pattern_t opener_ptrn, closer_ptrn;
			std::string opener_match, closer_match;
			size_t open_index = SIZE_T_MAX, close_index = 0;
			ssize_t open_count = 0, close_count = 0;
		};
	}

	static std::vector<std::pair<pattern_t, pattern_t>> character_pairs (scope::context_t const& scope, std::string const& key)
	{
		std::vector<std::pair<pattern_t, pattern_t>> res;
		plist::any_t value = bundles::value_for_setting(key, scope);
		if(plist::array_t const* array = boost::get<plist::array_t>(&value))
		{
			for(auto const& pair : *array)
			{
				if(plist::array_t const* value = boost::get<plist::array_t>(&pair))
				{
					std::string const* a1 = value->size() == 2 ? boost::get<std::string>(&(*value)[0]) : NULL;
					std::string const* a2 = value->size() == 2 ? boost::get<std::string>(&(*value)[1]) : NULL;
					if(a1 && a2 && *a1 != *a2)
						res.emplace_back(*a1, *a2);
				}
			}
		}
		else
		{
			res.emplace_back(pattern_t("("), pattern_t(")"));
			res.emplace_back(pattern_t("{"), pattern_t("}"));
			res.emplace_back(pattern_t("["), pattern_t("]"));
		}
		return res;
	}

	static bool does_match (regexp::pattern_t const& ptrn, buffer_t const& buffer, size_t from, size_t to, std::string* didMatch)
	{
		if(ptrn)
		{
			size_t bol = std::min(from, to), eol = std::max(from, to);
			std::string line = buffer.substr(bol, eol);
			if(auto m = regexp::search(ptrn, line.data(), line.data() + line.size(), line.data() + (from - bol), line.data() + (to - bol), ONIG_OPTION_FIND_NOT_EMPTY))
			{
				*didMatch = line.substr(m.begin(), m.end() - m.begin());
				return true;
			}
		}
		return false;
	}

	static bool does_match_left (pattern_t const& ptrn, buffer_t const& buffer, size_t index, std::string* didMatch)
	{
		if(!ptrn.is_regexp && ptrn.plain.size() <= index && ptrn.plain == buffer.substr(index - ptrn.plain.size(), index))
		{
			*didMatch = ptrn.plain;
			return true;
		}
		return ptrn.is_regexp && does_match(ptrn.right_anchored_regexp, buffer, index, buffer.begin(buffer.convert(index).line), didMatch);
	}

	static bool does_match_right (pattern_t const& ptrn, buffer_t const& buffer, size_t index, std::string* didMatch)
	{
		if(!ptrn.is_regexp && index + ptrn.plain.size() <= buffer.size() && ptrn.plain == buffer.substr(index, index + ptrn.plain.size()))
		{
			*didMatch = ptrn.plain;
			return true;
		}
		return ptrn.is_regexp && does_match(ptrn.left_anchored_regexp, buffer, index, buffer.eol(buffer.convert(index).line), didMatch);
	}

	static enclosed_range_t find_enclosed_range (buffer_t const& buffer, size_t index, std::vector<std::pair<pattern_t, pattern_t>> const& pairs)
	{
		std::vector<enclosed_range_t> records(pairs.begin(), pairs.end());

		size_t left = index, right = index;
		while(0 < left || right < buffer.size())
		{
			for(auto& r : records)
			{
				std::string match;
				if(0 < left && r.open_index == SIZE_T_MAX)
				{
					if(does_match_left(r.opener_ptrn, buffer, left, &match) && ++r.open_count == 1)
					{
						r.open_index = left;
						r.opener_match = match;
					}
					else if(does_match_left(r.closer_ptrn, buffer, left, &match))
					{
						--r.open_count;
					}
				}

				if(right < buffer.size() && r.close_index == 0)
				{
					if(does_match_right(r.closer_ptrn, buffer, right, &match) && ++r.close_count == 1)
					{
						r.close_index = right;
						r.closer_match = match;
					}
					else if(does_match_right(r.opener_ptrn, buffer, right, &match))
					{
						--r.close_count;
					}
				}

				if(r)
					return r;
			}

			if(0 < left)
				left -= buffer[left-1].size();
			if(right < buffer.size())
				right += buffer[right].size();
		}
		return enclosed_range_t();
	}

	static match_t first_match (buffer_t const& buffer, size_t index, std::vector<std::pair<pattern_t, pattern_t>> const& pairs, bool(*matcher)(pattern_t const&, buffer_t const&, size_t, std::string*))
	{
		std::string didMatch;
		for(auto const& pair : pairs)
		{
			if(matcher(pair.first, buffer, index, &didMatch))
				return match_t(didMatch, pair.second, true);
			else if(matcher(pair.second, buffer, index, &didMatch))
				return match_t(didMatch, pair.first, false);
		}
		return match_t();
	}

	static size_t begin_of_typing_pair (buffer_t const& buffer, size_t caret, bool moveToBefore)
	{
		size_t orgCaret = caret;
		auto pairs = character_pairs(buffer.scope(caret), "highlightPairs");

		pattern_t openerPtrn;
		if(!moveToBefore)
		{
			if(auto m = first_match(buffer, caret, pairs, &does_match_left))
			{
				openerPtrn = m.matched_opener ? pattern_t() : m.counterpart_ptrn;
				caret -= m.match.size();
			}
		}

		if(auto range = find_enclosed_range(buffer, caret, pairs))
		{
			caret = range.open_index;
			if(moveToBefore || does_match_left(openerPtrn, buffer, caret, &range.opener_match))
				caret -= range.opener_match.size();
			return caret;
		}

		return orgCaret;
	}

	static size_t end_of_typing_pair (buffer_t const& buffer, size_t caret, bool moveToAfter)
	{
		size_t orgCaret = caret;
		auto pairs = character_pairs(buffer.scope(caret), "highlightPairs");

		pattern_t closerPtrn;
		if(!moveToAfter)
		{
			if(auto m = first_match(buffer, caret, pairs, &does_match_right))
			{
				closerPtrn = m.matched_opener ? m.counterpart_ptrn : pattern_t();
				caret += m.match.size();
			}
		}

		if(auto range = find_enclosed_range(buffer, caret, pairs))
		{
			caret = range.close_index;
			if(moveToAfter || does_match_right(closerPtrn, buffer, caret, &range.closer_match))
				caret += range.closer_match.size();
			return caret;
		}

		return orgCaret;
	}

	// ====================
	// = Columnar Support =
	// ====================

	bool not_empty (buffer_t const& buffer, ranges_t const& selection)
	{
		bool isEmpty = true;
		iterate(range, selection)
			isEmpty = isEmpty && (range->empty() || (range->columnar && count_columns(buffer, range->first) == count_columns(buffer, range->last)));
		return !isEmpty;
	}

	bool multiline (buffer_t const& buffer, ranges_t const& selection)
	{
		bool isMultiline = false;
		iterate(range, selection)
			isMultiline = isMultiline || buffer.convert(range->first.index).line != buffer.convert(range->last.index).line;
		return isMultiline;
	}

	ranges_t dissect_columnar (buffer_t const& buffer, ranges_t const& selection)
	{
		ranges_t res;
		iterate(range, selection)
		{
			if(range->columnar && !range->empty())
			{
				size_t colA = count_columns(buffer, range->first);
				size_t colB = count_columns(buffer, range->last);

				size_t lineA = buffer.convert(range->first.index).line;
				size_t lineB = buffer.convert(range->last.index).line;

				size_t fromLine = std::min(lineA, lineB), fromCol = std::min(colA, colB);
				size_t toLine   = std::max(lineA, lineB), toCol   = std::max(colA, colB);

				for(size_t n = fromLine; n <= toLine; ++n)
				{
					index_t from = at_column(buffer, n, fromCol), to = at_column(buffer, n, toCol);
					res.push_back(range_t(from, to, false, from.carry || to.carry));
				}
			}
			else
			{
				res.push_back(range_t(range->min(), range->max(), range->columnar, range->freehanded));
			}
		}
		return res;
	}

	ranges_t toggle_columnar (ranges_t const& selection)
	{
		ranges_t res = selection;
		if(res.size() != 0)
		{
			range_t& last = res.last();
			last = range_t(last.first, last.last, !last.empty() && !last.columnar, last.freehanded);
		}
		return res;
	}

	// =================
	// = Move Caret(s) =
	// =================

	static bool all_spaces (buffer_t const& buffer, size_t from, size_t to)
	{
		bool allSpaces = true;
		while(from < to && allSpaces)
			allSpaces = allSpaces && buffer[from++] == " ";
		return allSpaces;
	}

	static bool all_whitespace (buffer_t const& buffer, size_t from, size_t to)
	{
		bool res = true;
		static std::string const whitespaceChars[] = { " ", "\t", "\n" };
		while(from < to && res)
			res = res && oak::contains(std::begin(whitespaceChars), std::end(whitespaceChars), buffer[from++]);
		return res;
	}

	static size_t end_of_leading_indent (buffer_t const& buffer, size_t line)
	{
		size_t bol = buffer.begin(line);
		size_t eol = buffer.eol(line);
		while(bol < eol && isspace(utf8::to_ch(buffer[bol])))
			++bol;
		return bol;
	}

	static index_t move (buffer_t const& buffer, index_t const& index, move_unit_type unit, layout_movement_t const* layout)
	{
		size_t const caret = index.index;
		size_t const line  = buffer.convert(caret).line;

		if((unit == kSelectionMoveLeft || unit == kSelectionMoveRight) && buffer.indent().soft_tabs())
		{
			size_t const bol = buffer.begin(line);
			if((caret - bol) % buffer.indent().indent_size() == 0)
			{
				if(all_spaces(buffer, bol, caret))
				{
					size_t const tabSize = buffer.indent().tab_size();
					if(unit == kSelectionMoveLeft && caret != bol)
						return caret - tabSize;
					else if(unit == kSelectionMoveRight && caret + tabSize <= buffer.eol(line) && all_spaces(buffer, caret, caret + tabSize))
						return caret + tabSize;
				}
			}
		}

		switch(unit)
		{
			case kSelectionMoveToBeginOfDocument:     return 0; 
			case kSelectionMoveToEndOfDocument:       return buffer.size();
			case kSelectionMoveToBeginOfParagraph:    return buffer.begin(line);
			case kSelectionMoveToEndOfParagraph:      return buffer.eol(line);
			case kSelectionMoveToBeginOfLine:         return buffer.begin(line);
			case kSelectionMoveToEndOfLine:           return buffer.eol(line);
			case kSelectionMoveToBeginOfSoftLine:     return layout ? layout->index_at_bol_for(caret) : buffer.begin(line);
			case kSelectionMoveToEndOfSoftLine:       return layout ? layout->index_at_eol_for(caret) : buffer.eol(line);
			case kSelectionMoveUp:                    return layout ? layout->index_above(index) : (line ? at_column(buffer, line-1, count_columns(buffer, index)) : 0);
			case kSelectionMoveDown:                  return layout ? layout->index_below(index) : (line+1 < buffer.lines() ? at_column(buffer, line+1, count_columns(buffer, index)) : buffer.size());
			case kSelectionMoveLeft:                  return layout ? layout->index_left_of(caret)  : (caret ? caret - buffer[caret-1].size() : caret);
			case kSelectionMoveRight:                 return layout ? layout->index_right_of(caret) : (caret < buffer.size() ? caret + buffer[caret].size() : caret);
			case kSelectionMoveToBeginOfTypingPair:   return begin_of_typing_pair(buffer, caret, false);
			case kSelectionMoveToEndOfTypingPair:     return end_of_typing_pair(buffer, caret, false);
			case kSelectionMovePageUp:                return layout ? layout->page_up_for(index)   : index;
			case kSelectionMovePageDown:              return layout ? layout->page_down_for(index) : index;

			case kSelectionMoveToBeginOfIndentedLine:
			{
				size_t bol = buffer.begin(line);
				size_t eoi = end_of_leading_indent(buffer, line);
				return eoi < caret ? eoi : bol;
			}
			break;

			case kSelectionMoveToEndOfIndentedLine:
			{
				size_t eoi = end_of_leading_indent(buffer, line);
				size_t eol = buffer.eol(line);
				return caret < eoi ? eoi : eol;
			}
			break;

			case kSelectionMoveToBeginOfHardParagraph:
			{
				if(line == 0)
					return 0;

				for(size_t n = line-1; n > 0; --n)
				{
					std::string const& line = buffer.substr(buffer.begin(n), buffer.end(n));
					if(text::is_blank(line.data(), line.data() + line.size()))
						return buffer.begin(n+1);
				}
				return 0;
			}
			break;

			case kSelectionMoveToEndOfHardParagraph:
			{
				for(size_t n = line+1; n < buffer.lines(); ++n)
				{
					std::string const& line = buffer.substr(buffer.begin(n), buffer.end(n));
					if(text::is_blank(line.data(), line.data() + line.size()))
						return buffer.begin(n);
				}
				return buffer.size();
			}
			break;

			case kSelectionMoveFreehandedLeft:
			{
				if(index.carry != 0)
					return index_t(caret, index.carry - 1);
				else if(caret != 0 && buffer[caret-1] == "\t")
					return at_column(buffer, line, count_columns(buffer, caret) - 1);
				else
					return move(buffer, index, kSelectionMoveLeft, layout); // FIXME With soft tabs this might move a full tab stop.
			}
			break;

			case kSelectionMoveFreehandedRight:
			{
				if(caret == buffer.size() || buffer[caret] == "\n")
					return index_t(caret, index.carry + 1);
				else if(buffer[caret] == "\t")
					return at_column(buffer, line, count_columns(buffer, caret) + index.carry + 1);
				else
					return move(buffer, index, kSelectionMoveRight, layout); // FIXME With soft tabs this might move a full tab stop.
			}
			break;

			case kSelectionMoveToBeginOfWord:
			{
				size_t i = caret;
				size_t bol = buffer.begin(line);
				bol = i == bol && line != 0 ? buffer.begin(line-1) : bol;
				if(i == bol)
					return bol;

				std::string charType = character_class(buffer, i-1);
				while(bol < i && character_class(buffer, i-1) == charType)
					i -= buffer[i-1].size();

				if((charType == kCharacterClassSpace || charType == kCharacterClassOther) && bol < i && i + buffer[i].size() == caret)
				{
					std::string charType = character_class(buffer, i-1);
					while(bol < i && character_class(buffer, i-1) == charType)
						i -= buffer[i-1].size();
				}

				return i;
			}
			break;

			case kSelectionMoveToEndOfWord:
			{
				size_t i = caret;
				size_t eol = buffer.eol(line);
				eol = caret == eol && line+1 < buffer.lines() ? buffer.eol(line+1) : eol;
				if(caret == eol)
					return eol;

				std::string charType = character_class(buffer, i);
				while(i < eol && character_class(buffer, i) == charType)
					i += buffer[i].size();

				if((charType == kCharacterClassSpace || charType == kCharacterClassOther) && i < eol && i == caret + buffer[caret].size())
				{
					charType = character_class(buffer, i);
					while(i < eol && character_class(buffer, i) == charType)
						i += buffer[i].size();
				}

				return i;
			}
			break;

			case kSelectionMoveToBeginOfSubWord:
			{
				static regexp::pattern_t ptrn("(\\p{Upper}\\p{Lower}+|\\p{Upper}+|\\p{Lower}+)[^\\p{Upper}\\p{Lower}]?$|[^\\p{Upper}\\p{Lower}]+$");

				size_t n = line && caret == buffer.begin(line) ? line-1 : line;
				std::string const& line = buffer.substr(buffer.begin(n), caret);
				if(regexp::match_t const& m = search(ptrn, line.data(), line.data() + line.size()))
					return buffer.begin(n) + m.begin();
			}
			break;

			case kSelectionMoveToEndOfSubWord:
			{
				static regexp::pattern_t ptrn("\\G([^\\p{Upper}\\p{Lower}]?("
					 "\\p{Upper}\\p{Upper}+?(?=\\p{Upper}\\p{Lower})" // NS‸String
					"|\\p{Upper}\\p{Upper}+"                          // NDEBUG‸
					"|\\p{Upper}\\p{Lower}*"                          // Camel‸Case || Camel‸_case
					"|\\p{Lower}+"                                    // camel‸Case || camel‸_case
					")|[^\\p{Upper}\\p{Lower}]*)");                   //    ‸leading_whitespace

				size_t n = line+1 < buffer.lines() && caret == buffer.eol(line) ? line+1 : line;
				std::string const& line = buffer.substr(caret, buffer.eol(n));
				if(regexp::match_t const& m = search(ptrn, line.data(), line.data() + line.size()))
					return caret + m.end();
			}
			break;

			case kSelectionMoveToBeginOfColumn:
			{
				size_t index = caret;
				if(index == buffer.size() || !is_part_of_word(buffer, index))
				{
					while(buffer.convert(index).column && !is_part_of_word(buffer, index-1))
						index -= buffer[index-1].size();
				}
				while(buffer.convert(index).column && is_part_of_word(buffer, index-1))
					index -= buffer[index-1].size();

				size_t orgCol = count_columns(buffer, caret);
				size_t col    = count_columns(buffer, index);
				size_t n      = line;
				for(; n != 0; --n)
				{
					index_t newIndex = at_column(buffer, n-1, col);
					if(newIndex.carry || (col && is_part_of_word(buffer, newIndex.index-1)) || !is_part_of_word(buffer, newIndex.index))
						break;
				}

				if(n && n == line)
				{
					while(--n != 0)
					{
						index_t newIndex = at_column(buffer, n, orgCol);
						if(!newIndex.carry && ((orgCol && is_part_of_word(buffer, newIndex.index-1)) || is_part_of_word(buffer, newIndex.index)))
							break;
					}
				}

				return at_column(buffer, n, orgCol);
			}
			break;

			case kSelectionMoveToEndOfColumn:
			{
				size_t index = caret;
				if(index == buffer.size() || !is_part_of_word(buffer, index))
				{
					while(buffer.convert(index).column && !is_part_of_word(buffer, index-1))
						index -= buffer[index-1].size();
				}
				while(buffer.convert(index).column && is_part_of_word(buffer, index-1))
					index -= buffer[index-1].size();

				size_t orgCol = count_columns(buffer, caret);
				size_t col    = count_columns(buffer, index);
				size_t n      = line;
				for(; n+1 != buffer.lines(); ++n)
				{
					index_t newIndex = at_column(buffer, n+1, col);
					if(newIndex.carry || newIndex.index == buffer.size() || (col && is_part_of_word(buffer, newIndex.index-1)) || !is_part_of_word(buffer, newIndex.index))
						break;
				}

				if(n+1 != buffer.lines() && n == line)
				{
					while(++n != buffer.lines()-1)
					{
						index_t newIndex = at_column(buffer, n, orgCol);
						if(!newIndex.carry && newIndex.index != buffer.size() && ((orgCol && is_part_of_word(buffer, newIndex.index-1)) || is_part_of_word(buffer, newIndex.index)))
							break;
					}
				}

				return at_column(buffer, n, orgCol);
			}
			break;
		}
		return index;
	}

	ranges_t move (buffer_t const& buffer, ranges_t const& selection, move_unit_type const orgUnit, layout_movement_t const* layout)
	{
		static std::set<move_unit_type> const leftward  = { kSelectionMoveLeft,  kSelectionMoveFreehandedRight, kSelectionMoveToBeginOfLine, kSelectionMoveToBeginOfParagraph, kSelectionMoveToBeginOfTypingPair, kSelectionMoveToBeginOfSoftLine, kSelectionMoveToBeginOfSubWord, kSelectionMoveToBeginOfWord };
		static std::set<move_unit_type> const rightward = { kSelectionMoveRight, kSelectionMoveFreehandedLeft, kSelectionMoveToEndOfLine, kSelectionMoveToEndOfParagraph, kSelectionMoveToEndOfTypingPair, kSelectionMoveToEndOfSoftLine, kSelectionMoveToEndOfSubWord, kSelectionMoveToEndOfWord            };

		bool isLeftward  = leftward.find(orgUnit)  != leftward.end();
		bool isRightward = rightward.find(orgUnit) != rightward.end();

		ranges_t res;
		citerate(range, isLeftward || isRightward ? dissect_columnar(buffer, selection) : selection)
		{
			move_unit_type unit = orgUnit;
			index_t index       = range->last;
			bool freehanded     = range->freehanded;

			if(!range->empty())
			{
				if(isLeftward || isRightward)
				{
					index = isLeftward ? range->min() : range->max();

					static std::set<move_unit_type> const left_right = { kSelectionMoveLeft,  kSelectionMoveRight, kSelectionMoveFreehandedRight, kSelectionMoveFreehandedLeft                                                                                                           };
					if(left_right.find(unit) != left_right.end())
						unit = kSelectionMoveNowhere;
				}
				else if(!range->columnar && (unit == kSelectionMoveUp || unit == kSelectionMoveDown) && buffer.convert(range->first.index).line != buffer.convert(range->last.index).line)
				{
					index = unit == kSelectionMoveUp ? range->min() : range->max();
					if(unit == kSelectionMoveDown && buffer.begin(buffer.convert(index.index).line) == index.index)
						unit = kSelectionMoveNowhere;
				}
			}

			if(freehanded)
			{
				if(unit == kSelectionMoveLeft)
					unit = kSelectionMoveFreehandedLeft;
				else if(unit == kSelectionMoveRight)
					unit = kSelectionMoveFreehandedRight;
			}

			switch(unit)
			{
				case kSelectionMoveToBeginOfSelection: index = range->min();                      break;
				case kSelectionMoveToEndOfSelection:   index = range->max();                      break;
				default:                               index = move(buffer, index, unit, layout); break;
			}

			if(freehanded && orgUnit == kSelectionMoveLeft && range->empty() && buffer.convert(index.index).line + 1 == buffer.convert(range->last.index).line)
				freehanded = false;

			res.push_back(range_t(index, index, false, freehanded));
		}

		if(res.size() > 1 && (orgUnit == kSelectionMoveUp || orgUnit == kSelectionMoveDown))
		{
			std::set<size_t> lines;
			iterate(range, res)
				lines.insert(buffer.convert(range->first.index).line);

			if(lines.size() > 1)
			{
				index_t min(SIZE_T_MAX), max(0);
				iterate(range, res)
				{
					min = std::min(min, range->min());
					max = std::max(max, range->max());
				}
				res = orgUnit == kSelectionMoveUp ? min : max;
			}
		}

		return sanitize(buffer, res);
	}

	// ====================
	// = Extend Selection =
	// ====================

	static range_t extend (buffer_t const& buffer, range_t const& range, select_unit_type unit, layout_movement_t const* layout)
	{
		index_t first      = range.first;
		index_t last       = range.last;
		index_t const& min = range.min();
		index_t const& max = range.max();

		if(first == last && !range.freehanded)
			first.carry = last.carry = 0;

		if(range.unanchored)
		{
			static std::set<select_unit_type> towardBegin = { kSelectionExtendLeft, kSelectionExtendFreehandedLeft, kSelectionExtendUp, kSelectionExtendToBeginOfWord, kSelectionExtendToBeginOfSubWord, kSelectionExtendToBeginOfSoftLine, kSelectionExtendToBeginOfIndentedLine, kSelectionExtendToBeginOfLine, kSelectionExtendToBeginOfParagraph, kSelectionExtendToBeginOfColumn, kSelectionExtendToBeginOfDocument, kSelectionExtendPageUp };
			static std::set<select_unit_type> towardEnd   = { kSelectionExtendRight, kSelectionExtendFreehandedRight, kSelectionExtendDown, kSelectionExtendToEndOfWord, kSelectionExtendToEndOfSubWord, kSelectionExtendToEndOfSoftLine, kSelectionExtendToEndOfIndentedLine, kSelectionExtendToEndOfLine, kSelectionExtendToEndOfParagraph, kSelectionExtendToEndOfColumn, kSelectionExtendToEndOfDocument, kSelectionExtendPageDown };

			if(first < last && towardBegin.find(unit) != towardBegin.end())
				std::swap(first, last);
			else if(last < first && towardEnd.find(unit) != towardEnd.end())
				std::swap(first, last);
		}

		if(range.freehanded || range.columnar)
		{
			if(unit == kSelectionExtendLeft)
				unit = kSelectionExtendFreehandedLeft;
			else if(unit == kSelectionExtendRight)
				unit = kSelectionExtendFreehandedRight;
		}

		switch(unit)
		{
			case kSelectionExtendLeft:                 return range_t(first, move(buffer, last, kSelectionMoveLeft,               layout), range.columnar, range.freehanded);
			case kSelectionExtendRight:                return range_t(first, move(buffer, last, kSelectionMoveRight,              layout), range.columnar, range.freehanded);
			case kSelectionExtendFreehandedLeft:       return range_t(first, move(buffer, last, kSelectionMoveFreehandedLeft,     layout), range.columnar, range.freehanded);
			case kSelectionExtendFreehandedRight:      return range_t(first, move(buffer, last, kSelectionMoveFreehandedRight,    layout), range.columnar, range.freehanded);
			case kSelectionExtendUp:                   return range_t(first, move(buffer, last, kSelectionMoveUp,                 layout), range.columnar, range.freehanded);
			case kSelectionExtendDown:                 return range_t(first, move(buffer, last, kSelectionMoveDown,               layout), range.columnar, range.freehanded);
			case kSelectionExtendToBeginOfWord:        return range_t(first, move(buffer, last, kSelectionMoveToBeginOfWord,      layout), range.columnar, range.freehanded);
			case kSelectionExtendToEndOfWord:          return range_t(first, move(buffer, last, kSelectionMoveToEndOfWord,        layout), range.columnar, range.freehanded);
			case kSelectionExtendToBeginOfSubWord:     return range_t(first, move(buffer, last, kSelectionMoveToBeginOfSubWord,   layout), range.columnar, range.freehanded);
			case kSelectionExtendToEndOfSubWord:       return range_t(first, move(buffer, last, kSelectionMoveToEndOfSubWord,     layout), range.columnar, range.freehanded);
			case kSelectionExtendToBeginOfIndentedLine: return range_t(first, move(buffer, last, kSelectionMoveToBeginOfIndentedLine, layout), range.columnar, range.freehanded);
			case kSelectionExtendToEndOfIndentedLine:  return range_t(first, move(buffer, last, kSelectionMoveToEndOfIndentedLine, layout), range.columnar, range.freehanded);
			case kSelectionExtendToBeginOfSoftLine:    return range_t(first, move(buffer, last, kSelectionMoveToBeginOfSoftLine,  layout), range.columnar, range.freehanded);
			case kSelectionExtendToEndOfSoftLine:      return range_t(first, move(buffer, last, kSelectionMoveToEndOfSoftLine,    layout), range.columnar, range.freehanded);
			case kSelectionExtendToBeginOfLine:        return range_t(first, move(buffer, last, kSelectionMoveToBeginOfLine,      layout), range.columnar, range.freehanded);
			case kSelectionExtendToEndOfLine:          return range_t(first, move(buffer, last, kSelectionMoveToEndOfLine,        layout), range.columnar, range.freehanded);
			case kSelectionExtendToBeginOfParagraph:   return range_t(first, move(buffer, last, kSelectionMoveToBeginOfParagraph, layout), range.columnar, range.freehanded);
			case kSelectionExtendToEndOfParagraph:     return range_t(first, move(buffer, last, kSelectionMoveToEndOfParagraph,   layout), range.columnar, range.freehanded);
			case kSelectionExtendToBeginOfTypingPair:  return range_t(first, move(buffer, last, kSelectionMoveToBeginOfTypingPair, layout), range.columnar, range.freehanded);
			case kSelectionExtendToEndOfTypingPair:    return range_t(first, move(buffer, last, kSelectionMoveToEndOfTypingPair,  layout), range.columnar, range.freehanded);
			case kSelectionExtendToBeginOfColumn:      return range_t(first, move(buffer, last, kSelectionMoveToBeginOfColumn,    layout), range.columnar, range.freehanded);
			case kSelectionExtendToEndOfColumn:        return range_t(first, move(buffer, last, kSelectionMoveToEndOfColumn,      layout), range.columnar, range.freehanded);
			case kSelectionExtendToBeginOfDocument:    return range_t(first, move(buffer, last, kSelectionMoveToBeginOfDocument,  layout), range.columnar, range.freehanded);
			case kSelectionExtendToEndOfDocument:      return range_t(first, move(buffer, last, kSelectionMoveToEndOfDocument,    layout), range.columnar, range.freehanded);
			case kSelectionExtendPageUp:               return range_t(first, move(buffer, last, kSelectionMovePageUp,             layout), range.columnar, range.freehanded);
			case kSelectionExtendPageDown:             return range_t(first, move(buffer, last, kSelectionMovePageDown,           layout), range.columnar, range.freehanded);

			case kSelectionExtendToSoftLine:           return range_t(move(buffer, min, kSelectionMoveToBeginOfSoftLine,          layout), min.index != max.index && max.index == move(buffer, max, kSelectionMoveToBeginOfSoftLine, layout).index ? max : move(buffer, move(buffer, max, kSelectionMoveToEndOfSoftLine, layout), kSelectionMoveRight, layout), false, false, true);
			case kSelectionExtendToLine:               return range_t(move(buffer, min, kSelectionMoveToBeginOfLine,              layout), min.index != max.index && buffer.convert(max.index).column == 0 ? max : move(buffer, move(buffer, max, kSelectionMoveToEndOfLine, layout), kSelectionMoveRight, layout), false, false, true);
			case kSelectionExtendToLineExclLF:         return range_t(move(buffer, min, kSelectionMoveToBeginOfLine,              layout), min.index != max.index && buffer.convert(max.index).column == 0 ? max : move(buffer, max, kSelectionMoveToEndOfLine, layout), false, false, true);
			case kSelectionExtendToParagraph:          return range_t(move(buffer, min, kSelectionMoveToBeginOfHardParagraph,     layout), move(buffer, max, kSelectionMoveToEndOfHardParagraph, layout), false, false, true);
			case kSelectionExtendToAll:                return range_t(move(buffer, min, kSelectionMoveToBeginOfDocument,          layout), move(buffer, max, kSelectionMoveToEndOfDocument,  layout), false, false, true);

			case kSelectionExtendToWord:
			{
				size_t from = min.index, to = max.index;
				size_t bol = buffer.begin(buffer.convert(from).line);
				size_t eol = buffer.eol(buffer.convert(to).line);

				std::string outerLeftType  = from == bol ? kCharacterClassUnknown : character_class(buffer, from-1);
				std::string innerLeftType  = from == eol ? kCharacterClassUnknown : character_class(buffer, from);
				std::string innerRightType = to == bol   ? kCharacterClassUnknown : character_class(buffer, to-1);
				std::string outerRightType = to == eol   ? kCharacterClassUnknown : character_class(buffer, to);

				bool extendLeft = false, extendRight = false;

				if(from == to) // no existing selection
				{
					if(outerLeftType == outerRightType)
						extendLeft = extendRight = true;
					else if(outerRightType == kCharacterClassWord || outerLeftType == kCharacterClassSpace || outerLeftType == kCharacterClassUnknown || (outerLeftType == kCharacterClassOther && outerRightType != kCharacterClassSpace && outerRightType != kCharacterClassUnknown))
						extendRight = true;
					else
						extendLeft = true;
				}
				else
				{
					if(outerLeftType == innerLeftType && innerRightType != outerRightType)
						extendLeft = true;
					else if(outerLeftType != innerLeftType && outerRightType == innerRightType)
						extendRight = true;
					else if(outerLeftType == innerLeftType && outerRightType == innerRightType)
						extendLeft = extendRight = true;
				}

				if(extendLeft)
				{
					while(bol < from && character_class(buffer, from-1) == outerLeftType)
						from -= buffer[from-1].size();
				}

				if(extendRight)
				{
					while(to < eol && character_class(buffer, to) == outerRightType)
						to += buffer[to].size();
				}

				return range_t(from, to, range.columnar, false, true);
			}
			break;

			case kSelectionExtendToTypingPair:
			{
				size_t from = begin_of_typing_pair(buffer, min.index, true);
				size_t to   = end_of_typing_pair(buffer, max.index, true);

				if(from != min.index && to != max.index)
				{
					size_t firstLine = buffer.convert(from).line, lastLine = buffer.convert(to).line;
					size_t bol = buffer.begin(firstLine), eol = buffer.end(lastLine);
					return firstLine != lastLine && all_whitespace(buffer, bol, from) && all_whitespace(buffer, to, eol) ? range_t(bol, eol, false, false, true) : range_t(from, to, false, false, true);
				}
			}
			break;

			case kSelectionExtendToScope:
			{
				size_t from = min.index, to = max.index;

				scope::scope_t leftScope  = buffer.scope(from, false).left;
				scope::scope_t rightScope = buffer.scope(to, false).right;
				if(leftScope == rightScope)
				{
					// D(DBF_TextView_Internal, bug("select both sides: %s\n", to_s(leftScope).c_str()););
					from = extend_scope_left(buffer, from, leftScope);
					to   = extend_scope_right(buffer, to, rightScope);
				}
				else if(from != to)
				{
					scope::scope_t innerLeftScope  = buffer.scope(from, false).right;
					scope::scope_t innerRightScope = buffer.scope(to, false).left;

					// D(DBF_TextView_Internal, bug("%s\n", to_s(leftScope).c_str()););
					// D(DBF_TextView_Internal, bug("%s\n", to_s(innerLeftScope).c_str()););
					// D(DBF_TextView_Internal, bug("%s\n", to_s(innerRightScope).c_str()););
					// D(DBF_TextView_Internal, bug("%s\n", to_s(rightScope).c_str()););

					scope::scope_t scope = shared_prefix(shared_prefix(leftScope, innerLeftScope), shared_prefix(rightScope, innerRightScope));
					// D(DBF_TextView_Internal, bug("→ %s\n", to_s(scope).c_str()););

					from = extend_scope_left(buffer, from, scope);
					to   = extend_scope_right(buffer, to, scope);
				}
				else if(leftScope.has_prefix(rightScope))
				{
					scope::scope_t scope = leftScope;
					for(leftScope.pop_scope(); leftScope != rightScope; leftScope.pop_scope())
						scope = leftScope;
					// D(DBF_TextView_Internal, bug("select left side: %s\n", to_s(scope).c_str()););
					from = extend_scope_left(buffer, from, scope);
				}
				else if(rightScope.has_prefix(leftScope))
				{
					scope::scope_t scope = rightScope;
					for(rightScope.pop_scope(); rightScope != leftScope; rightScope.pop_scope())
						scope = rightScope;
					// D(DBF_TextView_Internal, bug("select right side: %s\n", to_s(scope).c_str()););
					to = extend_scope_right(buffer, to, scope);
				}
				else if(from == to && buffer.convert(to).column == 0)
				{
					to = extend_scope_right(buffer, to, rightScope);
				}
				else
				{
					// D(DBF_TextView_Internal, bug("intersection: %s\n           != %s\n", to_s(leftScope).c_str(), to_s(rightScope).c_str()););
					scope::scope_t const& scope = shared_prefix(leftScope, rightScope);
					// D(DBF_TextView_Internal, bug("→ %s\n", to_s(scope).c_str()););
					from = extend_scope_left(buffer, from, scope);
					to   = extend_scope_right(buffer, to, scope);
				}
				return range_t(from, to, false, false, true);
			}
			break;
		}
		return range;
	}

	ranges_t extend (buffer_t const& buffer, ranges_t const& selection, select_unit_type const unit, layout_movement_t const* layout)
	{
		static std::set<select_unit_type> const splittingUnits = { kSelectionExtendToWord, kSelectionExtendToTypingPair, kSelectionExtendToScope, kSelectionExtendToEndOfSoftLine, kSelectionExtendToEndOfIndentedLine, kSelectionExtendToEndOfLine, kSelectionExtendToEndOfParagraph, kSelectionExtendToBeginOfTypingPair, kSelectionExtendToEndOfTypingPair };
		bool isColumnar    = selection.size() == 1 && selection.last().columnar;
		bool shouldDissect = isColumnar && splittingUnits.find(unit) != splittingUnits.end();

		ranges_t res;
		citerate(range, shouldDissect ? dissect_columnar(buffer, selection) : selection)
			res.push_back(extend(buffer, *range, unit, layout));
		return sanitize(buffer, res);
	}

	ranges_t extend_if_empty (buffer_t const& buffer, ranges_t const& selection, select_unit_type const unit, layout_movement_t const* layout)
	{
		ranges_t res;
		iterate(range, selection)
		{
			if(not_empty(buffer, *range))
			{
				res.push_back(*range);
			}
			else
			{
				citerate(r, dissect_columnar(buffer, *range))
				{
					range_t range = extend(buffer, *r, unit, layout);
					if(unit == kSelectionExtendToEndOfParagraph && range.empty())
						range = extend(buffer, range, kSelectionExtendRight, layout);
					res.push_back(range);
				}
			}
		}
		return sanitize(buffer, res);
	}

	// ================
	// = Select Scope =
	// ================

	static size_t extend_scope_left (buffer_t const& buffer, size_t caret, scope::selector_t const& scopeSelector)
	{
		while(caret && scopeSelector.does_match(buffer.scope(caret).left))
			caret -= buffer[caret-1].size();
		return caret;
	}

	static size_t extend_scope_right (buffer_t const& buffer, size_t caret, scope::selector_t const& scopeSelector)
	{
		while(caret < buffer.size() && scopeSelector.does_match(buffer.scope(caret).right))
			caret += buffer[caret].size();
		return caret;
	}

	static range_t select_scope (buffer_t const& buffer, range_t const& range, scope::selector_t const& scopeSelector)
	{
		return range_t(extend_scope_left(buffer, range.min().index, scopeSelector), extend_scope_right(buffer, range.max().index, scopeSelector));
	}

	ranges_t select_scope (buffer_t const& buffer, ranges_t const& selection, scope::selector_t const& scopeSelector)
	{
		ranges_t res;
		iterate(range, selection)
			res.push_back(select_scope(buffer, *range, scopeSelector));
		return sanitize(buffer, res);
	}

	// ================
	// = Obtain Scope =
	// ================
	
	scope::context_t scope (buffer_t const& buffer, ranges_t const& selection, std::string const& extraAttributes)
	{
		scope::context_t res;
		if(selection.size() == 1)
		{
			range_t const range = selection.last();
			if(range.empty())
					res = buffer.scope(range.last.index);
			else	res = shared_prefix(buffer.scope(range.min().index).right, buffer.scope(range.max().index).left);
		}
		else
		{
			scope::scope_t scope;
			citerate(range, selection)
			{
				scope::scope_t newScope = shared_prefix(buffer.scope(range->min().index).right, buffer.scope(range->max().index).left);
				scope = scope.empty() ? newScope : shared_prefix(scope, newScope);
			}
			res = scope;
		}

		if(!extraAttributes.empty() && extraAttributes != NULL_STR)
		{
			for(auto const& str : text::tokenize(extraAttributes.begin(), extraAttributes.end(), ' '))
			{
				res.left.push_scope(str);
				res.right.push_scope(str);
			}
		}

		if(selection.size() > 1)
		{
			res.left.push_scope("dyn.caret.mixed");
			res.right.push_scope("dyn.caret.mixed");
		}
		else if(selection.last().columnar)
		{
			res.left.push_scope("dyn.caret.mixed.columnar");
			res.right.push_scope("dyn.caret.mixed.columnar");
		}
		else
		{
			size_t const leftCaret  = selection.last().min().index;
			size_t const rightCaret = selection.last().max().index;

			if(leftCaret == 0)
				res.left.push_scope("dyn.caret.begin.document");
			else if(leftCaret == buffer.begin(buffer.convert(leftCaret).line))
				res.left.push_scope("dyn.caret.begin.line");

			if(rightCaret == buffer.size())
				res.right.push_scope("dyn.caret.end.document");
			else if(rightCaret == buffer.eol(buffer.convert(rightCaret).line))
				res.right.push_scope("dyn.caret.end.line");
		}

		if(not_empty(buffer, selection))
		{
			res.left.push_scope("dyn.selection");
			res.right.push_scope("dyn.selection");
		}

		return res;
	}

	// ====================
	// = Highlight Ranges =
	// ====================

	ranges_t highlight_ranges_for_movement (buffer_t const& buffer, ranges_t const& oldSelection, ranges_t const& newSelection)
	{
		ranges_t res;
		if(oldSelection.size() != newSelection.size())
			return res;

		for(auto oldRange = oldSelection.begin(), newRange = newSelection.begin(); oldRange != oldSelection.end(); ++oldRange, ++newRange)
		{
			if(*oldRange == *newRange || !oldRange->empty() || !newRange->empty())
				continue;

			size_t prevCaret = oldRange->last.index;
			size_t curCaret  = newRange->last.index;
			auto pairs       = character_pairs(buffer.scope(std::min(prevCaret, curCaret)), "highlightPairs");

			std::string didMatch;
			if(prevCaret < curCaret && prevCaret + buffer[prevCaret].size() == curCaret) // moved right, check end character
			{
				auto m = first_match(buffer, curCaret, pairs, &does_match_left);
				if(m && !m.matched_opener)
				{
					size_t from = begin_of_typing_pair(buffer, curCaret - m.match.size(), true);
					if(from != prevCaret && does_match_right(m.counterpart_ptrn, buffer, from, &didMatch))
						res.push_back(range_t(from, from + didMatch.size()));
				}
			}
			else if(curCaret < prevCaret && curCaret + buffer[curCaret].size() == prevCaret) // moved left, check begin character
			{
				auto m = first_match(buffer, curCaret, pairs, &does_match_right);
				if(m && m.matched_opener)
				{
					size_t to = end_of_typing_pair(buffer, curCaret + m.match.size(), true);
					if(to != prevCaret && does_match_left(m.counterpart_ptrn, buffer, to, &didMatch))
						res.push_back(range_t(to - didMatch.size(), to));
				}
			}
		}

		return res;
	}

	// ========
	// = Find =
	// ========

	static bool is_subset (range_t const& range, ranges_t const& ranges)
	{
		iterate(r, ranges)
		{
			if(r->min() <= range.min() && range.max() <= r->max())
				return true;
		}
		return ranges.empty();
	}

	std::map< range_t, std::map<std::string, std::string> > find_all (buffer_t const& buffer, std::string const& searchFor, find::options_t options, ranges_t const& searchRanges)
	{
		std::map< range_t, std::map<std::string, std::string> > res;
		if(searchFor == NULL_STR || searchFor == "")
			return res;

		ranges_t ranges = dissect_columnar(buffer, searchRanges);
		find::find_t f(searchFor, (find::options_t)(options & ~find::backwards));

		ssize_t total = 0;
		iterate(memory, buffer)
		{
			char const* buf = (*memory).data();
			size_t len      = (*memory).size();

			for(ssize_t offset = 0; offset < len; )
			{
				std::map<std::string, std::string> captures;
				std::pair<ssize_t, ssize_t> const& m = f.match(buf + offset, len - offset, &captures);
				if(m.first <= m.second)
				{
					range_t r(total + offset + m.first, total + offset + m.second, false, false, true);
					if(is_subset(r, ranges))
						res.emplace(r, captures);
				}
				ASSERT_NE(m.second, 0); ASSERT_LE(m.second, len - offset);
				offset += m.second;
			}
			total += len;
		}

		std::map<std::string, std::string> captures;
		std::pair<ssize_t, ssize_t> m = f.match(NULL, 0, &captures);
		while(m.first <= m.second)
		{
			range_t r(total + m.first, total + m.second, false, false, true);
			if(is_subset(r, ranges))
				res.emplace(r, captures);
			captures.clear();
			m = f.match(NULL, 0, &captures);
		}

		return res;
	}

	static std::map< range_t, std::map<std::string, std::string> > regexp_find (buffer_t const& buffer, std::string const& searchFor, find::options_t options, ng::range_t const& range)
	{
		size_t first = (options & find::backwards) ? range.min().index : range.max().index;
		size_t last  = (options & find::backwards) ?                 0 :     buffer.size();

		std::map< range_t, std::map<std::string, std::string> > res;

		OnigOptionType ptrnOptions = ONIG_OPTION_NONE;
		if(options & find::ignore_case)
			ptrnOptions |= ONIG_OPTION_IGNORECASE;

		std::string str = buffer.substr(0, buffer.size());
		if(regexp::match_t m = search(regexp::pattern_t(searchFor, ptrnOptions), str.data(), str.data() + str.size(), str.data() + first, str.data() + last))
		{
			if(range.sorted() == ng::range_t(m.begin(), m.end()))
			{
				if(options & find::backwards)
				{
					if(0 < first)
						first -= buffer[first-1].size();
				}
				else
				{
					if(first < str.size())
						first += buffer[first].size();
				}
				m = search(regexp::pattern_t(searchFor, ptrnOptions), str.data(), str.data() + str.size(), str.data() + first, str.data() + last);
			}

			if(m && range.sorted() != ng::range_t(m.begin(), m.end()))
				res.emplace(ng::range_t(m.begin(), m.end()), m.captures());
		}

		return res;
	}

	std::map< range_t, std::map<std::string, std::string> > find (buffer_t const& buffer, ranges_t const& selection, std::string const& searchFor, find::options_t options, ranges_t const& searchRanges, bool* didWrap)
	{
		auto setDidWrap = [&didWrap](bool flag){ if(didWrap != nullptr) *didWrap = flag; };
		setDidWrap(false);

		std::map< range_t, std::map<std::string, std::string> > res;
		if(searchFor == NULL_STR || searchFor == "")
			return res;

		if(selection.size() == 1 && (options & (find::regular_expression|find::wrap_around|find::all_matches|find::extend_selection)) == find::regular_expression)
			return regexp_find(buffer, searchFor, options, selection.last());

		auto tmp = find_all(buffer, searchFor, options, searchRanges);
		if(options & find::all_matches)
			return tmp;

		if(options & find::extend_selection)
		{
			ng::index_t anchor(options & find::backwards ? buffer.size() : 0);
			iterate(range, selection)
			{
				anchor = options & find::backwards ? std::min(range->min(), anchor) : std::max(range->max(), anchor);
				res.emplace(*range, std::map<std::string, std::string>());
			}

			if(options & find::backwards)
			{
				auto it = tmp.lower_bound(anchor);
				if(it != tmp.begin())
					res.insert(*--it);
			}
			else
			{
				auto it = tmp.upper_bound(anchor);
				if(it != tmp.end())
					res.insert(*it);
			}

			return res;
		}

		if(tmp.empty())
			return tmp;

		for(auto const& range : dissect_columnar(buffer, selection))
		{
			if(options & find::backwards)
			{
				auto it = std::lower_bound(tmp.begin(), tmp.end(), range, [](decltype(tmp)::value_type const& candidate, ng::range_t const& range){
					return candidate.first.empty() ? candidate.first.max() < range.min() : candidate.first.max() <= range.min();
				});

				if(it == tmp.begin())
				{
					if(options & find::wrap_around)
					{
						it = --tmp.end();
						if(range.sorted() == it->first.sorted())
							continue;
						else if(!(range.min() <= it->first.min() && it->first.max() <= range.max()))
							setDidWrap(true);
					}
					else
					{
						it = std::upper_bound(tmp.begin(), tmp.end(), range, [](ng::range_t const& range, decltype(tmp)::value_type const& candidate){
							return range.max() < candidate.first.max();
						});

						if(it == tmp.begin())
							continue;
						--it;
					}
				}
				else
				{
					--it;
				}

				if(range.sorted() != it->first.sorted())
					res.insert(*it);
			}
			else
			{
				auto it = std::upper_bound(tmp.begin(), tmp.end(), range, [](ng::range_t const& range, decltype(tmp)::value_type const& candidate){
					return candidate.first.empty() ? range.max() < candidate.first.min() : range.max() <= candidate.first.min();
				});

				if(it == tmp.end())
				{
					if(options & find::wrap_around)
					{
						it = tmp.begin();
						if(range.sorted() == it->first.sorted())
							continue;
						else if(!(range.min() <= it->first.min() && it->first.max() <= range.max()))
							setDidWrap(true);
					}
					else
					{
						it = std::upper_bound(tmp.begin(), tmp.end(), range, [](ng::range_t const& range, decltype(tmp)::value_type const& candidate){
							return range.min() <= candidate.first.min();
						});

						if(it == tmp.end())
							continue;
					}
				}

				if(range.sorted() != it->first.sorted())
					res.insert(*it);
			}
		}

		return res;
	}

	// =============
	// = All Words =
	// =============

	ranges_t all_words (buffer_t const& buffer)
	{
		ranges_t res;

		std::string charType = kCharacterClassOther;
		size_t from = 0;
		for(size_t i = 0; i < buffer.size(); )
		{
			std::string newCharType = character_class(buffer, i);
			if(charType != newCharType)
			{
				if(charType != kCharacterClassSpace && charType != kCharacterClassOther && from != i)
					res.push_back(range_t(from, i));
				charType = newCharType;
				from = i;
			}
			i += buffer[i].size();
		}

		if(charType != kCharacterClassSpace && charType != kCharacterClassOther && from != buffer.size())
			res.push_back(range_t(from, buffer.size()));

		return res;
	}

	// ==================
	// = to/from string =
	// ==================

	ranges_t from_string (buffer_t const& buffer, std::string const& str)
	{
		ranges_t res;
		citerate(range, text::selection_t(str))
			res.push_back(range_t(index_t(buffer.convert(range->from), range->from.offset), index_t(buffer.convert(range->to), range->to.offset), range->columnar, range->from.offset || range->to.offset));
		return res;
	}

	std::string to_s (buffer_t const& buffer, ranges_t const& ranges)
	{
		text::selection_t res;
		iterate(range, ranges)
		{
			text::pos_t from = buffer.convert(range->first.index), to = buffer.convert(range->last.index);
			from.offset = range->freehanded ? range->first.carry : 0;
			to.offset   = range->freehanded ? range->last.carry  : 0;
			res.push_back(text::range_t(from, to, range->columnar));
		}
		return res;
	}

} /* ng */
