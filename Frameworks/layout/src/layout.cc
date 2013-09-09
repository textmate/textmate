#include "layout.h"
#include "render.h"
#include "ct.h"
#include "folds.h"
#include "paragraph.h"
#include <cf/cf.h>
#include <cf/cgrect.h>
#include <text/parse.h>
#include <text/utf8.h>
#include <text/ctype.h>
#include <oak/debug.h>

OAK_DEBUG_VAR(Layout);

// ====================
// = Helper Functions =
// ====================

static CGRect OakRectMake (CGFloat x, CGFloat y, CGFloat w, CGFloat h)
{
	return CGRectMake(round(x), round(y), round(x+w) - round(x), round(y+h) - round(y));
}

static size_t count_columns (ng::buffer_t const& buf, size_t from, size_t to)
{
	size_t const tabSize = buf.indent().tab_size();
	size_t col = 0;
	std::string const str = buf.substr(from, to);
	for(auto ch : diacritics::make_range(str.data(), str.data() + str.size()))
		col += (ch == '\t' ? tabSize - (col % tabSize) : (text::is_east_asian_width(ch) ? 2 : 1));
	return col;
}

namespace ng
{
	// ============
	// = layout_t =
	// ============

	layout_t::layout_t (ng::buffer_t& buffer, theme_ptr const& theme, bool softWrap, bool scrollPastEnd, size_t wrapColumn, std::string const& folded, ng::layout_t::margin_t const& margin) : _folds(std::make_shared<folds_t>(buffer)), _buffer(buffer), _theme(theme), _tab_size(buffer.indent().tab_size()), _wrapping(softWrap), _scroll_past_end(scrollPastEnd), _wrap_column(wrapColumn), _margin(margin)
	{
		struct parser_callback_t : ng::callback_t
		{
			parser_callback_t (layout_t& layout) : layout(layout) { }
			void did_replace (size_t from, size_t to, std::string const& str) { layout.did_erase(from, to); layout.did_insert(from, from + str.size()); }

		private:
			layout_t& layout;
		};

		setup_font_metrics();

		_rows.insert(_rows.end(), row_key_t(0, default_line_height()));
		_folds->set_folded_as_string(folded);
		did_insert(0, _buffer.size());

		_buffer_callback = new parser_callback_t(*this);
		_buffer.add_callback(_buffer_callback);
	}

	layout_t::~layout_t ()
	{
		_buffer.remove_callback(_buffer_callback);
		delete _buffer_callback;
	}

	void layout_t::setup_font_metrics ()
	{
		_metrics = std::make_shared<ct::metrics_t>(_theme->font_name(), _theme->font_size());
	}

	void layout_t::clear_text_widths ()
	{
		iterate(row, _rows)
		{
			row->value.reset_font_metrics(*_metrics);
			update_row(row);
		}
		_dirty_rects.push_back(OakRectMake(0, 0, width(), height()));
	}

	// ============
	// = Settings =
	// ============

	void layout_t::set_theme (theme_ptr const& theme)
	{
		_theme = theme;
		clear_text_widths();
	}

	void layout_t::set_font (std::string const& fontName, CGFloat fontSize)
	{
		if(fontName == _theme->font_name() && fontSize == _theme->font_size())
			return;
		_theme = _theme->copy_with_font_name_and_size(fontName, fontSize);
		setup_font_metrics();
		clear_text_widths();
	}

	void layout_t::set_character_mapping (std::string const& invisibles)
	{
		enum state_t { kWaiting, kExclude, kSpace, kTab, kNewline } state = kWaiting;
		for(auto ch : diacritics::make_range(invisibles.data(), invisibles.data() + invisibles.size()))
		{
			if(state == kWaiting)
			{
				switch(ch)
				{
					case '~':  state = kExclude; break;
					case ' ':  state = kSpace;   break;
					case '\t': state = kTab;     break;
					case '\n': state = kNewline; break;
				}
			}
			else
			{
				switch(state)
				{
					case kExclude:
					{
						switch(ch)
						{
							case ' ':  _invisibles.space   = ""; break;
							case '\t': _invisibles.tab     = ""; break;
							case '\n': _invisibles.newline = ""; break;
						}
					}
					break;

					case kSpace:   _invisibles.space   = utf8::to_s(ch); break;
					case kTab:     _invisibles.tab     = utf8::to_s(ch); break;
					case kNewline: _invisibles.newline = utf8::to_s(ch); break;
				}
				state = kWaiting;
			}
		}
	}

	void layout_t::set_tab_size (size_t tabSize)
	{
		if(tabSize == _tab_size)
			return;
		_tab_size = tabSize;
		iterate(row, _rows)
		{
			row->value.set_tab_size(tabSize, *_metrics);
			update_row(row);
		}
		_dirty_rects.push_back(OakRectMake(0, 0, width(), height()));
	}

	void layout_t::set_margin (margin_t const& margin)
	{
		_dirty_rects.push_back(OakRectMake(0, 0, width(), height()));
		_margin = margin;
		_dirty_rects.push_back(OakRectMake(0, 0, width(), height()));
	}

	void layout_t::set_wrapping (bool softWrap, size_t wrapColumn)
	{
		if(_wrapping == softWrap && _wrap_column == wrapColumn)
			return;

		_wrapping    = softWrap;
		_wrap_column = wrapColumn;

		iterate(row, _rows)
		{
			row->value.set_wrapping(effective_soft_wrap(row), effective_wrap_column(), *_metrics);
			update_row(row);
		}

		_dirty_rects.push_back(OakRectMake(0, 0, width(), height()));
	}
	
	void layout_t::set_scroll_past_end (bool scrollPastEnd)
	{
		_scroll_past_end = scrollPastEnd;
	}


	// ======================
	// = Display Attributes =
	// ======================

	void layout_t::set_is_key (bool isKey)
	{
		if(_is_key == isKey)
			return;
		_is_key = isKey;
		_dirty_rects.insert(_dirty_rects.end(), _pre_refresh_selections.begin(), _pre_refresh_selections.end());
	}

	void layout_t::set_draw_caret (bool drawCaret)
	{
		_draw_caret = drawCaret;
	}

	void layout_t::set_draw_wrap_column (bool drawWrapColumn)
	{
		_draw_wrap_column = drawWrapColumn;
		_dirty_rects.push_back(OakRectMake(0, 0, width(), height()));
	}

	void layout_t::set_drop_marker (ng::index_t dropMarkerIndex)
	{
		if(dropMarkerIndex == _drop_marker)
			return;

		if(_drop_marker)
			_dirty_rects.push_back(rect_at_index(_drop_marker));
		_drop_marker = dropMarkerIndex;
		if(_drop_marker)
			_dirty_rects.push_back(rect_at_index(_drop_marker));
	}

	void layout_t::set_viewport_size (CGSize size)
	{
		if(CGSizeEqualToSize(size, _viewport_size))
			return;

		size_t oldWrapColumn = effective_wrap_column();
		_viewport_size = size;
		if(oldWrapColumn != effective_wrap_column())
		{
			iterate(row, _rows)
			{
				if(effective_soft_wrap(row))
				{
					row->value.set_wrapping(true, effective_wrap_column(), *_metrics);
					update_row(row);
				}
			}
			_dirty_rects.push_back(OakRectMake(0, 0, width(), height()));
		}
	}

	// ======================

	void layout_t::did_fold (size_t from, size_t to)
	{
		did_erase(from, to);
		did_insert(from, to);
	}

	// ==================
	// = Node Placement =
	// ==================

	layout_t::row_tree_t::iterator layout_t::row_for_offset (size_t i) const
	{
		auto rowIter = _rows.upper_bound(i, &row_offset_comp);
		if(rowIter != _rows.begin())
			--rowIter;
		return rowIter;
	}

	CGFloat layout_t::default_line_height (CGFloat minAscent, CGFloat minDescent, CGFloat minLeading) const
	{
		return _metrics->line_height(minAscent, minDescent, minLeading);
	}

	CGRect layout_t::rect_for (row_tree_t::iterator rowIter) const
	{
		return CGRectMake(0, _margin.top + rowIter->offset._height, width(), rowIter->key._height);
	}

	CGRect layout_t::full_width (CGRect const& rect) const
	{
		return OakRectMake(0, CGRectGetMinY(rect), width(), CGRectGetHeight(rect));
	}

	CGRect layout_t::full_height (CGRect const& rect) const
	{
		return OakRectMake(CGRectGetMinX(rect), CGRectGetMinY(rect), CGRectGetWidth(rect), height() - CGRectGetMinY(rect));
	}

	bool layout_t::effective_soft_wrap (row_tree_t::iterator rowIter) const
	{
		bundles::item_ptr softWrapItem;
		scope::context_t scope(_buffer.scope(rowIter->offset._length, false).right, _buffer.scope(rowIter->offset._length + rowIter->key._length, false).left);
		plist::any_t const& softWrapValue = bundles::value_for_setting("softWrap", scope, &softWrapItem);
		return softWrapItem ? plist::is_true(softWrapValue) : _wrapping;
	}

	size_t layout_t::effective_wrap_column () const
	{
		return _wrap_column > 0 ? _wrap_column : std::max(floor((_viewport_size.width - _margin.left - _margin.right) / _metrics->column_width()), 10.0);
	}

	// ================
	// = Measurements =
	// ================

	CGRect layout_t::rect_at_index (ng::index_t const& index) const
	{
		auto row = row_for_offset(index.index);
		return row->value.rect_at_index(index, *_metrics, _buffer, row->offset._length, CGPointMake(_margin.left, _margin.top + row->offset._height));
	}

	CGRect layout_t::rect_for_range (size_t first, size_t last) const
	{
		ASSERT_LE(first, last);

		auto r1 = rect_at_index(first);
		auto r2 = rect_at_index(last);
		auto res = CGRectZero;

		if(CGRectGetMinY(r1) == CGRectGetMinY(r2) && CGRectGetHeight(r1) == CGRectGetHeight(r2))
		{
			CGFloat x1 = CGRectGetMinX(r1), x2 = CGRectGetMinX(r2)-1;
			res = OakRectMake(x1, CGRectGetMinY(r1), x2 - x1, CGRectGetHeight(r1));
		}
		else
		{
			CGFloat width = CGRectGetMinX(r2)-1 - _margin.left;

			auto firstIter = row_for_offset(first), lastIter = row_for_offset(last);
			for(auto it = firstIter; ; ++it)
			{
				width = std::max(width, it->value.width()); // TODO paragraph_t::width covers the entire paragraph which is too much when wrapped or when “last” is not at end of the paragraph.
				if(it == lastIter)
					break;
			}

			res = OakRectMake(_margin.left, CGRectGetMinY(r1), width, CGRectGetMaxY(r2) - CGRectGetMinY(r1));
		}
		return res;
	}

	std::vector<CGRect> layout_t::rects_for_ranges (ng::ranges_t const& ranges, kRectsIncludeMode mode) const
	{
		std::vector<CGRect> res;
		for(auto const& range : ng::dissect_columnar(_buffer, ranges))
		{
			if(mode == kRectsIncludeCarets && !range.empty() || mode == kRectsIncludeSelections && range.empty())
				continue;

			bool includeCarry = range.freehanded;
			auto r1 = rect_at_index(ng::index_t(range.first.index, includeCarry ? range.first.carry : 0));
			auto r2 = rect_at_index(ng::index_t(range.last.index,  includeCarry ? range.last.carry  : 0));

			if(CGRectEqualToRect(r1, r2))
			{
				res.push_back(OakRectMake(r1.origin.x, r1.origin.y, 1, r1.size.height));
			}
			else
			{
				auto firstRowIter = row_for_offset(range.first.index), lastRowIter = row_for_offset(range.last.index);
				if(CGRectGetMinY(r1) == CGRectGetMinY(r2) && CGRectGetHeight(r1) == CGRectGetHeight(r2))
				{
					res.push_back(OakRectMake(std::min(r1.origin.x, r2.origin.x), r1.origin.y, fabs(r2.origin.x - r1.origin.x), r1.size.height));
				}
				else
				{
					if(r2.origin.y < r1.origin.y)
					{
						std::swap(firstRowIter, lastRowIter);
						std::swap(r1, r2);
					}

					res.push_back(OakRectMake(r1.origin.x, r1.origin.y, content_width() - r1.origin.x + _margin.left, r1.size.height));
					res.push_back(OakRectMake(_margin.left, CGRectGetMaxY(r1), content_width(), CGRectGetMinY(r2) - CGRectGetMaxY(r1)));
					res.push_back(OakRectMake(_margin.left, r2.origin.y, r2.origin.x - _margin.left, r2.size.height));
				}
			}
		}
		return res;
	}

	ng::index_t layout_t::index_at_point (CGPoint point) const
	{
		CGFloat clickedY = point.y - _margin.top;
		auto rowIter = _rows.upper_bound(clickedY, &row_y_comp);
		if(rowIter != _rows.begin())
			--rowIter;

		if(clickedY < rowIter->offset._height)
			return rowIter->offset._length;
		else if(clickedY < rowIter->offset._height + rowIter->key._height)
			return rowIter->value.index_at_point(point, *_metrics, _buffer, rowIter->offset._length, CGPointMake(_margin.left, _margin.top + rowIter->offset._height));
		else
			return rowIter->offset._length + rowIter->key._length;
	}

	ng::line_record_t layout_t::line_record_for (CGFloat y) const
	{
		size_t index = index_at_point(CGPointMake(0, y)).index;
		auto row = row_for_offset(index);
		if(row != _rows.end())
			return row->value.line_record_for(_buffer.convert(index).line, index, *_metrics, _buffer, row->offset._length, CGPointMake(_margin.left, _margin.top + row->offset._height));
		return ng::line_record_t(0, 0, 0, 0, 0);
	}

	ng::line_record_t layout_t::line_record_for (text::pos_t const& pos) const
	{
		size_t n = std::min(pos.line, _buffer.lines()-1);
		size_t index = _buffer.convert(text::pos_t(n, 0)) + std::min(pos.column, _buffer.end(n) - _buffer.begin(n));
		auto row = row_for_offset(index);
		if(row != _rows.end())
			return row->value.line_record_for(n, index, *_metrics, _buffer, row->offset._length, CGPointMake(_margin.left, _margin.top + row->offset._height));
		return ng::line_record_t(0, 0, 0, 0, 0);
	}

	CGFloat layout_t::width () const  { return _margin.left + content_width() + _margin.right; }
	CGFloat layout_t::height () const { return _margin.top + content_height() + _margin.bottom + (_scroll_past_end ? std::min(_rows.aggregated()._height, _viewport_size.height) - default_line_height() * 1.5 : 0); }

	// ===================
	// = Updating Layout =
	// ===================

	bool layout_t::update_row (row_tree_t::iterator rowIter)
	{
		CGFloat oldHeight = rowIter->key._height;
		rowIter->key._length = rowIter->value.length();
		rowIter->key._width  = rowIter->value.width();
		rowIter->key._height = rowIter->value.height(*_metrics);

		_rows.update_key(rowIter);
		return oldHeight != rowIter->key._height;
	}

	void layout_t::update_metrics (CGRect visibleRect)
	{
		CGFloat const yMin = CGRectGetMinY(visibleRect) - _margin.top;
		CGFloat const yMax = CGRectGetMaxY(visibleRect) - _margin.top;

		auto firstY = _rows.upper_bound(yMin, &row_y_comp);
		if(firstY != _rows.begin())
			--firstY;

		foreach(row, firstY, _rows.lower_bound(yMax, &row_y_comp))
		{
			if(row->value.layout(_theme, effective_soft_wrap(row), effective_wrap_column(), *_metrics, visibleRect, _buffer, row->offset._length))
			{
				bool didUpdateHeight = update_row(row);
				if(_refresh_counter)
				{
					CGRect lineRect = full_width(rect_for(row));
					_dirty_rects.push_back(full_width(didUpdateHeight ? full_height(lineRect) : lineRect));
				}
			}
		}
	}

	// ================
	// = Insert/erase =
	// ================

	void layout_t::did_erase (size_t from, size_t to)
	{
		D(DBF_Layout, bug("%zu-%zu\n", from, to););
		ASSERT_LE(from, to);
		if(from == to)
			return;

		auto fromRow = row_for_offset(from);
		auto toRow   = row_for_offset(to);

		bool fullRefresh = false;
		if(fromRow == toRow)
		{
			fromRow->value.erase(from, to, _buffer, fromRow->offset._length);
			fullRefresh = update_row(fromRow) || fullRefresh;
		}
		else
		{
			size_t base = fromRow->offset._length;
			size_t prefixLenToErase  = to   - toRow->offset._length;
			size_t prefixLenToInsert = from - fromRow->offset._length;
			toRow->value.erase(base, base + prefixLenToErase, _buffer, base);
			toRow->value.insert(base, prefixLenToInsert, _buffer, base);
			update_row(toRow);
			_rows.erase(fromRow, toRow);

			std::vector< std::pair<size_t, size_t> > foldedRanges;
			ssize_t nestCount = 0;
			for(auto const& pair : _folds->folded())
			{
				if(pair.second && ++nestCount == 1)
					foldedRanges.push_back(std::make_pair(pair.first, pair.first));
				else if(!pair.second && --nestCount == 0)
					foldedRanges.back().second = pair.first;
			}

			size_t insertFrom = base, insertTo = base + prefixLenToInsert;
			for(auto const& range : foldedRanges)
			{
				D(DBF_Layout, bug("range: %zu-%zu\n", range.first, range.second););
				if(range.second <= insertFrom || insertTo <= range.first)
					continue;

				did_erase(range.first, range.second);
				auto row = row_for_offset(range.first);
				row->value.insert_folded(range.first, range.second - range.first, _buffer, row->offset._length);
				fullRefresh = update_row(row) || fullRefresh;
			}

			fullRefresh = true;
		}
		refresh_line_at_index(from, fullRefresh);
	}

	void layout_t::did_insert (size_t first, size_t last)
	{
		D(DBF_Layout, bug("%zu-%zu\n", first, last););
		ASSERT_LE(first, last);
		if(first == last)
			return;

		bool fullRefresh = false;
		auto row = row_for_offset(first);

		size_t suffixLen = 0;
		if(_buffer.convert(first).line != _buffer.convert(last).line)
		{
			suffixLen = row->offset._length + row->value.length() - first;
			D(DBF_Layout, bug("multi-line insert, erase %zu-%zu\n", first, first + suffixLen););
			row->value.erase(first, first + suffixLen, _buffer, row->offset._length);
		}

		for(size_t pos = first; pos != last; )
		{
			size_t eol = std::min(_buffer.eol(_buffer.convert(pos).line), last);
			row->value.insert(pos, eol - pos + (eol != last ? 1 : 0), _buffer, row->offset._length);
			fullRefresh = update_row(row) || fullRefresh;
			if(eol != last)
			{
				row = _rows.insert(++row, row_key_t(0, default_line_height()));
				++eol;
				fullRefresh = true;
			}
			pos = eol;
		}

		if(suffixLen)
		{
			D(DBF_Layout, bug("multi-line insert, re-insert %zu-%zu\n", last, last + suffixLen););
			row->value.insert(last, suffixLen, _buffer, row->offset._length);
			fullRefresh = update_row(row) || fullRefresh;
		}

		// ===================
		// = Update foldings =
		// ===================

		std::vector< std::pair<size_t, size_t> > foldedRanges;
		ssize_t nestCount = 0;
		for(auto const& pair : _folds->folded())
		{
			if(pair.second && ++nestCount == 1)
				foldedRanges.push_back(std::make_pair(pair.first, pair.first));
			else if(!pair.second && --nestCount == 0)
				foldedRanges.back().second = pair.first;
		}

		for(auto const& range : foldedRanges)
		{
			D(DBF_Layout, bug("folded range: %zu-%zu\n", range.first, range.second););
			if(range.second <= first || last + suffixLen <= range.first)
				continue;

			did_erase(range.first, range.second);
			auto row = row_for_offset(range.first);
			row->value.insert_folded(range.first, range.second - range.first, _buffer, row->offset._length);
			fullRefresh = update_row(row) || fullRefresh;
		}

		refresh_line_at_index(first, fullRefresh);
	}

	// =======================
	// = Modified Rectangles =
	// =======================

	void layout_t::begin_refresh_cycle (ng::ranges_t const& selection, ng::ranges_t const& highlightRanges)
	{
		if(++_refresh_counter == 1)
		{
			_pre_refresh_carets     = _draw_caret && !_drop_marker ? rects_for_ranges(selection, kRectsIncludeCarets) : std::vector<CGRect>();
			_pre_refresh_selections = rects_for_ranges(selection, kRectsIncludeSelections);
			_pre_refresh_revision   = _buffer.revision();
			_pre_refresh_caret      = selection.last().last.index;

			for(auto const& range : highlightRanges)
			{
				CGRect const r = rect_for_range(range.min().index, range.max().index);
				OakRectDifference(CGRectInset(r, -2, -2), CGRectInset(r, -1, -1), back_inserter(_pre_refresh_highlight_border));
				_pre_refresh_highlight_interior.push_back(CGRectInset(r, -1, -1));
			}
		}
	}

	static std::pair<size_t, size_t> misspelled_range_at (ng::buffer_t const& buffer, size_t index)
	{
		size_t line = buffer.convert(index).line;
		size_t from = buffer.begin(line);
		size_t to   = buffer.eol(line);

		auto map = buffer.misspellings(from, to);
		auto it  = map.upper_bound(index - from);
		if(it != map.begin())
			--it;
		if(it != map.begin() && !it->second)
			--it;

		if(it != map.end())
		{
			size_t first = from + it->first;
			size_t last  = ++it != map.end() ? from + it->first : to;
			if(oak::cap(first, index, last) == index)
				return std::make_pair(first, last);
		}
		return std::make_pair(0, 0);
	}

	std::vector<CGRect> layout_t::end_refresh_cycle (ng::ranges_t const& selection, CGRect visibleRect, ng::ranges_t const& highlightRanges)
	{
		std::vector<CGRect> res;
		if(_refresh_counter == 1)
		{
			set_tab_size(_buffer.indent().tab_size());
			update_metrics(visibleRect);

			if(_pre_refresh_revision == _buffer.revision() && _pre_refresh_caret != selection.last().last.index)
			{
				std::pair<size_t, size_t> ranges[] = { misspelled_range_at(_buffer, _pre_refresh_caret), misspelled_range_at(_buffer, selection.last().last.index) };
				if(ranges[0] != ranges[1])
				{
					for(auto range : ranges)
					{
						if(range.first != range.second)
							_dirty_rects.push_back(full_width(rect_for_range(range.first, range.second))); // We extend to full width since the spelling underline may extend a few pixels beyond the last character
					}
				}
			}

			auto postCarets     = _draw_caret && !_drop_marker ? rects_for_ranges(selection, kRectsIncludeCarets) : std::vector<CGRect>();
			auto postSelections = rects_for_ranges(selection, kRectsIncludeSelections);

			OakRectSymmetricDifference(_pre_refresh_carets,     postCarets,     back_inserter(_dirty_rects));
			OakRectSymmetricDifference(_pre_refresh_selections, postSelections, back_inserter(_dirty_rects));

			std::vector<CGRect> postHighlightBorder;
			std::vector<CGRect> postHighlightInterior;
			for(auto const& range : highlightRanges)
			{
				CGRect const r = rect_for_range(range.min().index, range.max().index);
				OakRectDifference(CGRectInset(r, -2, -2), CGRectInset(r, -1, -1), back_inserter(postHighlightBorder));
				postHighlightInterior.push_back(CGRectInset(r, -1, -1));
			}

			OakRectSymmetricDifference(_pre_refresh_highlight_border,   postHighlightBorder,   back_inserter(_dirty_rects));
			OakRectSymmetricDifference(_pre_refresh_highlight_interior, postHighlightInterior, back_inserter(_dirty_rects));

			_pre_refresh_carets.clear();
			_pre_refresh_selections.clear();
			_pre_refresh_highlight_border.clear();
			_pre_refresh_highlight_interior.clear();
			res.swap(_dirty_rects);
		}
		--_refresh_counter;
		return res;
	}

	void layout_t::refresh_line_at_index (size_t index, bool fullRefresh)
	{
		if(_refresh_counter)
		{
			auto row = row_for_offset(index);
			CGRect lineRefreshRect = full_width(rect_for(row));
			_dirty_rects.push_back(fullRefresh ? full_height(lineRefreshRect) : lineRefreshRect);
		}
	}

	void layout_t::did_update_scopes (size_t from, size_t to)
	{
		foreach(row, _rows.lower_bound(from, &row_offset_comp), _rows.lower_bound(to, &row_offset_comp))
		{
			row->value.did_update_scopes(from, to, _buffer, row->offset._length);
			refresh_line_at_index(row->offset._length, false);
		}
	}

	// ============
	// = Movement =
	// ============

	static index_t advance (buffer_t const& buffer, size_t caret, size_t column, size_t eol)
	{
		size_t const tabSize = buffer.indent().tab_size();
		size_t len = 0;
		std::string const str = buffer.substr(caret, eol);
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

	ng::index_t layout_t::index_above (ng::index_t const& index) const
	{
		CGRect r = rect_at_index(index);
		ng::index_t const indexAbove = index_at_point(CGPointMake(CGRectGetMinX(r), CGRectGetMinY(r)-1));
		if(_buffer.convert(index.index).line == 0)
			return indexAbove;

		ng::index_t const bol      = std::max(_buffer.begin(_buffer.convert(index.index).line),      index_at_bol_for(index).index);
		ng::index_t const bolAbove = std::max(_buffer.begin(_buffer.convert(indexAbove.index).line), index_at_bol_for(indexAbove).index);
		ng::index_t const eolAbove = index_at_eol_for(bolAbove).index;
		return advance(_buffer, bolAbove.index, count_columns(_buffer, bol.index, index.index) + index.carry, eolAbove.index);
	}

	ng::index_t layout_t::index_right_of (ng::index_t const& index) const
	{
		auto row = row_for_offset(index.index);
		size_t res = row->value.index_right_of(index.index, _buffer, row->offset._length);
		if(res == index.index && res != _buffer.size())
			res += _buffer[res].size();;
		return res;
	}

	ng::index_t layout_t::index_below (ng::index_t const& index) const
	{
		CGRect r = rect_at_index(index);
		ng::index_t const indexBelow = index_at_point(CGPointMake(CGRectGetMinX(r), CGRectGetMaxY(r)));
		if(_buffer.convert(index.index).line+1 == _buffer.lines())
			return indexBelow;

		ng::index_t const bol      = std::max(_buffer.begin(_buffer.convert(index.index).line),      index_at_bol_for(index).index);
		ng::index_t const bolBelow = std::max(_buffer.begin(_buffer.convert(indexBelow.index).line), index_at_bol_for(indexBelow).index);
		ng::index_t const eolBelow = index_at_eol_for(bolBelow).index;
		return advance(_buffer, bolBelow.index, count_columns(_buffer, bol.index, index.index) + index.carry, eolBelow.index);
	}

	ng::index_t layout_t::index_left_of (ng::index_t const& index) const
	{
		auto row = row_for_offset(index.index);
		size_t res = row->value.index_left_of(index.index, _buffer, row->offset._length);
		if(res == index.index && res != 0)
			res -= _buffer[res-1].size();;
		return res;
	}

	ng::index_t layout_t::index_at_bol_for (ng::index_t const& index) const
	{
		auto row = row_for_offset(index.index);
		return row->value.bol(index.index, _buffer, row->offset._length);
	}

	ng::index_t layout_t::index_at_eol_for (ng::index_t const& index) const
	{
		auto row = row_for_offset(index.index);
		return row->value.eol(index.index, _buffer, row->offset._length);
	}

	ng::index_t layout_t::page_up_for (index_t const& index) const
	{
		CGRect r = rect_at_index(index);
		ng::index_t const indexPageUp = index_at_point(CGPointMake(CGRectGetMinX(r), CGRectGetMinY(r) - _viewport_size.height));
		if(_buffer.convert(index.index).line == 0)
			return indexPageUp;

		ng::index_t const bol       = std::max(_buffer.begin(_buffer.convert(index.index).line),       index_at_bol_for(index).index);
		ng::index_t const bolPageUp = std::max(_buffer.begin(_buffer.convert(indexPageUp.index).line), index_at_bol_for(indexPageUp).index);
		ng::index_t const eolPageUp = index_at_eol_for(bolPageUp).index;
		return advance(_buffer, bolPageUp.index, count_columns(_buffer, bol.index, index.index) + index.carry, eolPageUp.index);
	}

	ng::index_t layout_t::page_down_for (index_t const& index) const
	{
		CGRect r = rect_at_index(index);
		ng::index_t const indexPageDown = index_at_point(CGPointMake(CGRectGetMinX(r), CGRectGetMinY(r) + _viewport_size.height));
		if(_buffer.convert(index.index).line+1 == _buffer.lines())
			return indexPageDown;

		ng::index_t const bol         = std::max(_buffer.begin(_buffer.convert(index.index).line),         index_at_bol_for(index).index);
		ng::index_t const bolPageDown = std::max(_buffer.begin(_buffer.convert(indexPageDown.index).line), index_at_bol_for(indexPageDown).index);
		ng::index_t const eolPageDown = index_at_eol_for(bolPageDown).index;
		return advance(_buffer, bolPageDown.index, count_columns(_buffer, bol.index, index.index) + index.carry, eolPageDown.index);
	}

	// =============
	// = Rendering =
	// =============

	namespace
	{
		struct base_colors_t
		{
			CGColorRef marked_text_foreground = nil;
			CGColorRef marked_text_background = nil;
			CGColorRef marked_text_border     = nil;
			CGColorRef margin_indicator       = nil;
			CGColorRef drop_marker            = nil;
		};

		base_colors_t const& get_base_colors (bool darkTheme)
		{
			static base_colors_t bright, dark;

			static dispatch_once_t onceToken = 0;
			dispatch_once(&onceToken, ^{
				dark.marked_text_foreground   = CGColorRetain(CGColorGetConstantColor(kCGColorWhite));
				dark.marked_text_background   = CGColorRetain(CGColorGetConstantColor(kCGColorBlack));
				dark.marked_text_border       = CGColorRetain(CGColorGetConstantColor(kCGColorWhite));
				dark.margin_indicator         = CGColorCreateGenericGray(0.50, 0.50);
				dark.drop_marker              = CGColorCreateGenericGray(0.50, 0.50);

				bright.marked_text_foreground = CGColorRetain(CGColorGetConstantColor(kCGColorBlack));
				bright.marked_text_background = CGColorRetain(CGColorGetConstantColor(kCGColorWhite));
				bright.marked_text_border     = CGColorRetain(CGColorGetConstantColor(kCGColorBlack));
				bright.margin_indicator       = CGColorCreateGenericGray(0.25, 0.50);
				bright.drop_marker            = CGColorCreateGenericGray(0.25, 0.50);
			});

			return darkTheme ? dark : bright;
		}

	}

	void layout_t::draw (ng::context_t const& context, CGRect visibleRect, bool isFlipped, bool showInvisibles, ng::ranges_t const& selection, ng::ranges_t const& highlightRanges, bool drawBackground)
	{
		_invisibles.enabled = showInvisibles;
		update_metrics(visibleRect);

		CGContextSetTextMatrix(context, CGAffineTransformMake(1, 0, 0, 1, 0, 0));

		CGColorRef background = _theme->background(scope::to_s(_buffer.scope(0).left));
		if(drawBackground)
			render::fill_rect(context, background, visibleRect);

		CGFloat const yMin = CGRectGetMinY(visibleRect) - _margin.top;
		CGFloat const yMax = CGRectGetMaxY(visibleRect) - _margin.top;

		auto firstY = _rows.upper_bound(yMin, &row_y_comp);
		if(firstY != _rows.begin())
			--firstY;

		if(drawBackground)
		{
			foreach(row, firstY, _rows.lower_bound(yMax, &row_y_comp))
				row->value.draw_background(_theme, *_metrics, context, isFlipped, visibleRect, _invisibles, background, _buffer, row->offset._length, CGPointMake(_margin.left, _margin.top + row->offset._height));
		}

		base_colors_t const& baseColors = get_base_colors(_theme->is_dark());
		if(_draw_wrap_column)
			render::fill_rect(context, baseColors.margin_indicator, OakRectMake(_margin.left + _metrics->column_width() * effective_wrap_column(), CGRectGetMinY(visibleRect), 1, CGRectGetHeight(visibleRect)));

		for(auto const& range : selection)
		{
			for(auto const& rect : rects_for_ranges(range, kRectsIncludeSelections))
			{
				CGColorRef selColor = _theme->styles_for_scope(_buffer.scope(range.min().index).right).selection();
				if(!_is_key)
					selColor = CGColorCreateCopyWithAlpha(selColor, 0.5 * CGColorGetAlpha(selColor));
				render::fill_rect(context, selColor, rect);
				if(!_is_key)
					CFRelease(selColor);
			}
		}

		for(auto const& range : highlightRanges)
		{
			CGRect const r = rect_for_range(range.min().index, range.max().index);
			render::fill_rect(context, baseColors.marked_text_border, CGRectInset(r, -2, -2));
			render::fill_rect(context, baseColors.marked_text_background, CGRectInset(r, -1, -1));
		}

		foreach(row, firstY, _rows.lower_bound(yMax, &row_y_comp))
			row->value.draw_foreground(_theme, *_metrics, context, isFlipped, visibleRect, _invisibles, _buffer, row->offset._length, selection, CGPointMake(_margin.left, _margin.top + row->offset._height));

		if(_draw_caret && !_drop_marker)
		{
			for(auto const& range : selection)
			{
				for(auto const& rect : rects_for_ranges(range, kRectsIncludeCarets))
					render::fill_rect(context, _theme->styles_for_scope(_buffer.scope(range.min().index).right).caret(), rect);
			}
		}

		if(_drop_marker)
			render::fill_rect(context, baseColors.drop_marker, rect_at_index(_drop_marker));
	}

	// ===================
	// = Folding Support =
	// ===================

	bool layout_t::is_line_folded (size_t n) const               { return _folds->has_folded(n); }
	bool layout_t::is_line_fold_start_marker (size_t n) const    { return _folds->has_start_marker(n); }
	bool layout_t::is_line_fold_stop_marker (size_t n) const     { return _folds->has_stop_marker(n); }
	std::string layout_t::folded_as_string () const              { return _folds->folded_as_string(); }

	void layout_t::fold (size_t from, size_t to)
	{
		_folds->fold(from, to);
		did_fold(from, to);
	}

	void layout_t::remove_enclosing_folds (size_t from, size_t to)
	{
		for(auto const& range : _folds->remove_enclosing(from, to))
			did_fold(range.first, range.second);
	}

	void layout_t::toggle_fold_at_line (size_t n, bool recursive)
	{
		auto range = _folds->toggle_at_line(n, recursive);
		if(range.first != range.second)
			did_fold(range.first, range.second);
	}

	void layout_t::toggle_all_folds_at_level (size_t level)
	{
		for(auto const& range : _folds->toggle_all_at_level(level))
			did_fold(range.first, range.second);
	}

	// =================
	// = Debug Support =
	// =================

	bool layout_t::structural_integrity () const
	{
		iterate(row, _rows)
		{
			if(!row->value.structural_integrity())
				return false;
		}
		return _rows.structural_integrity();
	}

	std::string layout_t::to_s () const
	{
		std::string const buffer = _buffer.substr(0, _buffer.size());
		return _rows.to_s([&buffer](row_tree_t::value_type info){
			return text::format("y: %.1f-%.1f, w: %1.f, bytes: %zu-%zu\n» %s\n%s", info.offset._height, info.offset._height + info.key._height, info.key._width, info.offset._length, info.offset._length + info.key._length, buffer.substr(info.offset._length, info.key._length).c_str(), ng::to_s(info.value).c_str());
		});
	}

} /* ng */
