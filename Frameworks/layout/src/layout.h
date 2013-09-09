#ifndef LAYOUT_H_VWUBRBQ1
#define LAYOUT_H_VWUBRBQ1

#include "paragraph.h"
#include <buffer/buffer.h>
#include <selection/selection.h>
#include <theme/theme.h>
#include <oak/basic_tree.h>
#include <oak/misc.h>

namespace ct { struct metrics_t; struct line_t; };

enum kRectsIncludeMode { kRectsIncludeAll, kRectsIncludeCarets, kRectsIncludeSelections };

namespace ng
{
	struct folds_t;

	struct PUBLIC layout_t : layout_movement_t
	{
		struct margin_t
		{
			margin_t (size_t m) : left(m), top(m), right(m), bottom(m) { }
			size_t left, top, right, bottom;
		};

		layout_t (ng::buffer_t& buffer, theme_ptr const& theme, bool softWrap = false, bool scrollPastEnd = false, size_t wrapColumn = 0, std::string const& folded = NULL_STR, margin_t const& margin = margin_t(8));
		~layout_t ();

		// _buffer_callback is managed with new/delete so can’t be copied
		layout_t (layout_t const& rhs) = delete;
		layout_t& operator= (layout_t const& rhs) = delete;

		// ============
		// = Settings =
		// ============

		void set_theme (theme_ptr const& theme);
		void set_font (std::string const& fontName, CGFloat fontSize);
		void set_character_mapping (std::string const& newInvisibles);
		void set_margin (margin_t const& margin);
		void set_wrapping (bool softWrap, size_t wrapColumn);
		void set_scroll_past_end (bool scrollPastEnd);

		theme_ptr const& theme () const         { return _theme; }
		std::string const& font_name () const   { return _theme->font_name(); }
		CGFloat font_size () const              { return _theme->font_size(); }
		size_t tab_size () const                { return _tab_size; }
		margin_t const& margin () const         { return _margin; }
		bool wrapping () const                  { return _wrapping; }
		size_t effective_wrap_column () const;

		// ======================
		// = Display Attributes =
		// ======================

		void set_is_key (bool isKey);
		void set_draw_caret (bool drawCaret);
		void set_draw_wrap_column (bool drawWrapColumn);
		void set_drop_marker (ng::index_t dropMarkerIndex);
		void set_viewport_size (CGSize size);

		bool draw_wrap_column () const          { return _draw_wrap_column; }
		bool scroll_past_end () const           { return _scroll_past_end; }

		// ======================

		void update_metrics (CGRect visibleRect);
		void draw (ng::context_t const& context, CGRect rectangle, bool isFlipped, bool showInvisibles, ng::ranges_t const& selection, ng::ranges_t const& highlightRanges = ng::ranges_t(), bool drawBackground = true);
		ng::index_t index_at_point (CGPoint point) const;
		CGRect rect_at_index (ng::index_t const& index) const;
		CGRect rect_for_range (size_t first, size_t last) const;
		std::vector<CGRect> rects_for_ranges (ng::ranges_t const& ranges, kRectsIncludeMode mode = kRectsIncludeAll) const;

		CGFloat width () const;
		CGFloat height () const;

		void begin_refresh_cycle (ng::ranges_t const& selection, ng::ranges_t const& highlightRanges = ng::ranges_t());
		std::vector<CGRect> end_refresh_cycle (ng::ranges_t const& selection, CGRect visibleRect, ng::ranges_t const& highlightRanges = ng::ranges_t());
		void did_update_scopes (size_t from, size_t to);

		ng::index_t index_above (ng::index_t const& index) const;
		ng::index_t index_right_of (ng::index_t const& index) const;
		ng::index_t index_below (ng::index_t const& index) const;
		ng::index_t index_left_of (ng::index_t const& index) const;
		ng::index_t index_at_bol_for (ng::index_t const& index) const;
		ng::index_t index_at_eol_for (ng::index_t const& index) const;
		ng::index_t page_up_for (index_t const& index) const;
		ng::index_t page_down_for (index_t const& index) const;

		// ===================
		// = Folding Support =
		// ===================

		bool is_line_folded (size_t n) const;
		bool is_line_fold_start_marker (size_t n) const;
		bool is_line_fold_stop_marker (size_t n) const;
		void fold (size_t from, size_t to);
		void remove_enclosing_folds (size_t from, size_t to);
		void toggle_fold_at_line (size_t n, bool recursive);
		void toggle_all_folds_at_level (size_t level);
		std::string folded_as_string () const;

		// =======================
		// = Gutter view support =
		// =======================

		line_record_t line_record_for (CGFloat y) const;
		line_record_t line_record_for (text::pos_t const& pos) const;

		// =================
		// = Debug support =
		// =================

		bool structural_integrity () const;
		std::string to_s () const;

	private:
		struct row_key_t
		{
			row_key_t (size_t length = 0, CGFloat height = 0, CGFloat width = 0) : _length(length), _height(height), _width(width) { }

			bool operator==     (row_key_t const& rhs) const { return _length == rhs._length && _height == rhs._height && _width == rhs._width; }
			row_key_t operator+ (row_key_t const& rhs) const { return row_key_t(_length + rhs._length, _height + rhs._height, std::max(_width, rhs._width)); }
			row_key_t operator- (row_key_t const& rhs) const { return row_key_t(_length - rhs._length, _height - rhs._height, std::min(_width, rhs._width)); }

			size_t _length;
			CGFloat _height;
			CGFloat _width;
		};

		typedef oak::basic_tree_t<row_key_t, paragraph_t> row_tree_t;

		CGFloat content_width () const         { return ceil(std::max(_rows.aggregated()._width, _viewport_size.width - _margin.left - _margin.right)); }
		CGFloat content_height () const        { return ceil(std::max(_rows.aggregated()._height, _viewport_size.height - _margin.top - _margin.bottom)); }

		row_tree_t::iterator row_for_offset (size_t i) const;
		CGFloat default_line_height (CGFloat minAscent = 0, CGFloat minDescent = 0, CGFloat minLeading = 0) const;
		CGRect rect_for (row_tree_t::iterator rowIter) const;
		CGRect full_width (CGRect const& rect) const;
		CGRect full_height (CGRect const& rect) const;
		bool effective_soft_wrap (row_tree_t::iterator rowIter) const;

		void set_tab_size (size_t tabSize);
		void did_insert (size_t first, size_t last);
		void did_erase (size_t from, size_t to);

		void setup_font_metrics ();
		void clear_text_widths ();

		bool update_row (row_tree_t::iterator rowIter);

		void refresh_line_at_index (size_t index, bool fullRefresh);
		void did_fold (size_t from, size_t to);

		static int row_y_comp (CGFloat y, row_key_t const& offset, row_key_t const& node)       { return y < offset._height ? -1 : (y == offset._height ? 0 : +1); }
		static int row_offset_comp (size_t i, row_key_t const& offset, row_key_t const& node)   { return i < offset._length ? -1 : (i == offset._length ? 0 : +1); }

		static std::string row_to_s (row_tree_t::value_type const& info);

		mutable row_tree_t _rows;
		std::shared_ptr<folds_t> _folds;

		ng::buffer_t&      _buffer;
		ng::callback_t*    _buffer_callback;

		theme_ptr          _theme;
		size_t             _tab_size;
		bool               _wrapping;
		bool               _scroll_past_end;
		bool               _draw_wrap_column = false;
		size_t             _wrap_column;
		margin_t           _margin;
		CGSize             _viewport_size = CGSizeZero;
		invisibles_t       _invisibles;

		bool               _is_key = false;
		bool               _draw_caret = false;
		ng::index_t        _drop_marker;

		std::shared_ptr<ct::metrics_t> _metrics;

		size_t _pre_refresh_revision;
		size_t _pre_refresh_caret;
		std::vector<CGRect> _pre_refresh_carets;
		std::vector<CGRect> _pre_refresh_selections;
		std::vector<CGRect> _pre_refresh_highlight_border;
		std::vector<CGRect> _pre_refresh_highlight_interior;
		std::vector<CGRect> _dirty_rects;
		size_t _refresh_counter = 0;
	};

} /* ng */

#endif /* end of include guard: LAYOUT_H_VWUBRBQ1 */
