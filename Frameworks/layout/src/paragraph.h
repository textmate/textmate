#ifndef PARAGRAPH_H_PZ1GB7JU
#define PARAGRAPH_H_PZ1GB7JU

#include "ct.h"
#include <buffer/src/buffer.h>
#include <selection/src/types.h>
#include <theme/src/theme.h>
namespace ng
{
	struct line_record_t
	{
		line_record_t (size_t line, size_t softline, CGFloat top, CGFloat bottom, CGFloat baseline) : line(line), softline(softline), top(top), bottom(bottom), baseline(baseline) { }

		size_t line;
		size_t softline;
		CGFloat top;
		CGFloat bottom;
		CGFloat baseline;
	};

	struct line_t;

	struct paragraph_t
	{
		size_t length () const;

		void insert (size_t pos, size_t len, ng::buffer_t const& buffer, size_t bufferOffset);
		void insert_folded (size_t pos, size_t len, ng::buffer_t const& buffer, size_t bufferOffset);
		void erase (size_t from, size_t to, ng::buffer_t const& buffer, size_t bufferOffset);
		void did_update_scopes (size_t from, size_t to, ng::buffer_t const& buffer, size_t bufferOffset);
		bool layout (theme_ptr const& theme, bool softWrap, size_t wrapColumn, ct::metrics_t const& metrics, CGRect visibleRect, ng::buffer_t const& buffer, size_t bufferOffset);

		void draw_background (theme_ptr const& theme, ct::metrics_t const& metrics, ng::context_t const& context, bool isFlipped, CGRect visibleRect, CGColorRef backgroundColor, ng::buffer_t const& buffer, size_t bufferOffset, CGPoint anchor) const;
		void draw_foreground (theme_ptr const& theme, ct::metrics_t const& metrics, ng::context_t const& context, bool isFlipped, CGRect visibleRect, ng::buffer_t const& buffer, size_t bufferOffset, ng::ranges_t const& selection, CGPoint anchor) const;

		ng::index_t index_at_point (CGPoint point, ct::metrics_t const& metrics, ng::buffer_t const& buffer, size_t bufferOffset, CGPoint anchor) const;
		CGRect rect_at_index (ng::index_t const& index, ct::metrics_t const& metrics, ng::buffer_t const& buffer, size_t bufferOffset, CGPoint anchor, bool bol_as_eol, bool wantsBaseline) const;

		ng::line_record_t line_record_for (size_t line, size_t pos, ct::metrics_t const& metrics, ng::buffer_t const& buffer, size_t bufferOffset, CGPoint anchor) const;
		ng::range_t folded_range_at_point (CGPoint point, ct::metrics_t const& metrics, ng::buffer_t const& buffer, size_t bufferOffset, CGPoint anchor) const;

		size_t bol (size_t index, ng::buffer_t const& buffer, size_t bufferOffset) const;
		size_t eol (size_t index, ng::buffer_t const& buffer, size_t bufferOffset) const;

		size_t index_left_of (size_t index, ng::buffer_t const& buffer, size_t bufferOffset) const;
		size_t index_right_of (size_t index, ng::buffer_t const& buffer, size_t bufferOffset) const;

		size_t softline_count (ct::metrics_t const& metrics, bool softBreaksOnNewline = false) const;
		size_t softline_for_index (ng::index_t const& index, ng::buffer_t const& buffer, size_t bufferOffset, size_t softlineOffset, ct::metrics_t const& metrics, bool softBreaksOnNewline = false) const;
		ng::range_t range_for_softline (size_t softline, ng::buffer_t const& buffer, size_t bufferOffset, size_t softlineOffset, ct::metrics_t const& metrics, bool softBreaksOnNewline = false) const;

		void set_wrapping (bool softWrap, size_t wrapColumn, ct::metrics_t const& metrics);
		void set_tab_size (ct::metrics_t const& metrics);
		void reset_font_metrics (ct::metrics_t const& metrics);

		CGFloat width () const;
		CGFloat height (ct::metrics_t const& metrics) const;

		bool structural_integrity () const { return true; }

	private:
		enum node_type_t { kNodeTypeText, kNodeTypeUnprintable, kNodeTypeFolding, kNodeTypeSoftBreak, kNodeTypeNewline };

		struct node_t
		{
			node_t (node_type_t type, size_t length = 0, CGFloat width = 0) : _type(type), _length(length), _width(width) { }

			void insert (size_t i, size_t len);
			void erase (size_t from, size_t to);
			void did_update_scopes (size_t from, size_t to);

			void layout (CGFloat x, size_t tabSize, theme_ptr const& theme, bool softWrap, size_t wrapColumn, ct::metrics_t const& metrics, ng::buffer_t const& buffer, size_t bufferOffset, std::string const& fillStr);
			void reset_font_metrics (ct::metrics_t const& metrics);
			void draw_background (theme_ptr const& theme, ng::context_t const& context, bool isFlipped, CGRect visibleRect, CGColorRef backgroundColor, ng::buffer_t const& buffer, size_t bufferOffset, CGPoint anchor, CGFloat lineHeight) const;
			void draw_foreground (theme_ptr const& theme, ng::context_t const& context, bool isFlipped, CGRect visibleRect, ng::buffer_t const& buffer, size_t bufferOffset, std::vector< std::pair<size_t, size_t> > const& misspelled, CGPoint anchor, CGFloat baseline) const;

			node_type_t type () const                      { return _type; }
			size_t length () const                         { return _length; }
			std::shared_ptr<ct::line_t> line () const { return _line; }
			CGFloat width () const;
			void update_tab_width ();

		private:
			node_type_t _type;
			size_t _length;
			CGFloat _width;

			std::shared_ptr<ct::line_t> _line;
		};

		std::vector<node_t>::iterator iterator_at (size_t i);

		void insert_text (size_t i, size_t len);
		void insert_unprintable (size_t i, size_t len);
		void insert_newline (size_t i, size_t len);

		struct softline_t
		{
			softline_t (size_t offset, CGFloat x, CGFloat y, CGFloat baseline, CGFloat height, size_t first, size_t last) : offset(offset), x(x), y(y), baseline(baseline), height(height), first(first), last(last) { }

			size_t offset;
			CGFloat x;
			CGFloat y;
			CGFloat baseline;
			CGFloat height;
			size_t first;
			size_t last;
		};

		std::vector<softline_t> softlines (ct::metrics_t const& metrics, bool softBreaksOnNewline = false) const;

		std::vector<node_t> _nodes;
		bool _dirty = true;
	};

	std::string to_s (paragraph_t const& paragraph);

} /* ng */

#endif /* end of include guard: PARAGRAPH_H_PZ1GB7JU */
