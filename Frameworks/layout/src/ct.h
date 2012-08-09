#ifndef CT_H_IWVOT7CS
#define CT_H_IWVOT7CS

#include <theme/theme.h>

namespace ct
{
	struct metrics_t
	{
		metrics_t (std::string const& fontName, CGFloat fontSize);

		CGFloat ascent () const       { return _ascent;       }
		CGFloat descent () const      { return _descent;      }
		CGFloat leading () const      { return _leading;      }
		CGFloat column_width () const { return _column_width; }

		CGFloat baseline (CGFloat minAscent = 0) const { return round(std::max(minAscent, _ascent) + _ascent_delta); }

		CGFloat line_height (CGFloat minAscent = 0, CGFloat minDescent = 0, CGFloat minLeading = 0) const;

	private:
		CGFloat _ascent;
		CGFloat _descent;
		CGFloat _leading;
		CGFloat _column_width;

		CGFloat _ascent_delta  = 1;
		CGFloat _leading_delta = 1;
	};

	struct line_t
	{
		line_t (std::string const& text, std::map<size_t, scope::scope_t> const& scopes, theme_ptr const& theme, std::string fontName, CGFloat fontSize, CGColorRef textColor = NULL);

		void draw_foreground (CGPoint pos, CGContextRef context, bool isFlipped, std::vector< std::pair<size_t, size_t> > const& misspelled) const;
		void draw_background (CGPoint pos, CGFloat height, CGContextRef context, bool isFlipped, CGColorRef currentBackground) const;

		CGFloat width (CGFloat* ascent = NULL, CGFloat* descent = NULL, CGFloat* leading = NULL) const;

		size_t index_for_offset (CGFloat offset) const;
		CGFloat offset_for_index (size_t index) const;

	private:
		typedef std::tr1::shared_ptr<struct __CTLine const> CTLinePtr;
		typedef std::tr1::shared_ptr<struct CGColor> CGColorPtr;

		std::string _text;
		CTLinePtr _line;
		std::vector< std::pair<CFRange, CGColorPtr> > _backgrounds;
		std::vector< std::pair<CFRange, CGColorPtr> > _underlines;
	};

} /* ct */

#endif /* end of include guard: CT_H_IWVOT7CS */
