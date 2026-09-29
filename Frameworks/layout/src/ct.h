#ifndef CT_H_IWVOT7CS
#define CT_H_IWVOT7CS

#include <theme/src/theme.h>

namespace ng
{
	struct context_t
	{
		context_t (CGContextRef context, std::string const& invisibleMap = NULL_STR, CGImageRef spellingDot = nil, std::function<CGImageRef(double, double)> foldingDotsFactory = std::function<CGImageRef(double, double)>());
		~context_t ();

		operator CGContextRef () const      { return _context; }

		std::string const& space () const   { return _space; }
		std::string const& tab () const     { return _tab; }
		std::string const& newline () const { return _newline; }

		CGImageRef spelling_dot () const    { return _spelling_dot; }
		CGImageRef folding_dots (double, double) const;

	private:
		void setup_invisibles_mapping (std::string const& str);

		CGContextRef _context;

		std::string _space   = "·";
		std::string _tab     = "‣";
		std::string _newline = "¬";

		CGImageRef _spelling_dot;
		std::function<CGImageRef(double, double)> _folding_dots_create;
		mutable std::map<std::pair<double, double>, CGImageRef> _folding_dots_cache;
	};

} /* ng */

namespace ct
{
	struct metrics_t
	{
		metrics_t (std::string const& fontName, CGFloat fontSize);

		CGFloat ascent () const       { return _ascent;       }
		CGFloat descent () const      { return _descent;      }
		CGFloat leading () const      { return _leading;      }
		CGFloat x_height () const     { return _x_height;     }
		CGFloat cap_height () const   { return _cap_height;   }
		CGFloat column_width () const { return _column_width; }

		CGFloat baseline (CGFloat minAscent = 0) const { return round(std::max(minAscent, _ascent) + _ascent_delta); }

		CGFloat line_height (CGFloat minAscent = 0, CGFloat minDescent = 0, CGFloat minLeading = 0) const;

	private:
		CGFloat _ascent;
		CGFloat _descent;
		CGFloat _leading;
		CGFloat _x_height;
		CGFloat _cap_height;
		CGFloat _column_width;

		CGFloat _ascent_delta  = 1;
		CGFloat _leading_delta = 1;
	};

	struct line_t
	{
		line_t (std::string const& text, std::map<size_t, scope::scope_t> const& scopes, theme_ptr const& theme, size_t tabSize, ct::metrics_t const& metrics, CGColorRef textColor = NULL);

		void draw_foreground (CGPoint pos, ng::context_t const& context, bool isFlipped, std::vector< std::pair<size_t, size_t> > const& misspelled, theme_ptr const& theme) const;
		void draw_background (CGPoint pos, CGFloat height, ng::context_t const& context, bool isFlipped, CGColorRef currentBackground) const;

		CGFloat width (CGFloat* ascent = NULL, CGFloat* descent = NULL, CGFloat* leading = NULL) const;

		size_t index_for_offset (CGFloat offset) const;
		CGFloat offset_for_index (size_t index) const;

	private:
		void draw_invisible (std::vector<size_t> locations, CGPoint pos, std::string const& text, styles_t const& styles, ng::context_t const& context, bool isFlipped) const;

		typedef std::shared_ptr<std::remove_pointer<CTLineRef>::type> CTLinePtr;
		typedef std::shared_ptr<std::remove_pointer<CGColorRef>::type> CGColorPtr;

		std::string _text;
		CTLinePtr _line;
		std::vector< std::pair<CFRange, CGColorPtr> > _backgrounds;
		std::vector< std::pair<CFRange, CGColorPtr> > _underlines;
		std::vector< std::pair<CFRange, CGColorPtr> > _strikethroughs;
		std::vector<size_t> _tab_locations;
		std::vector<size_t> _space_locations;
		CGFloat _x_height; // For centering strikethrough line
	};

} /* ct */

#endif /* end of include guard: CT_H_IWVOT7CS */
