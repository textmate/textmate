#ifndef CF_COLOR_H_2BFCUP1C
#define CF_COLOR_H_2BFCUP1C

#include <oak/misc.h>

namespace cf
{
	struct PUBLIC color_t
	{
		color_t () : _empty(true) { }
		color_t (std::string const& str);
		color_t (CGFloat r, CGFloat g, CGFloat b, CGFloat a) : _red(r), _green(g), _blue(b), _alpha(a) { }

		bool operator== (color_t const& rhs) const;
		bool operator!= (color_t const& rhs) const;

		operator CGColorRef () const;
		explicit operator bool () const { return !_empty; }

		CGFloat red () const   { return _red;   }
		CGFloat green () const { return _green; }
		CGFloat blue () const  { return _blue;  }
		CGFloat alpha () const { return _alpha; }

	private:
		CGFloat _red, _green, _blue, _alpha;
		mutable std::shared_ptr<struct CGColor> cachedValue;
		bool _empty = false;

		friend std::string to_s (color_t const& c);
		friend bool color_is_dark (color_t const& color);
	};

	PUBLIC std::string to_s (color_t const& c);
	PUBLIC bool color_is_dark (color_t const& color);

} /* cf */

#endif /* end of include guard: CF_COLOR_H_2BFCUP1C */
