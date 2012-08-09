#ifndef CF_COLOR_H_2BFCUP1C
#define CF_COLOR_H_2BFCUP1C

#include <oak/misc.h>

namespace cf
{
	struct PUBLIC color_t
	{
		color_t (std::string const& str = NULL_STR);

		CGFloat alpha () const;
		void set_alpha (CGFloat newAlpha);

		bool operator== (color_t const& rhs) const { return string_value == rhs.string_value; }
		bool operator!= (color_t const& rhs) const { return string_value != rhs.string_value; }

		operator CGColorRef () const    { return value.get(); }
		EXPLICIT operator bool () const { return value.get(); }

	private:
		friend std::string to_s (color_t const& c);
		typedef std::tr1::shared_ptr<struct CGColor> CGColorPtr;
		CGColorPtr parse (std::string const& color);
		CGColorPtr value;
		std::string string_value;
	};

	PUBLIC std::string to_s (color_t const& c);
	PUBLIC bool color_is_dark (CGColorRef color);

} /* cf */

#endif /* end of include guard: CF_COLOR_H_2BFCUP1C */
