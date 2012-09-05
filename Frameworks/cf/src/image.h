#ifndef CF_IMAGE_H_KU6HL51
#define CF_IMAGE_H_KU6HL51

#include <oak/misc.h>

namespace cf
{
	struct PUBLIC image_t
	{
		image_t (std::string const& path, std::string const& bundleId = NULL_STR);
		image_t ()                      { }
		operator CGImageRef () const    { return _value.get(); }
		explicit operator bool () const { return _value.get() ? true : false; }
	private:
		typedef std::shared_ptr<struct CGImage> CGImagePtr;
		CGImagePtr _value;
	};

} /* cf */

#endif /* end of include guard: CF_IMAGE_H_KU6HL51 */
