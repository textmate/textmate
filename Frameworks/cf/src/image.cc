#include "image.h"
#include "cf.h"
#include <oak/debug.h>

namespace cf
{
	image_t::image_t (std::string const& path, std::string const& bundleId)
	{
		ASSERT(!path.empty() && path != NULL_STR);
		if(path[0] != '/')
		{
			if(CFBundleRef bundle = bundleId == NULL_STR ? CFBundleGetMainBundle() : CFBundleGetBundleWithIdentifier(cf::wrap(bundleId)))
			{
				std::string::size_type ext = path.find('.');
				ASSERT_NE(ext, std::string::npos);
				if(CFURLRef imageURL = CFBundleCopyResourceURL(bundle, cf::wrap(path.substr(0, ext)), cf::wrap(path.substr(ext + 1)), NULL))
				{
					if(CGImageSourceRef imageSource = CGImageSourceCreateWithURL(imageURL, NULL))
					{
						_value.reset(CGImageSourceCreateImageAtIndex(imageSource, 0, NULL), CGImageRelease);
						CFRelease(imageSource);
					}
					CFRelease(imageURL);
				}
			}
		}
	}

} /* cf */
