#include "locations.h"
#include <io/src/path.h>
#include <OakSystem/src/application.h>
#include <oak/duration.h>
#include <oak/debug.h>

namespace bundles
{
	static std::vector<std::string>& locations_vector ()
	{
		static std::string const BundleLocations[] =
		{
			path::join(path::home(), "Library/Application Support/TextMate"),
			path::join(path::home(), "Library/Application Support/TextMate/Pristine Copy"),
			path::join(path::home(), "Library/Application Support/TextMate/Managed"),
			path::join("/",          "Library/Application Support/TextMate"),
			path::join("/",          "Library/Application Support/TextMate/Pristine Copy"),
			oak::application_t::path("Contents/SharedSupport"),
		};
		static std::vector<std::string> res(std::begin(BundleLocations), std::end(BundleLocations));
		return res;
	}

	std::vector<std::string> const& locations ()
	{
		return locations_vector();
	}

	void set_locations (std::vector<std::string> const& newLocations)
	{
		locations_vector() = newLocations;
	}

} /* bundles */
