#pragma once

#include <bundles/load.h>
#include <bundles/locations.h>

namespace theme_editor
{
	inline auto load_bundle_index (std::string const& cachePath = path::join(path::home(), "Library/Caches/com.macromates.TextMate/BundlesIndex.plist"))
	{
		plist::cache_t cache;
		cache.load(cachePath);
		std::vector<std::string> paths;
		for(auto const& location : bundles::locations())
		{
			paths.push_back(path::join(location, "Bundles"));
			// The shared cache may predate changes made by another editor.
			cache.reload(paths.back(), true);
		}
		return create_bundle_index(paths, cache);
	}
}
