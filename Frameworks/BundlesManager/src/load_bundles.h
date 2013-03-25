#ifndef LOAD_BUNDLES_H_C8BVI372
#define LOAD_BUNDLES_H_C8BVI372

#include "fs_cache.h"
#include <bundles/index.h>
#include <oak/misc.h>

std::pair<std::vector<bundles::item_ptr>, std::map< oak::uuid_t, std::vector<oak::uuid_t>>> create_bundle_index (std::vector<std::string> const& bundlesPaths, fs::cache_t& cache);

#endif /* end of include guard: LOAD_BUNDLES_H_C8BVI372 */
