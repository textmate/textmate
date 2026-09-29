#ifndef LOAD_H_C8BVI372
#define LOAD_H_C8BVI372

#include "item.h"
#include <plist/src/fs_cache.h>

std::pair<std::vector<bundles::item_ptr>, std::map< oak::uuid_t, std::vector<oak::uuid_t>>> create_bundle_index (std::vector<std::string> const& bundlesPaths, plist::cache_t& cache);

#endif /* end of include guard: LOAD_H_C8BVI372 */
