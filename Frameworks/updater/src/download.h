#ifndef UPDATER_DOWNLOAD_H_842XT36M
#define UPDATER_DOWNLOAD_H_842XT36M

#include <network/src/key_chain.h>
#include <oak/oak.h>

namespace bundles_db
{
	key_chain_t key_chain ();
	std::string download_etag (std::string const& url, key_chain_t const& keyChain, std::string* etag, double* progress, double min, double max);

} /* bundles_db */

#endif /* end of include guard: UPDATER_DOWNLOAD_H_842XT36M */
