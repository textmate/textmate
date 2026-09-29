#include "download.h"
#include <network/src/network.h>
#include <network/src/download_tbz.h>
#include <text/src/decode.h>

namespace bundles_db
{
	std::string download_etag (std::string const& url, key_chain_t const& keyChain, std::string* etag, double* progress, double min, double max)
	{
		network::check_signature_t validator(keyChain, kHTTPSigneeHeader, kHTTPSignatureHeader);
		network::save_t archiver(false);
		network::header_t collect_etag("etag");

		std::string error = NULL_STR;
		long res = network::download(network::request_t(url, &validator, &archiver, &collect_etag, nullptr).set_entity_tag(etag ? *etag : NULL_STR).update_progress_variable(progress, min, max), &error);
		if(res == 304) // not modified
		{
			path::remove(archiver.path);
		}
		else if(res == 200)
		{
			if(etag)
				*etag = collect_etag.value();
			return archiver.path;
		}
		else
		{
			if(res != 0)
					os_log_error(OS_LOG_DEFAULT, "download_etag(‘%{public}s’): got ‘%ld’ from server (expected 200)", url.c_str(), res);
			else	os_log_error(OS_LOG_DEFAULT, "download_etag(‘%{public}s’): %{public}s", url.c_str(), error.c_str());
		}
		return NULL_STR;
	}
}
