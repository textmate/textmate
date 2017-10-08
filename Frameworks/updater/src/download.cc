#include "download.h"
#include <network/network.h>
#include <network/download_tbz.h>
#include <text/decode.h>

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
					fprintf(stderr, "*** download_etag(‘%s’): got ‘%ld’ from server (expected 200)\n", url.c_str(), res);
			else	fprintf(stderr, "*** download_etag(‘%s’): %s\n", url.c_str(), error.c_str());
		}
		return NULL_STR;
	}
}
