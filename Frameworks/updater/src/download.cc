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
		etag_t collect_etag;

		std::string error = NULL_STR;
		long res = network::download(network::request_t(url, &validator, &archiver, &collect_etag, NULL).set_entity_tag(etag ? *etag : NULL_STR).update_progress_variable(progress, min, max), &error);
		if(res == 304) // not modified
		{
			path::remove(archiver.path);
			return NULL_STR;
		}
		else if(res == 200)
		{
			if(etag)
				*etag = collect_etag.etag;
			return archiver.path;
		}
		else if(res != 0)
		{
			error = text::format("got ‘%ld’ from server (expected 200)", res);
		}

		fprintf(stderr, "*** download_etag(‘%s’): %s\n", url.c_str(), res == 0 ? error.c_str() : "unexpected server response");
		return NULL_STR;
	}
}
