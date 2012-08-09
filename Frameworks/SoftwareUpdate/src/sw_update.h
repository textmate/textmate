#ifndef SW_UPDATE_H_TW02442V
#define SW_UPDATE_H_TW02442V

#include <network/network.h>
#include <oak/debug.h>

namespace sw_update
{
	struct version_info_t
	{
		version_info_t (long version = 0, std::string const& url = NULL_STR) : version(version), url(url) { }

		long version;
		std::string url;
	};

	PUBLIC version_info_t download_info (std::string const& url, std::string* error = NULL);
	PUBLIC std::string download_update (std::string const& url, key_chain_t const& keyChain, std::string* error, double* progress = NULL, bool const* stopFlag = NULL);
	PUBLIC std::string install_update (std::string const& src);
}

#endif /* end of include guard: SW_UPDATE_H_TW02442V */
