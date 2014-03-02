#include "post.h"
#include "proxy.h"
#include "user_agent.h"
#include <text/case.h>
#include <text/encode.h>
#include <oak/oak.h>

static size_t receive_data (void* ptr, size_t size, size_t nmemb, void* udata)
{
	std::map<std::string, std::string>* map = (std::map<std::string, std::string>*)udata;

	std::string line((char*)ptr, ((char*)ptr) + nmemb * size);
	while(line.size() > 1 && line.rfind("\r\n") == line.size()-2)
		line.resize(line.size()-2);

	auto sep = line.find(':');
	if(sep != std::string::npos)
		map->emplace(std::make_pair(text::lowercase(line.substr(0, sep)), line.substr(line.find_first_not_of(" \t", sep+1))));

	return size * nmemb;
}

long post_to_server (std::string const& url, std::map<std::string, std::string> const& payload, std::map<std::string, std::string>* headersOut)
{
	struct curl_httppost* formpost = NULL;
	struct curl_httppost* lastptr = NULL;

	for(auto const& pair : payload)
	{
		if(pair.second.find('@') == 0)
		{
			curl_formadd(&formpost, &lastptr,
				CURLFORM_PTRNAME,    pair.first.data(),
				CURLFORM_NAMELENGTH, pair.first.size(),
				CURLFORM_FILE,       pair.second.substr(1).c_str(),
				CURLFORM_END);
		}
		else if(pair.second != NULL_STR)
		{
			curl_formadd(&formpost, &lastptr,
				CURLFORM_PTRNAME,        pair.first.data(),
				CURLFORM_NAMELENGTH,     pair.first.size(),
				CURLFORM_PTRCONTENTS,    pair.second.data(),
				CURLFORM_CONTENTSLENGTH, pair.second.size(),
				CURLFORM_END);
		}
	}

	long serverReply = 0;
	if(CURL* handle = curl_easy_init())
	{
		std::string const userAgent = create_agent_info_string();
		char errorbuf[CURL_ERROR_SIZE];

		curl_easy_setopt(handle, CURLOPT_URL,              url.c_str());
		curl_easy_setopt(handle, CURLOPT_HTTPPOST,         formpost);
		curl_easy_setopt(handle, CURLOPT_USERAGENT,        userAgent.c_str());

		curl_easy_setopt(handle, CURLOPT_ERRORBUFFER,      errorbuf);
		curl_easy_setopt(handle, CURLOPT_FAILONERROR,      true);
		curl_easy_setopt(handle, CURLOPT_NOSIGNAL,         1);

		if(headersOut)
		{
			curl_easy_setopt(handle, CURLOPT_HEADERFUNCTION,   &receive_data);
			curl_easy_setopt(handle, CURLOPT_HEADERDATA,       headersOut);
		}

		if(proxy_settings_t const& proxySettings = get_proxy_settings(url))
		{
			curl_easy_setopt(handle, CURLOPT_PROXY,     proxySettings.server.c_str());
			curl_easy_setopt(handle, CURLOPT_PROXYPORT, proxySettings.port);
			curl_easy_setopt(handle, CURLOPT_PROXYTYPE, proxySettings.socks ? CURLPROXY_SOCKS4 : CURLPROXY_HTTP);
			if(proxySettings.password != NULL_STR)
				curl_easy_setopt(handle, CURLOPT_PROXYUSERPWD, (proxySettings.user + ":" + proxySettings.password).c_str());
		}

		CURLcode rc = curl_easy_perform(handle);
		if(rc == 0)
				curl_easy_getinfo(handle, CURLINFO_RESPONSE_CODE, &serverReply);
		else	fprintf(stderr, "curl error (%d): %s\n", rc, errorbuf);

		curl_easy_cleanup(handle);
	}

	curl_formfree(formpost);
	return serverReply;
}
