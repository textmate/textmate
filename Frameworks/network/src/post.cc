#include "post.h"
#include "proxy.h"
#include "user_agent.h"
#include <text/encode.h>
#include <oak/oak.h>

// size_t receive_data (void* ptr, size_t size, size_t nmemb, void* udata)
// {
// 	std::string& buf = *(std::string*)udata;
// 	buf.insert(buf.size(), (const char*)ptr, size * nmemb);
// 	return size * nmemb;
// }

long post_to_server (std::string const& url, std::map<std::string, std::string> const& payload)
{
	struct curl_httppost* formpost = NULL;
	struct curl_httppost* lastptr = NULL;

	iterate(pair, payload)
	{
		if(pair->second.find('@') == 0)
		{
			curl_formadd(&formpost, &lastptr,
				CURLFORM_PTRNAME,    pair->first.data(),
				CURLFORM_NAMELENGTH, pair->first.size(),
				CURLFORM_FILE,       pair->second.substr(1).c_str(),
				CURLFORM_END);
		}
		else if(pair->second != NULL_STR)
		{
			curl_formadd(&formpost, &lastptr,
				CURLFORM_PTRNAME,        pair->first.data(),
				CURLFORM_NAMELENGTH,     pair->first.size(),
				CURLFORM_PTRCONTENTS,    pair->second.data(),
				CURLFORM_CONTENTSLENGTH, pair->second.size(),
				CURLFORM_END);
		}
	}

	long serverReply = 0;
	if(CURL* handle = curl_easy_init())
	{
		std::string const userAgent = create_agent_info_string();

		char errorbuf[CURL_ERROR_SIZE];
		// std::string head = "", body = "";

		curl_easy_setopt(handle, CURLOPT_URL,              url.c_str());
		curl_easy_setopt(handle, CURLOPT_HTTPPOST,         formpost);
		curl_easy_setopt(handle, CURLOPT_USERAGENT,        userAgent.c_str());

		curl_easy_setopt(handle, CURLOPT_ERRORBUFFER,      errorbuf);
		curl_easy_setopt(handle, CURLOPT_FAILONERROR,      true);
		
		// curl_easy_setopt(handle, CURLOPT_HEADERFUNCTION,   &receive_data);
		// curl_easy_setopt(handle, CURLOPT_HEADERDATA,       &head);
		// 
		// curl_easy_setopt(handle, CURLOPT_WRITEFUNCTION,    &receive_data);
		// curl_easy_setopt(handle, CURLOPT_WRITEDATA,        &body);

		if(proxy_settings_t const& proxySettings = get_proxy_settings(url))
		{
			curl_easy_setopt(handle, CURLOPT_PROXY,     proxySettings.server.c_str());
			curl_easy_setopt(handle, CURLOPT_PROXYPORT, proxySettings.port);
			if(proxySettings.password != NULL_STR)
				curl_easy_setopt(handle, CURLOPT_PROXYUSERPWD, (proxySettings.user + ":" + proxySettings.password).c_str());
		}

		CURLcode rc = curl_easy_perform(handle);
		if(rc == 0)
				curl_easy_getinfo(handle, CURLINFO_RESPONSE_CODE, &serverReply);
		else	fprintf(stderr, "curl error (%d): %s\n", rc, errorbuf);

		// fprintf(stderr, "HEAD:\n%s\n", head.c_str());
		// fprintf(stderr, "BODY:\n%s\n", body.c_str());

		curl_easy_cleanup(handle);
	}

	curl_formfree(formpost);
	return serverReply;
}
