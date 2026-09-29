#include "download.h"
#include "proxy.h"
#include "user_agent.h"
#include <cf/src/cf.h>
#include <text/src/format.h>
#include <text/src/case.h>
#include <text/src/hexdump.h>
#include <io/src/path.h>
#include <oak/debug.h>

namespace network
{
	static char const kCRLF[] = "\r\n";

	// =============
	// = request_t =
	// =============

	request_t::request_t (std::string const& url, filter_t* firstFilter, ...) : _url(url)
	{
		va_list ap;
		va_start(ap, firstFilter);
		for(; firstFilter; firstFilter = va_arg(ap, filter_t*))
			_filters.push_back(firstFilter);
		va_end(ap);
	}

	request_t& request_t::add_filter (filter_t* filter)                              { _filters.push_back(filter); return *this; }
	request_t& request_t::set_user_agent (std::string const& user_agent)             { _user_agent = user_agent; return *this; }
	request_t& request_t::set_entity_tag (std::string const& entity_tag)             { _entity_tag = entity_tag; return *this; }
	request_t& request_t::watch_stop_flag (bool const* stopFlag)                     { _stop_flag = stopFlag; return *this; }

	request_t& request_t::update_progress_variable (double* percentDone, double min, double max)
	{
		_progress     = percentDone;
		_progress_min = min;
		_progress_max = max;
		return *this;
	}

	// ============
	// = Download =
	// ============

	namespace
	{
		struct user_data_t
		{
			user_data_t (request_t const& request) : request(request) { }

			request_t const& request;
			bool receiving_body = false;
			std::string error = NULL_STR;
		};
	}

	int receive_progress (void* udata, double dltotal, double dlnow, double ultotal, double ulnow)
	{
		user_data_t& userData = *((user_data_t*)udata);
		if(userData.request._progress && userData.receiving_body)
			*userData.request._progress = userData.request._progress_min + (userData.request._progress_max - userData.request._progress_min) * (dltotal ? dlnow / dltotal : 0);
		return userData.request._stop_flag ? *userData.request._stop_flag : false;
	}

	size_t receive_header (void* ptr, size_t size, size_t nmemb, void* udata)
	{
		user_data_t& userData = *((user_data_t*)udata);

		char const* bytes = (char const*)ptr;
		size_t len = nmemb * size;

		if(len > 5 && strncmp("HTTP/", bytes, 5) == 0 && std::find(bytes, bytes + len, ':') == bytes + len)
		{
			if(len > 12 && strncmp("HTTP/1", bytes, 6) == 0 && bytes[9] == '2')
				userData.receiving_body = true;

			char const* first = (char const*)ptr;
			char const* last  = std::search(first, first + size * nmemb, &kCRLF[0], &kCRLF[2]);
			for(auto const& filter : userData.request._filters)
			{
				if(!filter->receive_status(std::string(first, last)))
				{
					userData.error = text::format("%s: receiving status", filter->name().c_str());
					return 0;
				}
			}
		}
		else if(len == 2 && strncmp("\r\n", bytes, 2) == 0)
		{
		}
		else
		{
			while(len && (bytes[len-1] == '\r' || bytes[len-1] == '\n'))
				--len;

			size_t keyLast    = std::find(bytes, bytes + len, ':') - bytes;
			size_t valueFirst = keyLast;
			while(valueFirst < len && bytes[++valueFirst] == ' ')
				;

			if(valueFirst != len)
			{
				for(auto const& filter : userData.request._filters)
				{
					if(!filter->receive_header(text::lowercase(std::string(bytes, bytes + keyLast)), std::string(bytes + valueFirst, bytes + len)))
					{
						userData.error = text::format("%s: receiving header", filter->name().c_str());
						return 0;
					}
				}
			}
		}
		return size * nmemb;
	}

	size_t receive_data (void* ptr, size_t size, size_t nmemb, void* udata)
	{
		user_data_t& userData = *((user_data_t*)udata);
		for(auto const& filter : userData.request._filters)
		{
			if(!filter->receive_data((char const*)ptr, size * nmemb))
			{
				userData.error = text::format("%s: receiving data", filter->name().c_str());
				return 0;
			}
		}
		return size * nmemb;
	}

	long download (request_t const& request, std::string* error)
	{
		for(auto const& filter : request._filters)
		{
			if(!filter->setup())
			{
				if(error)
					*error = text::format("%s: setup", filter->name().c_str());
				return 0;
			}
		}

		long res = 0;
		if(CURL* handle = curl_easy_init())
		{
			curl_easy_setopt(handle, CURLOPT_URL,              request._url.c_str());
			curl_easy_setopt(handle, CURLOPT_FOLLOWLOCATION,   true);
			curl_easy_setopt(handle, CURLOPT_FAILONERROR,      true);
			curl_easy_setopt(handle, CURLOPT_ENCODING,         "");
			curl_easy_setopt(handle, CURLOPT_NOSIGNAL,         1);

			std::string const userAgent = request._user_agent == NULL_STR ? create_agent_info_string() : request._user_agent;
			curl_easy_setopt(handle, CURLOPT_USERAGENT,        userAgent.c_str());

			char errorbuf[CURL_ERROR_SIZE];
			curl_easy_setopt(handle, CURLOPT_ERRORBUFFER,      errorbuf);

			user_data_t userData(request);
			curl_easy_setopt(handle, CURLOPT_WRITEFUNCTION,    &receive_data);
			curl_easy_setopt(handle, CURLOPT_WRITEDATA,        &userData);

			curl_easy_setopt(handle, CURLOPT_PROGRESSFUNCTION, &receive_progress);
			curl_easy_setopt(handle, CURLOPT_PROGRESSDATA,     &userData);
			curl_easy_setopt(handle, CURLOPT_NOPROGRESS,       false);

			curl_easy_setopt(handle, CURLOPT_HEADERFUNCTION,   &receive_header);
			curl_easy_setopt(handle, CURLOPT_HEADERDATA,       &userData);

			struct curl_slist* headers = nullptr;
			if(request._entity_tag != NULL_STR)
			{
				headers = curl_slist_append(headers, text::format("If-None-Match: %.*s", (int)request._entity_tag.size(), request._entity_tag.data()).c_str());
				curl_easy_setopt(handle, CURLOPT_HTTPHEADER,    headers);
			}

			if(auto proxySettings = get_proxy_settings(request._url))
			{
				curl_easy_setopt(handle, CURLOPT_PROXY,     proxySettings.server.c_str());
				curl_easy_setopt(handle, CURLOPT_PROXYPORT, proxySettings.port);
				curl_easy_setopt(handle, CURLOPT_PROXYTYPE, proxySettings.socks ? CURLPROXY_SOCKS4 : CURLPROXY_HTTP);
				if(proxySettings.password != NULL_STR)
					curl_easy_setopt(handle, CURLOPT_PROXYUSERPWD, (proxySettings.user + ":" + proxySettings.password).c_str());
			}

			CURLcode rc = curl_easy_perform(handle);
			curl_easy_getinfo(handle, CURLINFO_RESPONSE_CODE, &res);
			curl_easy_cleanup(handle);
			curl_slist_free_all(headers);

			if(rc == 0)
			{
				if(res == 304) // not modified so ignore filter errors
				{
					for(auto const& filter : request._filters)
						filter->receive_end(userData.error);
				}
				else
				{
					for(auto const& filter : request._filters)
					{
						if(!filter->receive_end(userData.error))
						{
							if(error)
								*error = userData.error;
							return 0;
						}
					}
				}
			}
			else
			{
				if(error)
					*error = errorbuf;
			}
		}
		return res;
	}

} /* network */
