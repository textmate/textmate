#include "download_tbz.h"
#include "filter_check_signature.h"
#include "constants.h"
#include "user_agent.h"
#include "proxy.h"
#include "tbz.h"
#include <io/src/path.h>
#include <io/src/move_path.h>
#include <text/src/case.h>
#include <text/src/decode.h>
#include <text/src/format.h>

namespace network
{
	static std::string const kHTTPEntityTagAttribute = "org.w3.http.etag";

	namespace
	{
		struct user_data_t
		{
			user_data_t (key_chain_t const& keychain, double* progress, double start_progress, double stop_progress, bool const* stop_flag, int tbz_fd, int tmp_fd) : progress(progress), start_progress(start_progress), stop_progress(stop_progress), stop_flag(stop_flag), tbz_fd(tbz_fd), tmp_fd(tmp_fd), verify_signature(keychain, kHTTPSigneeHeader, kHTTPSignatureHeader)
			{
				verify_signature.setup();
			}

			void receive (size_t len)
			{
				received += len;
				if(progress)
					*progress = start_progress + (stop_progress - start_progress) * (total ? received / (double)total : 0);
			}

			bool should_stop () const
			{
				return stop_flag && *stop_flag;
			}

			bool modified = true;

			double* progress;
			double start_progress;
			double stop_progress;

			bool const* stop_flag;

			std::string etag = NULL_STR;

			int tbz_fd;
			int tmp_fd;

			network::check_signature_t verify_signature;

			size_t received = 0;
			size_t total = 0;
		};

		static size_t receive_header (void* ptr, size_t size, size_t nmemb, void* udata)
		{
			user_data_t& data = *static_cast<user_data_t*>(udata);

			char const* bytes = (char const*)ptr;
			size_t len = nmemb * size;

			if(len > 7 && strncmp("HTTP/1.", bytes, 7) == 0 && std::find(bytes, bytes + len, ':') == bytes + len)
			{
				static char const kCRLF[] = "\r\n";
				char const* last  = std::search(bytes, bytes + size * nmemb, &kCRLF[0], &kCRLF[2]);
				if(std::string(bytes, last).find("HTTP/1.1 304") == 0)
					data.modified = false;
			}
			else if(len != 2 || strncmp("\r\n", bytes, 2) != 0)
			{
				while(len && (bytes[len-1] == '\r' || bytes[len-1] == '\n'))
					--len;

				size_t keyLast    = std::find(bytes, bytes + len, ':') - bytes;
				size_t valueFirst = keyLast;
				while(valueFirst < len && bytes[++valueFirst] == ' ')
					;

				if(valueFirst != len)
				{
					std::string header = text::lowercase(std::string(bytes, bytes + keyLast));
					std::string value(bytes + valueFirst, bytes + len);

					if(header == "etag")
						data.etag = value;
					else if(header == "content-length")
						data.total = strtol(value.c_str(), nullptr, 10);
					else if(header == kHTTPSigneeHeader || header == kHTTPSignatureHeader)
						data.verify_signature.receive_header(header, value);
				}
			}
			return data.should_stop() ? 0 : size * nmemb;
		}

		static size_t receive_data (void* ptr, size_t size, size_t nmemb, void* udata)
		{
			user_data_t& data = *static_cast<user_data_t*>(udata);

			write(data.tbz_fd, ptr, size * nmemb);
			write(data.tmp_fd, ptr, size * nmemb);
			data.verify_signature.receive_data((char const*)ptr, size * nmemb);
			data.receive(size * nmemb);
			return data.should_stop() ? 0 : size * nmemb;
		}
	}

	std::string download_tbz (std::string const& url, key_chain_t const& keyChain, std::string const& destination, std::string& error, double* progress, double progressStart, double progressStop, bool const* stopFlag)
	{
		std::string res = NULL_STR;
		if(CURL* handle = curl_easy_init())
		{
			std::string tbzDestination = path::cache("dl_archive_contents");
			mkdir(tbzDestination.c_str(), S_IRWXU|S_IRWXG|S_IRWXO);
			tbz_t tbz(tbzDestination);

			std::string tmpPath = path::temp("dl_bytes");
			int tmpInput = open(tmpPath.c_str(), O_CREAT|O_TRUNC|O_WRONLY|O_CLOEXEC, S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH);

			// ========
			// = Curl =
			// ========

			user_data_t data(keyChain, progress, progressStart, progressStop, stopFlag, tbz.input_fd(), tmpInput);

			curl_easy_setopt(handle, CURLOPT_URL,              url.c_str());
			curl_easy_setopt(handle, CURLOPT_FOLLOWLOCATION,   true);
			curl_easy_setopt(handle, CURLOPT_FAILONERROR,      true);
			curl_easy_setopt(handle, CURLOPT_ENCODING,         "");
			curl_easy_setopt(handle, CURLOPT_USERAGENT,        create_agent_info_string().c_str());
			curl_easy_setopt(handle, CURLOPT_NOSIGNAL,         1);

			char errorbuf[CURL_ERROR_SIZE];
			curl_easy_setopt(handle, CURLOPT_ERRORBUFFER,      errorbuf);

			curl_easy_setopt(handle, CURLOPT_WRITEFUNCTION,    &receive_data);
			curl_easy_setopt(handle, CURLOPT_WRITEDATA,        &data);
			curl_easy_setopt(handle, CURLOPT_HEADERFUNCTION,   &receive_header);
			curl_easy_setopt(handle, CURLOPT_HEADERDATA,       &data);

			struct curl_slist* headers = nullptr;
			std::string const etag = path::get_attr(destination, kHTTPEntityTagAttribute);
			if(etag != NULL_STR)
			{
				headers = curl_slist_append(headers, text::format("If-None-Match: %.*s", (int)etag.size(), etag.data()).c_str());
				curl_easy_setopt(handle, CURLOPT_HTTPHEADER,    headers);
			}

			if(auto proxySettings = get_proxy_settings(url))
			{
				curl_easy_setopt(handle, CURLOPT_PROXY,     proxySettings.server.c_str());
				curl_easy_setopt(handle, CURLOPT_PROXYPORT, proxySettings.port);
				curl_easy_setopt(handle, CURLOPT_PROXYTYPE, proxySettings.socks ? CURLPROXY_SOCKS4 : CURLPROXY_HTTP);
				if(proxySettings.password != NULL_STR)
					curl_easy_setopt(handle, CURLOPT_PROXYUSERPWD, (proxySettings.user + ":" + proxySettings.password).c_str());
			}

			long serverReply = 0;
			switch(curl_easy_perform(handle))
			{
				case 0:                 curl_easy_getinfo(handle, CURLINFO_RESPONSE_CODE, &serverReply);  break;
				case CURLE_WRITE_ERROR: error = (stopFlag && *stopFlag ? "Download stopped." : errorbuf); break;
				default:                error = errorbuf;                                                 break;
			}

			curl_easy_cleanup(handle);
			curl_slist_free_all(headers);

			// =============
			// = Post Curl =
			// =============

			close(tmpInput);

			bool goodSignature = false;
			if(serverReply == 200)
			{
				if((goodSignature = data.verify_signature.receive_end(error)))
				{
					path::set_attr(tmpPath, kHTTPEntityTagAttribute, data.etag);
					path::set_attr(tmpPath, kHTTPSigneeHeader,       data.verify_signature.signee());
					path::set_attr(tmpPath, kHTTPSignatureHeader,    data.verify_signature.signature());
					path::rename_or_copy(tmpPath, destination);
				}
			}
			else if(serverReply == 304)
			{
				struct stat buf;
				int fd = open(destination.c_str(), O_RDONLY|O_CLOEXEC);
				if(fd != -1 && fstat(fd, &buf) != -1)
				{
					char bytes[4096];
					data.total = buf.st_size;
					while(data.received < data.total && !data.should_stop())
					{
						ssize_t len = read(fd, bytes, sizeof(bytes));
						if(len == -1)
							break;

						write(tbz.input_fd(), bytes, len);
						data.receive(len);
					}
					close(fd);
				}
			}
			else if(serverReply != 0)
			{
				error = text::format("Unexpected server reply (%ld).", serverReply);
			}

			if(!goodSignature) // If not, tmpPath has been moved to destination
				unlink(tmpPath.c_str());

			if(tbz.wait_for_tbz())
			{
				if(serverReply == 304 || goodSignature)
					res = tbzDestination;
			}
			else if(serverReply == 200 || serverReply == 304)
			{
				error = "Extracting archive.";
			}

			if(res == NULL_STR)
				path::remove(tbzDestination);
		}
		return res;
	}

} /* network */
