#ifndef DOWNLOAD_H_S5YBYD2
#define DOWNLOAD_H_S5YBYD2

#include <oak/oak.h>

struct filter_t
{
	virtual ~filter_t () { }

	virtual bool setup ()                                                             { return true; }
	virtual bool receive_status (std::string const& statusString)                     { return true; }
	virtual bool receive_header (std::string const& header, std::string const& value) { return true; }
	virtual bool receive_data (char const* bytes, size_t len)                         { return true; }
	virtual bool receive_end (std::string& error)                                     { return true; }
	virtual std::string name ()                                                       { return "filter"; }
};

namespace network
{
	struct request_t
	{
		request_t (std::string const& url, filter_t* firstFilter = NULL, ...);
		request_t& add_filter (filter_t* filter);
		request_t& set_user_agent (std::string const& user_agent);
		request_t& set_entity_tag (std::string const& entity_tag);
		request_t& watch_stop_flag (bool const* stopFlag);
		request_t& update_progress_variable (double* percentDone, double min = 0, double max = 1);

	private:
		friend long download (request_t const& request, std::string* error);
		friend int receive_progress (void* udata, double dltotal, double dlnow, double ultotal, double ulnow);
		friend size_t receive_header (void* ptr, size_t size, size_t nmemb, void* udata);
		friend size_t receive_data (void* ptr, size_t size, size_t nmemb, void* udata);

		std::string            _url;
		std::vector<filter_t*> _filters;
		std::string            _user_agent   = NULL_STR;
		std::string            _entity_tag   = NULL_STR;
		bool const*            _stop_flag    = NULL;
		double*                _progress     = NULL;
		double                 _progress_min = 0;
		double                 _progress_max = 0;
	};

	long download (request_t const& request, std::string* error);

} /* net */

#endif /* end of include guard: DOWNLOAD_NG_H_S5YBYD2 */
