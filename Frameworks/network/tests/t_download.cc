#include <network/download.h>
#include <text/format.h>
#include <test/web_server.h>
#include <test/jail.h>

#define WEB_SERVER_PORT 61706

static class WebServerFixture : public CxxTest::GlobalFixture
{
	static void* main (void* arg)
	{
		web::run_server(path::join(__FILE__, ".."));
		return NULL;
	}

	pthread_t _thread;

public:
	bool setUpWorld()
	{
		if(web::setup_server(WEB_SERVER_PORT))
		{
			if(pthread_create(&_thread, NULL, &main, this) == 0)
			{
				pthread_detach(_thread);
				return true;
			}
		}
		return false;
	}

} fixture;

class DownloadTests : public CxxTest::TestSuite
{
public:
	void test_download ()
	{
		struct my_filter : filter_t
		{
			my_filter (std::string& status, std::map<std::string, std::string>& headers, std::string& body) : _status(status), _headers(headers), _body(body) { }

			bool receive_status (std::string const& status)
			{
				_status = status;
				return true;
			}

			bool receive_header (std::string const& header, std::string const& value)
			{
				_headers.insert(std::make_pair(header, value));
				return true;
			}

			bool receive_data (char const* bytes, size_t len)
			{
				_body.insert(_body.end(), bytes, bytes + len);
				return true;
			}

			bool receive_end (std::string& error)
			{
				return true;
			}

		private:
			std::string& _status;
			std::map<std::string, std::string>& _headers;
			std::string& _body;
		};

		std::string status, body, error;
		std::map<std::string, std::string> headers;
		my_filter myFilter(status, headers, body);

		static std::string const url = "http://localhost:" STRINGIFY(WEB_SERVER_PORT) "/t_download.cc";
		TS_ASSERT_EQUALS(network::download(network::request_t(url, &myFilter, NULL), &error), 200);

		struct stat buf;
		int fd = open(__FILE__, O_RDONLY);
		if(fd != -1 && fstat(fd, &buf) != -1)
		{
			size_t fileSize = buf.st_size;
			char fileContent[fileSize];
			if(read(fd, fileContent, fileSize) == fileSize)
			{
				// GlimmerBlocker.org might cause a "HTTP/1.1" response
				TS_ASSERT(strcmp(status.c_str(),"HTTP/1.0 200 OK")==0 || strcmp(status.c_str(),"HTTP/1.1 200 OK")==0);
				TS_ASSERT(headers.find("content-length") != headers.end());
				TS_ASSERT_EQUALS(headers.find("content-length")->second, text::format("%zu", fileSize));
				TS_ASSERT_EQUALS(body, std::string(fileContent, fileContent + fileSize));
			}
			close(fd);
		}
	}
};
