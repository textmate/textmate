#include <network/src/download.h>
#include <text/src/format.h>
#include <test/web_server.h>
#include <test/jail.h>

#define WEB_SERVER_PORT 61706

void setup_fixtures ()
{
	if(!web::setup_server(WEB_SERVER_PORT))
	{
		fprintf(stderr, "*** unable to setup server for http://localhost:%d/\n", WEB_SERVER_PORT);
		abort();
	}
	std::thread([]{ web::run_server(path::join(__FILE__, "..")); }).detach();
}

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
	OAK_ASSERT_EQ(network::download(network::request_t(url, &myFilter, nullptr), &error), 200);

	struct stat buf;
	int fd = open(__FILE__, O_RDONLY|O_CLOEXEC);
	if(fd != -1 && fstat(fd, &buf) != -1)
	{
		size_t fileSize = buf.st_size;
		char fileContent[fileSize];
		if(read(fd, fileContent, fileSize) == fileSize)
		{
			OAK_ASSERT_EQ(status, "HTTP/1.0 200 OK");
			OAK_ASSERT(headers.find("content-length") != headers.end());
			OAK_ASSERT_EQ(headers.find("content-length")->second, std::to_string(fileSize));
			OAK_ASSERT_EQ(body, std::string(fileContent, fileContent + fileSize));
		}
		close(fd);
	}
}
