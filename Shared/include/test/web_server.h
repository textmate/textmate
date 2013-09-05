#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <arpa/inet.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>
#include <string>
#include <map>
#include <io/path.h>

static const char kCRLF[] = "\r\n";

static int create_socket (int port)
{
	int sock = socket(AF_INET, SOCK_STREAM, 0);
	if(sock != -1)
	{
		fcntl(sock, F_SETFD, FD_CLOEXEC);
		static int const on = 1;
		setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, &on, sizeof(on));

		struct sockaddr_in addr = { sizeof(sockaddr_in), AF_INET, htons(port), { htonl(INADDR_LOOPBACK) } };
		if(bind(sock, (sockaddr*)&addr, sizeof(addr)) != -1)
		{
			if(listen(sock, 256) != -1)
				return sock;
		}
		close(sock);
	}
	return -1;
}

struct request_t
{
	static std::string const kGet;
	static std::string const kHead;
	static std::string const kPost;
	static std::string const kUnknown;

	std::string method, uri, version;
	std::map<std::string, std::string> headers;

	template <typename _InputIter>
	std::string decode_uri (_InputIter const& first, _InputIter const& last)
	{
		std::string res = "";
		char prevDigit = 0;
		enum state_t { kNormal, kFirstDigit, kSecondDigit } state = kNormal;
		for(_InputIter it = first; it != last; ++it)
		{
			char ch = *it;
			switch(state)
			{
				case kNormal:      if(ch != '%') res.push_back(ch); else state = kFirstDigit;                               break;
				case kFirstDigit:  assert(isxdigit(ch)); prevDigit = digittoint(ch); state = kSecondDigit;                  break;
				case kSecondDigit: assert(isxdigit(ch)); res.push_back((prevDigit << 4) + digittoint(ch)); state = kNormal; break;
			}
		}
		assert(state == kNormal);
		return res;
	}

	template <typename _InputIter>
	request_t (_InputIter const& first, _InputIter const& last)
	{
		_InputIter eol = std::search(first, last, &kCRLF[0], &kCRLF[2]);
		assert(eol != last);
		assert(std::count(first, eol, ' ') == 2); // require HTTP/1.0 or higher

		_InputIter mtd = first;
		_InputIter uri = std::find(mtd, eol, ' ') + 1;
		_InputIter ver = std::find(uri, eol, ' ') + 1;

		std::string const mtdStr = std::string(mtd, uri-1);
		if(mtdStr == "GET")
			this->method = kGet;
		else if(mtdStr == "HEAD")
			this->method = kHead;
		else if(mtdStr == "POST")
			this->method = kPost;
		else
			this->method = kUnknown;

		this->uri     = decode_uri(uri, ver-1);
		this->version = std::string(ver, eol);

		for(_InputIter it = eol; it != last; )
		{
			std::advance(it, 2);
			_InputIter bol = it;
			it = std::search(it, last, &kCRLF[0], &kCRLF[2]);
			if(bol == it) // empty line
				break;

			_InputIter value = std::find(bol, it, ':');
			assert(value != it);
			std::string key(bol, value);
			while(++value != it && *value == ' ')
				continue;
			this->headers.emplace(key, std::string(value, it));
		}
	}
};

std::string const request_t::kGet     = "GET";
std::string const request_t::kHead    = "HEAD";
std::string const request_t::kPost    = "POST";
std::string const request_t::kUnknown = "(unknown)";

static request_t parse_request (int sock)
{
	std::string buf;

	ssize_t n;
	char tmp[512];
	while((n = read(sock, tmp, sizeof(tmp))) > 0)
	{
		buf.insert(buf.end(), &tmp[0], &tmp[0] + n);
		if(buf.size() >= 4 && buf.substr(buf.size()-4) == "\r\n\r\n")
			break;
	}

	return request_t(buf.begin(), buf.end());
}

static void send_head (int sock, int rc = 200, char const* status = "OK", std::string const& extraHeaders = "")
{
	static char const* const tpl = "HTTP/1.0 %d %s\r\nServer: OakWebServer/0.1\r\nConnection: close\r\n";

	char* res = NULL;
	if(asprintf(&res, tpl, rc, status) != -1)
	{
		write(sock, res, strlen(res));
		free(res);
		if(!extraHeaders.empty())
			write(sock, extraHeaders.data(), extraHeaders.size());
		write(sock, kCRLF, strlen(kCRLF));
	}
}

static void send_file_head (int sock, std::string const& path)
{
	int fd = open(path.c_str(), O_RDONLY);
	if(fd != -1)
	{
		std::string headers = "Content-Type: text/html\r\n";
		struct stat buf;
		if(fstat(fd, &buf) == 0)
		{
			char str[256];
			snprintf(str, sizeof(str), "Content-Length: %ld\r\n", (long int)buf.st_size);
			headers += str;
		}

		citerate(pair, path::attributes(path))
			headers += pair->first + ": " + pair->second + "\r\n";

		close(fd);
		send_head(sock, 200, "OK", headers.c_str());
	}
	else if(access(path.c_str(), F_OK) == 0)
	{
		send_head(sock, 403, "Forbidden");
	}
	else
	{
		send_head(sock, 404, "Not Found");
	}
}

static void send_file (int sock, std::string const& path)
{
	send_file_head(sock, path);

	int fd = open(path.c_str(), O_RDONLY);
	if(fd != -1)
	{
		ssize_t len;
		char buf[1024];
		while((len = read(fd, buf, sizeof(buf))) > 0)
			write(sock, buf, len);
		close(fd);
	}
}

namespace web
{
	static int server_socket = -1;

	static bool setup_server (int port = 80)
	{
		server_socket = create_socket(port);
		if(server_socket == -1)
			perror("*** error");
		return server_socket != -1;
	}

	static void run_server (std::string const& dir)
	{
		while(true)
		{
			struct sockaddr_in client;
			socklen_t size = sizeof(client);
			int fd = accept(server_socket, (sockaddr*)&client, &size);
			if(fd == -1)
				break;

			request_t const& r = parse_request(fd);
			if(r.method == request_t::kHead)
				send_file_head(fd, dir + r.uri);
			else if(r.method == request_t::kGet)
				send_file(fd, dir + r.uri);
			else
				send_head(fd, 501, "Not Implemented");

			shutdown(fd, SHUT_RDWR);
			close(fd);
		}
	}

} /* web */
