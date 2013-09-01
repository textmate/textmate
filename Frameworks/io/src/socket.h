#ifndef IO_SOCKET_H_TNW4NXOL
#define IO_SOCKET_H_TNW4NXOL

#include <oak/debug.h>

struct socket_t
{
	WATCH_LEAKS(socket_t);

	socket_t ()                     { }
	socket_t (int fd)               { helper = std::make_shared<helper_t>(fd); }
	operator int () const           { ASSERT(helper); return helper->fd; }
	explicit operator bool () const { return helper ? helper->fd != -1 : false; }

private:
	struct helper_t
	{
		WATCH_LEAKS(helper_t);

		helper_t (int fd) : fd(fd) { if(fd != -1) fcntl(fd, F_SETFD, FD_CLOEXEC); }
		~helper_t ()               { if(fd != -1) close(fd); }
		int fd;
	};

	std::shared_ptr<helper_t> helper;
};

#endif /* end of include guard: IO_SOCKET_H_TNW4NXOL */
