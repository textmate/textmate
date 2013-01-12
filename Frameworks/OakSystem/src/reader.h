#ifndef IO_READER_H_EX4Z6E26
#define IO_READER_H_EX4Z6E26

#include <oak/misc.h>

namespace io
{
	struct reader_server_t;
	typedef std::shared_ptr<reader_server_t> reader_server_ptr;

	struct PUBLIC reader_t
	{
		reader_t (int fd = -1);
		virtual ~reader_t ();
		void set_fd (int fd);
		virtual void receive_data (char const* bytes, size_t len) = 0;

	protected:
		int fd;
	private:
		size_t client_key;
		reader_server_ptr reader_server;
	};

} /* oak */ 

#endif /* end of include guard: IO_READER_H_EX4Z6E26 */
