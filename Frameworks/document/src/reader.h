#ifndef READER_H_DD5U5J92
#define READER_H_DD5U5J92

#include "document.h" // document::document_t::reader_t
#include <file/bytes.h>
#include <oak/misc.h>
#include <oak/debug.h>
#include <sys/mman.h>

namespace reader
{
	struct async_t : document::document_t::reader_t
	{
		WATCH_LEAKS(async_t);

		async_t (std::string const& path);
		~async_t ();
		io::bytes_ptr next ();

	private:
		void setup_request ();
		aiocb* request;

		int fd;
		off_t offset, file_size;
	};

	struct mapped_t : document::document_t::reader_t
	{
		WATCH_LEAKS(mapped_t);

		mapped_t (std::string const& path);
		~mapped_t ();
		io::bytes_ptr next ();

	private:
		char* memory;
		int fd;
		off_t offset, file_size, mapped_size;
	};

	struct fopen_t : document::document_t::reader_t
	{
		WATCH_LEAKS(fopen_t);

		fopen_t (std::string const& path);
		~fopen_t ();
		io::bytes_ptr next ();

	private:
		FILE* fp;
	};

	struct open_t : document::document_t::reader_t
	{
		WATCH_LEAKS(open_t);

		open_t (std::string const& path);
		~open_t ();
		io::bytes_ptr next ();

	protected:
		int fd;
	};

} /* reader */

#endif /* end of include guard: READER_H_DD5U5J92 */
