#include "reader.h"

#define BLOCK_SIZE (8 * 1024)
#define MAP_SIZE   (32 * 1024 * 1024)

namespace reader
{
	// ============
	// = Async IO =
	// ============

	async_t::async_t (std::string const& path)
	{
		if((fd = open(path.c_str(), O_RDONLY|O_CLOEXEC)) != -1)
		{
			fcntl(fd, F_NOCACHE, 1);

			struct stat buf;
			fstat(fd, &buf);
			offset = 0;
			file_size = buf.st_size;

			setup_request();
		}
	}

	async_t::~async_t ()
	{
		if(fd != -1)
			close(fd);
	}

	void async_t::setup_request ()
	{
		request = NULL;
		if(off_t len = std::min<off_t>(BLOCK_SIZE, file_size - offset))
		{
			request = new aiocb;
			bzero(request, sizeof(*request));

			request->aio_fildes = fd;
			request->aio_offset = offset;
			request->aio_buf    = new char[len];
			request->aio_nbytes = len;
			aio_read(request);
		}
	}

	io::bytes_ptr async_t::next ()
	{
		if(fd == -1 || !request)
			return io::bytes_ptr();

		int rc = aio_suspend(&request, 1, NULL);
		if(rc == -1)
			return io::bytes_ptr();
		ssize_t len = aio_return(request);
		if(len == -1)
			return io::bytes_ptr();

		offset += request->aio_nbytes;
		auto res = std::make_shared<io::bytes_t>((char const*)request->aio_buf, request->aio_nbytes);

		delete request;
		setup_request();

		return res;
	}

	// =================
	// = Memory Mapped =
	// =================

	mapped_t::mapped_t (std::string const& path)
	{
		offset = 0;
		mapped_size = 0;
		if((fd = open(path.c_str(), O_RDONLY|O_CLOEXEC)) != -1)
		{
			fcntl(fd, F_NOCACHE, 1);

			struct stat buf;
			fstat(fd, &buf);
			file_size = buf.st_size;
		}
	}

	mapped_t::~mapped_t ()
	{
		if(mapped_size)
		{
			if(munmap(memory, mapped_size) != 0)
				perror("munmap");
		}

		if(fd != -1)
			close(fd);
	}

	io::bytes_ptr mapped_t::next ()
	{
		if(fd == -1)
			return io::bytes_ptr();

		if(mapped_size)
		{
			if(munmap(memory, mapped_size) != 0)
				perror("munmap");
		}

		offset += mapped_size;
		mapped_size = std::min<off_t>(MAP_SIZE, file_size - offset);

		if(mapped_size == 0)
			return io::bytes_ptr();

		memory = (char*)mmap(NULL, mapped_size, PROT_READ, MAP_PRIVATE, fd, offset);
		return std::make_shared<io::bytes_t>(memory, mapped_size, false);
	}

	// ===========
	// = fopen() =
	// ===========

	fopen_t::fopen_t (std::string const& path)
	{
		if(fp = fopen(path.c_str(), "r"))
			fcntl(fileno(fp), F_NOCACHE, 1);
	}

	fopen_t::~fopen_t ()
	{
		if(fp)
			fclose(fp);
	}

	io::bytes_ptr fopen_t::next ()
	{
		if(!fp)
			return io::bytes_ptr();

		char* buf = new char[BLOCK_SIZE];
		if(size_t len = fread(buf, 1, BLOCK_SIZE, fp))
			return std::make_shared<io::bytes_t>(buf, len);
		delete[] buf;
		return io::bytes_ptr();
	}

	// ==========
	// = open() =
	// ==========

	open_t::open_t (std::string const& path)
	{
		if((fd = open(path.c_str(), O_RDONLY|O_CLOEXEC)) != -1)
			fcntl(fd, F_NOCACHE, 1);
	}

	open_t::~open_t ()
	{
		if(fd != -1)
			close(fd);
	}

	io::bytes_ptr open_t::next ()
	{
		if(fd == -1)
			return io::bytes_ptr();

		char* buf = new char[BLOCK_SIZE];
		ssize_t len = read(fd, buf, BLOCK_SIZE);
		if(len > 0)
			return std::make_shared<io::bytes_t>(buf, len);
		delete[] buf;
		return io::bytes_ptr();
	}

} /* reader */
