#include "watch.h"

#include <oak/debug.h>
#include <io/io.h>
#include <oak/oak.h>
#include <oak/compat.h>

OAK_DEBUG_VAR(Document_WatchFS);

namespace document
{
	struct watch_server_t
	{
		watch_server_t ();
		~watch_server_t ();

		// called by watch_base_t
		size_t add (std::string const& path, watch_base_t* callback);
		void remove (size_t client_id);

	private:
		// used for book-keeping in master thread
		std::map<size_t, watch_base_t*> clients;
		size_t next_client_id;

		struct watch_info_t
		{
			watch_info_t (std::string const& path) : path(path)
			{
				D(DBF_Document_WatchFS, bug("%s\n", path.c_str()););
			}

			~watch_info_t ()
			{
				D(DBF_Document_WatchFS, bug("%d\n", fd););
			}

			int fd;
			std::string path, path_watched; // path_watched != path when path does not exist
		};

		// used for book-keeping in server thread
		std::map<size_t, watch_info_t*> watch_info;

		pthread_t server_thread;
		int event_queue;
		int read_from_server_pipe, write_to_master_pipe;
		int read_from_master_pipe, write_to_server_pipe;

		static void* server_run_stub (void* arg)
		{
			((watch_server_t*)arg)->server_run();
			return NULL;
		}

		void server_run ();

		void server_add (size_t client_id, std::string const& path);
		void server_remove (size_t client_id);
		void observe (watch_info_t& info, size_t client_id);

		static void data_from_server_stub (CFSocketRef s, CFSocketCallBackType callbackType, CFDataRef address, void const* data, void* info);
		void data_from_server ();
	};

	static watch_server_t& server ()
	{
		static watch_server_t instance;
		return instance;
	}

	static std::string existing_parent (std::string path)
	{
		while(path != "/" && access(path.c_str(), F_OK) != 0)
			path = path::parent(path);
		return path;
	}

	// ================
	// = watch_base_t =
	// ================

	watch_base_t::watch_base_t (std::string const& path)
	{
		client_id = server().add(path, this);
		D(DBF_Document_WatchFS, bug("%s, got client key %zu\n", path.c_str(), client_id););
	}

	watch_base_t::~watch_base_t ()
	{
		D(DBF_Document_WatchFS, bug("client key %zu\n", client_id););
		server().remove(client_id);
	}

	void watch_base_t::callback (int flags, std::string const& newPath)
	{
#ifndef NDEBUG
		static struct { int flag; char const* name; } const flagNames[] =
		{
			{ NOTE_RENAME, ", rename" },
			{ NOTE_WRITE,  ", write"  },
			{ NOTE_DELETE, ", delete" },
			{ NOTE_ATTRIB, ", attribute change" },
			{ NOTE_CREATE, ", create" },
		};
#endif

		D(DBF_Document_WatchFS,
			std::string change = "";
			for(size_t i = 0; i < sizeofA(flagNames); ++i)
				change += (flags & flagNames[i].flag) ? flagNames[i].name : "";
			bug("(%02x)%s\n", flags, change.c_str());
		);
	}

	// ==================
	// = watch_server_t =
	// ==================

	watch_server_t::watch_server_t () : next_client_id(1)
	{
		io::create_pipe(read_from_server_pipe, write_to_master_pipe, true);
		io::create_pipe(read_from_master_pipe, write_to_server_pipe, true);
		pthread_create(&server_thread, NULL, &watch_server_t::server_run_stub, this);

		// attach to run-loop
		CFSocketRef socket = CFSocketCreateWithNative(kCFAllocatorDefault, read_from_server_pipe, kCFSocketReadCallBack, &watch_server_t::data_from_server_stub, NULL);
		CFRunLoopSourceRef source = CFSocketCreateRunLoopSource(kCFAllocatorDefault, socket, 0);
		CFRunLoopAddSource(CFRunLoopGetCurrent(), source, kCFRunLoopDefaultMode);
		CFRelease(source);
		CFRelease(socket);
	}

	watch_server_t::~watch_server_t ()
	{
		D(DBF_Document_WatchFS, bug("\n"););
		close(write_to_server_pipe);  // tell server to shutdown
		close(read_from_server_pipe); // causes server to get -1 when sending us data, another way to tell it to quit
		pthread_join(server_thread, NULL);
	}

	// ============================
	// = Running in master thread =
	// ============================

	size_t watch_server_t::add (std::string const& path, watch_base_t* callback)
	{
		D(DBF_Document_WatchFS, bug("%zu: %s — %p\n", next_client_id, path.c_str(), callback););
		clients.emplace(next_client_id, callback);
		struct { size_t client_id; std::string* path; } packet = { next_client_id, new std::string(path) };
		write(write_to_server_pipe, &packet, sizeof(packet));
		return next_client_id++;
	}

	void watch_server_t::remove (size_t client_id)
	{
		D(DBF_Document_WatchFS, bug("%zu\n", client_id););
		clients.erase(clients.find(client_id));
		struct { size_t client_id; std::string* path; } packet = { client_id, NULL };
		write(write_to_server_pipe, &packet, sizeof(packet));
	}
	
	// ====================
	// = Run-loop related =
	// ====================

	void watch_server_t::data_from_server_stub (CFSocketRef s, CFSocketCallBackType callbackType, CFDataRef address, void const* data, void* info)
	{
		document::server().data_from_server();
	}

	void watch_server_t::data_from_server ()
	{
		struct { size_t client_id; int flags; std::string* path; } packet;
		ssize_t len = read(read_from_server_pipe, &packet, sizeof(packet));
		if(len == sizeof(packet))
		{
			std::map<size_t, watch_base_t*>::iterator it = clients.find(packet.client_id);
			if(it != clients.end())
				it->second->callback(packet.flags, packet.path ? *packet.path : NULL_STR);
			delete packet.path;
		}
	}

	// ============================
	// = Running in server thread =
	// ============================
	
	void watch_server_t::server_add (size_t client_id, std::string const& path)
	{
		D(DBF_Document_WatchFS, bug("%zu: %s\n", client_id, path.c_str()););
		watch_info_t* info = new watch_info_t(path);
		watch_info.emplace(client_id, info);
		observe(*info, client_id);
	}

	void watch_server_t::server_remove (size_t client_id)
	{
		std::map<size_t, watch_info_t*>::iterator it = watch_info.find(client_id);
		D(DBF_Document_WatchFS, bug("client %zu, exists %s\n", client_id, BSTR(it != watch_info.end())););
		if(it != watch_info.end())
		{
			if(it->second->fd != -1)
				close(it->second->fd);
			delete it->second;
			watch_info.erase(it);
		}
	}

	void watch_server_t::observe (watch_info_t& info, size_t client_id)
	{
		info.path_watched = existing_parent(info.path);
		info.fd = open(info.path_watched.c_str(), O_EVTONLY|O_CLOEXEC, 0);
		if(info.fd != -1)
		{
			struct kevent changeList;
			struct timespec timeout = { };
			EV_SET(&changeList, info.fd, EVFILT_VNODE, EV_ADD | EV_ENABLE | EV_CLEAR, NOTE_DELETE | NOTE_WRITE | NOTE_RENAME | NOTE_ATTRIB, 0, (void*)client_id);
			int n = kevent(event_queue, &changeList, 1 /* number of changes */, NULL /* event list */, 0 /* number of events */, &timeout);
			if(n == -1)
				fprintf(stderr, "error observing path, kevent(\"%s\"): %s\n", info.path_watched.c_str(), strerror(errno));
		}
		else
		{
			fprintf(stderr, "error observing path, open(\"%s\"): %s\n", info.path_watched.c_str(), strerror(errno));
		}
	}

	void watch_server_t::server_run ()
	{
		oak::set_thread_name("document::watch_server_t");

		event_queue = kqueue();

		struct kevent changeList;
		struct timespec timeout = { };
		EV_SET(&changeList, read_from_master_pipe, EVFILT_READ, EV_ADD | EV_ENABLE | EV_CLEAR, 0, 0, (void*)0);
		int n = kevent(event_queue, &changeList, 1 /* number of changes */, NULL /* event list */, 0 /* number of events */, &timeout);
		if(n == -1)
			perror("watch server, error monitoring pipe");
		
		struct kevent changed;
		while(kevent(event_queue, NULL /* change list */, 0 /* number of changes */, &changed /* event list */, 1 /* number of events */, NULL) == 1)
		{
			if(changed.filter == EVFILT_READ)
			{
				if(changed.flags & EV_EOF) // master thread closed channel, time to quit
					break;

				struct { size_t client_id; std::string* path; } packet;
				ssize_t len = read(read_from_master_pipe, &packet, sizeof(packet));
				D(DBF_Document_WatchFS, bug("%zd bytes from master\n", len););
				if(len == sizeof(packet))
				{
					if(packet.path)
							server_add(packet.client_id, *packet.path);
					else	server_remove(packet.client_id);
					delete packet.path;
				}
			}
			else if(changed.filter == EVFILT_VNODE)
			{
				size_t client_id = (size_t)changed.udata;
				
				std::map<size_t, watch_info_t*>::iterator it = watch_info.find(client_id);
				if(it != watch_info.end())
				{
					bool did_exist = it->second->path == it->second->path_watched;
					bool does_exist = it->second->path == existing_parent(it->second->path);

					if(did_exist || does_exist)
					{
						int flags = did_exist ? changed.fflags : NOTE_CREATE;
						if(does_exist && (changed.fflags & (NOTE_DELETE | NOTE_WRITE)) == NOTE_DELETE)
							flags ^= (NOTE_DELETE | NOTE_WRITE);

						if((flags & NOTE_RENAME) == NOTE_RENAME)
						{
							for(size_t i = 0; i < 100; ++i)
							{
								if(path::exists(it->second->path))
								{
									flags ^= NOTE_RENAME | (~flags & NOTE_WRITE);
									close(it->second->fd);
									observe(*it->second, it->first);
									break;
								}
								usleep(10);
							}
						}

						std::string path = (flags & NOTE_RENAME) == NOTE_RENAME ? path::for_fd(it->second->fd) : NULL_STR;
						struct { size_t client_id; int flags; std::string* path; } packet = { client_id, flags, path == NULL_STR ? NULL : new std::string(path) };
						if(write(write_to_master_pipe, &packet, sizeof(packet)) == -1)
							break; // channel to master is gone, let’s quit
					}

					if((changed.fflags & NOTE_DELETE) || it->second->path_watched != existing_parent(it->second->path))
					{
						close(it->second->fd);
						observe(*it->second, it->first);
					}
				}
			}
		}

		close(event_queue);
		close(write_to_master_pipe);
		close(read_from_master_pipe);
	}
}
