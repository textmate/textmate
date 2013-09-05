#include "reader.h"
#include <io/pipe.h>
#include <oak/oak.h>
#include <oak/compat.h>
#include <cf/callback.h>

namespace command
{
	struct reader_server_t
	{
		reader_server_t ();
		~reader_server_t ();

		size_t add (int fd, reader_t* callback);
		void remove (int fd, size_t client_key);

	private:
		size_t next_client_key;
		std::map<size_t, reader_t*> client_to_callback;
		std::set<size_t> clients_consumed_eof;

		void server_run ();
		void master_run ();

		pthread_t server_thread;
		int read_from_master, write_to_server;

		cf::callback_ptr run_loop_source;
		struct data_received_t { size_t client_key; char* bytes; ssize_t length; };
		std::vector<data_received_t> data_received;
		pthread_mutex_t data_received_mutex;
	};

	static reader_server_ptr server ()
	{
		static auto instance = std::make_shared<reader_server_t>();
		return instance;
	}

	// ============
	// = reader_t =
	// ============

	reader_t::reader_t (int fd) : fd(-1)
	{
		reader_server = server();
		if(fd != -1)
			set_fd(fd);
	}

	reader_t::~reader_t ()
	{
		if(fd != -1)
			reader_server->remove(fd, client_key);
	}

	void reader_t::set_fd (int fd)
	{
		if(this->fd != -1)
			reader_server->remove(this->fd, client_key);

		this->fd = fd;
		client_key = reader_server->add(fd, this);
	}
	
	// ===================
	// = reader_server_t =
	// ===================
	
	reader_server_t::reader_server_t () : next_client_key(1)
	{
		struct runner_t {
			static void* server (void* arg) { ((reader_server_t*)arg)->server_run(); return NULL; }
		};

		io::create_pipe(read_from_master, write_to_server, true);
		run_loop_source = cf::create_callback(std::bind(&reader_server_t::master_run, this));

		pthread_mutex_init(&data_received_mutex, NULL);
		pthread_create(&server_thread, NULL, &runner_t::server, this);
	}

	reader_server_t::~reader_server_t ()
	{
		close(write_to_server);
		pthread_join(server_thread, NULL);
		pthread_mutex_destroy(&data_received_mutex);
	}

	size_t reader_server_t::add (int fd, reader_t* callback)
	{
		client_to_callback.emplace(next_client_key, callback);
		struct packet_t { size_t client_key; int fd; } packet = { next_client_key, fd };
		write(write_to_server, &packet, sizeof(packet));
		return next_client_key++;
	}

	void reader_server_t::remove (int fd, size_t client_key)
	{
		client_to_callback.erase(client_to_callback.find(client_key));
		clients_consumed_eof.erase(client_key);
		struct packet_t { size_t client_key; int fd; } packet = { 0, fd };
		write(write_to_server, &packet, sizeof(packet));
	}

	void reader_server_t::master_run ()
	{
		pthread_mutex_lock(&data_received_mutex);
		std::vector<data_received_t> tmp;
		data_received.swap(tmp);
		pthread_mutex_unlock(&data_received_mutex);

		iterate(it, tmp)
		{
			// required check as we may receive a post-EOF “bytes available” with ‘it->length == 0’
			if(clients_consumed_eof.find(it->client_key) == clients_consumed_eof.end())
			{
				std::map<size_t, reader_t*>::iterator client = client_to_callback.find(it->client_key);
				if(client != client_to_callback.end())
					client->second->receive_data(it->bytes, it->length);
				if(it->length == 0)
					clients_consumed_eof.insert(it->client_key);
			}
			delete it->bytes;
		}
	}

	void reader_server_t::server_run ()
	{
		oak::set_thread_name("oak::reader_server_t");

		int event_queue = kqueue();

		struct kevent changeList[] = { { (uintptr_t)read_from_master, EVFILT_READ, EV_ADD | EV_ENABLE | EV_CLEAR, 0, 0, 0 } };
		if(-1 == kevent(event_queue, changeList, sizeofA(changeList), NULL /* event list */, 0 /* number of events */, NULL))
			perror("reader server, error monitoring pipe");

		struct kevent changed;
		while(kevent(event_queue, NULL /* change list */, 0 /* number of changes */, &changed /* event list */, 1 /* number of events */, NULL) == 1)
		{
			if(changed.filter != EVFILT_READ)
				continue;

			if(changed.ident == read_from_master)
			{
				struct packet_t { size_t client_key; int fd; } packet;
				ssize_t len = read(read_from_master, &packet, sizeof(packet));
				if(len != sizeof(packet))
				{
					break;
				}
				else if(packet.client_key != 0)
				{
					struct kevent changeList = { (uintptr_t)packet.fd, EVFILT_READ, EV_ADD | EV_ENABLE | EV_CLEAR, 0, 0, (void*)packet.client_key };
					if(-1 == kevent(event_queue, &changeList, 1, NULL /* event list */, 0 /* number of events */, NULL))
						perror("reader server, running, error monitoring pipe");
				}
				else
				{
					close(packet.fd);
				}
			}
			else
			{
				size_t client_key = (size_t)changed.udata;

				pthread_mutex_lock(&data_received_mutex);
				if(changed.data > 0)
				{
					char* buf = new char[changed.data];
					ssize_t len = read(changed.ident, buf, changed.data);
					data_received.push_back((data_received_t){ client_key, buf, len });
				}
				if(changed.flags & EV_EOF)
				{
					data_received.push_back((data_received_t){ client_key, NULL, 0 });
				}
				pthread_mutex_unlock(&data_received_mutex);

				run_loop_source->signal();
			}
		}

		close(read_from_master);
	}

} /* command */ 
