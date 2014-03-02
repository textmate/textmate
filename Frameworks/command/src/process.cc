#include "process.h"
#include <io/io.h>
#include <oak/oak.h>
#include <oak/datatypes.h>
#include <oak/compat.h>
#include <oak/server.h>
#include <text/format.h>
#include <cf/callback.h>

OAK_DEBUG_VAR(Process);

namespace command
{
	struct process_server_t
	{
		process_server_t ();
		~process_server_t ();

		size_t add (pid_t pid, process_t* callback);
		void remove (size_t client_key);

	private:
		size_t next_client_key;
		std::map<size_t, process_t*> client_to_callback;

		void server_run ();
		void master_run ();
		void wait_for_process (int pid, size_t client_key, bool wasRunning = true);

		pthread_t server_thread;
		int read_from_master, write_to_server;

		cf::callback_ptr run_loop_source;
		struct process_exit_t { size_t client_key; int return_code; };
		std::vector<process_exit_t> process_exit;
		pthread_mutex_t process_exit_mutex;
	};

	static process_server_ptr server ()
	{
		static auto instance = std::make_shared<process_server_t>();
		return instance;
	}

	// ===========================
	// = Cleanup Temporary Files =
	// ===========================

	struct cleanup_server_t
	{
		~cleanup_server_t ()
		{
			for(auto const& path : _paths)
				unlink(path.c_str());
		}

		void insert (std::string const& path) { _paths.insert(path); }
		void erase (std::string const& path)  { _paths.erase(path); }

	private:
		std::set<std::string> _paths;
	};

	static cleanup_server_ptr cleaner ()
	{
		static auto instance = std::make_shared<cleanup_server_t>();
		return instance;
	}

	// =============
	// = process_t =
	// =============

	process_t::process_t () : input_fd(-1), is_running(false)
	{
		D(DBF_Process, bug("\n"););

		temp_path = strdup(path::join(path::temp(), "textmate_command.XXXXXX").c_str());
		client_key = 0;
		process_server = server();
		cleanup_server = cleaner();
	}

	process_t::~process_t ()
	{
		D(DBF_Process, bug("\n"););

		if(client_key)
			process_server->remove(client_key);

		cleanup_server->erase(temp_path);
		unlink(temp_path);
		free(temp_path);
	}

	pid_t process_t::launch ()
	{
		for(auto const& it : environment)
		{
			if(it.first.size() + it.second.size() + 2 >= ARG_MAX)
				fprintf(stderr, "*** variable exceeds ARG_MAX: %s\n", it.first.c_str());
		}

		ASSERT(command.find("#!") == 0);

		int cmd_fd = mkstemp(temp_path);
		cleanup_server->insert(temp_path);
		fchmod(cmd_fd, S_IRWXU);
		write(cmd_fd, command.data(), command.size());
		close(cmd_fd);

		D(DBF_Process, bug("launch script:\n%s\n", command.c_str()););
		
		int inputPipe[2], outputPipe[2], errorPipe[2];
		if(input_fd == -1)
		{
			pipe(inputPipe);
			fcntl(inputPipe[1], F_SETFD, FD_CLOEXEC);
		}
		else
		{
			inputPipe[0] = input_fd;
		}

		pipe(outputPipe);
		pipe(errorPipe);
		fcntl(outputPipe[0], F_SETFD, FD_CLOEXEC);
		fcntl(errorPipe[0],  F_SETFD, FD_CLOEXEC);
		output_fd = outputPipe[0];
		error_fd = errorPipe[0];

		std::string const wdString = directory != NULL_STR ? directory : path::temp();
		char const* workingDir = wdString.c_str();
		oak::c_array env(environment);

		process_id = vfork();
		if(process_id == 0)
		{
			signal(SIGINT,  SIG_DFL);
			signal(SIGTERM, SIG_DFL);
			signal(SIGPIPE, SIG_DFL);
			signal(SIGUSR1, SIG_DFL);
			setpgid(0, getpid());

			int const oldOutErr[] = { 0, 1, 2 };
			int const newOutErr[] = { inputPipe[0], outputPipe[1], errorPipe[1] };
			for(int fd : oldOutErr) close(fd);
			for(int fd : newOutErr) dup(fd);
			for(int fd : newOutErr) close(fd);

			chdir(workingDir);
			fcntl(0, F_SETOWN, getppid());

			char* argv[] = { temp_path, NULL };
			execve(temp_path, argv, env);
			perror("interpreter failed");
			_exit(0);
		}
		D(DBF_Process, bug("vfork() → %d\n", process_id););

		if(process_id == -1)
		{
			perror("vfork() failed");
		}

		is_running = true;
		client_key = process_server->add(process_id, this);

		int const fds[] = { inputPipe[0], outputPipe[1], errorPipe[1] };
		for(int fd : fds) close(fd);

		if(input_fd == -1)
			input_fd = inputPipe[1];

		return process_id;
	}

	void process_t::did_exit (int rc)
	{
		D(DBF_Process, bug("pid %d, rc %d\n", process_id, rc););
		is_running = false;
	}

	// ====================
	// = process_server_t =
	// ====================

	process_server_t::process_server_t () : next_client_key(1)
	{
		struct runner_t {
			static void* server (void* arg) { ((process_server_t*)arg)->server_run(); return NULL; }
		};

		io::create_pipe(read_from_master, write_to_server, true);
		run_loop_source = cf::create_callback(std::bind(&process_server_t::master_run, this));

		pthread_mutex_init(&process_exit_mutex, NULL);
		pthread_create(&server_thread, NULL, &runner_t::server, this);
	}

	process_server_t::~process_server_t ()
	{
		D(DBF_Process, bug("\n"););
		close(write_to_server);
		pthread_join(server_thread, NULL);
		pthread_mutex_destroy(&process_exit_mutex);
	}

	size_t process_server_t::add (pid_t pid, process_t* callback)
	{
		D(DBF_Process, bug("pid %d, client key %zu\n", pid, next_client_key););
		client_to_callback.emplace(next_client_key, callback);
		struct packet_t { size_t client_key; pid_t pid; } packet = { next_client_key, pid };
		write(write_to_server, &packet, sizeof(packet));
		return next_client_key++;
	}

	void process_server_t::remove (size_t client_key)
	{
		D(DBF_Process, bug("client key %zu\n", client_key););
		client_to_callback.erase(client_to_callback.find(client_key));
	}

	void process_server_t::master_run ()
	{
		std::vector<process_exit_t> tmp;
		pthread_mutex_lock(&process_exit_mutex);
		tmp.swap(process_exit);
		pthread_mutex_unlock(&process_exit_mutex);

		for(auto const& it : tmp)
		{
			std::map<size_t, process_t*>::iterator client = client_to_callback.find(it.client_key);
			if(client != client_to_callback.end())
				client->second->did_exit(it.return_code);
		}
	}

	void process_server_t::server_run ()
	{
		oak::set_thread_name("oak::process_server_t");

		int event_queue = kqueue();

		struct kevent changeList[] = { { (uintptr_t)read_from_master, EVFILT_READ, EV_ADD | EV_ENABLE | EV_CLEAR, 0, 0, 0 } };
		if(-1 == kevent(event_queue, changeList, sizeofA(changeList), NULL /* event list */, 0 /* number of events */, NULL))
			perror("process server, error monitoring pipe");

		struct kevent changed;
		while(kevent(event_queue, NULL /* change list */, 0 /* number of changes */, &changed /* event list */, 1 /* number of events */, NULL) == 1)
		{
			if(changed.ident == read_from_master)
			{
				struct packet_t { size_t client_key; pid_t pid; } packet;
				ssize_t len = read(read_from_master, &packet, sizeof(packet));
				if(len != sizeof(packet))
				{
					break;
				}
				else
				{
					struct kevent changeList = { (uintptr_t)packet.pid, EVFILT_PROC, EV_ADD | EV_ENABLE | EV_ONESHOT, NOTE_EXIT, 0, (void*)packet.client_key };
					int res = kevent(event_queue, &changeList, 1, NULL /* event list */, 0 /* number of events */, NULL);
					if(res == -1 && errno == ESRCH)
						wait_for_process(packet.pid, packet.client_key, false);
					else if(res == -1)
						perror("observing process via kevent()");
				}
			}
			else
			{
				wait_for_process((pid_t)changed.ident, (size_t)changed.udata);
			}
		}

		close(read_from_master);
	}

	void process_server_t::wait_for_process (int pid, size_t client_key, bool wasRunning)
	{
		int status = 0;
		bool didFindProcess = waitpid(pid, &status, 0) == pid;
		int rc = didFindProcess && WIFEXITED(status) ? WEXITSTATUS(status) : (WIFSIGNALED(status) ? 0 : -1);

		if(!didFindProcess)
			fprintf(stderr, "*** no process for pid %d, was running %s\n", pid, wasRunning ? "YES" : "NO");
		else if(WIFSIGNALED(status))
			fprintf(stderr, "*** process terminated: %s\n", strsignal(WTERMSIG(status)));
		else if(!WIFEXITED(status))
			fprintf(stderr, "*** process terminated abnormally %d, was running %s\n", status, wasRunning ? "YES" : "NO");

		pthread_mutex_lock(&process_exit_mutex);
		process_exit.push_back((process_exit_t){ client_key, rc });
		pthread_mutex_unlock(&process_exit_mutex);

		run_loop_source->signal();
	}

} /* command */ 
