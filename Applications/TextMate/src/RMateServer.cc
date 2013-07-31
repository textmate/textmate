#include <oak/oak.h>
#include <text/parse.h>
#include <text/hexdump.h>
#include <document/collection.h>
#include <oak/debug.h>
#include <authorization/authorization.h>
#include <io/io.h>
#include <OakAppKit/IOAlertPanel.h>

OAK_DEBUG_VAR(RMateServer);

/*
	open
	path: [«path»|-]
	real-path: «path»
	token: «string»
	display-name: «string»
	selection: «line»[:«column»][-«line»[:«column»]]
	file-type: «scope»
	project-uuid: «uuid»
	add-to-recents: «boolean»
	re-activate: «boolean»
	authorization: «blob»
	wait: «boolean»
	data-on-save: «boolean»
	data-on-close: «boolean»

	data: «integer»
	⋮
	data: «integer»
*/

// ============================
// = Socket run loop callback =
// ============================

struct socket_callback_t
{
	WATCH_LEAKS(socket_callback_t);

	template <typename F>
	socket_callback_t (F f, socket_t const& fd)
	{
		D(DBF_RMateServer, bug("%p, %d\n", this, (int)fd););

		helper.reset(new helper_t<F>(f, fd, this));

		CFSocketContext const context = { 0, helper.get(), NULL, NULL, NULL };
		socket = CFSocketCreateWithNative(kCFAllocatorDefault, fd, kCFSocketReadCallBack, callback, &context);
		CFSocketSetSocketFlags(socket, CFSocketGetSocketFlags(socket) & ~kCFSocketCloseOnInvalidate);
		run_loop_source = CFSocketCreateRunLoopSource(kCFAllocatorDefault, socket, 0);
		CFRunLoopAddSource(CFRunLoopGetCurrent(), run_loop_source, kCFRunLoopDefaultMode);
	}

	~socket_callback_t ()
	{
		D(DBF_RMateServer, bug("%p\n", this););
		ASSERT(CFRunLoopContainsSource(CFRunLoopGetCurrent(), run_loop_source, kCFRunLoopDefaultMode));

		CFRunLoopRemoveSource(CFRunLoopGetCurrent(), run_loop_source, kCFRunLoopDefaultMode);
		CFSocketInvalidate(socket);
		CFRelease(run_loop_source);
		CFRelease(socket);
	}

	static void callback (CFSocketRef s, CFSocketCallBackType callbackType, CFDataRef address, void const* data, void* info)
	{
		(*(helper_base_t*)info)();
	}

private:
	struct helper_base_t
	{
		WATCH_LEAKS(helper_base_t);

		virtual ~helper_base_t () { }
		virtual void operator() () = 0;
	};

	template <typename F>
	struct helper_t : helper_base_t
	{
		helper_t (F f, socket_t const& socket, socket_callback_t* parent) : f(f), socket(socket), parent(parent) { }
		void operator() () { if(!f(socket)) delete parent; }
	private:
		F f;
		socket_t socket;
		socket_callback_t* parent;
	};

	typedef std::shared_ptr<helper_base_t> helper_ptr;

	helper_ptr helper;
	CFSocketRef socket;
	CFRunLoopSourceRef run_loop_source;
};

typedef std::shared_ptr<socket_callback_t> socket_callback_ptr;

// ======================
// = Return system info =
// ======================

static std::string sys_info (int field)
{
	char buf[1024];
	size_t bufSize = sizeof(buf);
	int request[] = { CTL_KERN, field };

	if(sysctl(request, sizeofA(request), buf, &bufSize, NULL, 0) != -1)
		return std::string(buf, buf + bufSize - 1);

	return "?";
}

static bool rmate_connection_handler_t (socket_t const& socket);

namespace
{
	static char const* socket_path ()
	{
		static std::string const str = text::format("/tmp/textmate-%d.sock", getuid());
		return str.c_str();
	}

	struct mate_server_t
	{
		mate_server_t ()
		{
			_socket_path = socket_path();
			D(DBF_RMateServer, bug("%s\n", _socket_path););
			if(unlink(_socket_path) == -1 && errno != ENOENT)
			{
				OakRunIOAlertPanel("Unable to delete socket left from old instance:\n%s", _socket_path);
				return;
			}

			socket_t fd(socket(AF_UNIX, SOCK_STREAM, 0));
			fcntl(fd, F_SETFD, FD_CLOEXEC);
			struct sockaddr_un addr = { 0, AF_UNIX };
			strcpy(addr.sun_path, _socket_path);
			addr.sun_len = SUN_LEN(&addr);
			if(bind(fd, (sockaddr*)&addr, sizeof(addr)) == -1)
				OakRunIOAlertPanel("Could not bind to socket:\n%s", _socket_path);
			else if(listen(fd, 5) == -1)
				OakRunIOAlertPanel("Could not listen to socket");

			_callback.reset(new socket_callback_t(&rmate_connection_handler_t, fd));
		}

		~mate_server_t ()
		{
			D(DBF_RMateServer, bug("%s\n", _socket_path););
			unlink(_socket_path);
		}

	private:
		char const* _socket_path;
		socket_callback_ptr _callback;
	};

	struct rmate_server_t
	{
		rmate_server_t (uint16_t port, bool listenForRemoteClients) : _port(port), _listen_for_remote_clients(listenForRemoteClients)
		{
			D(DBF_RMateServer, bug("port %ud, remote clients %s\n", _port, BSTR(_listen_for_remote_clients)););

			static int const on = 1;
			socket_t fd(socket(AF_INET6, SOCK_STREAM, 0));
			setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &on, sizeof(on));

			fcntl(fd, F_SETFD, FD_CLOEXEC);
			struct sockaddr_in6 iaddr = { sizeof(sockaddr_in6), AF_INET6, htons(_port) };
			iaddr.sin6_addr   = listenForRemoteClients ? in6addr_any : in6addr_loopback;
			if(-1 == bind(fd, (sockaddr*)&iaddr, sizeof(iaddr)))
				fprintf(stderr, "bind(): %s\n", strerror(errno));
			if(-1 == listen(fd, 5))
				fprintf(stderr, "listen(): %s\n", strerror(errno));

			_callback.reset(new socket_callback_t(&rmate_connection_handler_t, fd));
		}

		~rmate_server_t ()
		{
			D(DBF_RMateServer, bug("port %ud, remote clients %s\n", _port, BSTR(_listen_for_remote_clients)););
		}

		uint16_t port () const                  { return _port; }
		bool listen_for_remote_clients () const { return _listen_for_remote_clients; }

	private:
		uint16_t _port;
		bool _listen_for_remote_clients;
		socket_callback_ptr _callback;
	};
}

void setup_rmate_server (bool enabled, uint16_t port, bool listenForRemoteClients)
{
	static mate_server_t mate_server;

	static std::shared_ptr<rmate_server_t> rmate_server;
	if(!enabled || !rmate_server || port != rmate_server->port() || listenForRemoteClients != rmate_server->listen_for_remote_clients())
	{
		rmate_server.reset();
		if(enabled)
			rmate_server.reset(new rmate_server_t(port, listenForRemoteClients));
	}
}

// ==================================

struct temp_file_t
{
	temp_file_t ()
	{
		path = path::temp("rmate_buffer");
		D(DBF_RMateServer, bug("create temp file: %s\n", path.c_str()););
	}

	operator char const* () const { return path.c_str(); }
	~temp_file_t ()               { unlink(path.c_str()); }

private:
	std::string path;
};

typedef std::shared_ptr<temp_file_t> temp_file_ptr;

struct record_t
{
	WATCH_LEAKS(record_t);

	record_t (std::string const& command) : command(command) { }
	~record_t ()                                             { }

	std::string command;
	std::map<std::string, std::string> arguments;
	temp_file_ptr file;

	void accept_data (char const* first, char const* last)
	{
		if(!file)
		{
			file.reset(new temp_file_t);
			arguments["data"] = std::string(*file);
		}

		if(FILE* fp = fopen(*file, "a"))
		{
			fwrite(first, 1, last - first, fp);
			fclose(fp);
		}
	}
};

namespace // wrap in anonymous namespace to avoid clashing with other callbacks named the same
{
	struct base_t : document::document_t::callback_t
	{
		WATCH_LEAKS(base_t);

		virtual void save_document (document::document_ptr document)  { }
		virtual void close_document (document::document_ptr document) { }

		void handle_document_event (document::document_ptr document, event_t event)
		{
			if(event == did_change_open_status && !document->is_open())
			{
				close_document(document);
				D(DBF_RMateServer, bug("%p\n", this););
				document->remove_callback(this);
				delete this;
			}
			else if(event == did_save)
			{
				save_document(document);
			}
		}
	};

	struct retain_temp_file_callback_t : base_t
	{
		retain_temp_file_callback_t (temp_file_ptr file) : file(file) { }
		temp_file_ptr file;
	};

	struct save_close_callback_t : base_t
	{
		WATCH_LEAKS(save_close_callback_t);

		save_close_callback_t (std::string const& path, socket_t const& socket, bool data_on_save, bool data_on_close, std::string const& token) : path(path), socket(socket), data_on_save(data_on_save), data_on_close(data_on_close), token(token)
		{
			D(DBF_RMateServer, bug("%p\n", this););
		}

		void save_document (document::document_ptr document)
		{
			D(DBF_RMateServer, bug("%s\n", document->path().c_str()););
			bool res = true;
			res = res && write(socket, "save\r\n", 6) == 6;
			res = res && write_token();
			if(data_on_save)
				res = res && write_data();
			res = res && write(socket, "\r\n", 2) == 2;
			if(!res)
				fprintf(stderr, "*** rmate: callback failed to save ‘%s’\n", document->display_name().c_str());
		}

		void close_document (document::document_ptr document)
		{
			D(DBF_RMateServer, bug("%s\n", document->path().c_str()););
			bool res = true;
			res = res && write(socket, "close\r\n", 7) == 7;
			res = res && write_token();
			if(data_on_close)
				res = res && write_data();
			res = res && write(socket, "\r\n", 2) == 2;
			if(!res)
				fprintf(stderr, "*** rmate: callback failed while closing ‘%s’\n", document->display_name().c_str());
		}

	private:
		bool write_token () const
		{
			if(token != NULL_STR)
			{
				std::string str = "token: " + token + "\r\n";
				return write(socket, str.data(), str.size()) == str.size();
			}
			return true;
		}

		bool write_data () const
		{
			bool res = false;
			if(FILE* fp = fopen(path.c_str(), "r"))
			{
				res = true;
				char buf[1024];
				while(size_t len = fread(buf, 1, sizeof(buf), fp))
				{
					std::string str = text::format("data: %zu\r\n", len);
					res = res && write(socket, str.data(), str.size()) == str.size();
					res = res && write(socket, buf, len) == len;
				}
				fclose(fp);
			}
			return res;
		}

		std::string path;
		socket_t socket;
		bool data_on_save;
		bool data_on_close;
		std::string token;
	};

	struct reactivate_callback_t : base_t
	{
		WATCH_LEAKS(reactivate_callback_t);

		struct helper_t { helper_t () : open_documents(0) { } size_t open_documents; WATCH_LEAKS(reactivate_callback_t); };
		typedef std::shared_ptr<helper_t> helper_ptr;

		reactivate_callback_t () : helper(helper_ptr(new helper_t))
		{
			D(DBF_RMateServer, bug("%p\n", this););
			GetFrontProcess(&psn);
		}

		void watch_document (document::document_ptr document)
		{
			++helper->open_documents;
			document->add_callback(new reactivate_callback_t(*this));
		}

		void close_document (document::document_ptr document)
		{
			D(DBF_RMateServer, bug("%zu → %zu\n", helper->open_documents, helper->open_documents - 1););
			if(--helper->open_documents == 0)
				SetFrontProcess(&psn);
		}

	private:
		helper_ptr helper;
		struct ProcessSerialNumber psn;
	};
}

// ==================
// = Handle Request =
// ==================

struct socket_observer_t
{
	WATCH_LEAKS(socket_observer_t);

	socket_observer_t () : state(command), bytesLeft(0) { }

	std::vector<record_t> records;
	enum { command, arguments, data, done } state;
	std::string line;
	ssize_t bytesLeft;

	bool operator() (socket_t const& socket)
	{
		char buf[1024];
		ssize_t len = read(socket, buf, sizeof(buf));
		D(DBF_RMateServer, bug("%p, %d — %zd bytes\n", this, (int)socket, len););
		if(len == 0)
			return false;

		if(len != -1)
		{
			receive_data(buf, len);
			parse();
			if(state == done)
			{
				D(DBF_RMateServer, bug("done\n"););
				if(records.empty() || records.begin()->command == "open") // we treat no command as ‘open’ to bring our application to front
					open_documents(socket);
				else
					fprintf(stderr, "*** error unsupported command: %s\n", records.begin()->command.c_str());
				return false;
			}
		}
		return true;
	}

	void receive_data (char const* buf, ssize_t len)
	{
		if(state == data)
		{
			ssize_t dataLen = std::min(len, bytesLeft);
			D(DBF_RMateServer, bug("Got data, %zd bytes\n", dataLen););
			records.back().accept_data(buf, buf + dataLen);
			bytesLeft -= dataLen;
			state = bytesLeft == 0 ? arguments : data;

			line.insert(line.end(), buf + dataLen, buf + len);
		}
		else
		{
			line.insert(line.end(), buf, buf + len);
		}
	}

	void parse ()
	{
		while(line.find('\n') != std::string::npos)
		{
			std::string::size_type eol = line.find('\n');
			std::string str = line.substr(0, eol);
			if(!str.empty() && str[str.size()-1] == '\r')
				str.resize(str.size()-1);
			line.erase(line.begin(), line.begin() + eol + 1);

			if(str.empty())
			{
				D(DBF_RMateServer, bug("Got ‘end of record’\n"););
				state = command;
			}
			else if(state == command)
			{
				if(str == ".")
				{
					state = done;
				}
				else
				{
					records.emplace_back(str);
					state = arguments;
				}
				D(DBF_RMateServer, bug("Got command ‘%s’\n", str.c_str()););
			}
			else if(state == arguments)
			{
				std::string::size_type n = str.find(':');
				if(n != std::string::npos)
				{
					std::string const key   = str.substr(0, n);
					std::string const value = str.substr(n+2);

					if(key == "data")
					{
						bytesLeft = strtol(value.c_str(), NULL, 10);
						size_t dataLen = std::min((ssize_t)line.size(), bytesLeft);
						D(DBF_RMateServer, bug("Got data of size %zd (%zu in this packet)\n", bytesLeft, dataLen););
						records.back().accept_data(line.data(), line.data() + dataLen);
						line.erase(line.begin(), line.begin() + dataLen);
						bytesLeft -= dataLen;

						state = bytesLeft == 0 ? arguments : data;
					}
					else
					{
						D(DBF_RMateServer, bug("Got argument: %s = %s\n", key.c_str(), value.c_str()););
						if(!value.empty())
							records.back().arguments.insert(std::make_pair(key, value));
					}
				}
			}
		}
	}

	void open_documents (socket_t const& socket)
	{
		reactivate_callback_t reactivate_callback;

		std::vector<document::document_ptr> documents;
		iterate(record, records)
		{
			std::map<std::string, std::string>& args = record->arguments;
			bool wait             = args["wait"] == "yes";
			bool writeBackOnSave  = args["data-on-save"] == "yes";
			bool writeBackOnClose = args["data-on-close"] == "yes";
			std::string token     = args.find("token") != args.end() ? args["token"] : NULL_STR;
			bool reActivate       = args["re-activate"] == "yes";
			std::string fileType  = args.find("file-type") == args.end() ? NULL_STR : args["file-type"];

			document::document_ptr doc;
			if(args.find("path") != args.end())
			{
				if(path::is_directory(args["path"]))
				{
					document::show_browser(args["path"]);
					continue;
				}
				doc = document::create(args["path"]);
			}
			else if(args.find("data") != args.end())
			{
				if(writeBackOnSave || writeBackOnClose)
				{
					doc = document::create(args["data"]);
					doc->set_recent_tracking(false);
				}
				else
				{
					doc = document::from_content(path::content(args["data"]), fileType);
				}
			}
			else
			{
				doc = document::create();
			}

			if(fileType != NULL_STR)
				doc->set_file_type(fileType);

			if(args.find("real-path") != args.end())
			{
				D(DBF_RMateServer, bug("set document’s virtual path: %s\n", args["real-path"].c_str()););
				doc->set_virtual_path(args["real-path"]);
			}

			if(!args["display-name"].empty())
				doc->set_custom_name(args["display-name"]);

			if(!args["selection"].empty())
				doc->set_selection(args["selection"]);

			if(args["add-to-recents"] != "yes")
				doc->set_recent_tracking(false);

			if(wait || writeBackOnSave || writeBackOnClose)
				doc->add_callback(new save_close_callback_t(doc->path(), socket, writeBackOnSave, writeBackOnClose, token));

			if(args.find("data") != args.end() && (writeBackOnSave || writeBackOnClose))
				doc->add_callback(new retain_temp_file_callback_t(record->file));

			if(reActivate)
				reactivate_callback.watch_document(doc);

			// std::string folder;         // when there is no path we still may provide a default folder
			// enum fallback_t { must_share_path, should_share_path, frontmost, create_new } project_fallback;
			// bool bring_to_front;

			if(args.find("authorization") != args.end())
				doc->set_authorization(args["authorization"]);

			if(oak::uuid_t::is_valid(args["project-uuid"]))
					document::show(doc, args["project-uuid"]);
			else	documents.push_back(doc);
		}

		if(documents.empty())
				SetFrontProcess(&(ProcessSerialNumber){ 0, kCurrentProcess });
		else	document::show(documents);
	}
};

static bool rmate_connection_handler_t (socket_t const& socket)
{
	socklen_t dummyLen = std::max(sizeof(sockaddr_un), sizeof(sockaddr_in));
	char dummy[dummyLen];
	int newFd = accept(socket, (sockaddr*)&dummy[0], &dummyLen);

	std::string welcome = "220 " + sys_info(KERN_HOSTNAME) + " RMATE TextMate (" + sys_info(KERN_OSTYPE) + " " + sys_info(KERN_OSRELEASE) + ")\n";
	ssize_t len = write(newFd, welcome.data(), welcome.size());
	if(len == -1)
		fprintf(stderr, "error writing: %s\n", strerror(errno));

	new socket_callback_t(socket_observer_t(), newFd);
	return true;
}
