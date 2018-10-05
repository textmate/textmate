#include <oak/oak.h>
#include <text/parse.h>
#include <text/hexdump.h>
#include <document/OakDocument.h>
#include <document/OakDocumentController.h>
#include <oak/debug.h>
#include <authorization/authorization.h>
#include <io/io.h>
#include <ns/ns.h>
#include <OakAppKit/IOAlertPanel.h>

OAK_DEBUG_VAR(RMateServer);

/*
	open
	path: [«path»|-]
	uuid: «uuid»
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

	socket_callback_t (std::function<bool(socket_t const&)> const& f, socket_t const& fd)
	{
		D(DBF_RMateServer, bug("%p, %d\n", this, (int)fd););

		helper = std::make_shared<helper_t>(f, fd, this);

		CFSocketContext const context = { 0, helper.get(), NULL, NULL, NULL };
		if(socket = CFSocketCreateWithNative(kCFAllocatorDefault, fd, kCFSocketReadCallBack, callback, &context))
		{
			CFSocketSetSocketFlags(socket, CFSocketGetSocketFlags(socket) & ~kCFSocketCloseOnInvalidate);
			if(run_loop_source = CFSocketCreateRunLoopSource(kCFAllocatorDefault, socket, 0))
					CFRunLoopAddSource(CFRunLoopGetCurrent(), run_loop_source, kCFRunLoopDefaultMode);
			else	fprintf(stderr, "*** CFSocketCreateRunLoopSource() failed\n");
		}
		else
		{
			fprintf(stderr, "*** CFSocketCreateWithNative() failed: fd = %d\n", (int)fd);
		}
	}

	~socket_callback_t ()
	{
		D(DBF_RMateServer, bug("%p\n", this););
		ASSERT(CFRunLoopContainsSource(CFRunLoopGetCurrent(), run_loop_source, kCFRunLoopDefaultMode));

		if(socket)
		{
			CFSocketInvalidate(socket);
			if(run_loop_source)
			{
				CFRunLoopRemoveSource(CFRunLoopGetCurrent(), run_loop_source, kCFRunLoopDefaultMode);
				CFRelease(run_loop_source);
			}
			CFRelease(socket);
		}
	}

	static void callback (CFSocketRef s, CFSocketCallBackType callbackType, CFDataRef address, void const* data, void* info)
	{
		(*(helper_t*)info)();
	}

private:
	struct helper_t
	{
		helper_t (std::function<bool(socket_t const&)> const& f, socket_t const& socket, socket_callback_t* parent) : f(f), socket(socket), parent(parent) { }
		void operator() () { if(!f(socket)) delete parent; }

	private:
		std::function<bool(socket_t const&)> f;
		socket_t socket;
		socket_callback_t* parent;
	};

	std::shared_ptr<helper_t> helper;
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
			else if(listen(fd, SOMAXCONN) == -1)
				OakRunIOAlertPanel("Could not listen to socket");

			_callback = std::make_shared<socket_callback_t>(&rmate_connection_handler_t, fd);
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
			socket_t fd(socket(PF_INET6, SOCK_STREAM, 0));
			setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &on, sizeof(on));

			fcntl(fd, F_SETFD, FD_CLOEXEC);
			struct sockaddr_in6 iaddr = { sizeof(sockaddr_in6), AF_INET6, htons(_port) };
			iaddr.sin6_addr = listenForRemoteClients ? in6addr_any : in6addr_loopback;
			if(-1 == bind(fd, (sockaddr*)&iaddr, sizeof(iaddr)))
				perror("rmate: bind");
			if(-1 == listen(fd, SOMAXCONN))
				perror("rmate: listen");

			_callback = std::make_shared<socket_callback_t>(&rmate_connection_handler_t, fd);
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
			rmate_server = std::make_shared<rmate_server_t>(port, listenForRemoteClients);
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
			file = std::make_shared<temp_file_t>();
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
	struct base_t
	{
		WATCH_LEAKS(base_t);

		base_t (OakDocument* document)
		{
			_close_observer = [[NSNotificationCenter defaultCenter] addObserverForName:OakDocumentWillCloseNotification object:document queue:nil usingBlock:^(NSNotification*){
				this->close_document(document);
				delete this;
			}];
			_save_observer = [[NSNotificationCenter defaultCenter] addObserverForName:OakDocumentDidSaveNotification object:document queue:nil usingBlock:^(NSNotification*){
				this->save_document(document);
			}];
		}

		virtual ~base_t ()
		{
			[[NSNotificationCenter defaultCenter] removeObserver:_save_observer];
			[[NSNotificationCenter defaultCenter] removeObserver:_close_observer];
		}

		virtual void save_document (OakDocument* document)  { }
		virtual void close_document (OakDocument* document) { }

	private:
		id _save_observer;
		id _close_observer;
	};

	struct retain_temp_file_callback_t : base_t
	{
		retain_temp_file_callback_t (OakDocument* document, temp_file_ptr file) : base_t(document), file(file) { }
		temp_file_ptr file;
	};

	struct save_close_callback_t : base_t
	{
		WATCH_LEAKS(save_close_callback_t);

		save_close_callback_t (OakDocument* document, std::string const& path, socket_t const& socket, bool data_on_save, bool data_on_close, std::string const& token) : base_t(document), path(path), socket(socket), data_on_save(data_on_save), data_on_close(data_on_close), token(token)
		{
			D(DBF_RMateServer, bug("%p\n", this););
		}

		void save_document (OakDocument* document)
		{
			D(DBF_RMateServer, bug("%s\n", document.path.UTF8String););
			bool res = true;
			res = res && write(socket, "save\r\n", 6) == 6;
			res = res && write_token();
			if(data_on_save)
				res = res && write_data();
			res = res && write(socket, "\r\n", 2) == 2;
			if(!res)
				fprintf(stderr, "*** rmate: callback failed to save ‘%s’\n", document.displayName.UTF8String);
		}

		void close_document (OakDocument* document)
		{
			D(DBF_RMateServer, bug("%s\n", document.path.UTF8String););
			bool res = true;
			res = res && write(socket, "close\r\n", 7) == 7;
			res = res && write_token();
			if(data_on_close)
				res = res && write_data();
			res = res && write(socket, "\r\n", 2) == 2;
			if(!res)
				fprintf(stderr, "*** rmate: callback failed while closing ‘%s’\n", document.displayName.UTF8String);
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

	struct reactivate_callback_t
	{
		WATCH_LEAKS(reactivate_callback_t);

		reactivate_callback_t () : _shared_count(std::make_shared<size_t>(0))
		{
			D(DBF_RMateServer, bug("%p\n", this););
			_terminal = std::make_shared<NSRunningApplication*>([[NSWorkspace sharedWorkspace] frontmostApplication]);

			auto terminal = _terminal;
			if([*terminal isEqual:NSRunningApplication.currentApplication])
			{
				// If we call ‘mate -w’ in quick succession there is a chance that we have not yet re-activated the terminal app when we are asked to open a new document. For this reason, we monitor the NSApplicationDidResignActiveNotification for 200 ms to see if the “real” frontmost application becomes active.

				__weak __block id observerId = [[NSNotificationCenter defaultCenter] addObserverForName:NSApplicationDidResignActiveNotification object:NSApp queue:nil usingBlock:^(NSNotification*){
					[[NSNotificationCenter defaultCenter] removeObserver:observerId];
					*terminal = [[NSWorkspace sharedWorkspace] frontmostApplication];
				}];

				dispatch_after(dispatch_time(DISPATCH_TIME_NOW, NSEC_PER_SEC / 5), dispatch_get_main_queue(), ^{
					[[NSNotificationCenter defaultCenter] removeObserver:observerId];
				});
			}
		}

		void watch_document (OakDocument* document)
		{
			auto counter  = _shared_count;
			auto terminal = _terminal;

			++*counter;
			__weak __block id observerId = [[NSNotificationCenter defaultCenter] addObserverForName:OakDocumentWillCloseNotification object:document queue:nil usingBlock:^(NSNotification*){
				D(DBF_RMateServer, bug("%zu → %zu\n", *counter, *counter - 1););
				if(--*counter == 0)
					[*terminal activateWithOptions:NSApplicationActivateIgnoringOtherApps];
				[[NSNotificationCenter defaultCenter] removeObserver:observerId];
			}];
		}

	private:
		std::shared_ptr<size_t> _shared_count;
		std::shared_ptr<NSRunningApplication*> _terminal;
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
					handle_marks(socket);
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
			if(!str.empty() && str.back() == '\r')
				str.resize(str.size()-1);
			line.erase(line.begin(), line.begin() + eol + 1);

			if(str.empty())
			{
				D(DBF_RMateServer, bug("Got ‘end of record’\n"););
				state = command;
			}
			else if(state == command)
			{
				if(str == "." || strcasecmp(str.c_str(), "quit") == 0)
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
					std::string const value = str.substr(std::min(n+2, str.size()));

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
						{
							std::string unescaped;
							bool unescape_next = false;
							for(auto const& ch : value)
							{
								if(unescape_next)
								{
									unescaped += ch == 'n' ? '\n' : ch;
									unescape_next = false;
								}
								else if(ch == '\\')
								{
									unescape_next = true;
								}
								else
								{
									unescaped += ch;
								}
							}
							records.back().arguments.emplace(key, unescaped);
						}
					}
				}
			}
		}
	}

	void open_documents (socket_t const& socket)
	{
		reactivate_callback_t reactivate_callback;

		NSMutableArray<OakDocument*>* documents = [NSMutableArray array];
		for(auto& record : records)
		{
			std::map<std::string, std::string>& args = record.arguments;
			bool wait             = args["wait"] == "yes";
			bool writeBackOnSave  = args["data-on-save"] == "yes";
			bool writeBackOnClose = args["data-on-close"] == "yes";
			std::string token     = args.find("token") != args.end() ? args["token"] : NULL_STR;
			bool reActivate       = args["re-activate"] == "yes";
			std::string fileType  = args.find("file-type") == args.end() ? NULL_STR : args["file-type"];

			OakDocument* doc;
			if(args.find("path") != args.end())
			{
				if(path::is_directory(args["path"]))
				{
					[OakDocumentController.sharedInstance showFileBrowserAtPath:to_ns(args["path"])];
					continue;
				}
				doc = [OakDocumentController.sharedInstance documentWithPath:to_ns(args["path"])];
			}
			else if(args.find("uuid") != args.end())
			{
				if(!(doc = [OakDocumentController.sharedInstance findDocumentWithIdentifier:[[NSUUID alloc] initWithUUIDString:to_ns(args["uuid"])]]))
					continue;
			}
			else if(args.find("data") != args.end())
			{
				if(writeBackOnSave || writeBackOnClose)
				{
					doc = [OakDocumentController.sharedInstance documentWithPath:to_ns(args["data"])];
					doc.recentTrackingDisabled = YES;
				}
				else
				{
					doc = [OakDocument documentWithData:[NSData dataWithContentsOfFile:to_ns(args["data"])] fileType:to_ns(fileType) customName:nil];
				}
			}
			else
			{
				doc = [OakDocumentController.sharedInstance untitledDocument];
			}

			if(fileType != NULL_STR)
				doc.fileType = to_ns(fileType);

			if(args.find("real-path") != args.end())
			{
				D(DBF_RMateServer, bug("set document’s virtual path: %s\n", args["real-path"].c_str()););
				doc.virtualPath = to_ns(args["real-path"]);
			}

			if(!args["display-name"].empty())
				doc.customName = to_ns(args["display-name"]);

			if(!args["selection"].empty())
				doc.selection = to_ns(args["selection"]);

			if(args["add-to-recents"] != "yes")
				doc.recentTrackingDisabled = YES;

			if(wait || writeBackOnSave || writeBackOnClose)
				new save_close_callback_t(doc, to_s(doc.path), socket, writeBackOnSave, writeBackOnClose, token);

			if(args.find("data") != args.end() && (writeBackOnSave || writeBackOnClose))
				new retain_temp_file_callback_t(doc, record.file);

			if(reActivate)
				reactivate_callback.watch_document(doc);

			// std::string folder;         // when there is no path we still may provide a default folder
			// enum fallback_t { must_share_path, should_share_path, frontmost, create_new } project_fallback;
			// bool bring_to_front;

			if(args.find("authorization") != args.end())
				doc.authorization = args["authorization"];

			if(oak::uuid_t::is_valid(args["project-uuid"]))
					[OakDocumentController.sharedInstance showDocument:doc inProject:[[NSUUID alloc] initWithUUIDString:to_ns(args["project-uuid"])] bringToFront:YES];
			else	[documents addObject:doc];
		}

		if(documents.count)
				[OakDocumentController.sharedInstance showDocuments:documents];
		else	[NSApp activateIgnoringOtherApps:YES];
	}

	void handle_marks (socket_t const& socket)
	{
		for(auto& record : records)
		{
			if(record.command != "clear-mark" && record.command != "set-mark")
				continue;

			auto& args = record.arguments;

			OakDocument* doc;
			if(args.find("uuid") != args.end())
				doc = [OakDocumentController.sharedInstance findDocumentWithIdentifier:[[NSUUID alloc] initWithUUIDString:to_ns(args["uuid"])]];
			else if(args.find("path") != args.end())
				doc = [OakDocumentController.sharedInstance documentWithPath:to_ns(args["path"])];

			text::pos_t line = args.find("line") != args.end() ? text::pos_t(args["line"]) : text::pos_t::undefined;
			std::string mark = args.find("mark") != args.end() ? args["mark"] : NULL_STR;

			if(record.command == "clear-mark")
			{
				if(doc)
						[doc removeMarkOfType:to_ns(mark) atPosition:line];
				else	[OakDocument removeAllMarksOfType:to_ns(mark)];
			}
			else if(record.command == "set-mark")
			{
				std::string::size_type n = mark.find(':');
				if(doc)
						[doc setMarkOfType:to_ns(n == std::string::npos ? mark : mark.substr(0, n)) atPosition:line content:n == std::string::npos ? nil : to_ns(mark.substr(n+1))];
				else	fprintf(stderr, "set-mark: no document\n");
			}
		}
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
		perror("rmate: write");

	new socket_callback_t(socket_observer_t(), newFd);
	return true;
}
