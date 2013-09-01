#include "open.h"
#include "constants.h"
#include "path_info.h"
#include "encoding.h"
#include "filter.h"
#include "type.h"
#include <authorization/constants.h>
#include <authorization/server.h>
#include <cf/cf.h>
#include <command/parser.h>
#include <command/runner.h>
#include <settings/settings.h>
#include <text/trim.h>
#include <text/utf8.h>
#include <oak/server.h>
#include <oak/debug.h>

OAK_DEBUG_VAR(File_Charset);

/*
	TODO Assign UUID to open request and keep with content
	TODO Harmonize line endings should do actual conversions (to make it reversable / not drop a single \r in a \n file)
	TODO Allow non-installed bundles to be used as import filters
*/

// ====================
// = Context Datatype =
// ====================

namespace
{
	struct open_file_context_t : file::open_context_t
	{
		open_file_context_t (std::string const& path, io::bytes_ptr existingContent, osx::authorization_t auth, file::open_callback_ptr callback, std::string const& virtualPath) : _state(kStateIdle), _next_state(kStateStart), _estimate_encoding_state(kEstimateEncodingStateBOM), _callback(callback), _path(path), _virtual_path(virtualPath), _authorization(auth), _content(existingContent), _file_type(kFileTypePlainText), _path_attributes(NULL_STR), _error(NULL_STR)
		{
		}

		~open_file_context_t ()
		{
			if(_state != kStateDone)
				_callback->show_error(_path, _error, _filter);
		}

		void set_authorization (osx::authorization_t auth)                                       { _authorization = auth;                  proceed(); }
		void set_content (io::bytes_ptr content)                                                 { _content = content;                     proceed(); }
		void set_content (io::bytes_ptr content, std::map<std::string, std::string> const& attr) { _attributes = attr; _content = content; proceed(); }
		void set_charset (std::string const& charset)                                            { _encoding.set_charset(charset);         proceed(); }
		void set_line_feeds (std::string const& newlines)                                        { _encoding.set_newlines(newlines);       proceed(); }
		void set_file_type (std::string const& fileType)                                         { _file_type  = fileType;                 proceed(); }

		void filter_error (bundle_command_t const& command, int rc, std::string const& out, std::string const& err)
		{
			_error = text::trim(err + out).empty() ? text::format("Command returned status code %d.", rc) : err + out;
			_filter = command.uuid;
		}

		void proceed ()
		{
			_state = _next_state;
			event_loop();
		}

	private:
		void event_loop ();

		enum state_t {
			kStateIdle,
			kStateStart,
			kStateObtainAuthorization,
			kStateLoadContent,
			kStateExecuteBinaryImportFilter,
			kStateEstimateEncoding,
			kStateSelectEncoding,
			kStateDecodeContent,
			kStateEstimateLineFeeds,
			kStateHarmonizeLineFeeds,
			kStateExecuteTextImportFilter,
			kStateEstimateFileType,
			kStateEstimateTabSettings,
			kStateShowContent,
			kStateDone
		};

		enum estimate_encoding_state_t {
			kEstimateEncodingStateBOM,
			kEstimateEncodingStateExtendedAttribute,
			kEstimateEncodingStateASCII,
			kEstimateEncodingStateUTF8,
			kEstimateEncodingStatePathSettings,
			kEstimateEncodingStateAskUser
		};

		state_t _state;
		state_t _next_state;
		int _estimate_encoding_state;

		file::open_callback_ptr _callback;

		std::string _path;
		std::string _virtual_path;
		osx::authorization_t _authorization;

		io::bytes_ptr _content;
		std::map<std::string, std::string> _attributes;
		encoding::type _encoding;
		std::string _file_type; // root grammar scope
		std::string _path_attributes;
		std::string _error;
		oak::uuid_t _filter; // this filter failed

		std::vector<oak::uuid_t> _binary_import_filters;
		std::vector<oak::uuid_t> _text_import_filters;
	};

	typedef std::shared_ptr<open_file_context_t> file_context_ptr;
}

// =================
// = Threaded Read =
// =================

namespace file
{
	struct read_t
	{
		struct request_t { std::string path; osx::authorization_t authorization; };
		struct result_t  { io::bytes_ptr bytes; std::map<std::string, std::string> attributes; int error_code; };

		WATCH_LEAKS(read_t);

		read_t (std::string const& path, osx::authorization_t auth, file_context_ptr context);
		virtual ~read_t ();

		static read_t::result_t handle_request (read_t::request_t const& request);
		void handle_reply (read_t::result_t const& result);

	private:
		size_t _client_key;
		file_context_ptr _context;
	};

	static oak::server_t<read_t>& read_server ()
	{
		static oak::server_t<read_t> server;
		return server;
	}

	read_t::read_t (std::string const& path, osx::authorization_t auth, file_context_ptr context) : _context(context)
	{
		_client_key = read_server().register_client(this);
		read_server().send_request(_client_key, (request_t){ path, auth });
	}

	read_t::~read_t ()
	{
		read_server().unregister_client(_client_key);
	}

	read_t::result_t read_t::handle_request (read_t::request_t const& request)
	{
		result_t result;
		result.error_code = 0;

		int fd = ::open(request.path.c_str(), O_RDONLY|O_CLOEXEC);
		if(fd != -1)
		{
			struct stat sbuf;
			if(fstat(fd, &sbuf) != -1)
			{
				fcntl(fd, F_NOCACHE, 1);
				result.bytes = std::make_shared<io::bytes_t>(sbuf.st_size);
				if(read(fd, result.bytes->get(), result.bytes->size()) != sbuf.st_size)
					result.bytes.reset();
			}
			else
			{
				result.error_code = errno;
			}
			close(fd);

			result.attributes = path::attributes(request.path);
		}
		else if(errno == EACCES)
		{
			if(connection_t conn = connect_to_auth_server(request.authorization))
			{
				conn << "read" << request.path;
				std::string content;
				conn >> content >> result.attributes;
				result.bytes = std::make_shared<io::bytes_t>(content);
			}
			else
			{
				result.error_code = EACCES;
			}
		}
		else
		{
			result.error_code = errno;
		}
		return result;
	}

	void read_t::handle_reply (read_t::result_t const& result)
	{
		if(!result.bytes && result.error_code == ENOENT)
				_context->set_content(std::make_shared<io::bytes_t>(""), result.attributes);
		else	_context->set_content(result.bytes, result.attributes);
		delete this;
	}

} /* file */

// ====================
// = Encoding Support =
// ====================

static bool not_ascii (char ch)
{
	return !(0x20 <= ch && ch < 0x80 || ch && strchr("\t\n\f\r\e", ch));
}

static io::bytes_ptr remove_bom (io::bytes_ptr content)
{
	if(content)
	{
		ASSERT_GE(content->size(), 3); ASSERT_EQ(std::string(content->get(), content->get() + 3), "\uFEFF");
		memmove(content->get(), content->get()+3, content->size()-3);
		content->resize(content->size()-3);
	}
	return content;
}

static io::bytes_ptr convert (io::bytes_ptr content, std::string const& from, std::string const& to, bool bom = false)
{
	if(from == kCharsetUnknown)
		return io::bytes_ptr();

	content = encoding::convert(content, from, to);
	return bom ? remove_bom(content) : content;
}

// =====================
// = Line Feed Support =
// =====================

template <typename _InputIter>
std::string find_line_endings (_InputIter const& first, _InputIter const& last)
{
	size_t cr_count = std::count(first, last, '\r');
	size_t lf_count = std::count(first, last, '\n');

	if(cr_count == 0)
		return kLF;
	else if(lf_count == 0)
		return kCR;
	else if(lf_count == cr_count)
		return kCRLF;
	else
		return kLF;
}

template <typename _InputIter>
_InputIter harmonize_line_endings (_InputIter first, _InputIter last, std::string const& lineFeeds)
{
	_InputIter out = first;
	while(first != last)
	{
		bool isCR = *first == '\r';
		if(out != first || isCR)
			*out = isCR ? '\n' : *first;
		if(++first != last && isCR && *first == '\n')
			++first;
		++out;
	}
	return out;
}

// ===================================
// = Default Callback Implementation =
// ===================================

namespace file
{
	void open_callback_t::obtain_authorization (std::string const& path, osx::authorization_t auth, open_context_ptr context)
	{
		if(auth.obtain_right(kAuthRightName))
			context->set_authorization(auth);
	}

	void open_callback_t::select_charset (std::string const& path, io::bytes_ptr content, open_context_ptr context)
	{
	}

	void open_callback_t::select_line_feeds (std::string const& path, io::bytes_ptr content, open_context_ptr context)
	{
		context->set_line_feeds(kLF);
	}

	void open_callback_t::select_file_type (std::string const& path, io::bytes_ptr content, open_context_ptr context)
	{
		context->set_file_type(kFileTypePlainText);
	}

} /* file */

// ====================
// = Context Datatype =
// ====================

namespace
{
	void open_file_context_t::event_loop ()
	{
		_next_state = kStateIdle;
		while(_state != kStateIdle && _state != kStateDone)
		{
			switch(_state)
			{
				case kStateStart:
				{
					_state      = kStateIdle;
					_next_state = kStateObtainAuthorization;

					_path_attributes = file::path_attributes(_path);
					if(_content)
						_next_state = kStateExecuteBinaryImportFilter;

					proceed();
				}
				break;

				case kStateObtainAuthorization:
				{
					_state      = kStateIdle;
					_next_state = kStateLoadContent;

					if(_path != NULL_STR && access(_path.c_str(), R_OK) == -1 && errno == EACCES)
							_callback->obtain_authorization(_path, _authorization, shared_from_this());
					else	proceed();
				}
				break;

				case kStateLoadContent:
				{
					_state      = kStateIdle;
					_next_state = kStateExecuteBinaryImportFilter;

					new file::read_t(_path, _authorization, std::static_pointer_cast<open_file_context_t>(shared_from_this()));
				}
				break;

				case kStateExecuteBinaryImportFilter:
				{
					_state      = kStateIdle;
					_next_state = kStateEstimateEncoding;

					if(!_content)
						break;

					std::vector<bundles::item_ptr> filters;
					citerate(item, filter::find(_path, _content, _path_attributes, filter::kBundleEventBinaryImport))
					{
						if(!oak::contains(_binary_import_filters.begin(), _binary_import_filters.end(), (*item)->uuid()))
						{
							filters.push_back(*item);
							_binary_import_filters.push_back((*item)->uuid());
							break; // FIXME see next FIXME
						}
					}

					if(filters.empty())
					{
						proceed();
					}
					else // FIXME we need to show dialog incase of multiple import hooks
					{
						_next_state = kStateExecuteBinaryImportFilter;
						filter::run(filters.back(), _path, _content, std::static_pointer_cast<open_file_context_t>(shared_from_this()));
					}
				}
				break;

				case kStateEstimateEncoding:
				{
					_state      = kStateIdle;
					_next_state = kStateDecodeContent;

					char const* first = _content->begin();
					char const* last  = _content->end();
					switch(_estimate_encoding_state++)
					{
						case kEstimateEncodingStateBOM:
						{
							std::string const charset = encoding::charset_from_bom(first, last);
							if(charset != kCharsetNoEncoding)
							{
								_encoding.set_charset(charset);
								_encoding.set_byte_order_mark(true);
							}
						}
						break;

						case kEstimateEncodingStateExtendedAttribute:
						{
							std::string const charset = path::get_attr(_path, "com.apple.TextEncoding");
							_encoding.set_charset(charset != NULL_STR ? charset.substr(0, charset.find(';')) : kCharsetUnknown);
						}
						break;

						case kEstimateEncodingStateASCII:
							_encoding.set_charset(std::find_if(first, last, &not_ascii) == last ? kCharsetNoEncoding : kCharsetUnknown);
						break;

						case kEstimateEncodingStateUTF8:
							_encoding.set_charset(utf8::is_valid(first, last) ? kCharsetUTF8 : kCharsetUnknown);
						break;

						case kEstimateEncodingStatePathSettings:
							_encoding.set_charset(settings_for_path(_path, "attr.file.unknown-encoding " + _path_attributes).get(kSettingsEncodingKey, kCharsetUnknown));
						break;

						case kEstimateEncodingStateAskUser:
							_next_state = kStateSelectEncoding;
							_estimate_encoding_state = kEstimateEncodingStateAskUser;
						break;
					}
					proceed();
				}
				break;

				case kStateSelectEncoding:
				{
					_state      = kStateIdle;
					_next_state = kStateDecodeContent;

					_callback->select_charset(_path, _content, shared_from_this());
				}
				break;

				case kStateDecodeContent:
				{
					_state      = kStateIdle;
					_next_state = kStateEstimateLineFeeds;

					if(io::bytes_ptr decodedContent = convert(_content, _encoding.charset() == kCharsetNoEncoding ? kCharsetASCII : _encoding.charset(), kCharsetUTF8, _encoding.byte_order_mark()))
							_content = decodedContent;
					else	_next_state = kStateEstimateEncoding;

					proceed();
				}
				break;

				case kStateEstimateLineFeeds:
				{
					_state      = kStateIdle;
					_next_state = kStateHarmonizeLineFeeds;

					_encoding.set_newlines(find_line_endings(_content->begin(), _content->end()));
					if(_encoding.newlines() != kMIX)
							proceed();
					else	_callback->select_line_feeds(_path, _content, shared_from_this());
				}
				break;

				case kStateHarmonizeLineFeeds:
				{
					if(_encoding.newlines() != kLF)
					{
						char* newEnd = harmonize_line_endings(_content->begin(), _content->end(), _encoding.newlines());
						_content->resize(newEnd - _content->begin());
					}
					_state = kStateExecuteTextImportFilter;
				}
				break;

				case kStateExecuteTextImportFilter:
				{
					_state      = kStateIdle;
					_next_state = kStateEstimateFileType;

					std::vector<bundles::item_ptr> filters;
					citerate(item, filter::find(_path, _content, _path_attributes, filter::kBundleEventTextImport))
					{
						if(!oak::contains(_text_import_filters.begin(), _text_import_filters.end(), (*item)->uuid()))
						{
							filters.push_back(*item);
							_text_import_filters.push_back((*item)->uuid());
							break; // FIXME see next FIXME
						}
					}

					if(filters.empty())
					{
						proceed();
					}
					else // FIXME we need to show dialog incase of multiple import hooks
					{
						_next_state = kStateExecuteTextImportFilter;
						filter::run(filters.back(), _path, _content, std::static_pointer_cast<open_file_context_t>(shared_from_this()));
					}
				}
				break;

				case kStateEstimateFileType:
				{
					_state      = kStateIdle;
					_next_state = kStateEstimateTabSettings;

					_file_type = file::type(_path, _content, _virtual_path);
					if(_file_type != NULL_STR)
							proceed();
					else	_callback->select_file_type(_virtual_path != NULL_STR ? _virtual_path : _path, _content, shared_from_this());
				}
				break;

				case kStateEstimateTabSettings:
				{
					_state = kStateShowContent;
					// TODO run some heuristic
				}
				break;

				case kStateShowContent:
				{
					_state = kStateDone;
					_callback->show_content(_path, _content, _attributes, _file_type, _encoding, _binary_import_filters, _text_import_filters);
				}
				break;
			}
		}
	}
}

// ====================

namespace file
{
	void open (std::string const& path, osx::authorization_t auth, open_callback_ptr cb, io::bytes_ptr existingContent, std::string const& virtualPath)
	{
		auto context = std::make_shared<open_file_context_t>(path, existingContent, auth, cb, virtualPath);
		context->proceed();
	}

} /* file */
