#include "save.h"
#include "status.h"
#include "encoding.h"
#include "filter.h"
#include "path_info.h"
#include <authorization/server.h>
#include <authorization/constants.h>
#include <io/intermediate.h>
#include <io/path.h>
#include <text/trim.h>
#include <text/newlines.h>
#include <settings/settings.h>
#include <command/parser.h>
#include <oak/debug.h>

OAK_DEBUG_VAR(File_Save);

namespace
{
	struct file_context_t : file::save_context_t
	{
		file_context_t (file::save_callback_ptr callback, std::string const& path, osx::authorization_t authorization, io::bytes_ptr content, std::map<std::string, std::string> const& attributes, std::string const& fileType, encoding::type const& encoding, std::vector<oak::uuid_t> const& binaryImportFilters, std::vector<oak::uuid_t> const& textImportFilters) : _state(kStateIdle), _next_state(kStateStart), _make_writable(false), _saved(false), _callback(callback), _path(path), _authorization(authorization), _content(content), _attributes(attributes), _file_type(fileType), _path_attributes(NULL_STR), _encoding(encoding), _error(NULL_STR), _binary_import_filters(binaryImportFilters), _text_import_filters(textImportFilters) { }

		~file_context_t ()
		{
			D(DBF_File_Save, bug("\n"););
			if(_state != kStateDone)
			{
				ASSERT(!_saved);
				_callback->did_save(_path, _content, _encoding, _saved, _error, _filter);
			}
		}

		void set_path (std::string const& path)                   { _path = path;                     proceed(); }
		void set_make_writable (bool flag)                        { _make_writable = flag;            proceed(); }
		void set_create_parent (bool flag)                        { _create_parent = flag;            proceed(); }
		void set_authorization (osx::authorization_t auth)        { _authorization = auth;            proceed(); }
		void set_content (io::bytes_ptr content)                  { _content = content;               proceed(); }
		void set_charset (std::string const& charset)             { _encoding.set_charset(charset);   proceed(); }

		void filter_error (bundle_command_t const& command, int rc, std::string const& out, std::string const& err)
		{
			_error = text::trim(err + out).empty() ? text::format("Command returned status code %d.", rc) : err + out;
			_filter = command.uuid;
		}

		void proceed ()
		{
			D(DBF_File_Save, bug("state %d\n", _state););
			_state = _next_state;
			event_loop();
		}

	private:
		void event_loop ();

		enum state_t {
			kStateIdle,
			kStateStart,
			kStateSelectPath,
			kStateMakeWritable,
			kStateObtainAuthorization,
			kStateExecuteTextExportFilter,
			kStateConvertLineFeeds,
			kStateSelectEncoding,
			kStateEncodeContent,
			kStateExecuteBinaryExportFilter,
			kStateSaveContent,
			kStateNotifyCallback,
			kStateDone
		};

		state_t                              _state;
		state_t                              _next_state;
		bool                                 _make_writable;
		bool                                 _create_parent = false;
		bool                                 _saved;

		file::save_callback_ptr              _callback;

		std::string                          _path;
		osx::authorization_t                 _authorization;

		io::bytes_ptr                        _content;
		std::map<std::string, std::string>   _attributes;

		std::string                          _file_type;
		std::string                          _path_attributes;
		encoding::type                       _encoding;

		std::string                          _error;
		oak::uuid_t                          _filter; // this filter failed

		std::vector<oak::uuid_t>             _binary_import_filters;
		std::vector<oak::uuid_t>             _text_import_filters;

		std::vector<oak::uuid_t>             _binary_export_filters;
		std::vector<oak::uuid_t>             _text_export_filters;
	};

	typedef std::shared_ptr<file_context_t> file_context_ptr;
}

// ====================
// = Context Datatype =
// ====================

namespace
{
	static std::string write_to_path (std::string const& path, io::bytes_ptr const& bytes, std::map<std::string, std::string> const& attributes, osx::authorization_t authorization)
	{
		std::string error = NULL_STR;
		file_status_t status = file::status(path);
		if(status == kFileTestWritable || status == kFileTestNotWritableButOwner)
		{
			if(status == kFileTestNotWritableButOwner)
			{
				struct stat sbuf;
				if(stat(path.c_str(), &sbuf) == 0)
					chmod(path.c_str(), sbuf.st_mode | S_IWUSR);
			}

			path::intermediate_t dest(path);

			int fd = open(dest, O_CREAT|O_TRUNC|O_WRONLY|O_CLOEXEC, S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH);
			if(fd == -1)
				error = text::format("open(\"%s\"): %s", (char const*)dest, strerror(errno));
			else if(write(fd, bytes->get(), bytes->size()) != bytes->size())
			{
				close(fd);
				error = text::format("write(): %s", strerror(errno));
			}
			else if(close(fd) != 0)
				error = text::format("close(): %s", strerror(errno));
			else if(!dest.commit())
				error = text::format("Atomic save: %s", strerror(errno));
			else if(!path::set_attributes(path, attributes))
				error = text::format("Setting extended attributes: %s", strerror(errno));
		}
		else if(status == kFileTestWritableByRoot || status == kFileTestNotWritable)
		{
			if(connection_t conn = connect_to_auth_server(authorization))
			{
				conn << "write" << path << std::string(bytes->begin(), bytes->end()) << attributes;
				conn >> error;
			}
			else
			{
				error = "Could not connect to privileged helper demon.";
			}
		}
		return error;
	}

	void file_context_t::event_loop ()
	{
		_next_state = kStateIdle;
		while(_state != kStateIdle && _state != kStateDone)
		{
			switch(_state)
			{
				case kStateStart:
				{
					_state      = kStateIdle;
					_next_state = kStateSelectPath;

					if(_path == NULL_STR)
							_callback->select_path(_path, _content, shared_from_this());
					else	proceed();
				}
				break;

				case kStateSelectPath:
				{
					_state      = kStateIdle;
					_next_state = kStateMakeWritable;

					if(_path != NULL_STR)
					{
						_path_attributes = file::path_attributes(_path);
						proceed();
					}
				}
				break;

				case kStateMakeWritable:
				{
					_state      = kStateIdle;
					_next_state = kStateObtainAuthorization;

					switch(file::status(_path))
					{
						case kFileTestNotWritable:
						case kFileTestNotWritableButOwner:
							_callback->select_make_writable(_path, _content, shared_from_this());
						break;

						case kFileTestWritable:
						case kFileTestWritableByRoot:
							proceed();
						break;

						case kFileTestNoParent:
						{
							if(_create_parent)
							{
								if(path::make_dir(path::parent(_path)))
									proceed();
							}
							else
							{
								_next_state = kStateMakeWritable;
								_callback->select_create_parent(_path, _content, shared_from_this());
							}
						}
						break;

						case kFileTestReadOnly:
							// TODO show error
						break;
					}
				}
				break;

				case kStateObtainAuthorization:
				{
					_state      = kStateIdle;
					_next_state = kStateExecuteTextExportFilter;

					file_status_t status = file::status(_path);
					if(status == kFileTestWritable || status == kFileTestNotWritableButOwner && _make_writable)
						proceed();
					else if(status == kFileTestWritableByRoot || status == kFileTestNotWritable && _make_writable)
						_callback->obtain_authorization(_path, _content, _authorization, shared_from_this());
				}
				break;

				case kStateExecuteTextExportFilter:
				{
					_state      = kStateIdle;
					_next_state = kStateConvertLineFeeds;

					std::vector<bundles::item_ptr> filters;
					for(auto const& item : filter::find(_path, _content, _path_attributes, filter::kBundleEventTextExport))
					{
						if(!oak::contains(_text_export_filters.begin(), _text_export_filters.end(), item->uuid()))
						{
							filters.push_back(item);
							_text_export_filters.push_back(item->uuid());
							break; // FIXME see next FIXME
						}
					}

					if(filters.empty())
					{
						proceed();
					}
					else // FIXME we need to show dialog incase of multiple import hooks
					{
						_next_state = kStateExecuteTextExportFilter;
						filter::run(filters.back(), _path, _content, std::static_pointer_cast<file_context_t>(shared_from_this()));
					}
				}
				break;

				case kStateConvertLineFeeds:
				{
					_state      = kStateIdle;
					_next_state = kStateEncodeContent;

					if(_encoding.newlines() != kLF)
					{
						std::string tmp;
						oak::replace_copy(_content->begin(), _content->end(), kLF.begin(), kLF.end(), _encoding.newlines().begin(), _encoding.newlines().end(), back_inserter(tmp));
						_content->set_string(tmp);
					}

					proceed();
				}
				break;

				case kStateSelectEncoding:
				{
					_state      = kStateIdle;
					_next_state = kStateEncodeContent;

					_callback->select_charset(_path, _content, _encoding.charset(), shared_from_this());
				}
				break;

				case kStateEncodeContent:
				{
					_state      = kStateIdle;
					_next_state = kStateExecuteBinaryExportFilter;

					if(_encoding.charset() == kCharsetNoEncoding)
					{
						_next_state = kStateSelectEncoding;
					}
					else
					{
						io::bytes_ptr encodedContent = _content;
						if(encodedContent = encoding::convert(_content, kCharsetUTF8, _encoding.charset()))
								_content = encodedContent;
						else	_next_state = kStateSelectEncoding;
					}

					proceed();
				}
				break;

				case kStateExecuteBinaryExportFilter:
				{
					_state      = kStateIdle;
					_next_state = kStateSaveContent;

					std::vector<bundles::item_ptr> filters;
					for(auto const& item : filter::find(_path, _content, _path_attributes, filter::kBundleEventBinaryExport))
					{
						if(!oak::contains(_binary_export_filters.begin(), _binary_export_filters.end(), item->uuid()))
						{
							filters.push_back(item);
							_binary_export_filters.push_back(item->uuid());
							break; // FIXME see next FIXME
						}
					}

					if(filters.empty())
					{
						proceed();
					}
					else // FIXME we need to show dialog incase of multiple import hooks
					{
						_next_state = kStateExecuteBinaryExportFilter;
						filter::run(filters.back(), _path, _content, std::static_pointer_cast<file_context_t>(shared_from_this()));
					}
				}
				break;

				case kStateSaveContent:
				{
					_state      = kStateIdle;
					_next_state = kStateNotifyCallback;

					auto retainedSelf = std::static_pointer_cast<file_context_t>(shared_from_this());
					dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
						std::string error = write_to_path(_path, _content, _attributes, _authorization);
						dispatch_async(dispatch_get_main_queue(), ^{
							_saved = error == NULL_STR;
							_error = error;
							retainedSelf->proceed();
						});
					});
				}
				break;

				case kStateNotifyCallback:
				{
					_state      = kStateIdle;
					_next_state = kStateDone;

					_callback->did_save(_path, _content, _encoding, _saved, _error, _filter);
					proceed();
				}
				break;
			}
		}
	}
}

namespace file
{
	// =================
	// = Save Callback =
	// =================

	void save_callback_t::select_path (std::string const& path, io::bytes_ptr content, save_context_ptr context)
	{
		context->set_path(NULL_STR);
	}

	void save_callback_t::select_make_writable (std::string const& path, io::bytes_ptr content, save_context_ptr context)
	{
		context->set_make_writable(false);
	}

	void save_callback_t::select_create_parent (std::string const& path, io::bytes_ptr content, save_context_ptr context)
	{
	}

	void save_callback_t::obtain_authorization (std::string const& path, io::bytes_ptr content, osx::authorization_t auth, save_context_ptr context)
	{
		if(auth.obtain_right(kAuthRightName))
			context->set_authorization(auth);
	}

	void save_callback_t::select_charset (std::string const& path, io::bytes_ptr content, std::string const& charset, save_context_ptr context)
	{
		context->set_charset(kCharsetUTF8);
	}

	// ==============
	// = Public API =
	// ==============

	// bool hasEncoding   = path::get_attr(path, "com.apple.TextEncoding") != NULL_STR;
	// bool storeEncoding = dstSettings.get(kSettingsStoreEncodingPerFileKey, hasEncoding);
	// if(storeEncoding || hasEncoding)
	// 	path::set_attr(path, "com.apple.TextEncoding", storeEncoding ? encoding : NULL_STR);

	void save (std::string const& path, save_callback_ptr cb, osx::authorization_t auth, io::bytes_ptr content, std::map<std::string, std::string> const& attributes, std::string const& fileType, encoding::type const& encoding, std::vector<oak::uuid_t> const& binaryImportFilters, std::vector<oak::uuid_t> const& textImportFilters)
	{
		auto context = std::make_shared<file_context_t>(cb, path, auth, content, attributes, fileType, encoding, binaryImportFilters, textImportFilters);
		context->proceed();
	}

} /* file */
