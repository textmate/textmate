#ifndef FILE_OPEN_H_13F9GMJX
#define FILE_OPEN_H_13F9GMJX

#include "bytes.h"
#include "encoding.h"
#include <authorization/src/authorization.h>
#include <plist/src/uuid.h>

struct bundle_command_t;

namespace file
{
	struct open_context_t : std::enable_shared_from_this<open_context_t>
	{
		virtual ~open_context_t () { }
		virtual void set_authorization (osx::authorization_t auth) = 0;
		virtual void set_charset (std::string const& charset) = 0;
		virtual void set_line_feeds (std::string const& lineFeeds) = 0;
		virtual void set_file_type (std::string const& fileType) = 0;
		virtual void filter_error (bundle_command_t const& command, int rc, std::string const& out, std::string const& err) = 0;
	};

	typedef std::shared_ptr<open_context_t> open_context_ptr;

	struct open_callback_t
	{
		virtual ~open_callback_t () { }
		virtual void obtain_authorization (std::string const& path, osx::authorization_t auth, open_context_ptr context);
		virtual void select_charset (std::string const& path, io::bytes_ptr content, open_context_ptr context);
		virtual void show_error (std::string const& path, std::string const& message, oak::uuid_t const& filter) = 0;
		virtual void show_content (std::string const& path, io::bytes_ptr content, std::map<std::string, std::string> const& attributes, encoding::type const& encoding, std::vector<oak::uuid_t> const& binaryImportFilters, std::vector<oak::uuid_t> const& textImportFilters) = 0;
	};

	typedef std::shared_ptr<open_callback_t> open_callback_ptr;

	void open (std::string const& path, osx::authorization_t auth, open_callback_ptr cb, io::bytes_ptr existingContent = io::bytes_ptr());

} /* file */

#endif /* end of include guard: FILE_OPEN_H_13F9GMJX */
