#ifndef FILE_OPEN_H_13F9GMJX
#define FILE_OPEN_H_13F9GMJX

#include "bytes.h"
#include <authorization/authorization.h>
#include <oak/misc.h>
#include <plist/uuid.h>

struct bundle_command_t;

namespace file
{
	struct PUBLIC open_context_t : std::tr1::enable_shared_from_this<open_context_t>
	{
		virtual ~open_context_t () { }
		virtual void set_authorization (osx::authorization_t auth) = 0;
		virtual void set_encoding (std::string const& encoding) = 0;
		virtual void set_line_feeds (std::string const& lineFeeds) = 0;
		virtual void set_file_type (std::string const& fileType) = 0;
		virtual void filter_error (bundle_command_t const& command, int rc, std::string const& out, std::string const& err) = 0;
	};

	typedef std::tr1::shared_ptr<open_context_t> open_context_ptr;

	struct PUBLIC open_callback_t
	{
		virtual ~open_callback_t () { }
		virtual void obtain_authorization (std::string const& path, osx::authorization_t auth, open_context_ptr context);
		virtual void select_encoding (std::string const& path, io::bytes_ptr content, open_context_ptr context);
		virtual void select_line_feeds (std::string const& path, io::bytes_ptr content, open_context_ptr context);
		virtual void select_file_type (std::string const& path, io::bytes_ptr content, open_context_ptr context);
		virtual void show_error (std::string const& path, std::string const& message, oak::uuid_t const& filter) = 0;
		virtual void show_content (std::string const& path, io::bytes_ptr content, std::map<std::string, std::string> const& attributes, std::string const& fileType, std::string const& pathAttributes, std::string const& encoding, bool bom, std::string const& lineFeeds, std::vector<oak::uuid_t> const& binaryImportFilters, std::vector<oak::uuid_t> const& textImportFilters) = 0;
	};

	typedef std::tr1::shared_ptr<open_callback_t> open_callback_ptr;

	PUBLIC void open (std::string const& path, osx::authorization_t auth, open_callback_ptr cb, io::bytes_ptr existingContent = io::bytes_ptr(), std::string const& virtualPath = NULL_STR);

} /* file */

#endif /* end of include guard: FILE_OPEN_H_13F9GMJX */
