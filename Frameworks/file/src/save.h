#ifndef FILE_SAVE_H_8GPV8Y8R
#define FILE_SAVE_H_8GPV8Y8R

#include "bytes.h"
#include <authorization/authorization.h>
#include <oak/misc.h>
#include <plist/uuid.h>

struct bundle_command_t;

namespace file
{
	struct PUBLIC save_context_t : std::tr1::enable_shared_from_this<save_context_t>
	{
		virtual ~save_context_t () { }
		virtual void set_path (std::string const& path) = 0;
		virtual void set_make_writable (bool flag) = 0;
		virtual void set_authorization (osx::authorization_t auth) = 0;
		virtual void set_encoding (std::string const& encoding, bool bom = false) = 0;
		virtual void filter_error (bundle_command_t const& command, int rc, std::string const& out, std::string const& err) = 0;
	};

	typedef std::tr1::shared_ptr<save_context_t> save_context_ptr;

	struct PUBLIC save_callback_t
	{
		virtual ~save_callback_t () { }
		virtual void select_path (std::string const& path, io::bytes_ptr content, save_context_ptr context);
		virtual void select_make_writable (std::string const& path, io::bytes_ptr content, save_context_ptr context);
		virtual void obtain_authorization (std::string const& path, io::bytes_ptr content, osx::authorization_t auth, save_context_ptr context);
		virtual void select_encoding (std::string const& path, io::bytes_ptr content, std::string const& encoding, save_context_ptr context);
		virtual void did_save (std::string const& path, io::bytes_ptr content, std::string const& pathAttributes, std::string const& encoding, bool bom, std::string const& lineFeeds, bool success, std::string const& message, oak::uuid_t const& filter) = 0;
	};

	typedef std::tr1::shared_ptr<save_callback_t> save_callback_ptr;

	PUBLIC void save (std::string const& path, save_callback_ptr cb, osx::authorization_t auth, io::bytes_ptr content, std::map<std::string, std::string> const& attributes, std::string const& fileType, std::string const& encoding, bool bom, std::string const& lineFeeds, std::vector<oak::uuid_t> const& binaryImportFilters, std::vector<oak::uuid_t> const& textImportFilters);

} /* file */

#endif /* end of include guard: FILE_SAVE_H_8GPV8Y8R */
