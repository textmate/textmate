#ifndef FILE_FILTER_H_7W5OVO3A
#define FILE_FILTER_H_7W5OVO3A

#include "bytes.h"
#include <plist/src/uuid.h>
#include <bundles/src/bundles.h>

struct bundle_command_t;

namespace filter
{
	extern std::string const kBundleEventBinaryImport;
	extern std::string const kBundleEventBinaryExport;
	extern std::string const kBundleEventTextImport;
	extern std::string const kBundleEventTextExport;

	struct callback_t
	{
		virtual ~callback_t () { }
		virtual void set_content (io::bytes_ptr bytes) = 0;
		virtual void filter_error (bundle_command_t const& command, int rc, std::string const& out, std::string const& err) = 0;
	};

	typedef std::shared_ptr<callback_t> callback_ptr;

	std::vector<bundles::item_ptr> find (std::string const& path, io::bytes_ptr content, std::string const& pathAttributes, std::string const& event);
	void run (bundles::item_ptr filter, std::string const& path, io::bytes_ptr content, callback_ptr callback);

	template <typename T>
	struct callback_wrapper_t : callback_t
	{
		callback_wrapper_t (T wrapped) : _wrapped(wrapped) { }
		void set_content (io::bytes_ptr bytes) { _wrapped->set_content(bytes); }
		void filter_error (bundle_command_t const& command, int rc, std::string const& out, std::string const& err)  { _wrapped->filter_error(command, rc, out, err); }
	private:
		T _wrapped;
	};

	template <typename T>
	void run (bundles::item_ptr filter, std::string const& path, io::bytes_ptr content, T callback)
	{
		run(filter, path, content, callback_ptr(new callback_wrapper_t<T>(callback)));
	}

} /* filter */

#endif /* end of include guard: FILE_FILTER_H_7W5OVO3A */
