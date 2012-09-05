#ifndef DOCUMENT_COLLECTION_LNUANNO
#define DOCUMENT_COLLECTION_LNUANNO

#include "document.h"
#include <command/parser.h>
#include <oak/misc.h>
#include <oak/callbacks.h>
#include <oak/debug.h>

namespace document
{
	struct PUBLIC run_callback_t
	{
		virtual void update_environment (std::map<std::string, std::string>& env) { }
	};

	typedef std::shared_ptr<run_callback_t> run_callback_ptr;

	PUBLIC extern oak::uuid_t const kCollectionNew;
	PUBLIC extern oak::uuid_t const kCollectionCurrent;

	PUBLIC void show (document_ptr document, oak::uuid_t const& collection = kCollectionCurrent, text::range_t const& selection = text::range_t::undefined, bool bringToFront = true);
	PUBLIC void show (std::vector<document_ptr> const& documents);
	PUBLIC void show_browser (std::string const& path);
	PUBLIC std::map<std::string, std::string> variables (document_ptr document, oak::uuid_t const& collection = kCollectionCurrent, std::map<std::string, std::string> env = std::map<std::string, std::string>());
	PUBLIC void run (bundle_command_t const& command, ng::buffer_t const& buffer, ng::ranges_t const& selection, document::document_ptr document, std::map<std::string, std::string> const& env = std::map<std::string, std::string>(), run_callback_ptr callback = run_callback_ptr());

	PUBLIC bool load_session ();
	PUBLIC void save_session (bool includeUntitled);

	struct PUBLIC ui_proxy_t
	{
		virtual ~ui_proxy_t () { }
		virtual void show_document (oak::uuid_t const& collection, document_ptr document, text::range_t const& range, bool bringToFront) const = 0;
		virtual void show_documents (std::vector<document_ptr> const& documents, std::string const& browserPath) = 0;
		virtual void run (bundle_command_t const& command, ng::buffer_t const& buffer, ng::ranges_t const& selection, document::document_ptr document, std::map<std::string, std::string> const& env, run_callback_ptr callback) = 0;
		virtual bool load_session (std::string const& path) const = 0;
		virtual bool save_session (std::string const& path, bool includeUntitled) const = 0;
	};

	PUBLIC void set_ui_proxy (ui_proxy_t* proxy);

} /* document */

#endif /* end of include guard: DOCUMENT_COLLECTION_LNUANNO */
