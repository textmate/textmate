#include "collection.h"
#include <OakSystem/application.h>
#include <plist/plist.h>
#include <regexp/format_string.h>
#include <text/format.h>
#include <io/path.h>

OAK_DEBUG_VAR(Document_Collection);

namespace document
{
	ui_proxy_t*& ui_proxy ();

	oak::uuid_t const kCollectionNew     = oak::uuid_t().generate();
	oak::uuid_t const kCollectionCurrent = oak::uuid_t().generate();

	void show (document_ptr document, oak::uuid_t const& collection, text::range_t const& selection, bool bringToFront)
	{
		ui_proxy()->show_document(collection, document, selection, bringToFront);
	}

	void show (std::vector<document_ptr> const& documents)
	{
		if(!documents.empty())
			ui_proxy()->show_documents(documents);
	}

	void show_browser (std::string const& path)
	{
		ui_proxy()->show_browser(path);
	}

	void run (bundle_command_t const& command, ng::buffer_t const& buffer, ng::ranges_t const& selection, document::document_ptr document, std::map<std::string, std::string> const& env, run_callback_ptr callback)
	{
		ui_proxy()->run(command, buffer, selection, document, env, callback);
	}

	// ============
	// = UI Proxy =
	// ============

	ui_proxy_t*& ui_proxy ()
	{
		static ui_proxy_t* proxy = NULL;
		return proxy;
	}

	void set_ui_proxy (ui_proxy_t* proxy)
	{
		ui_proxy() = proxy;
	}

} /* document */