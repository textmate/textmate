#ifndef DOCUMENT_COLLECTION_LNUANNO
#define DOCUMENT_COLLECTION_LNUANNO

#include "document.h"
#include <command/parser.h>
#include <oak/misc.h>
#include <oak/callbacks.h>
#include <oak/debug.h>

namespace document
{
	PUBLIC extern oak::uuid_t const kCollectionAny;

	PUBLIC void show (document_ptr document, oak::uuid_t const& collection = kCollectionAny, text::range_t const& selection = text::range_t::undefined, bool bringToFront = true);
	PUBLIC void show (std::vector<document_ptr> const& documents);

	struct PUBLIC ui_proxy_t
	{
		virtual ~ui_proxy_t () { }
		virtual void show_document (oak::uuid_t const& collection, document_ptr document, text::range_t const& range, bool bringToFront) const = 0;
		virtual void show_documents (std::vector<document_ptr> const& documents) const = 0;
	};

	PUBLIC void set_ui_proxy (ui_proxy_t* proxy);

} /* document */

#endif /* end of include guard: DOCUMENT_COLLECTION_LNUANNO */
