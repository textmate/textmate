#include "ODBEditorSuite.h"
#include <document/collection.h>
#include <oak/debug.h>
#include <text/hexdump.h>
#include <oak/oak.h>

OAK_DEBUG_VAR(ODBEditorSuite);

struct ae_record_t;
typedef std::shared_ptr<ae_record_t> ae_record_ptr;

struct ae_record_t
{
	WATCH_LEAKS(ae_record_t);

	ae_record_t (AEDesc const& value) : value(value) { }
	ae_record_t (AEDesc const* value)                { AEDuplicateDesc(value, &this->value); }
	~ae_record_t ()                                  { AEDisposeDesc(&value); }

	DescType type () const                           { return value.descriptorType; }
	std::string data () const                        { std::string str(AEGetDescDataSize(&value), ' '); AEGetDescData(&value, &str[0], str.size()); return str; }
	size_t array_size () const                       { long res; AECountItems(&value, &res); return res; }

	std::string path () const
	{
		UInt8 buf[PATH_MAX];
		std::string const& fsref = data();
		if(noErr == FSRefMakePath((FSRef const*)&fsref[0], buf, sizeof(buf)))
			return std::string((char*)buf);
		return NULL_STR;
	}

	ae_record_ptr record_at_index (size_t i, DescType type = typeWildCard)
	{
		AEDesc item;
		if(noErr == AEGetNthDesc(&value, i+1, type, NULL, &item))
			return std::make_shared<ae_record_t>(item);
		return ae_record_ptr();
	}

	ae_record_ptr record_for_key (AEKeyword key, DescType type = typeWildCard)
	{
		AEDesc item;
		if(noErr == AEGetParamDesc(&value, key, type, &item))
			return std::make_shared<ae_record_t>(item);
		return ae_record_ptr();
	}

private:
	AEDesc value;
};

namespace odb // wrap in namespace to avoid clashing with other callbacks named the same
{
	struct save_close_callback_t : document::document_t::callback_t
	{
		WATCH_LEAKS(save_close_callback_t);

		save_close_callback_t (std::string const& path, std::string const& token, ae_record_ptr const& sender) : path(path), token(token), sender(sender)
		{
		}

		void handle_document_event (document::document_ptr document, event_t event)
		{
			if(event == did_change_open_status && !document->is_open())
				send_event(kAEClosedFile);
			else if(event == did_save)
				send_event(kAEModifiedFile);

			if(event == did_change_open_status && !document->is_open())
			{
				document->remove_callback(this);
				delete this;
			}
		}

	private:
		void send_event (AEEventID eventId) const
		{
			D(DBF_ODBEditorSuite, int c = htonl(eventId); bug("‘%.4s’\n", (char*)&c););

			FSRef fsRef;
			if(noErr == FSPathMakeRef((UInt8 const*)path.c_str(), &fsRef, NULL))
			{
				AEAddressDesc target;
				std::string const& senderData = sender->data();
				D(DBF_ODBEditorSuite, int t = htonl(sender->type()); bug("send to: ‘%.*s’ (‘%.4s’)\n", (int)senderData.size(), senderData.data(), (char*)&t););
				AECreateDesc(sender->type() == typeType ? typeApplSignature : sender->type(), senderData.data(), senderData.size(), &target);

				AppleEvent event;
				AECreateAppleEvent(kODBEditorSuite, eventId, &target, kAutoGenerateReturnID, kAnyTransactionID, &event);

				AEPutParamPtr(&event, keyDirectObject, typeFSRef, &fsRef, sizeof(fsRef));
				if(token != NULL_STR)
					AEPutParamPtr(&event, keySenderToken, typeWildCard, token.data(), token.size());

				AppleEvent reply;
				OSStatus err DB_VAR = AESendMessage(&event, &reply, kAENoReply, kAEDefaultTimeout);
				D(DBF_ODBEditorSuite, if(err != noErr) bug("*** AESendMessage(): error %d\n", (int)err););

				AEDisposeDesc(&event);
				AEDisposeDesc(&target);
			}
		}

		std::string path;
		std::string token;
		ae_record_ptr sender;
	};
}

bool DidHandleODBEditorEvent (AppleEvent const* event)
{
	if(!event)
		return false;

	D(DBF_ODBEditorSuite, int c = htonl(event->descriptorType); bug("descriptor: ‘%.4s’\n", (char*)&c););
	// open content: file:///Developer/Documentation/DocSets/com.apple.ADC_Reference_Library.CoreReference.docset/Contents/Resources/Documents/documentation/AppleScript/Conceptual/AppleEvents/responding_aepg/chapter_6_section_4.html

	DescType attr;
	if(noErr != AEGetAttributePtr(event, keyEventClassAttr, typeType, NULL, &attr, sizeof(attr), NULL) || attr != kCoreEventClass)
		return false;

	if(noErr != AEGetAttributePtr(event, keyEventIDAttr, typeType, NULL, &attr, sizeof(attr), NULL) || attr != kAEOpenDocuments)
		return false;

	D(DBF_ODBEditorSuite, bug("Got ‘odoc’ event\n"););

	ae_record_t ae(event);
	ae_record_ptr files        = ae.record_for_key(keyDirectObject,    typeAEList);
	ae_record_ptr displayNames = ae.record_for_key(keyFileCustomPath,  typeAEList);
	ae_record_ptr positions    = ae.record_for_key(keyAEPosition,      typeAEList);
	ae_record_ptr sender       = ae.record_for_key(keyServerID,        typeWildCard);
	ae_record_ptr tokens       = ae.record_for_key(keyFileSenderToken, typeAEList);
	ae_record_ptr searchTexts  = ae.record_for_key(keyAESearchText,    typeAEList);

	if(ae_record_ptr const& extra = ae.record_for_key(keyAEPropData, typeAERecord))
	{
		if(!displayNames)
			displayNames = extra->record_for_key(keyFileCustomPath, typeAEList);
		if(!sender)
			sender = extra->record_for_key(keyServerID, typeWildCard);
		if(!tokens)
			tokens = extra->record_for_key(keyFileSenderToken, typeAEList);
	}

	if(sender || positions || displayNames)
	{
		std::vector<document::document_ptr> documents;
		for(size_t i = 0; i < files->array_size(); ++i)
		{
			ae_record_ptr file = files->record_at_index(i, typeFSRef);
			documents.push_back(document::create(file->path()));
			documents.back()->set_recent_tracking(false);

			if(displayNames && i < displayNames->array_size())
				documents.back()->set_custom_name(displayNames->record_at_index(i, typeUTF8Text)->data());

			if(positions && i < positions->array_size())
			{
				std::string const& v = positions->record_at_index(i)->data();
				if(v.size() == sizeof(PBX_SelectionRange))
				{
					PBX_SelectionRange& sel = *(PBX_SelectionRange*)&v[0];
					if(sel.lineNum >= 0)
						documents.back()->set_selection(text::pos_t(sel.lineNum, 0));
				}
			}

			if(sender)
			{
				std::string token = (tokens && i < tokens->array_size()) ? tokens->record_at_index(i)->data() : NULL_STR;
				documents.back()->add_callback(new odb::save_close_callback_t(file->path(), token, sender));

				D(DBF_ODBEditorSuite, int c = htonl(sender->type()); bug("server: ‘%.*s’ (‘%.4s’), token: ‘%s’\n", (int)sender->data().size(), sender->data().data(), (char*)&c, token != NULL_STR ? token.c_str() : "(none)"););
			}
		}

		document::show(documents);

		return true;
	}
	return false;
}
