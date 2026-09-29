#include "ODBEditorSuite.h"
#include <cf/src/cf.h>
#include <document/src/OakDocument.h>
#include <document/src/OakDocumentController.h>
#include <ns/src/ns.h>
#include <oak/debug.h>
#include <text/src/hexdump.h>
#include <oak/oak.h>

struct ae_record_t;
typedef std::shared_ptr<ae_record_t> ae_record_ptr;

struct ae_record_t
{
	ae_record_t (AEDesc const& value) : value(value) { }
	ae_record_t (AEDesc const* value)                { AEDuplicateDesc(value, &this->value); }
	~ae_record_t ()                                  { AEDisposeDesc(&value); }

	DescType type () const                           { return value.descriptorType; }
	std::string data () const                        { std::string str(AEGetDescDataSize(&value), ' '); AEGetDescData(&value, &str[0], str.size()); return str; }
	size_t array_size () const                       { long res; AECountItems(&value, &res); return res; }

	std::string path () const
	{
		std::string res = NULL_STR;
		AEDesc fileUrlDesc;
		if(noErr == AECoerceDesc(&value, typeFileURL, &fileUrlDesc))
		{
			std::string buf(AEGetDescDataSize(&fileUrlDesc), '\0');
			AEGetDescData(&fileUrlDesc, &buf[0], buf.size());
			if(CFURLRef url = CFURLCreateWithBytes(kCFAllocatorDefault, (UInt8*)buf.data(), buf.size(), kCFStringEncodingUTF8, NULL))
			{
				if(CFStringRef path = CFURLCopyFileSystemPath(url, kCFURLPOSIXPathStyle))
				{
					res = cf::to_s(path);
					CFRelease(path);
				}
				CFRelease(url);
			}
			AEDisposeDesc(&fileUrlDesc);
		}
		return res;
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
	struct save_close_callback_t
	{
		save_close_callback_t (OakDocument* document, std::string path, std::string token, ae_record_ptr sender)
		{
			_save_observer = [NSNotificationCenter.defaultCenter addObserverForName:OakDocumentDidSaveNotification object:document queue:nil usingBlock:^(NSNotification*){
				send_event(kAEModifiedFile, path, token, sender);
			}];

			_close_observer = [NSNotificationCenter.defaultCenter addObserverForName:OakDocumentWillCloseNotification object:document queue:nil usingBlock:^(NSNotification*){
				send_event(kAEClosedFile, path, token, sender);
				delete this;
			}];
		}

		~save_close_callback_t ()
		{
			[NSNotificationCenter.defaultCenter removeObserver:_save_observer];
			[NSNotificationCenter.defaultCenter removeObserver:_close_observer];
		}

	private:
		static void send_event (AEEventID eventId, std::string const& path, std::string const& token, ae_record_ptr sender)
		{
			if(CFURLRef url = CFURLCreateFromFileSystemRepresentation(kCFAllocatorDefault, (UInt8*)path.data(), path.size(), false))
			{
				AEAddressDesc target;
				std::string const& senderData = sender->data();
				AECreateDesc(sender->type() == typeType ? typeApplSignature : sender->type(), senderData.data(), senderData.size(), &target);

				AppleEvent event;
				AECreateAppleEvent(kODBEditorSuite, eventId, &target, kAutoGenerateReturnID, kAnyTransactionID, &event);

				std::string urlString = cf::to_s(CFURLGetString(url));
				AEPutParamPtr(&event, keyDirectObject, typeFileURL, urlString.data(), urlString.size());
				CFRelease(url);

				if(token != NULL_STR)
					AEPutParamPtr(&event, keySenderToken, typeWildCard, token.data(), token.size());

				AppleEvent reply;
				OSStatus err = AESendMessage(&event, &reply, kAENoReply, kAEDefaultTimeout);
				if(err != noErr)
					os_log_error(OS_LOG_DEFAULT, "AESendMessage() failed with error code %d", (int)err);

				AEDisposeDesc(&event);
				AEDisposeDesc(&target);
			}
		}

		id _save_observer;
		id _close_observer;
	};
}

bool DidHandleODBEditorEvent (AppleEvent const* event)
{
	if(!event)
		return false;

	// open content: file:///Developer/Documentation/DocSets/com.apple.ADC_Reference_Library.CoreReference.docset/Contents/Resources/Documents/documentation/AppleScript/Conceptual/AppleEvents/responding_aepg/chapter_6_section_4.html

	DescType attr;
	if(noErr != AEGetAttributePtr(event, keyEventClassAttr, typeType, NULL, &attr, sizeof(attr), NULL) || attr != kCoreEventClass)
		return false;

	if(noErr != AEGetAttributePtr(event, keyEventIDAttr, typeType, NULL, &attr, sizeof(attr), NULL) || attr != kAEOpenDocuments)
		return false;

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
		NSMutableArray<OakDocument*>* documents = [NSMutableArray array];
		for(size_t i = 0; i < files->array_size(); ++i)
		{
			ae_record_ptr file = files->record_at_index(i, typeFSRef);
			OakDocument* doc = [OakDocumentController.sharedInstance documentWithPath:to_ns(file->path())];
			[documents addObject:doc];
			doc.recentTrackingDisabled = YES;

			if(displayNames && i < displayNames->array_size())
				doc.customName = to_ns(displayNames->record_at_index(i, typeUTF8Text)->data());

			if(positions && i < positions->array_size())
			{
				std::string const& v = positions->record_at_index(i)->data();
				if(v.size() == sizeof(PBX_SelectionRange))
				{
					PBX_SelectionRange& sel = *(PBX_SelectionRange*)&v[0];
					if(sel.lineNum >= 0)
						doc.selection = to_ns(text::pos_t(sel.lineNum, 0));
				}
			}

			if(sender)
			{
				std::string token = (tokens && i < tokens->array_size()) ? tokens->record_at_index(i)->data() : NULL_STR;
				new odb::save_close_callback_t(doc, file->path(), token, sender);
			}
		}

		[OakDocumentController.sharedInstance showDocuments:documents];

		return true;
	}
	return false;
}
