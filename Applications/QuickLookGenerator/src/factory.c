#include "plugin.h"
#include <oak/misc.h>

// ==================
// = Plugin Factory =
// ==================

PUBLIC void* TextMateQuickLookGeneratorPlugInFactory (CFAllocatorRef allocator, CFUUIDRef typeID)
{
	if(!CFEqual(typeID, kQLGeneratorTypeID))
	{
		CFShow(CFSTR("Wrong type id"));
		return NULL;
	}
	return TextMateQuickLookPlugIn_Allocate();
}

TextMateQuickLookPlugIn* TextMateQuickLookPlugIn_Allocate ()
{
	TextMateQuickLookPlugIn* plugin = malloc(sizeof(TextMateQuickLookPlugIn));

	plugin->super = malloc(sizeof(QLGeneratorInterfaceStruct));
	plugin->super->AddRef         = &TextMateQuickLookPlugIn_AddRef;
	plugin->super->QueryInterface = &TextMateQuickLookPlugIn_QueryInterface;
	plugin->super->Release        = &TextMateQuickLookPlugIn_Release;
	plugin->super->GenerateThumbnailForURL   = &TextMateQuickLookPlugIn_GenerateThumbnailForURL;
	plugin->super->CancelThumbnailGeneration = &TextMateQuickLookPlugIn_CancelThumbnailGeneration;
	plugin->super->GeneratePreviewForURL     = &TextMateQuickLookPlugIn_GeneratePreviewForURL;
	plugin->super->CancelPreviewGeneration   = &TextMateQuickLookPlugIn_CancelPreviewGeneration;

	plugin->ref_count = 1;
	CFPlugInAddInstanceForFactory(kTextMateQuickLookGeneratorPlugInFactoryID);

	return plugin;
}

void TextMateQuickLookPlugIn_Deallocate (TextMateQuickLookPlugIn* plugin)
{
	free(plugin->super);
	free(plugin);
	CFPlugInRemoveInstanceForFactory(kTextMateQuickLookGeneratorPlugInFactoryID);
}

// ======================
// = IUnknown interface =
// ======================

ULONG TextMateQuickLookPlugIn_AddRef (void* instance)
{
	TextMateQuickLookPlugIn* plugin = (TextMateQuickLookPlugIn*)instance;
	return ++plugin->ref_count;
}

ULONG TextMateQuickLookPlugIn_Release (void* instance)
{
	TextMateQuickLookPlugIn* plugin = (TextMateQuickLookPlugIn*)instance;
	ULONG count = --plugin->ref_count;
	if(count == 0)
		TextMateQuickLookPlugIn_Deallocate(plugin);

	return count;
}

HRESULT TextMateQuickLookPlugIn_QueryInterface (void* instance, REFIID iid, LPVOID* ppv)
{
	TextMateQuickLookPlugIn* plugin = (TextMateQuickLookPlugIn*)instance;
	CFUUIDRef uuid = CFUUIDCreateFromUUIDBytes(NULL, iid);
	bool requestedCallbacks = CFEqual(uuid, kQLGeneratorCallbacksInterfaceID);
	CFRelease(uuid);

	if(!requestedCallbacks)
	{
		CFShow(CFSTR("Wrong interface"));
		*ppv = NULL;
		return E_NOINTERFACE;
	}

	TextMateQuickLookPlugIn_AddRef(plugin);
	*ppv = plugin;
	return S_OK;
}
