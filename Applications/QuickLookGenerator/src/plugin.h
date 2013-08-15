#ifndef PLUGIN_H_U378U6OH
#define PLUGIN_H_U378U6OH

#include <CoreFoundation/CFPlugInCOM.h>
#include <QuickLook/QuickLook.h>
#include <oak/misc.h>

#define kTextMateQuickLookGeneratorPlugInFactoryID CFUUIDGetConstantUUIDWithBytes(NULL, 0xB2, 0x11, 0xF2, 0x7A, 0x80, 0x6A, 0x47, 0xD3, 0x95, 0x2C, 0xA8, 0x54, 0x5A, 0xA8, 0x91, 0x8E)

#ifndef OAK_EXTERN_C_BEGIN
#ifdef __cplusplus
#define OAK_EXTERN_C_BEGIN extern "C" {
#else
#define OAK_EXTERN_C_BEGIN
#endif
#endif

#ifndef OAK_EXTERN_C_END
#ifdef __cplusplus
#define OAK_EXTERN_C_END }
#else
#define OAK_EXTERN_C_END
#endif
#endif

OAK_EXTERN_C_BEGIN

typedef struct {
	QLGeneratorInterfaceStruct *super;
	ULONG ref_count;
} TextMateQuickLookPlugIn;

TextMateQuickLookPlugIn* TextMateQuickLookPlugIn_Allocate ();
void TextMateQuickLookPlugIn_Deallocate (TextMateQuickLookPlugIn* plugin);

// IUnknown interface
HRESULT TextMateQuickLookPlugIn_QueryInterface (void*, REFIID, LPVOID*);
ULONG TextMateQuickLookPlugIn_AddRef (void*);
ULONG TextMateQuickLookPlugIn_Release (void*);

// QLGenerator interface
OSStatus TextMateQuickLookPlugIn_GenerateThumbnailForURL (void*, QLThumbnailRequestRef, CFURLRef, CFStringRef, CFDictionaryRef, CGSize);
void TextMateQuickLookPlugIn_CancelThumbnailGeneration (void*, QLThumbnailRequestRef);
OSStatus TextMateQuickLookPlugIn_GeneratePreviewForURL (void*, QLPreviewRequestRef, CFURLRef, CFStringRef, CFDictionaryRef);
void TextMateQuickLookPlugIn_CancelPreviewGeneration (void*, QLPreviewRequestRef);

OAK_EXTERN_C_END

#endif /* end of include guard: PLUGIN_H_U378U6OH */
