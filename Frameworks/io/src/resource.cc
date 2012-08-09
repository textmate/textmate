#include "resource.h"
#include "fsref.h"
#include "path.h"
#include <cf/cf.h>

namespace path
{
	bool is_text_clipping (std::string const& path)
	{
		bool res = false;
		if(extension(path) == "textClipping")
		{
			res = true;
		}
		else if(CFURLRef url = CFURLCreateWithFileSystemPath(kCFAllocatorDefault, cf::wrap(path), kCFURLPOSIXPathStyle, false))
		{
			LSItemInfoRecord info;
			if(noErr == LSCopyItemInfoForURL(url, kLSRequestTypeCreator, &info))
				res = info.filetype == kClippingTextType;
			CFRelease(url);
		}
		return res;
	}

	std::string resource (std::string const& path, ResType theType, ResID theID)
	{
		std::string res = NULL_STR;
		if(ResFileRefNum ref = FSOpenResFile(fsref_t(path), fsRdPerm))
		{
			if(Handle handle = Get1Resource(theType, theID))
			{
				HLock(handle);
				res = std::string(*handle, *handle + GetHandleSize(handle));
				HUnlock(handle);
				ReleaseResource(handle);
			}
			CloseResFile(ref);
		}
		return res;
	}

} /* path */
