#include "resource.h"
#include "fsref.h"
#include "path.h"
#include <cf/cf.h>
#include <crash/info.h>

namespace path
{
	bool is_text_clipping (std::string const& path)
	{
		crash_reporter_info_t info("Path: " + path);

		bool res = false;
		if(extension(path) == "textClipping")
		{
			res = true;
		}
		else if(CFURLRef url = CFURLCreateFromFileSystemRepresentation(kCFAllocatorDefault, (UInt8 const*)path.data(), path.size(), false))
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
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdeprecated-declarations"
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
#pragma clang diagnostic pop
		return res;
	}

} /* path */
