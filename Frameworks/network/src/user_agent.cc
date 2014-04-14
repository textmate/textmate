#include "user_agent.h"
#include <OakSystem/application.h>
#include <text/format.h>
#include <plist/plist.h>
#include <io/path.h>
#include <cf/cf.h>
#include <oak/compat.h>

static std::string hardware_info (int field, bool integer = false)
{
	char buf[1024];
	size_t bufSize = sizeof(buf);
	int request[] = { CTL_HW, field };

	if(sysctl(request, sizeofA(request), buf, &bufSize, NULL, 0) != -1)
	{
		if(integer && bufSize == 4)
			return std::to_string(*(int*)buf);
		return std::string(buf, buf + bufSize - 1);
	}

	return "???";
}

static std::string user_uuid ()
{
	std::string res = NULL_STR;
	if(CFStringRef str = (CFStringRef)CFPreferencesCopyAppValue(CFSTR("SoftwareUpdateUUID"), kCFPreferencesCurrentApplication))
	{
		if(CFGetTypeID(str) == CFStringGetTypeID())
			res = cf::to_s(str);
		CFRelease(str);
	}

	if(res == NULL_STR)
	{
		res = oak::uuid_t().generate();
		CFPreferencesSetAppValue(CFSTR("SoftwareUpdateUUID"), cf::wrap(res), kCFPreferencesCurrentApplication);
		CFPreferencesAppSynchronize(kCFPreferencesCurrentApplication);
	}
	return res;
}

std::string create_agent_info_string ()
{
	return text::format("%s/%s/%s %zu.%zu.%zu/%s/%s/%s", oak::application_t::name().c_str(), oak::application_t::revision().c_str(), user_uuid().c_str(),
		oak::os_major(), oak::os_minor(), oak::os_patch(),
		hardware_info(HW_MACHINE).c_str(),
		hardware_info(HW_MODEL).c_str(),
		hardware_info(HW_NCPU, true).c_str()
	);
}
