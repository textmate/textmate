#include "user_agent.h"
#include <OakSystem/application.h>
#include <text/format.h>
#include <plist/plist.h>
#include <io/path.h>

static std::string hardware_info (int field, bool integer = false)
{
	char buf[1024];
	size_t bufSize = sizeof(buf);
	int request[] = { CTL_HW, field };

	if(sysctl(request, sizeofA(request), buf, &bufSize, NULL, 0) != -1)
	{
		if(integer && bufSize == 4)
			return text::format("%d", *(int*)buf);
		return std::string(buf, buf + bufSize - 1);
	}

	return "???";
}

static std::string user_uuid ()
{
	oak::uuid_t uuid;
	return plist::get_key_path(plist::load(path::join(path::home(), "Library/Preferences/com.apple.CrashReporter.plist")), "userUUID", uuid) ? to_s(uuid) : "???";
}

std::string create_agent_info_string ()
{
	SInt32 osMajor, osMinor, osBugfix;
	Gestalt(gestaltSystemVersionMajor,  &osMajor);
	Gestalt(gestaltSystemVersionMinor,  &osMinor);
	Gestalt(gestaltSystemVersionBugFix, &osBugfix);

	return text::format("%s/%s/%s %zu.%zu.%zu/%s/%s/%s", oak::application_t::name().c_str(), oak::application_t::revision().c_str(), user_uuid().c_str(),
		(size_t)osMajor, (size_t)osMinor, (size_t)osBugfix,
		hardware_info(HW_MACHINE).c_str(),
		hardware_info(HW_MODEL).c_str(),
		hardware_info(HW_NCPU, true).c_str()
	);
}

