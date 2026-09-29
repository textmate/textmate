#include "user_agent.h"
#include <OakSystem/src/application.h>
#include <text/src/format.h>
#include <io/src/path.h>
#include <cf/src/cf.h>
#include <oak/compat.h>

static std::string hardware_info (int field, bool integer = false)
{
	char buf[1024];
	size_t bufSize = sizeof(buf);
	int request[] = { CTL_HW, field };

	if(sysctl(request, sizeofA(request), buf, &bufSize, nullptr, 0) != -1)
	{
		if(integer && bufSize == 4)
			return std::to_string(*(int*)buf);
		return std::string(buf, buf + bufSize - 1);
	}

	return "???";
}

std::string create_agent_info_string ()
{
	uuid_t uuid;
	timespec wait = { };
	gethostuuid(uuid, &wait);
	uuid_string_t uuidStr;
	uuid_unparse_upper(uuid, uuidStr);

	return text::format("%s/%s/%s %zu.%zu.%zu/%s/%s/%s", oak::application_t::name().c_str(), oak::application_t::version().c_str(), uuidStr,
		oak::os_major(), oak::os_minor(), oak::os_patch(),
		hardware_info(HW_MACHINE).c_str(),
		hardware_info(HW_MODEL).c_str(),
		hardware_info(HW_NCPU, true).c_str()
	);
}
