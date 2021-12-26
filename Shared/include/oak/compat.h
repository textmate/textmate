#ifndef COMPAT_H_RD1Z6YZA
#define COMPAT_H_RD1Z6YZA

namespace oak
{
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdeprecated-declarations"
	inline size_t get_gestalt (OSType selector)
	{
		SInt32 res;
		return Gestalt(selector, &res) == noErr ? res : 0;
	}

	inline size_t os_major () { return get_gestalt(gestaltSystemVersionMajor); }
	inline size_t os_minor () { return get_gestalt(gestaltSystemVersionMinor); }
	inline size_t os_patch () { return get_gestalt(gestaltSystemVersionBugFix); }

	inline OSStatus execute_with_privileges (AuthorizationRef authorization, std::string const& pathToTool, AuthorizationFlags options, char* const* arguments, FILE** communicationsPipe)
	{
		return AuthorizationExecuteWithPrivileges(authorization, pathToTool.c_str(), options, arguments, communicationsPipe);
	}
#pragma clang diagnostic pop

	// As of macOS 12.0, this system call behaves identically to the fork(2) system call, except without calling any handlers registered with pthread_atfork(2).
	// Consider migrating callers to posix_spawn();
	inline pid_t vfork() {
		NSOperatingSystemVersion monterey = {
			.majorVersion = 12,
			.minorVersion = 0,
			.patchVersion = 0,
		};
		if ([[NSProcessInfo processInfo] isOperatingSystemAtLeastVersion:monterey]) {
			return fork();
		} else {
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdeprecated-declarations"
			return vfork();
#pragma clang diagnostic pop
		}
	}
} /* oak */

#endif /* end of include guard: COMPAT_H_RD1Z6YZA */
