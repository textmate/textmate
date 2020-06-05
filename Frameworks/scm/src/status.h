#ifndef SCM_STATUS_H_KRMEHC4M
#define SCM_STATUS_H_KRMEHC4M

namespace scm
{
	namespace status
	{
		enum type
		{
			unknown     = 0,   // We should always know the state of a file so this should never happen
			none        = 1,   // File is known and clean
			unversioned = 2,   // File is not tracked by the version control system nor ignored
			modified    = 4,   // File has local changes
			added       = 8,   // File is locally marked for tracking
			deleted     = 16,  // File is locally marked for deletion
			conflicted  = 32,  // File has conflicts that the user should resolve
			ignored     = 64,  // File is being ignored by the version control system
			mixed       = 128, // Directory contains files with mixed state
		};
		std::string to_s (type status);
	};
	typedef std::map<std::string, scm::status::type> status_map_t;

} /* scm */

#endif /* end of include guard: SCM_STATUS_H_KRMEHC4M */
