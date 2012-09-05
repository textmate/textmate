#include "status.h"
#include <text/format.h>

namespace scm
{
	namespace status
	{
		std::string to_s (type status)
		{
			switch(status)
			{
				case unknown:      return "unknown";
				case none:         return "clean";
				case unversioned:  return "untracked";
				case modified:     return "modified";
				case added:        return "added";
				case deleted:      return "deleted";
				case conflicted:   return "conflicted";
				case ignored:      return "ignored";
				case mixed:        return "mixed";
				default:           return text::format("unknown (%d)", status);
			}
		}
	}
} /* scm */
