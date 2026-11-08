#ifndef SCM_GIT_PARSE_H_29C6B97D
#define SCM_GIT_PARSE_H_29C6B97D

#include <scm/status.h>

namespace scm::git
{
	scm::status::type resolve_porcelain_xy (char indexStatus, char workTreeStatus);
	void parse_porcelain (std::map<std::string, scm::status::type>& entries, std::string const& output);
}

#endif /* end of include guard: SCM_GIT_PARSE_H_29C6B97D */
