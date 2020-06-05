#ifndef IO_ENVIRONMENT_H_P8799509
#define IO_ENVIRONMENT_H_P8799509

namespace oak
{
	std::map<std::string, std::string> const& basic_environment ();
	void set_basic_environment (std::map<std::string, std::string> const& newEnvironment);

} /* io */

#endif /* end of include guard: IO_ENVIRONMENT_H_P8799509 */
