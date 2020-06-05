#ifndef IO_EXEC_H_4VCQB3PK
#define IO_EXEC_H_4VCQB3PK

namespace io
{
	struct process_t
	{
		explicit operator bool () const { return pid != -1; }

		pid_t pid = -1;
		int in = -1, out = -1, err = -1;
	};

	process_t spawn (std::vector<std::string> const& args);
	process_t spawn (std::vector<std::string> const& args, std::map<std::string, std::string> const& environment);
	void exhaust_fd (int fd, std::string* out);

	// takes NULL-terminated list of arguments
	std::string exec (std::string const cmd, ...);
	std::string exec (std::map<std::string, std::string> const& environment, std::string const cmd, ...);

} /* io */

#endif /* end of include guard: IO_EXEC_H_4VCQB3PK */
