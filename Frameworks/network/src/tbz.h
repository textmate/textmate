#ifndef NETWORK_TBZ_H_NEU56OWR
#define NETWORK_TBZ_H_NEU56OWR

#include <io/src/exec.h>
namespace network
{
	struct tbz_t
	{
		tbz_t (std::string const& dest);
		~tbz_t ();

		bool wait_for_tbz (std::string* output = nullptr, std::string* error = nullptr);

		int input_fd () const  { return _process.in; }
		operator bool () const { return _process.pid != -1; }

	private:
		dispatch_group_t _group = nullptr;
		io::process_t _process;
		std::string _output, _error;
		int _status;
	};

} /* network */

#endif /* end of include guard: NETWORK_TBZ_H_NEU56OWR */
