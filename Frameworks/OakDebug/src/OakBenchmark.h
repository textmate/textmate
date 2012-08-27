#ifndef OAKBENCHMARK_H_RNPP99KX
#define OAKBENCHMARK_H_RNPP99KX

#include <oak/duration.h>
#include "OakDebug.h"

namespace oak
{
	template <typename DBF>
	struct benchmark_t
	{
		typedef std::shared_ptr< benchmark_t<DBF> > ptr;

		benchmark_t (DBF flag, std::string const& message) : flag(flag), message(message), timer() { }
		~benchmark_t ()
		{
			D(flag, bug("%s ran in %.2fs\n", message.c_str(), timer.duration()););
		}

	private:
		DBF flag;
		std::string message;
		oak::duration_t timer;
	};
}

#define BENCHMARK(DBF, message) \
	if(oak::benchmark_t<typeof(DBF)>::ptr _benchmark = oak::benchmark_t<typeof(DBF)>::ptr(new oak::benchmark_t<typeof(DBF)>(DBF, message)))

#endif /* end of include guard: OAKBENCHMARK_H_RNPP99KX */
