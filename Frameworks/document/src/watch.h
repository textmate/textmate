#ifndef IO_WATCH_H_T0171LAJ
#define IO_WATCH_H_T0171LAJ

#include <oak/misc.h>

#ifndef NOTE_CREATE
#define NOTE_CREATE (NOTE_REVOKE << 1)
#endif

namespace document
{
	struct PUBLIC watch_base_t
	{
		watch_base_t (std::string const& path);
		virtual ~watch_base_t ();
		virtual void callback (int flags, std::string const& newPath);

	private:
		size_t client_id;
	};

} /* document */

#endif /* end of include guard: IO_WATCH_H_T0171LAJ */
