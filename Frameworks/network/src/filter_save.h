#ifndef ARCHIVE_H_XHCVW91K
#define ARCHIVE_H_XHCVW91K

#include "download.h" // filter_t
#include <io/src/io.h>
#include <text/src/format.h>

namespace network
{
	struct save_t : filter_t
	{
		save_t (bool cleanup = true) : path(path::temp("dl_save_filter")), _cleanup(cleanup) { }

		~save_t ()
		{
			if(_fp)
				fclose(_fp);

			if(_cleanup)
				path::remove(path);
		}

		bool setup ()
		{
			return _fp = fopen(path.c_str(), "w");
		}

		bool receive_data (char const* bytes, size_t len)
		{
			return fwrite(bytes, 1, len, _fp) == len;
		}

		bool receive_end (std::string& error)
		{
			bool res = fclose(_fp) == 0;
			_fp = NULL;
			return res;
		}

		std::string name ()
		{
			return "i/o";
		}

		std::string path;

	private:
		FILE* _fp = NULL;
		bool _cleanup;
	};

} /* network */

#endif /* end of include guard: ARCHIVE_H_XHCVW91K */
