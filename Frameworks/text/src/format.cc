#include "format.h"

namespace text
{
	std::string format_size (size_t inBytes)
	{
		double size         = (double)inBytes;
		char const* unitStr = inBytes == 1 ? "byte" : "bytes";

		if(inBytes > 1000 * 1024*1024)
		{
			size   /= 1024*1024*1024;
			unitStr = "GiB";
		}
		else if(inBytes > 1000 * 1024)
		{
			size   /= 1024*1024;
			unitStr = "MiB";
		}
		else if(inBytes > 1000)
		{
			size   /= 1024;
			unitStr = "KiB";
		}
		else
		{
			return text::format("%zu %s", inBytes, unitStr);
		}

		return text::format("%.1f %s", size, unitStr);
	}

} /* text */
