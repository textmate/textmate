#ifndef IO_RESOURCE_H_6LC0FV3L
#define IO_RESOURCE_H_6LC0FV3L

#include <oak/misc.h>

namespace path
{
	PUBLIC bool is_text_clipping (std::string const& path);
	PUBLIC std::string resource (std::string const& path, ResType theType, ResID theID);

} /* path */

#endif /* end of include guard: IO_RESOURCE_H_6LC0FV3L */
