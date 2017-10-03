#ifndef ENCODING_H_3OJVUZM1
#define ENCODING_H_3OJVUZM1

#include <oak/misc.h>

namespace encoding
{
	PUBLIC std::vector<std::string> charsets ();
	PUBLIC double probability (char const* first, char const* last, std::string const& charset);
	PUBLIC void learn (char const* first, char const* last, std::string const& charset);

} /* encoding */

#endif /* end of include guard: ENCODING_H_3OJVUZM1 */
