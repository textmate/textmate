#include "types.h"

namespace text
{
	pos_t pos_t::zero          = pos_t(0, 0);
	pos_t pos_t::undefined     = pos_t(SIZE_T_MAX, SIZE_T_MAX);
	range_t range_t::undefined = range_t(pos_t::undefined, pos_t::undefined);
};
