#ifndef OAKDEBUG_H
#define OAKDEBUG_H

#include "OakAssert.h"

#ifndef NDEBUG
#define DB_VAR
#else
#define DB_VAR __attribute__ ((unused))
#endif

#endif
