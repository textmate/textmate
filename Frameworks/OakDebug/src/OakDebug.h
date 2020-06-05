#ifndef OAKDEBUG_H
#define OAKDEBUG_H

#include "OakAssert.h"
#include "OakDebugLog.h"
#include "OakWatchLeaks.h"
#include "OakBenchmark.h"

#ifndef NDEBUG
#define DB(code) code
#define DB_VAR
#else
#define DB(code)
#define DB_VAR __attribute__ ((unused))
#endif

#endif
