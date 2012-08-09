#ifndef OAKDEBUG_H
#define OAKDEBUG_H

#ifndef PUBLIC
#define PUBLIC __attribute__((__visibility__("default")))
#endif

#ifdef OakDebug_EXPORTS
#undef NDEBUG
#endif

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
