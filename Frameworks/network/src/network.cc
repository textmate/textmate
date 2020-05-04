#include "network.h"

__attribute__((constructor)) static void setup_curl ()
{
	curl_global_init(CURL_GLOBAL_ALL);
}
