#ifndef POST_H_HD2H7K9B
#define POST_H_HD2H7K9B

#include <oak/misc.h>

PUBLIC long post_to_server (std::string const& url, std::map<std::string, std::string> const& payload, std::map<std::string, std::string>* headersOut = NULL);

#endif /* end of include guard: POST_H_HD2H7K9B */
