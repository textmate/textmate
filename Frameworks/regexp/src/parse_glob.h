#ifndef PARSE_GLOB_H_RJCIZVDD
#define PARSE_GLOB_H_RJCIZVDD

std::string convert_glob_to_regexp (std::string const& str, bool matchDotFiles = false);
std::vector<std::string> expand_braces (std::string const& str);

#endif /* end of include guard: PARSE_GLOB_H_RJCIZVDD */
