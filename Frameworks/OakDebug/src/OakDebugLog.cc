#include "OakDebug.h"
#ifndef NDEBUG
#include <oak/oak.h>

std::map<std::string, bool>& OakDebugBaseClass::registry ()
{
	static std::map<std::string, bool>* Registry = NULL;

	static bool didLoad = false;
	if(didLoad == false)
	{
		Registry = new std::map<std::string, bool>;

		char* tok;
		char* debug;
		if(debug = getenv("DebugFlags"))
		{
			while(tok = strtok(debug, ", ;"))
			{
				(*Registry)[tok] = true;
				debug = NULL;
			}
		}
		didLoad = true;
	}

	return *Registry;
}

std::string OakDebugBaseClass::sectionName (std::string const& title)
{
	size_t separatorIndex = title.find("_");
	if(separatorIndex != std::string::npos && separatorIndex != title.size() - 1)
		return title.substr(0, separatorIndex).c_str();
	else
		return title;
}

std::vector<std::string> OakDebugBaseClass::sectionNames ()
{
	std::map<std::string, size_t> sectionNames;
	iterate(it, registry())
		sectionNames[sectionName(it->first)]++;

	std::vector<std::string> res;
	iterate(it, sectionNames)
	{
		if(it->second > 1)
			res.push_back(it->first);
	}
	return res;
}

bool OakDebugBaseClass::checkForDebugVar (const char *name)
{
	return registry()["All"] || registry()[name];
}

void OakPrintF (char const* file, char const* function, int line, char const* format, ...)
{
	fprintf(stderr, "%s\t%s\t%d\t", file, function, line);

	va_list ap;
	va_start(ap, format);
	vfprintf(stderr, format, ap);
	va_end(ap);
}
#endif
