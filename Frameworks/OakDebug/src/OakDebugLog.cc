#include "OakDebug.h"
#ifndef NDEBUG
#include <oak/oak.h>

static std::mutex RegistryMutex;

std::map<std::string, bool>& OakDebugBaseClass::registry ()
{
	static std::map<std::string, bool>* Registry = nullptr;

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
				debug = nullptr;
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
		return title.substr(0, separatorIndex);
	else
		return title;
}

std::vector<std::string> OakDebugBaseClass::sectionNames ()
{
	std::map<std::string, size_t> sectionNames;

	std::lock_guard<std::mutex> lock(RegistryMutex);
	for(auto pair : registry())
		sectionNames[sectionName(pair.first)]++;

	std::vector<std::string> res;
	for(auto pair : sectionNames)
	{
		if(pair.second > 1)
			res.push_back(pair.first);
	}
	return res;
}

bool OakDebugBaseClass::checkForDebugVar (char const* name)
{
	std::lock_guard<std::mutex> lock(RegistryMutex);
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
