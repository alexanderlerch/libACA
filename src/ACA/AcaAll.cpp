#include <string>

#include "AcaAll.h"

static const std::string kAcaBuildDate = __DATE__;

std::string getAcaVersion()
{
	std::string sVersion = std::to_string(ACA_VERSION_MAJOR) + "." + std::to_string(ACA_VERSION_MINOR) + "." + std::to_string(ACA_VERSION_PATCH);

	return sVersion;
}

std::string getAcaBuildDate()
{
	return 	kAcaBuildDate;
}
