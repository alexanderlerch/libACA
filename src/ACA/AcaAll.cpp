#include "AcaAll.h"

#include <string>


static const std::string kAcaBuildDate = __DATE__;

/*! returns the version string
\return string
*/
std::string getAcaVersion()
{
	std::string sVersion = std::to_string(ACA_VERSION_MAJOR) + "." + std::to_string(ACA_VERSION_MINOR) + "." + std::to_string(ACA_VERSION_PATCH);

	return sVersion;
}


/*! returns the build date
\return string
*/
std::string getAcaBuildDate()
{
	return 	kAcaBuildDate;
}
