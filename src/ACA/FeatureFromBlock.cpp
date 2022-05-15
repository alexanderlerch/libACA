
#include "Vector.h"
#include "Util.h"

#include "FeatureFromBlock.h"


/*! returns size of output feature (1 in most cases)
\return int
*/

inline int CFeatureFromBlockIf::getFeatureDimensions() const
{
    return 1;
}
