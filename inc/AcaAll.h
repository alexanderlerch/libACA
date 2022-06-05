#if !defined(__AcaAll_hdr__)
#define __AcaAll_hdr__

#include "ACAConfig.h"

#include "AudioFileIf.h"
#include "BeatHisto.h"
#include "ErrorDef.h"
#include "Feature.h"
#include "FeatureFromBlock.h"
#include "Novelty.h"
#include "NoveltyFromBlock.h"
#include "Pitch.h"
#include "PitchFromBlock.h"
#include "Spectrogram.h"

#include "ToolBlockAudio.h"
#include "ToolCcf.h"
#include "ToolConversion.h"
#include "ToolGammatone.h"
#include "ToolInstFreq.h"
#include "ToolLowPass.h"
#include "ToolPreProc.h"
#include "ToolResample.h"
#include "ToolSimpleDtw.h"

/*! returns the version string of libACA
\return std::string
*/
std::string getAcaVersion();

/*! returns the build date string of libACA
\return std::string
*/
std::string getAcaBuildDate();

#endif // #if !defined(__AcaAll_hdr__)
