#if !defined(__AcaAll_HEADER_INCLUDED__)
#define __AcaAll_HEADER_INCLUDED__
#include <cstdint>

#include "ACAConfig.h"

#include "AudioFileIf.h"
#include "BeatHisto.h"
#include "Chord.h"
#include "ChordFromBlock.h"
#include "ErrorDef.h"
#include "Feature.h"
#include "FeatureFromBlock.h"
#include "Key.h"
#include "KeyFromChroma.h"
#include "Novelty.h"
#include "NoveltyFromBlock.h"
#include "Pitch.h"
#include "PitchFromBlock.h"
#include "Spectrogram.h"
#include "SubFingerprint.h"

#include "ToolBlockAudio.h"
#include "ToolCcf.h"
#include "ToolConversion.h"
#include "ToolFingerprint.h"
#include "ToolGammatone.h"
#include "ToolGmm.h"
#include "ToolGmmClassifier.h"
#include "ToolInstFreq.h"
#include "ToolLeaveOneOutCrossVal.h"
#include "ToolLowPass.h"
#include "ToolNmf.h"
#include "ToolPca.h"
#include "ToolPreProc.h"
#include "ToolResample.h"
#include "ToolSeqFeatureSel.h"
#include "ToolSimpleDtw.h"
#include "ToolSimpleKmeans.h"
#include "ToolSimpleKnn.h"
#include "ToolViterbi.h"

/*! returns the version string of libACA
\return std::string
*/
std::string getAcaVersion();

/*! returns the build date string of libACA
\return std::string
*/
std::string getAcaBuildDate();

#endif // #if !defined(__AcaAll_HEADER_INCLUDED__)
