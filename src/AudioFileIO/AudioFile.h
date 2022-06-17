#if !defined(__AudioFile_HEADER_INCLUDED__)
#define __AudioFile_HEADER_INCLUDED__

#include "AudioFileIf.h"

#define WITH_SNDLIB

/*! \brief open, read, and write raw audio files in 16 bit integer little endian format.
*/
class CAudioFileRaw : public CAudioFileIf
{
public:
    CAudioFileRaw ();
    virtual ~CAudioFileRaw ();

    Error_t openFile (std::string cAudioFileName, FileIoType_t eIoType, FileSpec_t const *psFileSpec = 0) override;
    Error_t closeFile () override;
    bool isEof () override;
    bool isOpen () override;

private:
    CAudioFileRaw(const CAudioFileRaw& that);
    Error_t freeMemory_ () override;
    Error_t allocMemory_ () override;
    long long int readDataIntern_ (float **ppfAudioData, long long int iNumFrames) override;
    long long int writeDataIntern_ (const float* const* const ppfAudioData, long long int iLength) override;
    long long getLengthIntern_() override;
    long long getPositionIntern_() override;
    Error_t setPositionIntern_( long long iFrame ) override;

    float scaleUp (float fSample2Clip);
    float scaleDown (float fSample2Scale);

    std::fstream        m_File;                 //!< raw pcm file handle
    short               *m_piTmpBuff;           //!< temporary buffer for 16 bit integer values
};


/*! \brief open, read, and write audio files with sndlib (CMake option WITH_SNDLIB has to be ON)
*/
class CAudioFileSndLib : public CAudioFileIf
{
public:
    CAudioFileSndLib ();
    virtual ~CAudioFileSndLib ();

    Error_t openFile (std::string cAudioFileName, FileIoType_t eIoType, FileSpec_t const *psFileSpec = 0) override;
    Error_t closeFile () override;
    bool isEof () override;
    bool isOpen () override;
    Error_t freeMemory_ () override;
    Error_t allocMemory_ () override;

private:
    CAudioFileSndLib(const CAudioFileSndLib& that);
    long long int readDataIntern_ (float **ppfAudioData, long long int iLength) override;
    long long int writeDataIntern_ (const float* const* const ppfAudioData, long long int iLength) override;
    long long getLengthIntern_() override;
    long long getPositionIntern_() override;
    Error_t setPositionIntern_( long long iFrame ) override;

    int m_FileHandle = 0;           //!< sndlib file handle

    long long m_lFrameCnt = 0;      //!< current file position in frames
    long long m_lFileLength = 0;    //!< file length in frames
    double    **m_ppdTmpBuff = 0;   //!< temporary buffer for double values

};

#endif  //__AudioFile_HEADER_INCLUDED__
