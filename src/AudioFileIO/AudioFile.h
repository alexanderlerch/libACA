#if !defined(__AudioFile_hdr__)
#define __AudioFile_hdr__

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
    Error_t freeMemory () override;
    Error_t allocMemory () override;
    long long int readDataIntern (float **ppfAudioData, long long int iNumFrames) override;
    long long int writeDataIntern (float **ppfAudioData, long long int iLength) override;
    long long getLengthIntern() override;
    long long getPositionIntern() override;
    Error_t setPositionIntern( long long iFrame ) override;

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
    Error_t freeMemory () override;
    Error_t allocMemory () override;

private:
    CAudioFileSndLib(const CAudioFileSndLib& that);
    long long int readDataIntern (float **ppfAudioData, long long int iLength) override;
    long long int writeDataIntern (float **ppfAudioData, long long int iLength) override;
    long long getLengthIntern() override;
    long long getPositionIntern() override;
    Error_t setPositionIntern( long long iFrame ) override;

    int m_FileHandle;                           //!< sndlib file handle

    long long m_lFrameCnt;                      //!< current file position in frames
    long long m_lFileLength;                    //!< file length in frames
    double    **m_ppdTmpBuff;                   //!< temporary buffer for double values

};

#endif  //__AudioFile_hdr__
