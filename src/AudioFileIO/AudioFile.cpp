
#include <cassert>
#include <iostream>

#include "Util.h"
#include "Vector.h"

#include "AudioFile.h"

#ifdef WITH_SNDLIB
#include "sndlib.h"
#endif //WITH_SNDLIB

using namespace std;


//////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////

CAudioFileRaw::CAudioFileRaw(): CAudioFileIf() ,
    m_piTmpBuff(0)
{
    reset (true);
}

CAudioFileRaw::~CAudioFileRaw()
{
    reset (true);
}

Error_t CAudioFileRaw::freeMemory()
{
    CAudioFileIf::freeMemory ();
    if (!m_piTmpBuff)
        return Error_t::kNoError;

    delete [] m_piTmpBuff;
    m_piTmpBuff  = 0;

    return Error_t::kNoError;
}

Error_t CAudioFileRaw::allocMemory()
{
    freeMemory ();

    m_piTmpBuff = new short [static_cast<unsigned int>(m_kiDefBlockLength)];

    if (!m_piTmpBuff)
        return Error_t::kMemError;
    else
        return Error_t::kNoError;
}

Error_t CAudioFileRaw::openFile( std::string cAudioFileName, FileIoType_t eIoType, FileSpec_t const *psFileSpec /*= 0*/ )
{
    if (cAudioFileName.empty())
        return Error_t::kFileOpenError;

    reset (true);
    setIoType(eIoType);

    // set file spec (required for raw streams)
    if (psFileSpec)
    {
        setFileSpec(psFileSpec);
        setInitialized(true);
    }
    // open file
    m_File.open (cAudioFileName.c_str(), ios::binary | ((eIoType & kFileRead)? ios::in : ios::out));

    if (!isOpen())
    {
        reset (true);
        return Error_t::kFileOpenError;
    }

    // allocate internal memory
    return allocMemory ();
}

Error_t CAudioFileRaw::closeFile()
{
    if (!isOpen())
    {
        return Error_t::kNoError;
    }    

    m_File.close ();

    // free internal memory
    return freeMemory ();
}

bool CAudioFileRaw::isEof()
{
    return m_File.eof();
}

bool CAudioFileRaw::isOpen()
{
    return m_File.is_open ();
}

float CAudioFileRaw::scaleUp( float fSample2Clip )
{
    float fScale        = static_cast<float>(1<<(getNumBitsPerSample()-1));
    fSample2Clip *= fScale;
    if (isClippingEnabled())
    {
        fSample2Clip = clip2Range(fSample2Clip, -fScale, fScale-1);
    }
    return fSample2Clip;
}

float CAudioFileRaw::scaleDown( float fSample2Scale )
{
    float fScale        = static_cast<float>(1<<(getNumBitsPerSample()-1));
    return fSample2Scale/fScale;
}

long long int CAudioFileRaw::readDataIntern( float **ppfAudioData, long long int iLength )
{
    int iNumChannels                = getNumChannels();
    long long int iNumFrames2Read   = min (iLength, m_kiDefBlockLength/iNumChannels);
    long long int iNumFrames        = 0;

    // sanity check
    assert(ppfAudioData);
    assert (ppfAudioData[0]);

    // ugly hack
    // a) only for 16 bit input
    // b) only for little endian
    while (iNumFrames2Read > 0)
    {
        long long int iCurrFrames = iNumFrames2Read;
        m_File.read (reinterpret_cast<char*>(m_piTmpBuff), convFrames2Bytes(iNumFrames2Read));

        iNumFrames2Read = min (iLength-iCurrFrames, m_kiDefBlockLength/iNumChannels);

        if (!m_File)
        {
            iCurrFrames     = static_cast<int>(convBytes2Frames(m_File.gcount ()));
            iNumFrames2Read = 0;
        }

        // copy the data
        for (int iCh = 0; iCh < iNumChannels; iCh++)
        {
            for (int i = 0; i < iCurrFrames; i++)
            {
                ppfAudioData[iCh][iNumFrames+i] = scaleDown(static_cast<float> (m_piTmpBuff[i*iNumChannels+iCh]));
            }

        }
        // update frame counters
        iLength        -= iCurrFrames;
        iNumFrames     += iCurrFrames;

        assert (iLength >= 0);
    }

    // update iLength to the number of frames actually read
    return iNumFrames;

}

long long int CAudioFileRaw::writeDataIntern( float **ppfAudioData, long long int iLength )
{
    long long int iIdx  = 0;
    int iNumChannels    = getNumChannels();

    // sanity check
    assert(ppfAudioData);
    assert(ppfAudioData[0]);

    // very ugly hack
    // a) only for 16 bit output
    // b) disregarded endianess
    while (iIdx < iLength)
    {
        long long int iNumFrames2Write = min (iLength-iIdx, m_kiDefBlockLength/iNumChannels);

        // copy the data
        for (int iCh = 0; iCh < iNumChannels; iCh++)
        {
            for (int i = 0; i < iNumFrames2Write; i++)
            {
                m_piTmpBuff[i*iNumChannels+iCh] = CUtil::float2int<short>(scaleUp(ppfAudioData[iCh][iIdx+i]));
            }
        }

        m_File.write (reinterpret_cast<char*>(m_piTmpBuff), convFrames2Bytes(iNumFrames2Write));

        if (!m_File)
        {
            break;
        }

        // update frame counter
        iIdx   += iNumFrames2Write;
    }
    return iIdx;
}

long long CAudioFileRaw::getLengthIntern() 
{
    assert(m_File);

    //static const int    iNumOfBytesPerSample = 2;
    long long iCurrPos  = getPositionIntern();
    long long iLength   = 0;

    m_File.seekg (0, m_File.end);

    iLength = convBytes2Frames(static_cast<long long>(m_File.tellg()));

    setPositionIntern(iCurrPos);

    return iLength;
}

long long CAudioFileRaw::getPositionIntern()
{
    assert(m_File);
    return convBytes2Frames(static_cast<long long>(m_File.tellg()));
}

Error_t CAudioFileRaw::setPositionIntern( long long iFrame )
{
    assert(m_File);
    assert(iFrame >= 0);

    m_File.seekg (convFrames2Bytes(iFrame), m_File.beg);

    return Error_t::kNoError;
}

//////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////
#ifdef WITH_SNDLIB
CAudioFileSndLib::CAudioFileSndLib(): CAudioFileIf() ,
    m_FileHandle(-1),
    m_lFrameCnt(0),
    m_lFileLength(0),
    m_ppdTmpBuff(0)
{
    reset (true);
}

CAudioFileSndLib::~CAudioFileSndLib()
{
    reset (true);
}


Error_t CAudioFileSndLib::openFile( std::string cAudioFileName, FileIoType_t eIoType, FileSpec_t const *psFileSpec /*= 0*/ )
{
    FileSpec_t  sSpec; 
    int iSndLibFileFormat = MUS_RIFF;

    if (cAudioFileName.empty())
        return Error_t::kFileOpenError;

    reset (true);
    setIoType(eIoType);

    if (!psFileSpec)
    {
        getFileSpec(sSpec);
    }
    else
    {
        CVector::copy(&sSpec, psFileSpec, 1);
    }

    switch (sSpec.eFormat)
    {
    case kFileFormatWav:
        iSndLibFileFormat = MUS_RIFF;
        break;
    case kFileFormatRaw:
        iSndLibFileFormat = MUS_RAW;
        break;
    case kFileFormatAiff:
        iSndLibFileFormat = MUS_AIFF;
        break;
    default:
        return Error_t::kFunctionInvalidArgsError;
    }

    // set file spec (required for raw streams)
    setFileSpec(&sSpec);

    if (getIoType() == kFileRead)
    {
        if (mus_sound_header_type(cAudioFileName.c_str()) == MUS_RAW)
        {
            mus_sound_set_chans(cAudioFileName.c_str(), sSpec.iNumChannels);
            mus_sound_set_srate(cAudioFileName.c_str(), CUtil::float2int<int>(sSpec.fSampleRateInHz));
            mus_sound_set_header_type(cAudioFileName.c_str(),  iSndLibFileFormat);
            mus_sound_set_data_format(cAudioFileName.c_str(), (sSpec.eBitStreamType == kFileBitStreamInt16)?MUS_LSHORT:MUS_LFLOAT);
        }
        else
        {
            FileSpec_t  sFileSpec;
            int iTmp = -1;
            sFileSpec.fSampleRateInHz   = static_cast<float>(mus_sound_srate(cAudioFileName.c_str()));
            sFileSpec.iNumChannels      = mus_sound_chans(cAudioFileName.c_str());
            iTmp                        = mus_sound_header_type(cAudioFileName.c_str());
            switch (iTmp)
            {
            case MUS_RIFF:
                sFileSpec.eFormat = kFileFormatWav;
                break;
            case MUS_AIFF:
                sFileSpec.eFormat = kFileFormatAiff;
                break;
            default:
                sFileSpec.eFormat = kFileFormatUnknown;
                break;
            }
            iTmp    = mus_sound_data_format(cAudioFileName.c_str());
            switch (iTmp)
            {
            case MUS_LSHORT:
            case MUS_BSHORT:
                sFileSpec.eBitStreamType = kFileBitStreamInt16;
                setNumBytesPerSample(2);
                break;
            case MUS_BFLOAT:
            case MUS_LFLOAT:
                sFileSpec.eBitStreamType = kFileBitStreamFloat32;
                setNumBytesPerSample(4);
                break;
            default:
                sFileSpec.eBitStreamType = kFileBitStreamUnknown;
                break;
            }
            setFileSpec(&sFileSpec);
        }

        m_FileHandle = mus_sound_open_input(cAudioFileName.c_str());
    }
    else
    {
        m_FileHandle = mus_sound_open_output(cAudioFileName.c_str(), 
            CUtil::float2int<int>(sSpec.fSampleRateInHz),
            sSpec.iNumChannels,
            (sSpec.eBitStreamType == kFileBitStreamInt16)?MUS_LSHORT:MUS_LFLOAT, // only write little endian files
            iSndLibFileFormat,
            0);
    }

    if (!isOpen())
    {
        reset (true);
        return Error_t::kFileOpenError;
    }

    m_lFileLength = mus_sound_frames(cAudioFileName.c_str());
    setInitialized(true);

    // allocate internal memory
    return allocMemory ();
}

Error_t CAudioFileSndLib::closeFile()
{
    if (!isOpen())
    {
        return Error_t::kNoError;
    }    

    if (getIoType() == kFileRead)
    {
        mus_sound_close_input(m_FileHandle);
    }
    else
    {        
        mus_sound_close_output(m_FileHandle, convFrames2Bytes(m_lFrameCnt));
    }

    m_lFrameCnt     = 0;
    m_FileHandle    = -1;
    m_lFileLength   = 0;

    // free internal memory
    return freeMemory ();
}

bool CAudioFileSndLib::isOpen()
{
    return (m_FileHandle >= 0);
}

long long int CAudioFileSndLib::readDataIntern( float **ppfAudioData, long long int iLength )
{
    int iNumChannels                = getNumChannels();
    long long int iNumFrames2Read   = min (iLength, m_kiDefBlockLength);
    long long int iNumFrames        = 0;

    // sanity check
    assert(ppfAudioData);
    assert(ppfAudioData[0]);

    // use internal buffer with fixed length
    while (iNumFrames2Read > 0)
    {
        long long int iCurrFrames = mus_sound_read(m_FileHandle, 0, iNumFrames2Read-1, getNumChannels(), m_ppdTmpBuff);

        if (iCurrFrames <= m_lFileLength-m_lFrameCnt)
        {
            iNumFrames2Read = std::min (iLength-iCurrFrames, m_kiDefBlockLength);
        }
        else
        {
            iCurrFrames     = static_cast<int>(m_lFileLength-m_lFrameCnt);
            iNumFrames2Read = 0;
        } 

        // copy the data
        for (int iCh = 0; iCh < iNumChannels; iCh++)
        {
            for (int i = 0; i < iCurrFrames; i++)
            {
                ppfAudioData[iCh][iNumFrames+i] = static_cast<float>(m_ppdTmpBuff[iCh][i]);
            }

        }
        // update frame counters
        iLength        -= iCurrFrames;
        iNumFrames     += iCurrFrames;
        m_lFrameCnt    += iCurrFrames;

        assert (iLength >= 0);
    }

    // update iLength to the number of frames actually read
    return iNumFrames;
}

long long int CAudioFileSndLib::writeDataIntern( float **ppfAudioData, long long int iLength )
{
    long long int iIdx            = 0;
    long long int iNumChannels    = getNumChannels();

    // sanity check
    assert(ppfAudioData);
    assert(ppfAudioData[0]);

    float afRange[2] = {std::numeric_limits<float>::min(), std::numeric_limits<float>::max()};
    if (isClippingEnabled())
    {
        afRange[0]  = -1.F;
        afRange[1]  = 1-1.F/static_cast<float>(1<<(getNumBitsPerSample()));
    }

    // use internal buffer with fixed length
    while (iIdx < iLength)
    {
        long long int iNumFrames2Write = min (iLength-iIdx, m_kiDefBlockLength);

        // copy the data
        for (int iCh = 0; iCh < iNumChannels; iCh++)
        {
            for (int i = 0; i < iNumFrames2Write; i++)
            {
                m_ppdTmpBuff[iCh][i]    = clip2Range(ppfAudioData[iCh][iIdx+i], afRange[0], afRange[1]);
            }
        }

        if (mus_sound_write(m_FileHandle, 0, iNumFrames2Write-1, getNumChannels(), m_ppdTmpBuff) == 0)
        {
            // update frame counter
            iIdx        += iNumFrames2Write;
            m_lFrameCnt += iNumFrames2Write;
        }
    }
    
    return iIdx;
}

long long CAudioFileSndLib::getLengthIntern()
{
    if (getIoType() == kFileWrite)
    {
        return m_lFrameCnt;
    }
    else
    {
        return m_lFileLength;
    }
}

long long CAudioFileSndLib::getPositionIntern()
{
    return m_lFrameCnt;
}

Error_t CAudioFileSndLib::setPositionIntern( long long iFrame )
{
    m_lFrameCnt = convBytes2Frames(mus_sound_seek_frame(m_FileHandle, iFrame));
    
    return Error_t::kNoError;
}

Error_t CAudioFileSndLib::freeMemory()
{
    CAudioFileIf::freeMemory ();
    if (!m_ppdTmpBuff)
        return Error_t::kNoError;

    for (int i = 0; i < getNumChannels(); i++)
        delete m_ppdTmpBuff[i];

    delete [] m_ppdTmpBuff;
    m_ppdTmpBuff  = 0;

    return Error_t::kNoError;
}

Error_t CAudioFileSndLib::allocMemory()
{
    int iNumChannels = getNumChannels();
    freeMemory ();

    m_ppdTmpBuff    = new double* [iNumChannels];
    for (int i = 0; i < iNumChannels; i++)
        m_ppdTmpBuff[i] = new double [static_cast<unsigned int>(m_kiDefBlockLength)];

    if (!m_ppdTmpBuff)
        return Error_t::kMemError;
    else
        return Error_t::kNoError;
}

bool CAudioFileSndLib::isEof()
{
    return (m_lFrameCnt >= m_lFileLength);
}

#endif //WITH_SNDLIB
