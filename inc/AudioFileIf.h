#if !defined(__AudioFileIf_hdr__)
#define __AudioFileIf_hdr__

#include <string>
#include <fstream>
#include <algorithm>
#include "ErrorDef.h"

/*! \brief open, read, and write audio files
*/
class CAudioFileIf
{
public:
    enum FileIoType_t
    {
        kFileRead   = 0x1L,     //!< open file for reading
        kFileWrite  = 0x2L,     //!< open file for writing

        kNumFileOpenTypes = 2
    };
    enum FileFormat_t
    {
        kFileFormatRaw,         //!< file is raw pcm format
        kFileFormatWav,         //!< file is wav riff format (not available without sndlib)
        kFileFormatAiff,        //!< file is aiff format (not available without sndlib)
        kFileFormatUnknown,     //!< file format is unknown

        kNumFileFormats
    };
    enum BitStream_t
    {
        kFileBitStreamInt16,    //!< a sample is two byte (int16)
        kFileBitStreamFloat32,  //!< a sample is four byte (float32)  (not available without sndlib)
        kFileBitStreamUnknown,  //!< word length is unknown

        kNumWordLengths
    };
    struct FileSpec_t
    {
        FileFormat_t    eFormat;            //!< file format (wav, aiff, raw)
        BitStream_t     eBitStreamType;     //!< word length and sample type
        int             iNumChannels;       //!< number of audio channels
        float           fSampleRateInHz;    //!< sample rate in Hz
    };

    /*! opens a new instance for audio file IO
    \param pCInstance
    \return Error_t
    */
    static Error_t create (CAudioFileIf*& pCInstance);

    /*! destroys and audio file IO instance
    \param pCInstance
    \return Error_t
    */
    static Error_t destroy (CAudioFileIf*& pCInstance);

    /*! reset instance to initial state
    \param  bFreeMemory: also free the internal memory if true
    \return Error_t
    */
    virtual Error_t reset (bool bFreeMemory = false);

    /*! open a file for reading or writing
    \param cAudioFileName
    \param eIoType
    \param psFileSpec
    \return Error_t
    */
    virtual Error_t openFile (std::string cAudioFileName, FileIoType_t eIoType, FileSpec_t const *psFileSpec = 0) = 0;
    
    /*! close the current file
    \return Error_t
    */
    virtual Error_t closeFile () = 0;

    /*! read data from file and increments read position
    \param ppfAudioData: [channels][iNumFrames]
    \param iNumFrames: number of frames to read (per channel), is updated to the number of frames actually read
    \return Error_t
    */
    virtual Error_t readData (float **ppfAudioData, long long int &iNumFrames);
    
    /*! write data to file and increments write position
    \param ppfAudioData: [channels][iNumFrames]
    \param iNumFrames: number of frames to write (per channel)
    \return Error_t
    */
    virtual Error_t writeData (float **ppfAudioData, long long int iNumFrames);

    /*! retrieve file specifications
    \param sFileSpec
    \return Error_t
    */
    Error_t getFileSpec (FileSpec_t &sFileSpec);

    /*! jump to new position in file
    \param iFrame: frame to jump to
    \return Error_t
    */
    virtual Error_t setPosition (long long iFrame = 0);
    /*! jump to new position in file
    \param dTimeInS: time to jump to
    \return Error_t
    */
    Error_t setPosition (double dTimeInS = .0);

    /*! enable clipping to avoid wrap-arounds
    \param bIsEnabled
    \return Error_t
    */
    Error_t setClippingEnabled (bool bIsEnabled = true);
    /*! check if clipping is enabled
    \return bool
    */
    bool isClippingEnabled () {return m_bWithClipping;};

    /*! get current position in file
    \param iFrame: current frame
    \return Error_t
    */
    Error_t getPosition (long long &iFrame);
    /*! get current position in file
    \param dTimeInS: current time in seconds
    \return Error_t
    */
    Error_t getPosition (double &dTimeInS);
    /*! get length of file
    \param iLengthInFrames: file length in frames
    \return Error_t
    */
    Error_t getLength (long long &iLengthInFrames) ;
    /*! get length of file
    \param dLengthInSeconds: file length in seconds
    \return Error_t
    */
    Error_t getLength (double &dLengthInSeconds) ;

    /*! check if EOF is true
    \return bool
    */
    virtual bool isEof () = 0;

    /*! check if a file is opened
    \return bool
    */
    virtual bool isOpen () = 0;

    /*! check is the instance is initialized
    \return bool
    */
    virtual bool isInitialized ();

protected:

    CAudioFileIf ();
    virtual ~CAudioFileIf ();
    virtual Error_t freeMemory ();
    virtual Error_t allocMemory ();

    Error_t setInitialized (bool bInitialized = true);
    Error_t setIoType (FileIoType_t eIoType);
    FileIoType_t getIoType () const;
    Error_t setFileSpec (const FileSpec_t *pFileSpec);
    int getNumChannels () const;
    long long convFrames2Bytes (long long iNumFrames);
    long long convBytes2Frames (long long iNumFrames);
    Error_t setNumBytesPerSample (int iNumBytes);
    int getNumBytesPerSample () const;
    int getNumBitsPerSample () const {return (m_iNumBytesPerSample<<3);}

    float clip2Range(float fSample2Clip, float fMin, float fMax)
    {
        fSample2Clip = std::min (fSample2Clip, fMax);
        fSample2Clip = std::max (fSample2Clip, fMin);
        return fSample2Clip;
    }

    static const long long int    m_kiDefBlockLength;     //!< buffer length for read and write operations

private:
    CAudioFileIf(const CAudioFileIf& that);
    virtual Error_t initDefaults ();

    virtual long long int readDataIntern (float **ppfAudioData, long long int iLength) = 0;
    virtual long long int writeDataIntern (float **ppfAudioData, long long int iLength) = 0;
    virtual long long getLengthIntern() = 0;
    virtual long long getPositionIntern() = 0;
    virtual Error_t setPositionIntern( long long iFrame ) = 0;

    FileSpec_t      m_CurrFileSpec;            //!< current file specifications
    FileIoType_t    m_eIoType;                  //!< read or write

    bool            m_bWithClipping;            //!< true if abs(values ) > 1 should be clipped
    bool            m_bIsInitialized;           //!< true if initialized
    int             m_iNumBytesPerSample;       //!< number of bytes per sample for the raw pcm IO option without sndlib

};

#endif // #if !defined(__AudioFileIf_hdr__)
