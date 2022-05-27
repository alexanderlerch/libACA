#if !defined(__NoveltyFromBlock_hdr__)
#define __NoveltyFromBlock_hdr__

#include <map>
#include <functional>

#include "ErrorDef.h"

#include "Novelty.h"

/*! \brief class for computation of a Novelty from a block of data (e.g., time or magnitude spectrum)
*/
class CNoveltyFromBlockIf
{
public:

    /*! initializes a NoveltyFromBlock instance
    \param pCInstance pointer to instance to be written
    \param eNoveltyIdx index of Novelty to extract
    \param iDataLength: block length
    \param fSampleRate: sample rate (only used when needed)
    \return Error_t
    */
    static Error_t create(CNoveltyFromBlockIf*& pCInstance, CNoveltyIf::Novelty_t eNoveltyIdx, int iDataLength, float fSampleRate = 1.F);

    /*! destroys a NoveltyFromBlock instance
    \param pCInstance pointer to instance to be destroyed
    \return Error_t
    */
    static Error_t destroy(CNoveltyFromBlockIf*& pCInstance);

    /*! returns size of output Novelty (1 in all currently implemented cases)
    \return int
    */
    virtual int getNoveltyDimension() const;

    /*! returns index of the Novelty to extract
    \return Novelty_t
    */
    CNoveltyIf::Novelty_t getNoveltyIdx() const
    {
        return m_eNoveltyIdx;
    }

    /*! performs the NoveltyFromBlock computation
    \param pfNovelty Novelty result (user-allocated, to be written, length from CNoveltyFromBlockIf::getNoveltyDimension)
    \param pfInput input data of length iDataLength
    \return Error_t
    */
    virtual Error_t compNovelty(float* pfNovelty, const float* pfInput);


    ////////////////////////////////////////////////////////////////////////////
    // static functions for some Noveltys where it makes sense (use at your own risk)
    static float compNoveltyFlux(const float* pfMagSpec, const float* pfPrevSpec, int iDataLength, float fSampleRate = 1.F);
    static float compNoveltyHainsworth(const float* pfMagSpec, const float* pfPrevSpec, int iDataLength, float fSampleRate = 1.F);
    static float compNoveltyLaroche(const float* pfMagSpec, const float* pfPrevSpec, int iDataLength, float fSampleRate = 1.F);

protected:
    CNoveltyFromBlockIf() {};
    CNoveltyFromBlockIf(CNoveltyIf::Novelty_t eNoveltyIdx, int iDataLength, float fSampleRate);
    virtual ~CNoveltyFromBlockIf();

    CNoveltyFromBlockIf(const CNoveltyFromBlockIf& that);

    CNoveltyIf::Novelty_t m_eNoveltyIdx = CNoveltyIf::kNumNoveltyFunctions;     //!< index of Novelty to extract

    int m_iDataLength = 0;                      //!< block length

    float m_fSampleRate = 0;                    //!< sample rate

    float* m_pfPrevSpec = 0;

    // dispatcher map for static functions without additional arguments
    const std::map<CNoveltyIf::Novelty_t, std::function<float(const float*, const float*, int, float)>> m_DispatchMap
    {
            {CNoveltyIf::kNoveltyFlux, &compNoveltyFlux},
            {CNoveltyIf::kNoveltyHainsworth, &compNoveltyHainsworth},
            {CNoveltyIf::kNoveltyLaroche, &compNoveltyLaroche}
    };
};



#endif // #if !defined(__NoveltyFromBlock_hdr__)



#pragma once
