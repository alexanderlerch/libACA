#if !defined(__ACA_rvfft_HEADER_INCLUDED__)
#define __ACA_rvfft_HEADER_INCLUDED__

namespace LaszloFft
{
    void realfft_split(float *data,long n);
    void irealfft_split(float *data,long n);
}

#endif //__ACA_rvfft_HEADER_INCLUDED__
