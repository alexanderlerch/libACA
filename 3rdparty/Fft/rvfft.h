#if !defined(__rvfft_hdr__)
#define __rvfft_hdr__

namespace LaszloFft
{
    void realfft_split(float *data,long n);
    void irealfft_split(float *data,long n);
}

#endif //__rvfft_hdr__
