#if !defined(__ACA_ErrorDef_HEADER_INCLUDED__)
#define __ACA_ErrorDef_HEADER_INCLUDED__

enum class Error_t
{
    kNoError,

    kFileOpenError,
    kFileAccessError,

    kFunctionInvalidArgsError,

    kNotInitializedError,
    kFunctionIllegalCallError,
    kInvalidString,

    kMemError,

    kUnknownError,

    kNumErrors
};

#endif // #if !defined(__ACA_ErrorDef_HEADER_INCLUDED__)
