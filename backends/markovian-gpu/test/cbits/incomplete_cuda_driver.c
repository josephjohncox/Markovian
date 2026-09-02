#include <stdio.h>
#include <stdlib.h>

#ifndef OMIT_CUINIT
int cuInit(unsigned int flags) {
    const char* marker = getenv("MARKOVIAN_CUDA_INIT_MARKER");
    (void)flags;
    if (marker != NULL && *marker != '\0') {
        FILE* output = fopen(marker, "ab");
        if (output != NULL) {
            (void)fputs("cuInit called\n", output);
            (void)fclose(output);
        }
    }
    return 0;
}
#endif

#define STUB(name) int name(void) { return 0; }
STUB(cuDriverGetVersion)
STUB(cuDeviceGetCount)
STUB(cuDeviceGet)
STUB(cuDeviceGetAttribute)
STUB(cuDeviceTotalMem_v2)
STUB(cuDeviceGetUuid_v2)
STUB(cuDeviceGetName)
STUB(cuCtxCreate_v4)
STUB(cuCtxDestroy_v2)
STUB(cuCtxPushCurrent_v2)
STUB(cuCtxPopCurrent_v2)
STUB(cuModuleLoadData)
STUB(cuModuleUnload)
STUB(cuModuleGetFunction)
STUB(cuStreamCreate)
STUB(cuStreamDestroy_v2)
STUB(cuStreamSynchronize)
STUB(cuMemAlloc_v2)
STUB(cuMemFree_v2)
STUB(cuMemcpyHtoD_v2)
STUB(cuMemcpyDtoH_v2)
#ifndef OMIT_CULAUNCHKERNEL
STUB(cuLaunchKernel)
#endif
STUB(cuGetErrorName)
STUB(cuGetErrorString)
