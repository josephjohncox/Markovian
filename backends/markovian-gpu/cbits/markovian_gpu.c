#include "markovian_gpu.h"

#include "/usr/local/cuda/include/cuda.h"
#include <stddef.h>
#include <time.h>

#include "markovian_dense_ptx.h"

static double elapsed_milliseconds(struct timespec start, struct timespec end) {
    double seconds = (double)(end.tv_sec - start.tv_sec) * 1000.0;
    double nanoseconds = (double)(end.tv_nsec - start.tv_nsec) / 1000000.0;
    return seconds + nanoseconds;
}

int markovian_gpu_available(void) {
    int count = 0;
    if (cuInit(0) != CUDA_SUCCESS) {
        return 0;
    }
    if (cuDeviceGetCount(&count) != CUDA_SUCCESS) {
        return 0;
    }
    return count > 0 ? 1 : 0;
}

int markovian_gpu_dense_apply(
    int rows,
    int columns,
    const double* matrix,
    const double* input,
    double* output,
    double* transfer_inclusive_milliseconds) {
    CUresult result;
    CUdevice device;
    CUcontext context = NULL;
    CUctxCreateParams context_parameters = {0};
    CUmodule module = NULL;
    CUfunction function;
    CUdeviceptr device_matrix = 0;
    CUdeviceptr device_input = 0;
    CUdeviceptr device_output = 0;
    struct timespec start;
    struct timespec end;
    size_t matrix_bytes;
    size_t input_bytes;
    size_t output_bytes;

    if (rows <= 0 || columns <= 0 || matrix == NULL || input == NULL ||
        output == NULL || transfer_inclusive_milliseconds == NULL) {
        return (int)CUDA_ERROR_INVALID_VALUE;
    }

    matrix_bytes = (size_t)rows * (size_t)columns * sizeof(double);
    input_bytes = (size_t)rows * sizeof(double);
    output_bytes = (size_t)columns * sizeof(double);

    timespec_get(&start, TIME_UTC);

    result = cuInit(0);
    if (result != CUDA_SUCCESS) goto cleanup;
    result = cuDeviceGet(&device, 0);
    if (result != CUDA_SUCCESS) goto cleanup;
    result = cuCtxCreate(&context, &context_parameters, 0, device);
    if (result != CUDA_SUCCESS) goto cleanup;
    result = cuModuleLoadData(&module, markovian_dense_ptx);
    if (result != CUDA_SUCCESS) goto cleanup;
    result = cuModuleGetFunction(&function, module, "markovian_dense_apply");
    if (result != CUDA_SUCCESS) goto cleanup;
    result = cuMemAlloc(&device_matrix, matrix_bytes);
    if (result != CUDA_SUCCESS) goto cleanup;
    result = cuMemAlloc(&device_input, input_bytes);
    if (result != CUDA_SUCCESS) goto cleanup;
    result = cuMemAlloc(&device_output, output_bytes);
    if (result != CUDA_SUCCESS) goto cleanup;
    result = cuMemcpyHtoD(device_matrix, matrix, matrix_bytes);
    if (result != CUDA_SUCCESS) goto cleanup;
    result = cuMemcpyHtoD(device_input, input, input_bytes);
    if (result != CUDA_SUCCESS) goto cleanup;

    {
        void* arguments[] = {
            &rows,
            &columns,
            &device_matrix,
            &device_input,
            &device_output
        };
        unsigned int block_size = 128;
        unsigned int grid_size = ((unsigned int)columns + block_size - 1) / block_size;
        result = cuLaunchKernel(
            function,
            grid_size, 1, 1,
            block_size, 1, 1,
            0,
            NULL,
            arguments,
            NULL);
    }
    if (result != CUDA_SUCCESS) goto cleanup;
    result = cuCtxSynchronize();
    if (result != CUDA_SUCCESS) goto cleanup;
    result = cuMemcpyDtoH(output, device_output, output_bytes);

cleanup:
    if (device_output != 0) cuMemFree(device_output);
    if (device_input != 0) cuMemFree(device_input);
    if (device_matrix != 0) cuMemFree(device_matrix);
    if (module != NULL) cuModuleUnload(module);
    if (context != NULL) cuCtxDestroy(context);

    timespec_get(&end, TIME_UTC);
    *transfer_inclusive_milliseconds = elapsed_milliseconds(start, end);
    return (int)result;
}
