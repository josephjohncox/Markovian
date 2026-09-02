#ifndef MARKOVIAN_GPU_H
#define MARKOVIAN_GPU_H

#include <stddef.h>

#define MARKOVIAN_CUDA_STATUS_INTS 20
#define MARKOVIAN_CUDA_MAX_CLEANUP_FAILURES 8
#define MARKOVIAN_CUDA_UUID_BYTES 16
#define MARKOVIAN_CUDA_NAME_BYTES 128

typedef struct markovian_cuda_executor markovian_cuda_executor;

int markovian_cuda_device_count(
    int* driver_version,
    int* device_count,
    int* failure_stage);

int markovian_cuda_probe_device(
    int ordinal,
    int* major,
    int* minor,
    size_t* total_memory,
    int* maximum_threads_per_block,
    unsigned char uuid[MARKOVIAN_CUDA_UUID_BYTES],
    char name[MARKOVIAN_CUDA_NAME_BYTES],
    int* failure_stage);

void markovian_cuda_executor_create(
    int ordinal,
    const unsigned char expected_uuid[MARKOVIAN_CUDA_UUID_BYTES],
    unsigned char verified_uuid[MARKOVIAN_CUDA_UUID_BYTES],
    markovian_cuda_executor** executor,
    int status[MARKOVIAN_CUDA_STATUS_INTS]);

void markovian_cuda_executor_destroy(
    markovian_cuda_executor* executor,
    int status[MARKOVIAN_CUDA_STATUS_INTS]);

void markovian_cuda_executor_matmul(
    markovian_cuda_executor* executor,
    int rows,
    int inner,
    int columns,
    const double* left,
    const double* right,
    double* output,
    double* transfer_inclusive_milliseconds,
    int status[MARKOVIAN_CUDA_STATUS_INTS]);

const char* markovian_cuda_error_name(int code);
const char* markovian_cuda_error_string(int code);

#endif
