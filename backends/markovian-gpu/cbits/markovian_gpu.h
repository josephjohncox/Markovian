#ifndef MARKOVIAN_GPU_H
#define MARKOVIAN_GPU_H

int markovian_gpu_available(void);

int markovian_gpu_dense_apply(
    int rows,
    int columns,
    const double* matrix,
    const double* input,
    double* output,
    double* transfer_inclusive_milliseconds);

#endif
