extern "C" __global__ void markovian_dense_apply(
    int rows,
    int columns,
    const double* matrix,
    const double* input,
    double* output) {
    int column = blockIdx.x * blockDim.x + threadIdx.x;
    if (column >= columns) {
        return;
    }

    double total = 0.0;
    for (int row = 0; row < rows; ++row) {
        total += input[row] * matrix[row * columns + column];
    }
    output[column] = total;
}
