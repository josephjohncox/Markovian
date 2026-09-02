/* The host ABI rejects rows*inner, inner*columns, and rows*columns above
 * INT_MAX before launch. Every signed product and sum below is therefore
 * representable in the kernel index type.
 */
extern "C" __global__ void markovian_f64_matmul(
    int rows,
    int inner,
    int columns,
    const double* left,
    const double* right,
    double* output) {
    int linear = blockIdx.x * blockDim.x + threadIdx.x;
    int total = rows * columns;
    if (linear >= total) {
        return;
    }

    int row = linear / columns;
    int column = linear - row * columns;
    double value = 0.0;
    for (int k = 0; k < inner; ++k) {
        value += left[row * inner + k] * right[k * columns + column];
    }
    output[linear] = value;
}
