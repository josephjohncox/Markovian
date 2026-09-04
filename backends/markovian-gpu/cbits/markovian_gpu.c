#define _POSIX_C_SOURCE 200809L

#include "markovian_gpu.h"
#include "markovian_cuda_profile.h"

#include <cuda.h>
#include <dlfcn.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "markovian_dense_ptx.h"

#if CUDA_VERSION != MARKOVIAN_CUDA_HEADER_VERSION
#error "markovian-gpu requires the profile-authority CUDA headers"
#endif

_Static_assert(CHAR_BIT == 8, "the bridge requires eight-bit bytes");
_Static_assert(sizeof(int) == 4, "Haskell CInt status elements require 32-bit C int");
_Static_assert(sizeof(double) == 8, "Haskell CDouble payloads require binary64 width");
_Static_assert(sizeof(void*) == sizeof(CUcontext), "CUcontext pointer width changed");
_Static_assert(sizeof(void*) == sizeof(CUmodule), "CUmodule pointer width changed");
_Static_assert(sizeof(void*) == sizeof(CUfunction), "CUfunction pointer width changed");
_Static_assert(sizeof(void*) == sizeof(CUstream), "CUstream pointer width changed");
_Static_assert(sizeof(CUdeviceptr) == sizeof(uint64_t), "CUdeviceptr width changed");
_Static_assert(sizeof(CUuuid) == MARKOVIAN_CUDA_UUID_BYTES, "CUuuid layout changed");
_Static_assert(MARKOVIAN_CUDA_NAME_BYTES == 128, "Haskell name buffer boundary changed");
_Static_assert(MARKOVIAN_CUDA_STATUS_INTS == 20, "Haskell status buffer boundary changed");
_Static_assert(4 + 2 * MARKOVIAN_CUDA_MAX_CLEANUP_FAILURES == MARKOVIAN_CUDA_STATUS_INTS,
               "cleanup diagnostics do not fit the status buffer");

#define MARKOVIAN_CUDA_ERROR_LIBRARY_UNAVAILABLE (-70001)
#define MARKOVIAN_CUDA_ERROR_SYMBOL_UNAVAILABLE (-70002)
#define MARKOVIAN_CUDA_ERROR_DRIVER_UNLOAD (-70003)
#define MARKOVIAN_CUDA_ERROR_UNSUPPORTED_DEVICE (-70004)
#define MARKOVIAN_CUDA_ERROR_EXECUTOR_POISONED (-70005)
#define MARKOVIAN_CUDA_ERROR_DEVICE_IDENTITY (-70006)

enum markovian_cuda_stage {
    MARKOVIAN_STAGE_NONE = 0,
    MARKOVIAN_STAGE_INITIALIZE = 1,
    MARKOVIAN_STAGE_DEVICE_COUNT = 2,
    MARKOVIAN_STAGE_DEVICE_SELECT = 3,
    MARKOVIAN_STAGE_CONTEXT_CREATE = 4,
    MARKOVIAN_STAGE_CONTEXT_PUSH = 5,
    MARKOVIAN_STAGE_MODULE_LOAD = 6,
    MARKOVIAN_STAGE_FUNCTION_LOOKUP = 7,
    MARKOVIAN_STAGE_STREAM_CREATE = 8,
    MARKOVIAN_STAGE_SELF_TEST = 9,
    MARKOVIAN_STAGE_ALLOCATE_LEFT = 10,
    MARKOVIAN_STAGE_ALLOCATE_RIGHT = 11,
    MARKOVIAN_STAGE_ALLOCATE_OUTPUT = 12,
    MARKOVIAN_STAGE_COPY_LEFT = 13,
    MARKOVIAN_STAGE_COPY_RIGHT = 14,
    MARKOVIAN_STAGE_LAUNCH = 15,
    MARKOVIAN_STAGE_SYNCHRONIZE = 16,
    MARKOVIAN_STAGE_COPY_OUTPUT = 17,
    MARKOVIAN_STAGE_FREE_OUTPUT = 18,
    MARKOVIAN_STAGE_FREE_RIGHT = 19,
    MARKOVIAN_STAGE_FREE_LEFT = 20,
    MARKOVIAN_STAGE_STREAM_DESTROY = 21,
    MARKOVIAN_STAGE_MODULE_UNLOAD = 22,
    MARKOVIAN_STAGE_CONTEXT_POP = 23,
    MARKOVIAN_STAGE_CONTEXT_DESTROY = 24,
    MARKOVIAN_STAGE_HOST_ARGUMENT = 25,
    MARKOVIAN_STAGE_DRIVER_LOAD = 26,
    MARKOVIAN_STAGE_SYMBOL_RESOLVE = 27,
    MARKOVIAN_STAGE_DRIVER_UNLOAD = 28,
    MARKOVIAN_STAGE_DEVICE_COMPATIBILITY = 29,
    MARKOVIAN_STAGE_EXECUTOR_POISONED = 30
};

typedef CUresult (CUDAAPI *markovian_cu_init_fn)(unsigned int);
typedef CUresult (CUDAAPI *markovian_cu_driver_get_version_fn)(int*);
typedef CUresult (CUDAAPI *markovian_cu_device_get_count_fn)(int*);
typedef CUresult (CUDAAPI *markovian_cu_device_get_fn)(CUdevice*, int);
typedef CUresult (CUDAAPI *markovian_cu_device_get_attribute_fn)(int*, CUdevice_attribute, CUdevice);
typedef CUresult (CUDAAPI *markovian_cu_device_total_mem_fn)(size_t*, CUdevice);
typedef CUresult (CUDAAPI *markovian_cu_device_get_uuid_fn)(CUuuid*, CUdevice);
typedef CUresult (CUDAAPI *markovian_cu_device_get_name_fn)(char*, int, CUdevice);
typedef CUresult (CUDAAPI *markovian_cu_ctx_create_fn)(CUcontext*, CUctxCreateParams*, unsigned int, CUdevice);
typedef CUresult (CUDAAPI *markovian_cu_ctx_destroy_fn)(CUcontext);
typedef CUresult (CUDAAPI *markovian_cu_ctx_push_current_fn)(CUcontext);
typedef CUresult (CUDAAPI *markovian_cu_ctx_pop_current_fn)(CUcontext*);
typedef CUresult (CUDAAPI *markovian_cu_module_load_data_fn)(CUmodule*, const void*);
typedef CUresult (CUDAAPI *markovian_cu_module_unload_fn)(CUmodule);
typedef CUresult (CUDAAPI *markovian_cu_module_get_function_fn)(CUfunction*, CUmodule, const char*);
typedef CUresult (CUDAAPI *markovian_cu_stream_create_fn)(CUstream*, unsigned int);
typedef CUresult (CUDAAPI *markovian_cu_stream_destroy_fn)(CUstream);
typedef CUresult (CUDAAPI *markovian_cu_stream_synchronize_fn)(CUstream);
typedef CUresult (CUDAAPI *markovian_cu_mem_alloc_fn)(CUdeviceptr*, size_t);
typedef CUresult (CUDAAPI *markovian_cu_mem_free_fn)(CUdeviceptr);
typedef CUresult (CUDAAPI *markovian_cu_memcpy_htod_fn)(CUdeviceptr, const void*, size_t);
typedef CUresult (CUDAAPI *markovian_cu_memcpy_dtoh_fn)(void*, CUdeviceptr, size_t);
typedef CUresult (CUDAAPI *markovian_cu_launch_kernel_fn)(
    CUfunction,
    unsigned int, unsigned int, unsigned int,
    unsigned int, unsigned int, unsigned int,
    unsigned int, CUstream, void**, void**);
typedef CUresult (CUDAAPI *markovian_cu_get_error_name_fn)(CUresult, const char**);
typedef CUresult (CUDAAPI *markovian_cu_get_error_string_fn)(CUresult, const char**);

_Static_assert(sizeof(markovian_cu_init_fn) == sizeof(void*),
               "POSIX dlsym and CUDA function-pointer widths differ");

struct markovian_cuda_driver {
    void* handle;
    markovian_cu_init_fn init;
    markovian_cu_driver_get_version_fn driver_get_version;
    markovian_cu_device_get_count_fn device_get_count;
    markovian_cu_device_get_fn device_get;
    markovian_cu_device_get_attribute_fn device_get_attribute;
    markovian_cu_device_total_mem_fn device_total_mem;
    markovian_cu_device_get_uuid_fn device_get_uuid;
    markovian_cu_device_get_name_fn device_get_name;
    markovian_cu_ctx_create_fn ctx_create;
    markovian_cu_ctx_destroy_fn ctx_destroy;
    markovian_cu_ctx_push_current_fn ctx_push_current;
    markovian_cu_ctx_pop_current_fn ctx_pop_current;
    markovian_cu_module_load_data_fn module_load_data;
    markovian_cu_module_unload_fn module_unload;
    markovian_cu_module_get_function_fn module_get_function;
    markovian_cu_stream_create_fn stream_create;
    markovian_cu_stream_destroy_fn stream_destroy;
    markovian_cu_stream_synchronize_fn stream_synchronize;
    markovian_cu_mem_alloc_fn mem_alloc;
    markovian_cu_mem_free_fn mem_free;
    markovian_cu_memcpy_htod_fn memcpy_htod;
    markovian_cu_memcpy_dtoh_fn memcpy_dtoh;
    markovian_cu_launch_kernel_fn launch_kernel;
    markovian_cu_get_error_name_fn get_error_name;
    markovian_cu_get_error_string_fn get_error_string;
};

struct markovian_cuda_executor {
    struct markovian_cuda_driver driver;
    CUdevice device;
    CUcontext context;
    CUmodule module;
    CUfunction matmul;
    CUstream stream;
    int poisoned;
    int retained_count;
    CUdeviceptr retained_allocations[3];
    int retained_stages[3];
#ifdef MARKOVIAN_CUDA_FAULT_INJECTION
    int fault_count;
    int fault_stage[MARKOVIAN_CUDA_MAX_CLEANUP_FAILURES];
    int fault_occurrence[MARKOVIAN_CUDA_MAX_CLEANUP_FAILURES];
    int fault_seen[MARKOVIAN_CUDA_MAX_CLEANUP_FAILURES];
#endif
};

_Static_assert(sizeof(markovian_cuda_executor*) == sizeof(void*),
               "Haskell opaque executor pointer width changed");

static const char* driver_library_name(void) {
#ifdef MARKOVIAN_CUDA_FAULT_INJECTION
    const char* override = getenv("MARKOVIAN_CUDA_DRIVER_LIBRARY");
    if (override != NULL && *override != '\0') return override;
#endif
    return MARKOVIAN_CUDA_DRIVER_LIBRARY;
}

static int unsupported_device_fixture_enabled(void) {
#ifdef MARKOVIAN_CUDA_FAULT_INJECTION
    const char* enabled = getenv("MARKOVIAN_CUDA_FAULT_UNSUPPORTED_DEVICE");
    return enabled != NULL && strcmp(enabled, "1") == 0;
#else
    return 0;
#endif
}

static int symbol_is_faulted(const char* name) {
#ifdef MARKOVIAN_CUDA_FAULT_INJECTION
    const char* faulted = getenv("MARKOVIAN_CUDA_FAULT_SYMBOL");
    return faulted != NULL && strcmp(faulted, name) == 0;
#else
    (void)name;
    return 0;
#endif
}

static int load_symbol(void* handle, const char* name, void* destination, size_t destination_size) {
    void* symbol;
    if (destination_size != sizeof(symbol) || symbol_is_faulted(name)) return 0;
    dlerror();
    symbol = dlsym(handle, name);
    if (symbol == NULL || dlerror() != NULL) return 0;
    memcpy(destination, &symbol, destination_size);
    return 1;
}

#define LOAD_SYMBOL(driver, field, symbol_name) \
    do { \
        if (!load_symbol((driver)->handle, (symbol_name), &(driver)->field, sizeof((driver)->field))) { \
            goto missing_symbol; \
        } \
    } while (0)

static int driver_open(struct markovian_cuda_driver* driver, int* stage) {
    memset(driver, 0, sizeof(*driver));
    *stage = MARKOVIAN_STAGE_DRIVER_LOAD;
#ifdef MARKOVIAN_CUDA_FAULT_INJECTION
    if (strcmp(driver_library_name(), "@self") == 0) {
        driver->handle = dlopen(NULL, RTLD_NOW | RTLD_LOCAL);
    } else {
        driver->handle = dlopen(driver_library_name(), RTLD_NOW | RTLD_LOCAL);
    }
#else
    driver->handle = dlopen(driver_library_name(), RTLD_NOW | RTLD_LOCAL);
#endif
    if (driver->handle == NULL) return MARKOVIAN_CUDA_ERROR_LIBRARY_UNAVAILABLE;

    *stage = MARKOVIAN_STAGE_SYMBOL_RESOLVE;
    LOAD_SYMBOL(driver, init, "cuInit");
    LOAD_SYMBOL(driver, driver_get_version, "cuDriverGetVersion");
    LOAD_SYMBOL(driver, device_get_count, "cuDeviceGetCount");
    LOAD_SYMBOL(driver, device_get, "cuDeviceGet");
    LOAD_SYMBOL(driver, device_get_attribute, "cuDeviceGetAttribute");
    LOAD_SYMBOL(driver, device_total_mem, "cuDeviceTotalMem_v2");
    LOAD_SYMBOL(driver, device_get_uuid, "cuDeviceGetUuid_v2");
    LOAD_SYMBOL(driver, device_get_name, "cuDeviceGetName");
    LOAD_SYMBOL(driver, ctx_create, "cuCtxCreate_v4");
    LOAD_SYMBOL(driver, ctx_destroy, "cuCtxDestroy_v2");
    LOAD_SYMBOL(driver, ctx_push_current, "cuCtxPushCurrent_v2");
    LOAD_SYMBOL(driver, ctx_pop_current, "cuCtxPopCurrent_v2");
    LOAD_SYMBOL(driver, module_load_data, "cuModuleLoadData");
    LOAD_SYMBOL(driver, module_unload, "cuModuleUnload");
    LOAD_SYMBOL(driver, module_get_function, "cuModuleGetFunction");
    LOAD_SYMBOL(driver, stream_create, "cuStreamCreate");
    LOAD_SYMBOL(driver, stream_destroy, "cuStreamDestroy_v2");
    LOAD_SYMBOL(driver, stream_synchronize, "cuStreamSynchronize");
    LOAD_SYMBOL(driver, mem_alloc, "cuMemAlloc_v2");
    LOAD_SYMBOL(driver, mem_free, "cuMemFree_v2");
    LOAD_SYMBOL(driver, memcpy_htod, "cuMemcpyHtoD_v2");
    LOAD_SYMBOL(driver, memcpy_dtoh, "cuMemcpyDtoH_v2");
    LOAD_SYMBOL(driver, launch_kernel, "cuLaunchKernel");
    LOAD_SYMBOL(driver, get_error_name, "cuGetErrorName");
    LOAD_SYMBOL(driver, get_error_string, "cuGetErrorString");
    return 0;

missing_symbol:
    (void)dlclose(driver->handle);
    memset(driver, 0, sizeof(*driver));
    return MARKOVIAN_CUDA_ERROR_SYMBOL_UNAVAILABLE;
}

#undef LOAD_SYMBOL

static int driver_close(struct markovian_cuda_driver* driver) {
    int result = 0;
    if (driver->handle != NULL && dlclose(driver->handle) != 0) {
        result = MARKOVIAN_CUDA_ERROR_DRIVER_UNLOAD;
    }
    memset(driver, 0, sizeof(*driver));
    return result;
}

#ifdef MARKOVIAN_CUDA_FAULT_INJECTION
/* Test-only syntax: MARKOVIAN_CUDA_FAULTS="stage:occurrence,...". */
static void load_faults(markovian_cuda_executor* executor) {
    const char* cursor = getenv("MARKOVIAN_CUDA_FAULTS");
    while (cursor != NULL && *cursor != '\0' &&
           executor->fault_count < MARKOVIAN_CUDA_MAX_CLEANUP_FAILURES) {
        char* end = NULL;
        long stage = strtol(cursor, &end, 10);
        long occurrence;
        if (end == cursor || *end != ':') break;
        cursor = end + 1;
        occurrence = strtol(cursor, &end, 10);
        if (end == cursor || stage <= 0 || stage > INT_MAX ||
            occurrence <= 0 || occurrence > INT_MAX) break;
        executor->fault_stage[executor->fault_count] = (int)stage;
        executor->fault_occurrence[executor->fault_count] = (int)occurrence;
        executor->fault_count += 1;
        if (*end == ',') cursor = end + 1;
        else if (*end == '\0') break;
        else break;
    }
}

static int fault_now(markovian_cuda_executor* executor, int stage) {
    int index;
    for (index = 0; index < executor->fault_count; ++index) {
        if (executor->fault_stage[index] == stage) {
            executor->fault_seen[index] += 1;
            if (executor->fault_seen[index] == executor->fault_occurrence[index]) return 1;
        }
    }
    return 0;
}
#else
static void load_faults(markovian_cuda_executor* executor) { (void)executor; }
static int fault_now(markovian_cuda_executor* executor, int stage) {
    (void)executor;
    (void)stage;
    return 0;
}
#endif

static void status_reset(int* status) {
    int index;
    for (index = 0; index < MARKOVIAN_CUDA_STATUS_INTS; ++index) status[index] = 0;
}

static void status_primary(int* status, int stage, int code) {
    if (status[0] == 0) {
        status[0] = code;
        status[1] = stage;
    }
}

static void status_cleanup(int* status, int stage, int code) {
    int count;
    int offset;
    if (code == 0) return;
    count = status[3];
    if (count >= MARKOVIAN_CUDA_MAX_CLEANUP_FAILURES) return;
    offset = 4 + count * 2;
    status[offset] = stage;
    status[offset + 1] = code;
    status[3] = count + 1;
}

static void retain_allocation(markovian_cuda_executor* executor, CUdeviceptr pointer, int stage) {
    int index;
    if (pointer == 0) return;
    for (index = 0; index < executor->retained_count; ++index) {
        if (executor->retained_allocations[index] == pointer) return;
    }
    if (executor->retained_count < 3) {
        executor->retained_allocations[executor->retained_count] = pointer;
        executor->retained_stages[executor->retained_count] = stage;
        executor->retained_count += 1;
    }
    executor->poisoned = 1;
}

static CUresult free_or_retain(markovian_cuda_executor* executor, CUdeviceptr pointer, int stage) {
    CUresult result;
    if (pointer == 0) return CUDA_SUCCESS;
    result = fault_now(executor, stage) ? CUDA_ERROR_UNKNOWN : executor->driver.mem_free(pointer);
    if (result != CUDA_SUCCESS) retain_allocation(executor, pointer, stage);
    return result;
}

static void retry_retained_allocations(markovian_cuda_executor* executor, int* status) {
    int source;
    int retained = 0;
    for (source = 0; source < executor->retained_count; ++source) {
        CUdeviceptr pointer = executor->retained_allocations[source];
        int stage = executor->retained_stages[source];
        CUresult result = fault_now(executor, stage) ? CUDA_ERROR_UNKNOWN : executor->driver.mem_free(pointer);
        status_cleanup(status, stage, (int)result);
        if (result != CUDA_SUCCESS) {
            executor->retained_allocations[retained] = pointer;
            executor->retained_stages[retained] = stage;
            retained += 1;
        }
    }
    executor->retained_count = retained;
    executor->poisoned = retained != 0;
}

static int checked_index_product(size_t first, size_t second) {
    if (first != 0 && second > SIZE_MAX / first) return 0;
    return first * second <= (size_t)INT_MAX;
}

static int checked_bytes(size_t first, size_t second, size_t* bytes) {
    size_t elements;
    if (first != 0 && second > SIZE_MAX / first) return 0;
    elements = first * second;
    if (elements > SIZE_MAX / sizeof(double)) return 0;
    *bytes = elements * sizeof(double);
    return 1;
}

static double elapsed_milliseconds(struct timespec start, struct timespec end) {
    double seconds = (double)(end.tv_sec - start.tv_sec) * 1000.0;
    double nanoseconds = (double)(end.tv_nsec - start.tv_nsec) / 1000000.0;
    return seconds + nanoseconds;
}

static CUresult launch_matmul(
    markovian_cuda_executor* executor,
    int rows,
    int inner,
    int columns,
    CUdeviceptr left,
    CUdeviceptr right,
    CUdeviceptr output) {
    unsigned int block_size = MARKOVIAN_DENSE_BLOCK_THREADS;
    unsigned int output_elements = (unsigned int)rows * (unsigned int)columns;
    unsigned int grid_size = (output_elements + block_size - 1U) / block_size;
    void* arguments[] = {&rows, &inner, &columns, &left, &right, &output};
    return executor->driver.launch_kernel(
        executor->matmul,
        grid_size, 1, 1,
        block_size, 1, 1,
        0,
        executor->stream,
        arguments,
        NULL);
}

static CUresult self_test(markovian_cuda_executor* executor) {
    const double left_host[2] = {2.0, -1.0};
    const double right_host[2] = {3.0, 4.0};
    double output_host = 0.0;
    CUdeviceptr left = 0;
    CUdeviceptr right = 0;
    CUdeviceptr output = 0;
    CUresult result = CUDA_SUCCESS;
    CUresult cleanup;

    result = executor->driver.mem_alloc(&left, sizeof(left_host));
    if (result != CUDA_SUCCESS) goto cleanup;
    result = executor->driver.mem_alloc(&right, sizeof(right_host));
    if (result != CUDA_SUCCESS) goto cleanup;
    result = executor->driver.mem_alloc(&output, sizeof(output_host));
    if (result != CUDA_SUCCESS) goto cleanup;
    result = executor->driver.memcpy_htod(left, left_host, sizeof(left_host));
    if (result != CUDA_SUCCESS) goto cleanup;
    result = executor->driver.memcpy_htod(right, right_host, sizeof(right_host));
    if (result != CUDA_SUCCESS) goto cleanup;
    result = launch_matmul(executor, 1, 2, 1, left, right, output);
    if (result != CUDA_SUCCESS) goto cleanup;
    result = executor->driver.stream_synchronize(executor->stream);
    if (result != CUDA_SUCCESS) goto cleanup;
    result = executor->driver.memcpy_dtoh(&output_host, output, sizeof(output_host));
    if (result == CUDA_SUCCESS && output_host != 2.0) result = CUDA_ERROR_UNKNOWN;

cleanup:
    if (output != 0) {
        cleanup = executor->driver.mem_free(output);
        if (result == CUDA_SUCCESS && cleanup != CUDA_SUCCESS) result = cleanup;
    }
    if (right != 0) {
        cleanup = executor->driver.mem_free(right);
        if (result == CUDA_SUCCESS && cleanup != CUDA_SUCCESS) result = cleanup;
    }
    if (left != 0) {
        cleanup = executor->driver.mem_free(left);
        if (result == CUDA_SUCCESS && cleanup != CUDA_SUCCESS) result = cleanup;
    }
    return result;
}

int markovian_cuda_device_count(int* driver_version, int* device_count, int* failure_stage) {
    struct markovian_cuda_driver driver;
    CUresult result;
    int close_result;
    int stage;
    int open_result;
    if (driver_version == NULL || device_count == NULL || failure_stage == NULL) return (int)CUDA_ERROR_INVALID_VALUE;
    *failure_stage = MARKOVIAN_STAGE_HOST_ARGUMENT;
    if (unsupported_device_fixture_enabled()) {
        *driver_version = CUDA_VERSION;
        *device_count = 1;
        *failure_stage = MARKOVIAN_STAGE_NONE;
        return 0;
    }
    open_result = driver_open(&driver, &stage);
    if (open_result != 0) {
        *failure_stage = stage;
        return open_result;
    }
    *failure_stage = MARKOVIAN_STAGE_INITIALIZE;
    result = driver.init(0);
    if (result == CUDA_SUCCESS) result = driver.driver_get_version(driver_version);
    if (result == CUDA_SUCCESS) {
        *failure_stage = MARKOVIAN_STAGE_DEVICE_COUNT;
        result = driver.device_get_count(device_count);
    }
    close_result = driver_close(&driver);
    if (result != CUDA_SUCCESS) return (int)result;
    if (close_result != 0) *failure_stage = MARKOVIAN_STAGE_DRIVER_UNLOAD;
    return close_result;
}

int markovian_cuda_probe_device(
    int ordinal,
    int* major,
    int* minor,
    size_t* total_memory,
    int* maximum_threads_per_block,
    unsigned char uuid[MARKOVIAN_CUDA_UUID_BYTES],
    char name[MARKOVIAN_CUDA_NAME_BYTES],
    int* failure_stage) {
    struct markovian_cuda_driver driver;
    CUdevice device;
    CUuuid device_uuid;
    CUresult result;
    int close_result;
    int stage;
    int open_result;
    if (ordinal < 0 || major == NULL || minor == NULL || total_memory == NULL ||
        maximum_threads_per_block == NULL || uuid == NULL || name == NULL || failure_stage == NULL) {
        return (int)CUDA_ERROR_INVALID_VALUE;
    }
    *failure_stage = MARKOVIAN_STAGE_HOST_ARGUMENT;
    if (unsupported_device_fixture_enabled()) {
        *major = 8;
        *minor = 0;
        *total_memory = 1024;
        *maximum_threads_per_block = 1024;
        memset(uuid, 0, MARKOVIAN_CUDA_UUID_BYTES);
        (void)snprintf(name, MARKOVIAN_CUDA_NAME_BYTES, "%s", "Markovian unsupported-device fixture");
        *failure_stage = MARKOVIAN_STAGE_NONE;
        return 0;
    }
    open_result = driver_open(&driver, &stage);
    if (open_result != 0) {
        *failure_stage = stage;
        return open_result;
    }
    *failure_stage = MARKOVIAN_STAGE_INITIALIZE;
    result = driver.init(0);
    if (result == CUDA_SUCCESS) {
        *failure_stage = MARKOVIAN_STAGE_DEVICE_SELECT;
        result = driver.device_get(&device, ordinal);
    }
    if (result == CUDA_SUCCESS) {
        *failure_stage = MARKOVIAN_STAGE_DEVICE_COMPATIBILITY;
        result = driver.device_get_attribute(major, CU_DEVICE_ATTRIBUTE_COMPUTE_CAPABILITY_MAJOR, device);
    }
    if (result == CUDA_SUCCESS) result = driver.device_get_attribute(minor, CU_DEVICE_ATTRIBUTE_COMPUTE_CAPABILITY_MINOR, device);
    if (result == CUDA_SUCCESS) result = driver.device_total_mem(total_memory, device);
    if (result == CUDA_SUCCESS) result = driver.device_get_attribute(maximum_threads_per_block, CU_DEVICE_ATTRIBUTE_MAX_THREADS_PER_BLOCK, device);
    if (result == CUDA_SUCCESS) result = driver.device_get_uuid(&device_uuid, device);
    if (result == CUDA_SUCCESS) memcpy(uuid, device_uuid.bytes, MARKOVIAN_CUDA_UUID_BYTES);
    if (result == CUDA_SUCCESS) {
        name[0] = '\0';
        result = driver.device_get_name(name, MARKOVIAN_CUDA_NAME_BYTES, device);
        name[MARKOVIAN_CUDA_NAME_BYTES - 1] = '\0';
    }
    close_result = driver_close(&driver);
    if (result != CUDA_SUCCESS) return (int)result;
    if (close_result != 0) *failure_stage = MARKOVIAN_STAGE_DRIVER_UNLOAD;
    return close_result;
}

void markovian_cuda_executor_create(
    int ordinal,
    const unsigned char expected_uuid[MARKOVIAN_CUDA_UUID_BYTES],
    unsigned char verified_uuid[MARKOVIAN_CUDA_UUID_BYTES],
    markovian_cuda_executor** output_executor,
    int status[MARKOVIAN_CUDA_STATUS_INTS]) {
    markovian_cuda_executor* executor = NULL;
    CUctxCreateParams context_parameters = {0};
    CUcontext popped = NULL;
    CUuuid native_uuid;
    CUresult result = CUDA_SUCCESS;
    int context_current = 0;
    int major = 0;
    int minor = 0;
    int maximum_threads_per_block = 0;
    int stage;
    int bridge_result;

    status_reset(status);
    if (output_executor == NULL || expected_uuid == NULL || verified_uuid == NULL || ordinal < 0) {
        status_primary(status, MARKOVIAN_STAGE_HOST_ARGUMENT, (int)CUDA_ERROR_INVALID_VALUE);
        return;
    }
    *output_executor = NULL;
    memset(verified_uuid, 0, MARKOVIAN_CUDA_UUID_BYTES);
    executor = (markovian_cuda_executor*)calloc(1, sizeof(*executor));
    if (executor == NULL) {
        status_primary(status, MARKOVIAN_STAGE_CONTEXT_CREATE, (int)CUDA_ERROR_OUT_OF_MEMORY);
        return;
    }
    load_faults(executor);
    bridge_result = driver_open(&executor->driver, &stage);
    if (bridge_result != 0) {
        status_primary(status, stage, bridge_result);
        goto failure;
    }
    result = executor->driver.init(0);
    if (result != CUDA_SUCCESS) {
        status_primary(status, MARKOVIAN_STAGE_INITIALIZE, (int)result);
        goto failure;
    }
    result = executor->driver.device_get(&executor->device, ordinal);
    if (result != CUDA_SUCCESS) {
        status_primary(status, MARKOVIAN_STAGE_DEVICE_SELECT, (int)result);
        goto failure;
    }
    result = executor->driver.device_get_uuid(&native_uuid, executor->device);
    if (result != CUDA_SUCCESS || memcmp(native_uuid.bytes, expected_uuid, MARKOVIAN_CUDA_UUID_BYTES) != 0) {
        status_primary(status, MARKOVIAN_STAGE_DEVICE_COMPATIBILITY, result == CUDA_SUCCESS ? MARKOVIAN_CUDA_ERROR_DEVICE_IDENTITY : (int)result);
        goto failure;
    }
    memcpy(verified_uuid, native_uuid.bytes, MARKOVIAN_CUDA_UUID_BYTES);
    result = executor->driver.device_get_attribute(&major, CU_DEVICE_ATTRIBUTE_COMPUTE_CAPABILITY_MAJOR, executor->device);
    if (result == CUDA_SUCCESS) result = executor->driver.device_get_attribute(&minor, CU_DEVICE_ATTRIBUTE_COMPUTE_CAPABILITY_MINOR, executor->device);
    if (result == CUDA_SUCCESS) result = executor->driver.device_get_attribute(&maximum_threads_per_block, CU_DEVICE_ATTRIBUTE_MAX_THREADS_PER_BLOCK, executor->device);
    if (result != CUDA_SUCCESS) {
        status_primary(status, MARKOVIAN_STAGE_DEVICE_COMPATIBILITY, (int)result);
        goto failure;
    }
    if (major != MARKOVIAN_CUDA_REQUIRED_MAJOR || minor != MARKOVIAN_CUDA_REQUIRED_MINOR ||
        maximum_threads_per_block < MARKOVIAN_CUDA_REQUIRED_THREADS) {
        status_primary(status, MARKOVIAN_STAGE_DEVICE_COMPATIBILITY, MARKOVIAN_CUDA_ERROR_UNSUPPORTED_DEVICE);
        goto failure;
    }
    result = executor->driver.ctx_create(&executor->context, &context_parameters, 0, executor->device);
    if (result != CUDA_SUCCESS) {
        status_primary(status, MARKOVIAN_STAGE_CONTEXT_CREATE, (int)result);
        goto failure;
    }
    context_current = 1;
    result = markovian_dense_ptx_length == 0U ? CUDA_ERROR_INVALID_PTX : executor->driver.module_load_data(&executor->module, markovian_dense_ptx);
    if (result != CUDA_SUCCESS) {
        status_primary(status, MARKOVIAN_STAGE_MODULE_LOAD, (int)result);
        goto failure;
    }
    result = executor->driver.module_get_function(&executor->matmul, executor->module, MARKOVIAN_DENSE_PTX_KERNEL);
    if (result != CUDA_SUCCESS) {
        status_primary(status, MARKOVIAN_STAGE_FUNCTION_LOOKUP, (int)result);
        goto failure;
    }
    result = executor->driver.stream_create(&executor->stream, CU_STREAM_NON_BLOCKING);
    if (result != CUDA_SUCCESS) {
        status_primary(status, MARKOVIAN_STAGE_STREAM_CREATE, (int)result);
        goto failure;
    }
    result = self_test(executor);
    if (result != CUDA_SUCCESS) {
        status_primary(status, MARKOVIAN_STAGE_SELF_TEST, (int)result);
        goto failure;
    }
    result = executor->driver.ctx_pop_current(&popped);
    if (result != CUDA_SUCCESS) {
        status_primary(status, MARKOVIAN_STAGE_CONTEXT_POP, (int)result);
        goto failure;
    }
    context_current = 0;
    *output_executor = executor;
    return;

failure:
    if (executor != NULL) {
        if (executor->stream != NULL) {
            result = fault_now(executor, MARKOVIAN_STAGE_STREAM_DESTROY) ? CUDA_ERROR_UNKNOWN : executor->driver.stream_destroy(executor->stream);
            status_cleanup(status, MARKOVIAN_STAGE_STREAM_DESTROY, (int)result);
            if (result == CUDA_SUCCESS) executor->stream = NULL;
        }
        if (executor->module != NULL) {
            result = fault_now(executor, MARKOVIAN_STAGE_MODULE_UNLOAD) ? CUDA_ERROR_UNKNOWN : executor->driver.module_unload(executor->module);
            status_cleanup(status, MARKOVIAN_STAGE_MODULE_UNLOAD, (int)result);
            if (result == CUDA_SUCCESS) executor->module = NULL;
        }
        if (context_current) {
            result = fault_now(executor, MARKOVIAN_STAGE_CONTEXT_POP) ? CUDA_ERROR_UNKNOWN : executor->driver.ctx_pop_current(&popped);
            status_cleanup(status, MARKOVIAN_STAGE_CONTEXT_POP, (int)result);
            if (result == CUDA_SUCCESS) context_current = 0;
        }
        if (executor->context != NULL) {
            result = fault_now(executor, MARKOVIAN_STAGE_CONTEXT_DESTROY) ? CUDA_ERROR_UNKNOWN : executor->driver.ctx_destroy(executor->context);
            status_cleanup(status, MARKOVIAN_STAGE_CONTEXT_DESTROY, (int)result);
            if (result == CUDA_SUCCESS) executor->context = NULL;
        }
        /* Function pointers remain owned while a context may still be live. */
        if (executor->context == NULL) {
            bridge_result = fault_now(executor, MARKOVIAN_STAGE_DRIVER_UNLOAD) ? MARKOVIAN_CUDA_ERROR_DRIVER_UNLOAD : driver_close(&executor->driver);
            status_cleanup(status, MARKOVIAN_STAGE_DRIVER_UNLOAD, bridge_result);
            if (bridge_result == 0) free(executor);
        }
    }
}

void markovian_cuda_executor_destroy(
    markovian_cuda_executor* executor,
    int status[MARKOVIAN_CUDA_STATUS_INTS]) {
    CUcontext popped = NULL;
    CUresult result;
    int pushed = 0;
    int bridge_result;
    status_reset(status);
    if (executor == NULL) {
        status_primary(status, MARKOVIAN_STAGE_HOST_ARGUMENT, (int)CUDA_ERROR_INVALID_VALUE);
        return;
    }
    result = fault_now(executor, MARKOVIAN_STAGE_CONTEXT_PUSH) ? CUDA_ERROR_UNKNOWN : executor->driver.ctx_push_current(executor->context);
    if (result != CUDA_SUCCESS) {
        status_cleanup(status, MARKOVIAN_STAGE_CONTEXT_PUSH, (int)result);
    } else {
        pushed = 1;
    }
    if (pushed && executor->retained_count != 0) retry_retained_allocations(executor, status);
    if (pushed && executor->stream != NULL) {
        result = fault_now(executor, MARKOVIAN_STAGE_SYNCHRONIZE) ? CUDA_ERROR_UNKNOWN : executor->driver.stream_synchronize(executor->stream);
        status_cleanup(status, MARKOVIAN_STAGE_SYNCHRONIZE, (int)result);
    }
    if (executor->stream != NULL) {
        result = fault_now(executor, MARKOVIAN_STAGE_STREAM_DESTROY) ? CUDA_ERROR_UNKNOWN : executor->driver.stream_destroy(executor->stream);
        status_cleanup(status, MARKOVIAN_STAGE_STREAM_DESTROY, (int)result);
        if (result == CUDA_SUCCESS) executor->stream = NULL;
    }
    if (executor->module != NULL) {
        result = fault_now(executor, MARKOVIAN_STAGE_MODULE_UNLOAD) ? CUDA_ERROR_UNKNOWN : executor->driver.module_unload(executor->module);
        status_cleanup(status, MARKOVIAN_STAGE_MODULE_UNLOAD, (int)result);
        if (result == CUDA_SUCCESS) executor->module = NULL;
    }
    if (pushed) {
        result = fault_now(executor, MARKOVIAN_STAGE_CONTEXT_POP) ? CUDA_ERROR_UNKNOWN : executor->driver.ctx_pop_current(&popped);
        status_cleanup(status, MARKOVIAN_STAGE_CONTEXT_POP, (int)result);
    }
    result = fault_now(executor, MARKOVIAN_STAGE_CONTEXT_DESTROY) ? CUDA_ERROR_UNKNOWN : executor->driver.ctx_destroy(executor->context);
    status_cleanup(status, MARKOVIAN_STAGE_CONTEXT_DESTROY, (int)result);
    if (result == CUDA_SUCCESS) executor->context = NULL;
    /* A failed context destruction keeps the driver handle and executor alive. */
    if (executor->context != NULL) return;
    bridge_result = fault_now(executor, MARKOVIAN_STAGE_DRIVER_UNLOAD) ? MARKOVIAN_CUDA_ERROR_DRIVER_UNLOAD : driver_close(&executor->driver);
    status_cleanup(status, MARKOVIAN_STAGE_DRIVER_UNLOAD, bridge_result);
    if (bridge_result == 0) free(executor);
}

void markovian_cuda_executor_matmul(
    markovian_cuda_executor* executor,
    int rows,
    int inner,
    int columns,
    const double* left_host,
    const double* right_host,
    double* output_host,
    double* transfer_inclusive_milliseconds,
    int status[MARKOVIAN_CUDA_STATUS_INTS]) {
    CUdeviceptr left = 0;
    CUdeviceptr right = 0;
    CUdeviceptr output = 0;
    CUcontext popped = NULL;
    CUresult result = CUDA_SUCCESS;
    size_t left_bytes;
    size_t right_bytes;
    size_t output_bytes;
    struct timespec start;
    struct timespec end;
    int pushed = 0;

    status_reset(status);
    if (executor != NULL && executor->poisoned) {
        status_primary(status, MARKOVIAN_STAGE_EXECUTOR_POISONED, MARKOVIAN_CUDA_ERROR_EXECUTOR_POISONED);
        status[2] = 1;
        return;
    }
    if (executor == NULL || rows <= 0 || inner <= 0 || columns <= 0 ||
        left_host == NULL || right_host == NULL || output_host == NULL ||
        transfer_inclusive_milliseconds == NULL ||
        !checked_index_product((size_t)rows, (size_t)inner) ||
        !checked_index_product((size_t)inner, (size_t)columns) ||
        !checked_index_product((size_t)rows, (size_t)columns) ||
        !checked_bytes((size_t)rows, (size_t)inner, &left_bytes) ||
        !checked_bytes((size_t)inner, (size_t)columns, &right_bytes) ||
        !checked_bytes((size_t)rows, (size_t)columns, &output_bytes)) {
        status_primary(status, MARKOVIAN_STAGE_HOST_ARGUMENT, (int)CUDA_ERROR_INVALID_VALUE);
        return;
    }

    clock_gettime(CLOCK_MONOTONIC, &start);
    result = executor->driver.ctx_push_current(executor->context);
    if (result != CUDA_SUCCESS) {
        status_primary(status, MARKOVIAN_STAGE_CONTEXT_PUSH, (int)result);
        goto cleanup;
    }
    pushed = 1;
    result = fault_now(executor, MARKOVIAN_STAGE_ALLOCATE_LEFT) ? CUDA_ERROR_OUT_OF_MEMORY : executor->driver.mem_alloc(&left, left_bytes);
    if (result != CUDA_SUCCESS) { status_primary(status, MARKOVIAN_STAGE_ALLOCATE_LEFT, (int)result); goto cleanup; }
    result = fault_now(executor, MARKOVIAN_STAGE_ALLOCATE_RIGHT) ? CUDA_ERROR_OUT_OF_MEMORY : executor->driver.mem_alloc(&right, right_bytes);
    if (result != CUDA_SUCCESS) { status_primary(status, MARKOVIAN_STAGE_ALLOCATE_RIGHT, (int)result); goto cleanup; }
    result = fault_now(executor, MARKOVIAN_STAGE_ALLOCATE_OUTPUT) ? CUDA_ERROR_OUT_OF_MEMORY : executor->driver.mem_alloc(&output, output_bytes);
    if (result != CUDA_SUCCESS) { status_primary(status, MARKOVIAN_STAGE_ALLOCATE_OUTPUT, (int)result); goto cleanup; }
    result = fault_now(executor, MARKOVIAN_STAGE_COPY_LEFT) ? CUDA_ERROR_UNKNOWN : executor->driver.memcpy_htod(left, left_host, left_bytes);
    if (result != CUDA_SUCCESS) { status_primary(status, MARKOVIAN_STAGE_COPY_LEFT, (int)result); goto cleanup; }
    result = fault_now(executor, MARKOVIAN_STAGE_COPY_RIGHT) ? CUDA_ERROR_UNKNOWN : executor->driver.memcpy_htod(right, right_host, right_bytes);
    if (result != CUDA_SUCCESS) { status_primary(status, MARKOVIAN_STAGE_COPY_RIGHT, (int)result); goto cleanup; }

    status[2] = 1;
    result = fault_now(executor, MARKOVIAN_STAGE_LAUNCH) ? CUDA_ERROR_UNKNOWN : launch_matmul(executor, rows, inner, columns, left, right, output);
    if (result != CUDA_SUCCESS) { status_primary(status, MARKOVIAN_STAGE_LAUNCH, (int)result); goto cleanup; }
    result = fault_now(executor, MARKOVIAN_STAGE_SYNCHRONIZE) ? CUDA_ERROR_UNKNOWN : executor->driver.stream_synchronize(executor->stream);
    if (result != CUDA_SUCCESS) { status_primary(status, MARKOVIAN_STAGE_SYNCHRONIZE, (int)result); goto cleanup; }
    result = fault_now(executor, MARKOVIAN_STAGE_COPY_OUTPUT) ? CUDA_ERROR_UNKNOWN : executor->driver.memcpy_dtoh(output_host, output, output_bytes);
    if (result != CUDA_SUCCESS) { status_primary(status, MARKOVIAN_STAGE_COPY_OUTPUT, (int)result); goto cleanup; }

cleanup:
    if (output != 0) {
        result = free_or_retain(executor, output, MARKOVIAN_STAGE_FREE_OUTPUT);
        status_cleanup(status, MARKOVIAN_STAGE_FREE_OUTPUT, (int)result);
    }
    if (right != 0) {
        result = free_or_retain(executor, right, MARKOVIAN_STAGE_FREE_RIGHT);
        status_cleanup(status, MARKOVIAN_STAGE_FREE_RIGHT, (int)result);
    }
    if (left != 0) {
        result = free_or_retain(executor, left, MARKOVIAN_STAGE_FREE_LEFT);
        status_cleanup(status, MARKOVIAN_STAGE_FREE_LEFT, (int)result);
    }
    if (pushed) status_cleanup(status, MARKOVIAN_STAGE_CONTEXT_POP, (int)executor->driver.ctx_pop_current(&popped));
    clock_gettime(CLOCK_MONOTONIC, &end);
    *transfer_inclusive_milliseconds = elapsed_milliseconds(start, end);
}

static const char* bridge_error_name(int code) {
    switch (code) {
        case MARKOVIAN_CUDA_ERROR_LIBRARY_UNAVAILABLE: return "MARKOVIAN_CUDA_DRIVER_UNAVAILABLE";
        case MARKOVIAN_CUDA_ERROR_SYMBOL_UNAVAILABLE: return "MARKOVIAN_CUDA_ABI_SYMBOL_UNAVAILABLE";
        case MARKOVIAN_CUDA_ERROR_DRIVER_UNLOAD: return "MARKOVIAN_CUDA_DRIVER_UNLOAD_FAILED";
        case MARKOVIAN_CUDA_ERROR_UNSUPPORTED_DEVICE: return "MARKOVIAN_CUDA_DEVICE_UNSUPPORTED";
        case MARKOVIAN_CUDA_ERROR_EXECUTOR_POISONED: return "MARKOVIAN_CUDA_EXECUTOR_POISONED";
        case MARKOVIAN_CUDA_ERROR_DEVICE_IDENTITY: return "MARKOVIAN_CUDA_DEVICE_IDENTITY_MISMATCH";
        default: return NULL;
    }
}

static const char* bridge_error_string(int code) {
    switch (code) {
        case MARKOVIAN_CUDA_ERROR_LIBRARY_UNAVAILABLE: return "libcuda.so.1 could not be loaded with RTLD_NOW and RTLD_LOCAL";
        case MARKOVIAN_CUDA_ERROR_SYMBOL_UNAVAILABLE: return "the loaded CUDA driver does not provide the complete required CUDA 13 symbol table";
        case MARKOVIAN_CUDA_ERROR_DRIVER_UNLOAD: return "the owned CUDA driver handle could not be unloaded after resource teardown";
        case MARKOVIAN_CUDA_ERROR_UNSUPPORTED_DEVICE: return "the device is outside the bounded sm_121 execution profile";
        case MARKOVIAN_CUDA_ERROR_EXECUTOR_POISONED: return "a device allocation could not be released; the executor rejects further launches";
        case MARKOVIAN_CUDA_ERROR_DEVICE_IDENTITY: return "the selected device UUID changed between probe and context creation";
        default: return NULL;
    }
}

static const char* copy_driver_diagnostic(int code, int want_name) {
    static _Thread_local char diagnostic[512];
    struct markovian_cuda_driver driver;
    const char* source = NULL;
    CUresult result;
    int stage;
    int open_result = driver_open(&driver, &stage);
    (void)stage;
    if (open_result != 0) return want_name ? "CUDA_ERROR_UNKNOWN_NAME" : "CUDA error description unavailable";
    result = want_name
        ? driver.get_error_name((CUresult)code, &source)
        : driver.get_error_string((CUresult)code, &source);
    if (result != CUDA_SUCCESS || source == NULL) {
        (void)driver_close(&driver);
        return want_name ? "CUDA_ERROR_UNKNOWN_NAME" : "CUDA error description unavailable";
    }
    (void)snprintf(diagnostic, sizeof(diagnostic), "%s", source);
    (void)driver_close(&driver);
    return diagnostic;
}

const char* markovian_cuda_error_name(int code) {
    const char* bridge = bridge_error_name(code);
    return bridge != NULL ? bridge : copy_driver_diagnostic(code, 1);
}

const char* markovian_cuda_error_string(int code) {
    const char* bridge = bridge_error_string(code);
    return bridge != NULL ? bridge : copy_driver_diagnostic(code, 0);
}
