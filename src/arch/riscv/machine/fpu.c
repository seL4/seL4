#include <arch/machine/fpu.h>

#ifdef CONFIG_HAVE_FPU
fpu_status_t fpu_status[CONFIG_MAX_NUM_NODES];
#endif
