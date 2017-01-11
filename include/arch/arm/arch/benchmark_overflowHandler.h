#ifndef ARCH_BENCHMARK_OV_H
#define ARCH_BENCHMARK_OV_H

#include <config.h>
#ifdef CONFIG_ENABLE_BENCHMARKS

#ifdef CONFIG_BENCHMARK_TRACK_UTILISATION
#include <model/statedata.h>
#ifndef CONFIG_PLAT_KZM
#include <armv/benchmark_irqHandler.h>
#endif /* CONFIG_PLAT_KZM */

extern bool_t benchmark_log_utilisation_enabled;

static inline void handleOverflowIRQ(void)
{
    if (likely(benchmark_log_utilisation_enabled)) {
        ksCurThread->benchmark.utilisation += 0xFFFFFFFFU - ksCurThread->benchmark.schedule_start_time;
        ksCurThread->benchmark.schedule_start_time = 0;

        ccnt_num_overflows++;
        armv_handleOverflowIRQ();
    }
}
#endif /* CONFIG_BENCHMARK_TRACK_UTILISATION */
#endif /* CONFIG_ENABLE_BENCHMARKS */
#endif /* ARCH_BENCHMARK_OV_H */
