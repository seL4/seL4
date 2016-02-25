/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <types.h>
#include <machine/io.h>
#include <kernel/vspace.h>
#include <arch/machine.h>
#include <arch/kernel/vspace.h>
#include <plat/machine.h>
#include <arch/linker.h>
#include <plat/machine/devices.h>
#include <plat/machine/hardware.h>

/* Available physical memory regions on platform (RAM) */
/* NOTE: Regions are not allowed to be adjacent! */
const p_region_t BOOT_RODATA avail_p_regs[] = {
    /* 1 GiB */
#if CONFIG_MAX_NUM_TRACE_POINTS > 0
#warning "NOTE: logging is currently untested on iMX7 Sabre"
    /* 1MB stolen for logging */
    { /* .start = */ 0x80000000, /* .end = */ 0x9fd00000 }
#else
    { /* .start = */ 0x80000000, /* .end = */ 0xc0000000 }
#endif /* CONFIG_MAX_NUM_TRACE_POINTS > 0 */
};

BOOT_CODE int
get_num_avail_p_regs(void)
{
    return sizeof(avail_p_regs) / sizeof(p_region_t);
}

BOOT_CODE p_region_t
get_avail_p_reg(word_t i)
{
    return avail_p_regs[i];
}

const p_region_t BOOT_RODATA dev_p_regs[] = {
    { GPIO0_PADDR,              GPIO0_PADDR             +   0x10000 },
    { GPIO1_PADDR,              GPIO1_PADDR             +   0x10000 },
    { GPIO2_PADDR,              GPIO2_PADDR             +   0x10000 },
    { GPIO3_PADDR,              GPIO3_PADDR             +   0x10000 },
    { GPIO4_PADDR,              GPIO4_PADDR             +   0x10000 },
    { GPIO5_PADDR,              GPIO5_PADDR             +   0x10000 },
    { GPIO6_PADDR,              GPIO6_PADDR             +   0x10000 },
    { WDOG0_PADDR,              WDOG0_PADDR             +   0x10000 },
    { WDOG1_PADDR,              WDOG1_PADDR             +   0x10000 },
    { WDOG2_PADDR,              WDOG2_PADDR             +   0x10000 },
    { WDOG3_PADDR,              WDOG3_PADDR             +   0x10000 },
    { IOMUXC_LPSR_PADDR,        IOMUXC_LPSR_PADDR       +   0x10000 },
    { GPT0_PADDR,               GPT0_PADDR              +   0x10000 },
    { GPT1_PADDR,               GPT1_PADDR              +   0x10000 },
    { GPT2_PADDR,               GPT2_PADDR              +   0x10000 },
    { GPT3_PADDR,               GPT3_PADDR              +   0x10000 },
    { IOMUXC_PADDR,             IOMUXC_PADDR            +   0x10000 },
    { GPR_PADDR,                GPR_PADDR               +   0x10000 },
    { OCOTP_PADDR,              OCOTP_PADDR             +   0x10000 },
    { ANATOP_PADDR,             ANATOP_PADDR            +   0x10000 },
    { CLKS_PADDR,               CLKS_PADDR              +   0x10000 },
    { SRC_PADDR,                SRC_PADDR               +   0x10000 },
    { PWM0_PADDR,               PWM0_PADDR              +   0x10000 },
    { PWM1_PADDR,               PWM1_PADDR              +   0x10000 },
    { PWM2_PADDR,               PWM2_PADDR              +   0x10000 },
    { PWM3_PADDR,               PWM3_PADDR              +   0x10000 },
    { UART1_PADDR,              UART1_PADDR             +   0x10000 },
    { UART2_PADDR,              UART2_PADDR             +   0x10000 },
    { UART3_PADDR,              UART3_PADDR             +   0x10000 },
    { UART4_PADDR,              UART4_PADDR             +   0x10000 },
    { UART5_PADDR,              UART5_PADDR             +   0x10000 },
    { UART6_PADDR,              UART6_PADDR             +   0x10000 },
    { I2C0_PADDR,               I2C0_PADDR              +   0x10000 },
    { I2C1_PADDR,               I2C1_PADDR              +   0x10000 },
    { I2C2_PADDR,               I2C2_PADDR              +   0x10000 },
    { I2C3_PADDR,               I2C2_PADDR              +   0x10000 },
    { USBOTG0_PADDR,            USBOTG0_PADDR           +   0x1000  },
    { USBOTG1_PADDR,            USBOTG1_PADDR           +   0x1000  },
    { USBH_PADDR,               USBH_PADDR              +   0x1000  },
    { USDHC0_PADDR,             USDHC0_PADDR            +   0x10000 },
    { USDHC1_PADDR,             USDHC1_PADDR            +   0x10000 },
    { USDHC2_PADDR,             USDHC2_PADDR            +   0x10000 },
    { FEC0_PADDR,               FEC0_PADDR              +   0x10000 },
    { FEC1_PADDR,               FEC1_PADDR              +   0x10000 },
};

BOOT_CODE int
get_num_dev_p_regs(void)
{
    return sizeof(dev_p_regs) / sizeof(p_region_t);
}

BOOT_CODE p_region_t
get_dev_p_reg(word_t i)
{
    return dev_p_regs[i];
}


/* Determine if the given IRQ should be reserved by the kernel. */
bool_t CONST
isReservedIRQ(irq_t irq)
{
    return irq == KERNEL_TIMER_IRQ;
}

/* Handle a platform-reserved IRQ. */
void
handleReservedIRQ(irq_t irq)
{
    printf("Received reserved IRQ: %d\n", (int)irq);
}


BOOT_CODE void
map_kernel_devices(void)
{
    /* map kernel device: GIC distributor and private timers */
    map_kernel_frame(
        ARM_MP_PADDR,
        ARM_MP_PPTR1,
        VMKernelOnly,
        vm_attributes_new(
            true,  /* armExecuteNever */
            false, /* armParityEnabled */
            false  /* armPageCacheable */
        )
    );

    /* map kernel device: GIC controller */
    map_kernel_frame(
        ARM_MP_PADDR + BIT(PAGE_BITS),
        ARM_MP_PPTR2,
        VMKernelOnly,
        vm_attributes_new(
            true,  /* armExecuteNever */
            false, /* armParityEnabled */
            false  /* armPageCacheable */
        )
    );

    /* map kernel device: GIC controller */
    map_kernel_frame(
        ARM_MP_PADDR + BIT(PAGE_BITS) * 2,
        ARM_MP_PPTR3,
        VMKernelOnly,
        vm_attributes_new(
            true,  /* armExecuteNever */
            false, /* armParityEnabled */
            false  /* armPageCacheable */
        )
    );

#if defined DEBUG || defined RELEASE_PRINTF
    /* map kernel device: UART */
    map_kernel_frame(
        UART_PADDR,
        UART_PPTR,
        VMKernelOnly,
        vm_attributes_new(
            true,  /* armExecuteNever */
            false, /* armParityEnabled */
            false  /* armPageCacheable */
        )
    );
#endif /* DEBUG */
}



/* co-processor code to read and write GPT */

static void
write_cntp_ctl(uint32_t v)
{
    asm volatile ("mcr p15, 0, %0, c14, c2, 1" ::"r"(v));
}

static void
write_cntp_tval(uint32_t v)
{
    asm volatile  ("mcr p15, 0, %0, c14, c2, 0" :: "r"(v));
}

static uint32_t
read_cntfrq(void)
{
    uint32_t val;
    asm volatile ("mrc  p15, 0, %0, c14, c0, 0" : "=r"(val));
    return val;
}

/* default 8 MHz */
#define GPT_DEFAULT_FREQ        0x7a1200
#define GPT_DEFAULT_FREQ_MHZ    8ull
static uint32_t gpt_cntp_tval = 0;

/* we use the count-down timer of the GPT as the kernel preemption timer */
void
initTimer(void)
{
    uint32_t freq = read_cntfrq();
    uint64_t tval = 0;

    if (freq != GPT_DEFAULT_FREQ) {
        printf("Default timer has a different frequency %x\n", freq);
    }

    tval = (uint64_t)CONFIG_TIMER_TICK_MS * (freq / 1000);
    if (tval > 0xffffffff) {
        printf("timer interval value out of range \n");
        halt();
    }

    gpt_cntp_tval = (uint32_t)tval;

    /* write the value */
    write_cntp_tval(gpt_cntp_tval);

    /* enable the timer */
    write_cntp_ctl(0x1);
}

/* need to reload the count-down value */
void
resetTimer(void)
{
    write_cntp_tval(gpt_cntp_tval);
}


/* Cortex-A7 uses an integrated L2 cache controller */

void
initL2Cache(void)
{
}
