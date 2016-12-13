/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __ARCH_ARMV6_MACHINE_H
#define __ARCH_ARMV6_MACHINE_H

static inline void wfi(void)
{
    /*
     * The wait-for-interrupt doesn't work on the KZM board, although,
     * according to the arm infocenter, it should. With the KZM currently
     * being the only supported ARMv6 platform, it is unclear at this
     * time wether it works for other SoCs (e.g. BCM2835), so we explicitly
     * disable only the KZM here.
     *
     * See: http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.faqs/ka13332.html
     */
#ifndef CONFIG_PLAT_KZM
    asm volatile("mcr p15, 0, %0, c7, c0, 4" : : "r"(0) : "memory");
#endif
}

static inline void dsb(void)
{
    asm volatile("mcr p15, 0, %0, c7, c10, 4" : : "r"(0) : "memory");
}

static inline void dmb(void)
{
    asm volatile("mcr p15, 0, %0, c7, c10, 5" : : "r"(0) : "memory");
}

static inline void isb(void)
{
    asm volatile("mcr p15, 0, %0, c7, c5, 4" : : "r"(0) : "memory");
}

#endif
