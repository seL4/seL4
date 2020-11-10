/*
 * Copyright 2020 Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

/*
 * This code was taken from the Linux kernel, file arch/arm/mach-rockchip/rockchip.c
 */

#define RK3288_TIMER6_7_PHYS 0xff810000

#define writel(v, a) (*(uint32_t *)(a) = (v))

BOOT_CODE void initTimer(void)
{
    /*
     * Most/all uboot versions for rk3288 don't enable timer7
     * which is needed for the architected timer to work.
     * So make sure it is running during early boot.
     */
    writel(0, RK3288_TIMER6_7_PHYS + 0x30);
    writel(0xffffffff, RK3288_TIMER6_7_PHYS + 0x20);
    writel(0xffffffff, RK3288_TIMER6_7_PHYS + 0x24);
    writel(1, RK3288_TIMER6_7_PHYS + 0x30);
    dsb();
    initGenericTimer();
}
