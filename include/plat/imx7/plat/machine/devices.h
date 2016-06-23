/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __PLAT_MACHINE_DEVICES_H
#define __PLAT_MACHINE_DEVICES_H

/* These devices are used by the seL4 kernel. */
#define UART_PADDR                  UART0_PADDR

#define UART_PPTR                   0xfff01000
#define WDT_PPTR                    0xfff02000
#define ARM_MP_PPTR1                0xfff03000
#define ARM_MP_PPTR2                0xfff04000
#define ARM_MP_PPTR3                0xfff05000
#define ARM_DEBUG_MMAPPING_PPTR     0xfff06000


#define ARM_MP_PRIV_TIMER_PPTR      (ARM_MP_PPTR1 + 0x600 )
#define ARM_MP_GLOBAL_TIMER_PPTR    (ARM_MP_PPTR1 + 0x200 )

#define GIC_PL390_CONTROLLER_PPTR   (ARM_MP_PPTR3)
#define GIC_PL390_DISTRIBUTOR_PPTR  (ARM_MP_PPTR2)


#define ARM_SCU_PADDR               0x31000000

/* All devices */
#define ARM_MP_PADDR              (ARM_SCU_PADDR) /*   2 pages */

/* addresses from Linux device tree */
#define GPIO0_PADDR               0x30200000      /* 0x10000 */
#define GPIO1_PADDR               0x30210000
#define GPIO2_PADDR               0x30220000
#define GPIO3_PADDR               0x30230000
#define GPIO4_PADDR               0x30240000
#define GPIO5_PADDR               0x30250000
#define GPIO6_PADDR               0x30260000
#define WDOG0_PADDR               0x30280000
#define WDOG1_PADDR               0x30290000
#define WDOG2_PADDR               0x302a0000
#define WDOG3_PADDR               0x302b0000
#define IOMUXC_LPSR_PADDR         0x302c0000
#define GPT0_PADDR                0x302d0000
#define GPT1_PADDR                0x302e0000
#define GPT2_PADDR                0x302f0000
#define GPT3_PADDR                0x30300000
#define IOMUXC_PADDR              0x30330000
#define GPR_PADDR                 0x30340000
#define OCOTP_PADDR               0x30350000
#define ANATOP_PADDR              0x30360000
#define SNVS_PADDR                0x30370000
#define CLKS_PADDR                0x30380000
#define SRC_PADDR                 0x30390000
#define PWM0_PADDR                0x30660000
#define PWM1_PADDR                0x30670000
#define PWM2_PADDR                0x30680000
#define PWM3_PADDR                0x30690000
#define UART0_PADDR               0x30860000
#define UART1_PADDR               0x30890000
#define UART2_PADDR               0x30880000
#define UART3_PADDR               0x30a60000
#define UART4_PADDR               0x30a70000
#define UART5_PADDR               0x30a80000
#define UART6_PADDR               0x30a90000
#define I2C0_PADDR                0x30a20000
#define I2C1_PADDR                0x30a30000
#define I2C2_PADDR                0x30a40000
#define I2C3_PADDR                0x30a50000
#define USBOTG0_PADDR             0x30b10000    /* 0x200 */
#define USBOTG1_PADDR             0x30b20000    /* 0x200 */
#define USBH_PADDR                0x30b30000    /* 0x200 */
/* addresses are not page-aligned */
#define USDHC0_PADDR              0x30b40000    /* 0x10000 */
#define USDHC1_PADDR              0x30b50000
#define USDHC2_PADDR              0x30b60000
#define FEC0_PADDR                0x30be0000
#define FEC1_PADDR                0x30bf0000


#endif /* !__PLAT_MACHINE_DEVICES_H */
