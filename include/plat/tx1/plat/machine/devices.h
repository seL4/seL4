/*
 * Copyright 2017, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_GPL)
 */

#ifndef __PLAT_MACHINE_DEVICES_H
#define __PLAT_MACHINE_DEVICES_H

#include <config.h>

/* These devices are used by the seL4 kernel. */
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
#define KDEV_PPTR                   0x0000ffffffff0000lu
#else
#define KDEV_PPTR                   0xffffffffffff0000lu
#endif

#define UARTA_PPTR                  (KDEV_PPTR)
#define GIC_DISTRIBUTOR_PPTR        (KDEV_PPTR + 0x3000)
#define GIC_CONTROLLER_PPTR         (KDEV_PPTR + 0x4000)
#define GICH_PPTR                   (KDEV_PPTR + 0x6000)

#define GIC_PL390_CONTROLLER_PPTR   GIC_CONTROLLER_PPTR
#define GIC_PL390_DISTRIBUTOR_PPTR  GIC_DISTRIBUTOR_PPTR

#define GIC_DISTRIBUTOR_PADDR       GICD_PADDR
#define GIC_CONTROLLER_PADDR        GICI_PADDR

#define GIC_PL400_VCPUCTRL_PPTR     GICH_PPTR


/* many of the device regions are not 4K page aligned, so some regions may contain more than
 * one devices. it is the user's responsibilty to figure out how to use these regions */
#define PCIE_A1_PADDR               (0x01000000)                /* 16 MB                      */
#define PCIE_A2_PADDR               (0x02000000)                /* 480 MB                     */
#define PCIE_A3_PADDR               (0x20000000)                /* 512 MB                     */
#define IRAM_A_PADDR                (0x40000000)                /* 64 KB                      */
#define IRAM_B_PADDR                (0x40010000)                /* 64 KB                      */
#define IRAM_C_PADDR                (0x40020000)                /* 64 KB                      */
#define IRAM_D_PADDR                (0x40030000)                /* 64 KB                      */
#define HOST1X_PADDR                (0x50000000)                /* 256 KB                     */
#define ARM_PERIPHBASE              (0x50040000)                /* 128 KB                     */
#define GICD_PADDR                  (ARM_PERIPHBASE + 0x1000)   /* interrupt distributor      */
#define GICI_PADDR                  (ARM_PERIPHBASE + 0x2000)   /* GIC CPU interface          */
#define GICH_PADDR                  (ARM_PERIPHBASE + 0x4000)   /* GIC hyp-view register      */
#define GICV_PADDR                  (ARM_PERIPHBASE + 0x6000)   /* GIC VCPU register          */
#define MSELECT_PADDR               (0x50060000)                /* 4KB                        */
#define GRAPHICS_HOST_PADDR         (0x54000000)                /* 16 MB                      */
#define GPU_PADDR                   (0x57000000)                /* 144 MB                     */
#define UP_TAG_PADDR                (0x60000000)                /* 4 KB uP-TAG                */
#define RES_SEM_PADDR               (0x60001000)                /* 4 KB resource semaphore    */
#define ARB_SEM_PADDR               (0x60002000)                /* 4 KB arbitration semaphore */
#define ARB_PRI_PADDR               (0x60003000)                /* 4 KB                       */
#define ICTLR_PADDR                 (0x60004000)
#define TMR_PADDR                   (0x60005000)                /* 4 KB                       */
#define CLK_RESET_PADDR             (0x60006000)                /* Clock and Reset            */
#define FLOW_CTL_PADDR              (0x60007000)                /* Flow Controller            */
#define APB_DMA_PADDR               (0x60020000)                /* 16 KB                      */
#define SYS_REG_PADDR               (0x6000c000)                /* System Registers           */
#define GPIO_PADDR                  (0x6000d000)                /* GPIO-1 to GPIO-8           */
#define UARTA_SYNC_PADDR            (0x70006000)                /* 12 KB, multiple            */
#define UARTA_PADDR                 (0x70006000)
#define I2C1_4_PADDR                (0x7000c000)                /* I2C 1 to 4                */
#define I2C5_6_SPI2B1_4_PADDR       (0x7000d000)                /* I2C 5 to 6, SPI2B 1 to 4  */
#define RTC_PMC_PADDR               (0x7000e000)                /* RTC and PMC               */
#define MISC_PADDR                  (0x70000000)                /* MISC 12 KB                */
#define PINMUX_AUX_PADDR            (0x70003000)
#define FUSE_KFUSE_PADDR            (0x7000f000)                /* FUSE and KFUSE            */
#define SE_PADDR                    (0x70012000)                /* 8 KB                      */
#define TSENSOR_PADDR               (0x70014000)
#define CEC_PADDR                   (0x70015000)
#define ATOMICS_PADDR               (0x70016000)                /* 8 KB                      */
#define MC_PADDR                    (0x70019000)
#define EMC_PADDR                   (0x7001b000)
#define MC0_PADDR                   (0x7001c000)
#define MC1_PADDR                   (0x7001d000)
#define EMC0_PADDR                  (0x7001e000)
#define EMC1_PADDR                  (0x7001f000)
#define SATA_PADDR                  (0x70020000)                /* 64 KB                      */
#define HDA_PADDR                   (0x70030000)                /* 64 KB                      */
#define APE_PADDR                   (0x702c0000)                /* 256 KB                     */
#define XUSB_PADCTL_PADDR           (0x7009f000)
#define XUSB_HOST_PADDR             (0x70090000)                /* 40 KB                      */
#define SDMMC1_4_PADDR              (0x700b0000)
#define SDMMC1B_PADDR               (0x700b1000)
#define SDMMC2B_PADDR               (0x700b2000)
#define SDMMC3B_PADDR               (0x700b3000)
#define SDMMC4B_PADDR               (0x700b4000)
#define XUSB_DEV_PADDR              (0x700d0000)                /* 40 KB                      */
#define DDS_PADDR                   (0x700a0000)                /* 8 KB                       */
#define SPEEDO_PADDR                (0x700c0000)                /* 32 KB                      */
#define SPEEDO_PMON_PADDR           (0x700c8000)                /* 32 KB                      */
#define SYSCTR0_PADDR               (0x700f0000)                /* 64 KB                      */
#define SYSCTR1_PADDR               (0x70100000)                /* 64 KB                      */
#define DP2_PADDR                   (0x700e0000)
#define APB2JTAG_PADDR              (0x700e1000)
#define SOC_THERM_PADDR             (0x700e2000)
#define MIPI_CAL_PADDR              (0x700e3000)
#define DVFS_PADDR                  (0x70110000)
#define CLUSTER_CLK_PADDR           (0x70040000)                /* 256 KB                      */
#define QSPI_PADDR                  (0x70410000)
#define STM_PADDR                   (0x71000000)                /* 16 MB                       */
#define CSITE_PADDR                 (0x72000000)                /* 32 MB                       */
#define AHB_A1_PADDR                (0x78000000)                /* 16 MB                       */
#define PPCS_PADDR                  (0x7c000000)                /* AHB ot MC flush 64 KB       */
#define TZRAM_PADDR                 (0x7c010000)                /* 64 KB                       */
#define USB_PADDR                   (0x7d000000)                /* 6 KB round up to 8 KB       */
#define USB2_PADDR                  (0x7d004000)                /* 6 KB round up to 8 KB       */

#endif /* __PLAT_MACHINE_DEVICES_H */
