/*
 * Copyright 2016, General Dynamics C4 Systems
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
#define UARTA_PPTR                  0xfff01000
#define UARTB_PPTR                  0xfff01040
#define UARTC_PPTR                  0xfff01200
#define UARTD_PPTR                  0xfff01300
#define GIC_DISTRIBUTOR_PPTR        0xfff03000
#define GIC_CONTROLLER_PPTR         0xfff04000
/* HYP mode kernel devices */
#define GIC_VCPUCTRL_PPTR           0xfff06000
/* SMMU registers */
#define SMMU_PPTR                   0Xfff07000
#define ARM_DEBUG_MMAPPING_PPTR     0xfff08000


#define GIC_PL390_CONTROLLER_PPTR   GIC_CONTROLLER_PPTR
#define GIC_PL390_DISTRIBUTOR_PPTR  GIC_DISTRIBUTOR_PPTR

#define GIC_DISTRIBUTOR_PADDR       GICD_PADDR
#define GIC_CONTROLLER0_PADDR       GICI_PADDR

#define GIC_PL400_VCPUCTRL_PPTR     GIC_VCPUCTRL_PPTR
#define GIC_VCPUCTRL_PADDR          (VGICI_REQ_PADDR)
/*
 * many of the device regions are not 4K page aligned, so some regions may contain more than
 * one devices. it is the user's responsibilty to figure out how to use these regions
 * */

#define ARM_PERIPHBASE              (0x50040000)                /* 128 KB                       */
#define GICD_PADDR                  (ARM_PERIPHBASE + 0x1000)   /* interrupt distributor        */
#define GICI_PADDR                  (ARM_PERIPHBASE + 0x2000)   /* GIC CPU interface            */
#define VGICI_REQ_PADDR             (ARM_PERIPHBASE + 0x4000)   /* hyp view for requesting CPU  */
#define VGICI_ALL_PADDR             (ARM_PERIPHBASE + 0x5000)   /* hyp view for all CPUs        */
#define VGICI_VM_PADDR              (ARM_PERIPHBASE + 0x6000)   /* hyp view for VM view         */
#define PCIE_0_CFG_PADDR            (0x01000000)                /* PCIE lane 0 config register  */
#define PCIE_1_CFG_PADDR            (0x01001000)                /* PCIE lane 1 config register  */
#define PCIE_PCA0_1_PADDR           (0x01002000)                /* PCA 0  and 1                 */
#define PCIE_PADS_AFI_PADDR         (0x01003000)                /* Pads and AFI                 */
#define PCIE_A1_PADDR               (0x01000000)                /* 16 MB                        */
#define PCIE_A2_PADDR               (0x02000000)                /* 224 MB                       */
#define PCIE_A3_PADDR               (0x10000000)                /* 763 MB                     */
#define R8169_NIC_PADDR             (0x13000000)                /* r8169 NIC 1 MB               */
#define GRAPH_HOST_REGS_PADDR       (0x50000000)                /* 208 KB                       */
#define GRAPH_HOST_PADDR            (0x54000000)                /* 16 MB                        */
#define GPU_PADDR                   (0x57000000)                /* 144 MB                       */
#define UP_TAG_PADDR                (0x60000000)                /* 4 KB                         */
#define RSEM_PADDR                  (0x60001000)                /* 4 KB                         */
#define ASEM_PADDR                  (0x60002000)                /* 4 KB                         */
#define ARB_PRI_PADDR               (0x60003000)                /* 4 KB                         */
#define ICTLR_PADDR                 (0x60004000)                /* 4 KB, includes several       */
#define TMR_PADDR                   (0x60005000)                /* 4 Kb,  1 KB                  */
#define CLK_RESET_PADDR             (0x60006000)                /* 4 KB                         */
#define FLOW_CTRL_PADDR             (0x60007000)                /* 4 KB                         */
#define AHB_DMA_PADDR               (0x60008000)                /* 8 KB                         */
#define AHB_DMA_CH_PADDR            (0x60009000)                /* 4 KB 4 channels, 32 bytes    */
#define APB_DMA_PADDR               (0x60020000)                /* 16 KB                        */
#define APB_DMA_CH_PADDR            (0x60021000)                /* 4KB 32 channels, 64 bytes    */
#define SYS_REGS_PADDR              (0x6000c000)                /* 768 bytes + 2 KB             */
#define GPIO_PADDR                  (0x6000d000)                /* 8 GPIOs, 265 bytes each      */
#define VCP_PADDR                   (0x6000e000)                /* 4 KB                         */
#define VPUCQ_PADDR                 (0x60010000)                /* 256 Bytes                    */
#define BSEA_PADDR                  (0x60011000)                /* 4 KB                         */
#define IPATCH_PADDR                (0x6001d000)                /* 4 KB offset 0xc00, 1 KB      */
#define VDE_FRAMEID_PADDR           (0x60030000)                /* 16 KB, multiple              */
#define MISC_AUX_PADDR              (0x70000000)                /* 12 KB: USB AUX, SATA AUX, etc*/
#define MISC_PINMUX_PADDR           (0x70003000)                /* 4 KB                         */
#define UARTA_SYNC_PADDR            (0x70006000)                /* 12 KB, multiple              */
#define SYNC_NOR_PADDR              (0x70009000)                /* 4 KB                         */
#define PWM_PADDR                   (0x7000a000)                /* 4 KB, 256 bytes              */
#define MIPIHSI_PADDR               (0x7000b000)                /* 4 KB                         */
#define I2C_I2C4_PADDR              (0x7000c000)                /* 4 KB                         */
#define I2C5_SPI2B_6_PADDR          (0x7000d000)                /* 4 KB                         */
#define RTC_KFUSE_PADDR             (0x7000e000)                /* 4 KB                         */
#define FUSE_KFUSE_PADDR            (0x7000f000)                /* 4 KB                         */
#define LA_PADDR                    (0x70010000)                /* 8 KB                         */
#define SE_PADDR                    (0x70012000)                /* 8 KB                         */
#define TSENSOR_PADDR               (0x70014000)                /* 4 KB                         */
#define CEC_PADDR                   (0x70015000)                /* 4 KB                         */
#define ATOMICS_PADDR               (0x70016000)                /* 8 KB                         */
#define MC_PADDR                    (0x70019000)                /* 4 KB                         */
#define EMC_PADDR                   (0x7001b000)                /* 4 KB                         */
#define SATA_PADDR                  (0x70020000)                /* 64 KB                        */
#define HDA_PADDR                   (0x70030000)                /* 64 KB                        */
#define MIOBFM_PADDR                (0x70200000)                /* 64 KB                        */
#define AUDIO_PADDR                 (0x70300000)                /* 64 KB                        */
#define XUSB_HOST_PADDR             (0x70090000)                /* 36 KB                        */
#define XUSB_PADCTL_PADDR           (0x7009f000)                /* 4  KB                        */
#define XUSB_DEV_PADDR              (0x700d0000)                /* 40 KB                        */
#define DDS_PADDR                   (0x700a0000)                /* 8KB 4608 bytes               */
#define SDMMC_1_PADDR               (0x700b0000)                /* 4KB 512 bytes                */
#define SDMMC_1B_4_PADDR            (0x700b1000)                /* 60KB 512 bytes    each       */
#define SPEEDO_PADDR                (0x700c0000)                /* 32 KB                        */
#define SPEEDO_PMON_PADDR           (0x700c8000)                /* 32 KB                        */
#define SYSCTR0_PADDR               (0x700f0000)                /* 64 KB                        */
#define SYSCTR1_PADDR               (0x70100000)                /* 64 KB                        */
#define DP2_PADDR                   (0x700e0000)                /* 256 Bytes                    */
#define APB2JTAG_PADDR              (0x700e1000)                /* 512 Bytes                    */
#define SOC_THERM_PADDR             (0x700e2000)                /* 4 KB                         */
#define MIPI_CAL_PADDR              (0x700e3000)                /* 265 Bytes                    */
#define DVFS_PADDR                  (0x70110000)                /* 1 KB                         */
#define CLUSTER_CLK_PADDR           (0x70040000)                /* 256 KB                       */
#define CSITE_PADDR                 (0x70800000)                /* 2 MB                         */
#define PPCS_PADDR                  (0x7c000000)                /* 64 KB                        */
#define TZRAM_PADDR                 (0x7c010000)                /* 64 KB                        */
#define USB_PADDR                   (0x7d000000)                /* 8 KB region, 6 KB            */
#define USB2_PADDR                  (0x7d004000)                /* 8 KB region, 6 KB            */
#define USB3_PADDR                  (0x7d008000)                /* 8 KB region, 6 KB            */
#define UARTA_PADDR                 (0x70006000)

/* Additional memory outside the kernel window that we export as a device */
#define TK1_EXTRA_RAM_START    0xb0000000
#define TK1_EXTRA_RAM_SIZE     0x40000000

#endif
