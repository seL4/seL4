/*
 * Copyright 2016, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>

#ifdef CONFIG_TK1_SMMU

#include <types.h>
#include <plat/machine/hardware_gen.h>

#define IOASID_SIZE_BITS    7

/* The SystemMMU control registers are part of memory controller */

typedef struct {
    uint32_t    intstatus;                  /* 0x00     */
    uint32_t    intmask;                    /* 0x04     */
    uint32_t    err_status;                 /* 0x08     */
    uint32_t    err_adr;                    /* 0x0c     */
    uint32_t    smmu_config;                /* 0x10     */
    uint32_t    smmu_tlb_config;            /* 0x14     */
    uint32_t    smmu_ptc_config;            /* 0x18     */
    uint32_t    smmu_ptb_asid;              /* 0x1c     */
    uint32_t    smmu_ptb_data;              /* 0x20     */
    uint32_t    reserved0;                  /* 0x24     */
    uint32_t    reserved1;                  /* 0x28     */
    uint32_t    reserved2;                  /* 0x2c     */
    uint32_t    smmu_tlb_flush;             /* 0x30     */
    uint32_t    smmu_ptc_flush;             /* 0x34     */
    uint32_t    reserved3[124];
    uint32_t    smmu_translation_enable_0;  /* 0x228    */
    uint32_t    smmu_translation_enable_1;  /* 0x22c    */
    uint32_t    smmu_translation_enable_2;  /* 0x230    */
    uint32_t    smmu_translation_enable_3;  /* 0x234    */
    uint32_t    smmu_afi_asid;              /* 0x238    */
    uint32_t    smmu_avpc_asid;             /* 0x23c    */
    uint32_t    smmu_dc_asid;               /* 0x240    */
    uint32_t    smmu_dcb_asid;              /* 0x244    */
    uint32_t    reserved4;                  /* 0x248    */
    uint32_t    reserved5;                  /* 0x24c    */
    uint32_t    smmu_hc_asid;               /* 0x250    */
    uint32_t    smmu_hda_asid;              /* 0x254    */
    uint32_t    smmu_isp2_asid;             /* 0x258    */
    uint32_t    reserved6;                  /* 0x25c    */
    uint32_t    reserved7;                  /* 0x260    */
    uint32_t    smmu_msenc_asid;            /* 0x264    */
    uint32_t    smmu_nv_asid;               /* 0x268    */
    uint32_t    smmu_nv2_asid;              /* 0x26c    */
    uint32_t    smmu_ppcs_asid;             /* 0x270    */
    uint32_t    smmu_sata_asid;             /* 0x274    */
    uint32_t    reserved7_1;                /* 0x278    */
    uint32_t    smmu_vde_asid;              /* 0x27c    */
    uint32_t    smmu_vi_asid;               /* 0x280    */
    uint32_t    smmu_vic_asid;              /* 0x284    */
    uint32_t    smmu_xusb_host_asid;        /* 0x288    */
    uint32_t    smmu_xusb_dev_asid;         /* 0x28c    */
    uint32_t    reserved8;                  /* 0x290    */
    uint32_t    smmu_tsec_asid;             /* 0x294    */
    uint32_t    smmu_ppcs1_asid;            /* 0x298    */
    uint32_t    reserved9[217];
    uint32_t    smmu_tlb_set_sel_mask;      /* 0x600    */
    uint32_t    reserved10[237];
    uint32_t    smmu_ptc_flush_1;           /* 0x9b8    */
    uint32_t    reserved11[51];
    uint32_t    smmu_dc1_asid;              /* 0xa88    */
    uint32_t    reserved12;                 /* 0xa8c    */
    uint32_t    reserved13;                 /* 0xa90    */
    uint32_t    smmu_sdmmc1a_asid;          /* 0xa94    */
    uint32_t    smmu_sdmmc2a_asid;          /* 0xa98    */
    uint32_t    smmu_sdmmc3a_asid;          /* 0xa9c    */
    uint32_t    smmu_sdmmc4a_asid;          /* 0xaa0    */
    uint32_t    smmu_isp2b_asid;            /* 0xaa4    */
    uint32_t    smmu_gpu_asid;              /* 0xaa8    */
    uint32_t    smmu_gpub_asid;             /* 0xaac    */
    uint32_t    smmu_ppcs2_asid;            /* 0xab0    */
} tk1_mc_regs_t;

/* we start to allocate IO ASIDs from 1, and each module's ASID
 * is fixed (i.e. users are not allowed to dynamically allocate
 * ASIDs and assign them to devices).
 */
#define SMMU_FIRST_ASID     1
#define SMMU_AFI_ASID       1
#define SMMU_AVPC_ASID      2
#define SMMU_DC_ASID        3
#define SMMU_DCB_ASID       4
#define SMMU_HC_ASID        5
#define SMMU_HDA_ASID       6
#define SMMU_ISP2_ASID      7
#define SMMU_MSENC_ASID     8
#define SMMU_NV_ASID        9
#define SMMU_NV2_ASID       10
#define SMMU_PPCS_ASID      11
#define SMMU_SATA_ASID      12
#define SMMU_VDE_ASID       13
#define SMMU_VI_ASID        14
#define SMMU_VIC_ASID       15
#define SMMU_XUSB_HOST_ASID 16
#define SMMU_XUSB_DEV_ASID  17
#define SMMU_TSEC_ASID      18
#define SMMU_PPCS1_ASID     19
#define SMMU_DC1_ASID       20
#define SMMU_SDMMC1A_ASID   21
#define SMMU_SDMMC2A_ASID   22
#define SMMU_SDMMC3A_ASID   23
#define SMMU_SDMMC4A_ASID   24
#define SMMU_ISP2B_ASID     25
#define SMMU_GPU_ASID       26
#define SMMU_GPUB_ASID      27
#define SMMU_PPCS2_ASID     28
#define SMMU_LAST_ASID      28

#define ARM_PLAT_NUM_SMMU   28

#define SMMU_PD_INDEX_BITS        12
#define SMMU_PT_INDEX_BITS        12

#define SMMU_IOPD_INDEX_MASK    0xffc00000
#define SMMU_IOPD_INDEX_SHIFT   22
#define SMMU_IOPT_INDEX_MASK    0x3ff000
#define SMMU_IOPT_INDEX_SHIFT   12

inline static uint32_t plat_smmu_iopd_index(word_t io_address)
{
    uint32_t ret = (io_address & SMMU_IOPD_INDEX_MASK) >> SMMU_IOPD_INDEX_SHIFT;
    return ret;
}

inline static uint32_t plat_smmu_iopt_index(word_t io_address)
{
    uint32_t ret = (io_address & SMMU_IOPT_INDEX_MASK) >> SMMU_IOPT_INDEX_SHIFT;
    return ret;
}

inline static uint32_t plat_smmu_get_asid_by_module_id(uint32_t mid)
{
    if (mid < SMMU_FIRST_ASID || mid > SMMU_LAST_ASID) {
        return asidInvalid;
    }

    /* we have one-to-one mapping from module id to ASID */
    return mid;

}

int plat_smmu_init(void);

void plat_smmu_tlb_flush_all(void);

void plat_smmu_ptc_flush_all(void);

iopde_t *plat_smmu_lookup_iopd_by_asid(uint32_t asid);
void plat_smmu_handle_interrupt(void);

#else /* !CONFIG_TK1_SMMU */

/* dummy functions */
static inline void plat_smmu_handle_interrupt(void)
{
    return;
}

#endif /* CONFIG_TK1_SMMU */

