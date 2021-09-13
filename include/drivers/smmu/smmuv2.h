/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <plat/machine/devices_gen.h>

/*the paddr address of the TX2 SMMU*/
#define SMMU_TX2_PADDR                         0x12000000

#define SMMU_PAGE_4KB            0x1000
#define SMMU_PAGE_64KB           0x10000

/*the high-level physical address layout according to SMMU definition*/
#define SMMU_GLOBAL_SIZE(num_page, page_size)   ((num_page) * (page_size))
#define SMMU_CB_SIZE(num_page, page_size)       ((num_page) * (page_size))
#define SMMU_CB_BASE_PADDR(global_size)               (SMMU_TX2_PADDR + (global_size))

/*SMMU's physical address space layout, defined by SMMU v2 standard*/
#define SMMU_GR0_PADDR                          SMMU_TX2_PADDR
#define SMMU_GR1_PADDR(page_size)               ((SMMU_GR0_PADDR) + 1 * (page_size))
#define SMMU_GID_PADDR(page_size)               ((SMMU_GR0_PADDR) + 2 * (page_size))
#define SMMU_PM_PADDR(page_size)                ((SMMU_GR0_PADDR) + 3 * (page_size))
#define SMMU_SSD_PADDR(page_size)               ((SMMU_GR0_PADDR) + 4 * (page_size))
#define SMMU_CBn_PADDR(cb_base, n ,page_size)   ((cb_base) + n * (page_size))

/* SMMU's virtual address space layout in kernel address space,
 * mapped by boot code.*/
#define SMMU_GR0_PPTR                           SMMU_PPTR
#define SMMU_GR1_PPTR                          (SMMU_PPTR + 1 * (SMMU_PAGE_4KB))
#define SMMU_GID_PPTR                          (SMMU_PPTR + 2 * (SMMU_PAGE_4KB))
#define SMMU_PM_PPTR                           (SMMU_PPTR + 3 * (SMMU_PAGE_4KB))
#define SMMU_SSD_PPTR                          (SMMU_PPTR + 4 * (SMMU_PAGE_4KB))
#define SSMU_CB_BASE_PPTR                      (SMMU_PPTR + 5 * (SMMU_PAGE_4KB))
#define SMMU_CBn_BASE_PPTR(n)                  ((SSMU_CB_BASE_PPTR) + (n) * (SMMU_PAGE_4KB))

/*global register space 0 registers*/
#define SMMU_sCR0                                0x000
#define SMMU_SCR1                                0x004
#define SMMU_sCR2                                0x008
#define SMMU_sACR                                0x010
#define SMMU_IDR0                                0x020
#define SMMU_IDR1                                0x024
#define SMMU_IDR2                                0x028
#define SMMU_IDR3                                0x02c
#define SMMU_IDR4                                0x030
#define SMMU_IDR5                                0x034
#define SMMU_IDR6                                0x038
#define SMMU_IDR7                                0x03c
#define SMMU_sGFAR                               0x040
#define SMMU_sGFSR                               0x048
#define SMMU_sGFSRRESTORE                        0x04c
#define SMMU_sGFSYNR0                            0x050
#define SMMU_sGFSYNR1                            0x054
#define SMMU_sGFSYNR2                            0x058
#define SMMU_STLBIALL                            0x060
#define SMMU_TLBIVMID                            0x064
#define SMMU_TLBIALLNSNH                         0x068
#define SMMU_TLBIALLH                            0x06c
#define SMMU_sTLBGSYNC                           0x070
#define SMMU_sTLBGSTATUS                         0x074
#define SMMU_TLBIVAH                             0x078
#define SMMU_STLBIVALM                           0x0a0
#define SMMU_STLBIVAM                            0x0a8
#define SMMU_TLBIVALH64                          0x0b0
#define SMMU_TLBIVMIDS1                          0x0b8
#define SMMU_STLBIALLM                           0x0bc
#define SMMU_TLBIVAH64                           0x0c0
#define SMMU_sGATS1UR                            0x100
#define SMMU_sGATS1UW                            0x108
#define SMMU_sGATS1PR                            0x110
#define SMMU_sGATS1PW                            0x118
#define SMMU_sGATS12UR                           0x120
#define SMMU_sGATS12UW                           0x128
#define SMMU_sGATS12PR                           0x130
#define SMMU_sGATS12PW                           0x138
#define SMMU_sGPAR                               0x180
#define SMMU_sGATSR                              0x188

/*SMMU_SMRn, stream matching register 0 to 127*/
#define SMMU_SMRn(n)                             (0x800 + (n) * 0x4)

/*SMMU_S2CRn, stream-to-context register 0 to 127*/
#define SMMU_S2CRn(n)                            (0xc00 + (n) * 0x4)

/*global register space 1*/
/*SMMU_CBARn, context bank attribute register 0 to 127*/
#define SMMU_CBARn(n)                            (0x000 + (n) * 0x4)

/*SMMU_CBFRSYNRAn, context bank fault restricted syndrome register A 0 to 127*/
#define SMMU_CBFRSYNRAn(n)                       (0x400 + (n) * 0x4)

/*SMMU_CBA2Rn, context bank attribute registers 0 to 127*/
#define SMMU_CBA2Rn(n)                           (0x800 + (n) * 0x4)

/*stage 1 and stage 2 translation context bank address space*/
#define SMMU_CBn_SCTLR                           0x000
#define SMMU_CBn_ACTLR                           0x004
#define SMMU_CBn_RESUME                          0x008
#define SMMU_CBn_TCR2                            0x010
#define SMMU_CBn_TTBR0                           0x020
#define SMMU_CBn_TTBR1                           0x028
#define SMMU_CBn_TCR                             0x030
#define SMMU_CBn_CONTEXTIDR                      0x034

/*the SMMU_CBn_MAIRm registers are used for AArch32 Long-descriptor or the AArch64*/
#define SMMU_CBn_MAIR0                           0x038
#define SMMU_CBn_MAIR1                           0x03c
/*the SMMU_CBn_PRRR and SMMU_CBn_NMRR registers are used for AArch32*/
#define SMMU_CBn_PRRR                            0x038
#define SMMU_CBn_NMRR                            0x03c

#define SMMU_CBn_PAR                             0x050
#define SMMU_CBn_FSR                             0x058
#define SMMU_CBn_FSRRESTORE                      0x05c
#define SMMU_CBn_FAR                             0x060
#define SMMU_CBn_FSYNR0                          0x068
#define SMMU_CBn_FSYNR1                          0x06c
#define SMMU_CBn_IPAFAR                          0x070

#define SMMU_CBn_TLBIVA                          0x600
#define SMMU_CBn_TLBIVAA                         0x608
#define SMMU_CBn_TLBIASID                        0x610
#define SMMU_CBn_TLBIALL                         0x618
#define SMMU_CBn_TLBIVAL                         0x620
#define SMMU_CBn_TLBIVAAL                        0x628
#define SMMU_CBn_TLBIIPAS2                       0x630
#define SMMU_CBn_TLBIIPAS2L                      0x638
#define SMMU_CBn_TLBSYNC                         0x7f0
#define SMMU_CBn_TLBSTATUS                       0x7f4

/*SMMU_CR0 non-secure register 0 bit assignments*/
#define CR0_VMID16EN                            BIT(31)
#define CR0_HYPMODE                             BIT(30)
#define CR0_WACFG(v)                            ((v) & 0x3 << 26)
#define CR0_RACFG(v)                            ((v) & 0x3 << 24)
#define CR0_SHCFG(v)                            ((v) & 0x3 << 22)
#define CR0_SMCFCFG                             BIT(21)
#define CR0_MTCFG                               BIT(20)
#define CR0_MemAttr(v)                          ((v) & 0xf << 16)
#define CR0_BSU(v)                              ((v) & 0x3 << 14)
#define CR0_FB                                  BIT(13)
#define CR0_PTM                                 BIT(12)
#define CR0_VMIDPNE                             BIT(11)
#define CR0_USFCFG                              BIT(10)
#define CR0_GSE                                 BIT(9)
#define CR0_STALLD                              BIT(8)
#define CR0_TRANSIENTCFG(v)                     ((v) & 0x3 << 6)
#define CR0_GCFGFIE                             BIT(5)
#define CR0_GCFGFRE                             BIT(4)
#define CR0_EXIDENABLE                          BIT(3)
#define CR0_GFIE                                BIT(2)
#define CR0_GFRE                                BIT(1)
#define CR0_CLIENTPD                            BIT(0)
#define CR0_BSU_ALL                             3

/*SMMU_IDR0 (read only) read mask*/
#define IDR0_SES                                 BIT(31)
#define IDR0_S1TS                                BIT(30)
#define IDR0_S2TS                                BIT(29)
#define IDR0_NTS                                 BIT(28)
#define IDR0_SMS                                 BIT(27)
#define IDR0_ATOSNS                              BIT(26)
#define IDR0_PTFS                                (0x3 << 24)
#define IDR0_PTFS_VAL(v)                         ((v) >> 24)
#define IDR0_NUMIRPT                             (0xff << 16)
#define IDR0_NUMIRPT_VAL(v)                      ((v) >> 16)
#define IDR0_EXSMRGS                             BIT(15)
#define IDR0_CTTW                                BIT(14)
#define IDR0_BTM                                 BIT(13)
#define IDR0_NUMSIDB                             (0xf << 9)
#define IDR0_NUMSIDB_VAL(v)                      ((v) >> 9)
#define IDR0_EXIDS                               BIT(8)
#define IDR0_NUMSMRG                             0xff

/*PTFS bits*/
#define PTFS_AARCH32S_AARCH32L                   0x0
#define PTFS_AARCH32L_ONLY                       0x1
#define PTFS_NO_AARCH32                          0x2

/*SMMU_IDR1 (read only) read mask*/
#define IDR1_PAGESIZE                            BIT(31)
#define IDR1_NUMPAGENDXB                         (0x7 << 28)
#define IDR1_NUMPAGENDXB_VAL(v)                  ((v) >> 28)
#define IDR1_HAFDBS                              (0x3 << 24)
#define IDR1_NUMS2CB                             (0xff << 16)
#define IDR1_NUMS2CB_VAL(v)                      ((v) >> 16)
#define IDR1_SMCD                                 BIT(15)
#define IDR1_SSDTP                               (0x3 << 12)
#define IDR1_NUMSSDNDXB                          (0xf << 8)
#define IDR1_NUMCB                               0xff

/*SMMU_IDR2 (read only) read mask*/
#define IDR2_VMID16S                             BIT(15)
#define IDR2_PTFSV8_64                           BIT(14)
#define IDR2_PTFSV8_16                           BIT(13)
#define IDR2_PTFSV8_4                            BIT(12)
#define IDR2_UBS                                 (0xf << 8)
#define IDR2_UBS_VAL(v)                          ((v) >> 8)
#define IDR2_OAS                                 (0xf << 4)
#define IDR2_OAS_VAL(v)                          ((v) >> 4)
#define IDR2_IAS                                 0xf

/*OAS bits*/
#define IDR2_OAS_32                                   0x0
#define IDR2_OAS_36                                   0x1
#define IDR2_OAS_40                                   0x2
#define IDR2_OAS_42                                   0x3
#define IDR2_OAS_44                                   0x4
#define IDR2_OAS_48                                   0x5

/*IAS bits*/
#define IDR2_IAS_32                                   0x0
#define IDR2_IAS_36                                   0x1
#define IDR2_IAS_40                                   0x2
#define IDR2_IAS_42                                   0x3
#define IDR2_IAS_44                                   0x4
#define IDR2_IAS_48                                   0x5

/*SMMU_IDR7*/
#define IDR7_MAJOR                             (0xf << 4)
#define IDR7_MAJOR_VAL(v)                      ((v) >> 4)
#define IDR7_MINOR                             (0xf)

/*SMMU_sGFSR r/w bit mask, write 1 to clear*/
#define GFSR_MULTI                              BIT(31)
#define GFSR_UUT                                BIT(8)
#define GFSR_PF                                 BIT(7)
#define GFSR_EF                                 BIT(6)
#define GFSR_CAF                                BIT(5)
#define GFSR_UCIF                               BIT(4)
#define GFSR_UCBF                               BIT(3)
#define GFSR_SMCF                               BIT(2)
#define GFSR_USF                                BIT(1)
#define GFSR_ICF                                BIT(0)

/*SMMU_S2CRn, r/w bit mask for translation context*/
#define S2CR_TRANSIENTCFG_SET(v)            ((v) << 28)
#define S2CR_INSTCFG_SET(v)                 ((v) << 26)
#define S2CR_PRIVCFG_SET(v)                 ((v) << 24)
#define S2CR_WACFG_SET(v)                   ((v) << 22)
#define S2CR_RACFG_SET(v)                   ((v) << 20)
#define S2CR_NSCFG_SET(v)                   ((v) << 18)
#define S2CR_TYPE_SET(v)                    ((v) << 16)
#define S2CR_MemAttr_SET(v)                 ((v) << 12)
#define S2CR_MTCFG_SET(v)                   ((v) << 11)
#define S2CR_EXIDVALID_SET(v)               ((v) << 10)
#define S2CR_SHCFG_SET(v)                   ((v) << 8)
#define S2CR_CBNDX_SET(v)                   ((v) & 0xff)

/*SMMU_S2CRn PRIVCFG values*/
#define S2CR_PRIVCFG_DEFAULT                0x0

/*SMMU_S2CRn type values*/
#define S2CR_TYPE_CB                        0x0
#define S2CR_TYPE_BYPASS                    0x1
#define S2CR_TYPE_FAULT                     0x2

/*SMMU_SMRn, r/w bit mask for stream match*/
#define SMR_VALID_SET(v)                    ((v) << 31)
#define SMR_MASK_SET(v)                     ((v) & 0x7fff << 16)
#define SMR_ID_SET(v)                       ((v) & 0x7fff)
/*valid /invalid*/
#define SMR_VALID_EN                         0x1
#define SMR_VALID_DIS                        0x0

/*SMMU_ACR, SMMU-500*/
#define ACR_CACHE_LOCK                       BIT(26)
#define ACR_S2CRB_TLBEN                      BIT(10)
#define ACR_SMTNMB_TLBEN                     BIT(8)

/*SMMU_CBn_FSR, write 1 to clear*/
#define CBn_FSR_MULTI                        BIT(31)
#define CBn_FSR_SS                           BIT(30)
#define CBn_FSR_UUT                          BIT(8)
#define CBn_FSR_ASF                          BIT(7)
#define CBn_FSR_TLBLKF                       BIT(6)
#define CBn_FSR_TLBLMCF                      BIT(5)
#define CBn_FSR_EF                           BIT(4)
#define CBn_FSR_PF                           BIT(3)
#define CBn_FSR_AFF                          BIT(2)
#define CBn_FSR_TF                           BIT(1)

#define CBn_FSR_CLEAR_ALL                    (CBn_FSR_MULTI | CBn_FSR_SS | \
                                             CBn_FSR_UUT | CBn_FSR_ASF | CBn_FSR_TLBLKF | \
                                             CBn_FSR_TLBLMCF | CBn_FSR_EF | CBn_FSR_PF | \
                                             CBn_FSR_AFF | CBn_FSR_TF)

/*SMMU_CBn_ACTLR defined in SMMU500*/
#define CBn_ACTLR_CPRE                       BIT(1)
#define CBn_ACTLR_CMTLB                      BIT(0)

/*mask for invalidate all TLB entries, used by GR0 registers*/
#define SMMU_TLB_INVALL_MASK                 0xffffffff

/*mask for init the TLB sync msg*/
#define SMMU_TLB_SYNC_MASK                   0xffffffff

/*TLB sync status used in SMMU_sTLBGSTATUS and SMMU_CBn_TLBSTATUS*/
#define TLBSTATUS_GSACTIVE                  BIT(0)
/*the kernel loops N times before declear a TLB invalidation failure*/
#define TLBSYNC_LOOP                         1000



/*SMMU_CBARn*/
#define CBARn_TYPE_SET(v)                     ((v) << 16)
#define CBARn_BPSHCFG_SET(v)                  ((v) << 8)
#define CBARn_VMID_SET(v)                     ((v) & 0xff)

#define CBARn_TYPE_STAGE2                      0
#define CBARn_TYPE_STAGE1                      1   /*stage 1 with stage 2 by pass*/

#define CBARn_BPSHCFG_OUTER                    1
#define CBARn_BPSHCFG_INNER                    2
#define CBARn_BPSHCFG_NONE                     3

#define CBARn_MemAttr_SET(v)                  ((v) << 12)
#define MemAttr_OWB_IWB                        0xf /*outer & inner write-back cacheable*/

/*SMMU_CBA2Rn*/
#define CBA2Rn_VMID_SET(v)                 (((v) & 0xffff) << 16)
#define CBA2Rn_VA64_SET                    1

/*SMMU_CBn_TCR stage1/2 when SMMU_CBn_CBA2R.VA64 is 1*/
#define CBn_TCR_TG1_SET(v)                 ((v) << 30)
#define CBn_TCR_SH1_SET(v)                 ((v) << 28)
#define CBn_TCR_ORGN1_SET(v)               ((v) << 26)
#define CBn_TCR_IRGN1_SET(v)               ((v) << 24)
#define CBn_TCR_EPD1_DIS                   (1 << 23)     /*translation disabled for TTBR1 region*/
#define CBn_TCR_A1_EN                      (1 << 22)
#define CBn_TCR_T1SZ_SET(v)                (((v) & 0x3f) << 16)
#define CBn_TCR_TG0_SET(v)                 ((v) << 14)
#define CBn_TCR_SH0_SET(v)                 ((v) << 12)
#define CBn_TCR_ORGN0_SET(v)               ((v) << 10)
#define CBn_TCR_IRGN0_SET(v)               ((v) << 8)
#define CBn_TCR_T0SZ_SET(v)                ((v) & 0x3f)


#define CBn_TCR_TG_4K                      0
#define CBn_TCR_TG_64K                     1
#define CBn_TCR_TG_16K                     2

#define CBn_TCR_SH_NONE                    0
#define CBn_TCR_SH_OUTER                   2
#define CBn_TCR_SH_INNER                   3

#define CBn_TCR_GN_NCACHE                  0
#define CBn_TCR_GN_WB_WA_CACHE             1
#define CBn_TCR_GN_WT_CACHE                2
#define CBn_TCR_GN_WB_NWA_CACHE            3

/*SMMU_CBn_TCR stage 2 when SMMU_CBn_CBA2R.VA64 is 1*/
#define CBn_TCR_PASize_SET(v)               ((v) << 16)
#define CBn_TCR_SL0_SET(v)                  ((v) << 6)
#define CBn_TCR_SL0_4KB_L2                  0
#define CBn_TCR_SL0_4KB_L1                  1
#define CBn_TCR_SL0_4KB_L0                  2

/*SMMU_CBn_TCR2*/
#define CBn_TCR2_SEP_SET(v)                 ((v) << 15)
#define CBn_TCR2_AS_SET(v)                  ((v) << 4)
#define CBn_TCR2_PASize_SET(v)              (v)


#define CBn_TCR2_SEP_UPSTREAM_SIZE           7
#define CBn_TCR2_AS_16                       1
#define CBn_TCR2_PASize_32                   0
#define CBn_TCR2_PASize_36                   1
#define CBn_TCR2_PASize_40                   2
#define CBn_TCR2_PASize_42                   3
#define CBn_TCR2_PASize_44                   4
#define CBn_TCR2_PASize_48                   5



/*SMMU_CBn_TTBRm*/
#define CBn_TTBRm_ASID_SET(v)                (((v) & 0xffffull) << 48)

/*SMMU_CBn_MAIRm,
this is the same as the MAIR in core*/

/*MAIR0*/
#define CBn_MAIRm_ATTR_DEVICE_nGnRnE         0x00
#define CBn_MAIRm_ATTR_ID_DEVICE_nGnRnE      0

#define CBn_MAIRm_ATTR_DEVICE_nGnRE          0x04
#define CBn_MAIRm_ATTR_ID_DEVICE_nGnRE       1

#define CBn_MAIRm_ATTR_DEVICE_GRE            0xc
#define CBn_MAIRm_ATTR_ID_DEVICE_GRE         2

#define CBn_MAIRm_ATTR_NC                    0x44   /*non-cacheable normal memory*/
#define CBn_MAIRm_ATTR_ID_NC                 3      /*index for non-cacheable attribute*/

/*MAIR1*/
/*R/W allocate, normal memory, outer/innner write back*/
#define CBn_MAIRm_ATTR_CACHE                0xff
#define CBn_MAIRm_ATTR_ID_CACHE              0

/*8 bit per attribute*/
#define CBn_MAIRm_ATTR_SHIFT(n)              ((n) << 3)

/*SMMU_CBn_SCTLR*/
#define CBn_SCTLR_CFIE                         (1 << 6)
#define CBn_SCTLR_CFRE                         (1 << 5)
#define CBn_SCTLR_AFE                          (1 << 2)
#define CBn_SCTLR_TRE                          (1 << 1)
#define CBn_SCTLR_M                            1

/*SMMU_CBn_TLBIASID*/
#define CBn_TLBIASID_SET(v)                    ((v) & 0xffff)

/*SMMU_TLBIVMID*/
#define TLBIVMID_SET(v)                        ((v) & 0xffff)

/*SMMU_CBn_TLBIVA*/
#define CBn_TLBIVA_SET(asid, vaddr)            (((asid) & 0xffff) << 48 | ((vaddr) >> 12 & 0xfffffffffff))

/*SMMU_CBn_TLBIIPAS2*/
#define CBn_TLBIIPAS2_SET(vaddr)               ((vaddr) >> 12 & 0xfffffffff)

void smmu_cb_assign_vspace(word_t cb, vspace_root_t *vspace, asid_t asid);
void smmu_sid_bind_cb(word_t sid, word_t cb);
void plat_smmu_init(void);
void smmu_tlb_invalidate_all(void);
void smmu_tlb_invalidate_cb(int cb, asid_t asid);
void smmu_tlb_invalidate_cb_va(int cb, asid_t asid, vptr_t vaddr);
void smmu_cb_disable(word_t cb, asid_t asid);
void smmu_sid_unbind(word_t sid);
void smmu_read_fault_state(uint32_t *status, uint32_t *syndrome_0, uint32_t *syndrome_1);
void smmu_clear_fault_state(void);
void smmu_cb_read_fault_state(int cb, uint32_t *status, word_t *address);
void smmu_cb_clear_fault_state(int cb);
