/*
 * Copyright 2018, DornerWorks
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DORNERWORKS_GPL)
 */

#include <types.h>
#include <config.h>

#ifdef CONFIG_ARM_SMMU_V2

#include <arch/machine/smmu.h>
#include <linker.h>
#include <plat/machine/hardware.h>
#include <object/structures.h>

#define ARM_SMMU_RD(base, offset) *(volatile uint32_t*)((word_t)(base) \
                                                        + (offset))
#define ARM_SMMU_WR(val, base, offset) *(volatile uint32_t*)((word_t)(base) \
                                                             + (offset)) = (val)

#define ARM_SMMU_GR0 (uint32_t*)SMMU_PPTR
#define ARM_SMMU_GR1 (uint32_t*)(SMMU_PPTR + (1 << 12))

#ifdef DEBUG
/* Identification registers */
#define ARM_SMMU_ID_REGS      8
#define ARM_SMMU_GR0_ID0      0x20
#define ARM_SMMU_GR0_ID1      0x24
#define ARM_SMMU_GR0_ID2      0x28
#define ARM_SMMU_GR0_ID3      0x2c
#define ARM_SMMU_GR0_ID4      0x30
#define ARM_SMMU_GR0_ID5      0x34
#define ARM_SMMU_GR0_ID6      0x38
#define ARM_SMMU_GR0_ID7      0x3c
#endif

#define ARM_SMMU_GR0_sGFSR    0x48
#define ARM_SMMU_GR0_sGFSYNR0 0x50
#define ARM_SMMU_GR0_sGFSYNR1 0x54
#define ARM_SMMU_GR0_sGFSYNR2 0x58

#define ARM_SMMU_GR0_SMR(n) (0x800 + ((n) << 2))
#define SMR_VALID           (1 << 31)
#define SMR_ID_SHIFT        0
#define SMR_ID_MASK         0x7fff

#define ARM_SMMU_GR0_S2CR(n) (0xc00 + ((n) << 2))
#define S2CR_TYPE_SHIFT 16
#define S2CR_TYPE_TRANS  (0 << S2CR_TYPE_SHIFT)
#define S2CR_TYPE_BYPASS (1 << S2CR_TYPE_SHIFT)
#define S2CR_TYPE_FAULT  (2 << S2CR_TYPE_SHIFT)

#define ARM_SMMU_CB_BASE (SMMU_PPTR + (SMMU_SIZE >> 1))
#define ARM_SMMU_CB(n)   (uint32_t*)(ARM_SMMU_CB_BASE + (((n) * (1 << 12))))

#define ARM_SMMU_CB_SCTLR    0x0
#define ARM_SMMU_CB_RESUME   0x8
#define ARM_SMMU_CB_TTBCR2   0x10
#define ARM_SMMU_CB_TTBR0_LO 0x20
#define ARM_SMMU_CB_TTBR0_HI 0x24
#define ARM_SMMU_CB_TTBCR    0x30
#define ARM_SMMU_CB_S1_MAIR0 0x38
#define ARM_SMMU_CB_FSR      0x58
#define ARM_SMMU_CB_FAR_LO   0x60
#define ARM_SMMU_CB_FAR_HI   0x64
#define ARM_SMMU_CB_FSYNR0   0x68

#define SCTLR_S1_ASIDPNE (1 << 12)
#define SCTLR_CFCFG      (1 << 7)
#define SCTLR_CFIE       (1 << 6)
#define SCTLR_CFRE       (1 << 5)
#define SCTLR_AFE        (1 << 2)
#define SCTLR_TRE        (1 << 1)
#define SCTLR_M          (1 << 0)
#define SCTLR_EAE_SBOP   (SCTLR_AFE | SCTLR_TRE)

#define TTBRn_HI_ASID_SHIFT 16

#define TTBCR_EAE    (1   << 31)

#define TTBCR_TG0_SHIFT 14
#define TTBCR_TG0_MASK  0x3
#define TTBCR_TG0_4K    (0x0 << TTBCR_TG0_SHIFT)

#define TTBCR_SH0_SHIFT 12
#define TTBCR_SH0_MASK  0x3
#define TTBCR_SH_NS     0

#define TTBCR_ORGN0_SHIFT 10
#define TTBCR_IRGN0_SHIFT 8
#define TTBCR_RGN_MASK    0x3
#define TTBCR_RGN_WBWA    1

#define TTBCR_SHT_IRGN0_WBWA (0 << 6)
#define TTBCR_SHT_NOS        (0 << 5)
#define TTBCR_SHT_RGN_WBWA   (0x1 << 3)
#define TTBCR_SHT_NS         (0 << 1)
#define TTBCR_SHT_IRGN1_WBWA 1

#define TTBCR_SL0_SHIFT       6
#define TTBCR_SL0_LVL_2       0
#define TTBCR_SL0_LVL_1       1
#define TTBCR_SL0_LVL_0       2

/* non-shareable WBWA tables */

#define TTBCR_T0SZ_SHIFT 0
#define TTBCR_PA_SHIFT   16
#define TTBCR_ADDR_32    (0 << TTBCR_PA_SHIFT)
#define TTBCR_ADDR_48    (5 << TTBCR_PA_SHIFT)

#define TTBCR2_SEP_SHIFT 15
#define TTBCR2_ADDR_32   0
#define TTBCR2_ADDR_48   5
#define TTBCR2_ADDR_UBS  0x7

#define MAIR_ATTR_SHIFT(n)  ((n) << 3)
#define MAIR_ATTR_NC        0x44
#define MAIR_ATTR_IDX_NC    0

/* Attribute registers */
#define ARM_SMMU_GR1_CBAR(n) (0x0 + ((n) << 2))
#define CBAR_TYPE_SHIFT       16
#define CBAR_S1_BPSHCFG_SHIFT 8
#define CBAR_S1_BPSHCFG_MASK  3
#define CBAR_S1_BPSHCFG_NSH   3
#define CBAR_S1_MEMATTR_SHIFT 12
#define CBAR_S1_MEMATTR_MASK  0xf
#define CBAR_S1_MEMATTR_WB    0xf
#define CBAR_TYPE_MASK        0x3
#define CBAR_TYPE_S2_TRANS             (0 << CBAR_TYPE_SHIFT)
#define CBAR_TYPE_S1_TRANS_S2_BYPASS   (1 << CBAR_TYPE_SHIFT)

#define ARM_SMMU_GR1_CBA2R(n) (0x800 + ((n) << 2))
#define CBA2R_RW64_32BIT      0
#define CBA2R_RW64_64BIT      1

/* TLB Registers */
#define ARM_SMMU_GR0_STLBIALL    0x60
#define ARM_SMMU_GR0_TLBIVMID    0x64
#define ARM_SMMU_GR0_TLBIALLNSNH 0x68
#define ARM_SMMU_GR0_TLBIALLH    0x6c
#define ARM_SMMU_GR0_sTLBGSYNC   0x70
#define ARM_SMMU_GR0_sTLBGSTATUS 0x74
#define sTLBGSTATUS_GSACTIVE     (1 << 0)
#define TLB_LOOP_TIMEOUT         1000000

/* Configuration */
#define ARM_SMMU_GR0_sCR0 0x0
#define sCR0_CLIENTPD     (1 << 0)
#define sCR0_GFRE         (1 << 1)
#define sCR0_GFIE         (1 << 2)
#define sCR0_GCFGFRE      (1 << 4)
#define sCR0_GCFGFIE      (1 << 5)
#define sCR0_USFCFG       (1 << 10)
#define sCR0_PTM          (1 << 12)
#define sCR0_FB           (1 << 13)
#define sCR0_BSU_SHIFT    14
#define sCR0_BSU_MASK     0x3

/* Faults */
#define FSR_MULTI  (1 << 31)
#define FSR_SS     (1 << 30)
#define FSR_UUT    (1 << 8)
#define FSR_ASF    (1 << 7)
#define FSR_TLBLKF (1 << 6)
#define FSR_TLBMCF (1 << 5)
#define FSR_EF     (1 << 4)
#define FSR_PF     (1 << 3)
#define FSR_AFF    (1 << 2)
#define FSR_TF     (1 << 1)

#define FSR_IGN (FSR_AFF | FSR_ASF | \
                 FSR_TLBMCF | FSR_TLBLKF)
#define FSR_FAULT (FSR_MULTI | FSR_SS | FSR_UUT | \
                   FSR_EF | FSR_PF | FSR_TF | FSR_IGN)

#define FSYNR0_WNR (1 << 4)

#define RESUME_TERM  1

#define SMMU_ASID_IDX(asid) ((asid) - 1)
#define SMMU_IDX_ASID(idx) ((idx) + 1)

#ifdef CONFIG_ARCH_AARCH64
#ifdef CONFIG_SMMU_S1_TRANS
#define SMMU_SIZE_BITS seL4_PGDBits
#else
#define SMMU_SIZE_BITS seL4_PUDBits
#endif
#else
#define SMMU_SIZE_BITS seL4_PDBits
#endif

static char smmu_pds[ARM_PLAT_NUM_CB][BIT(SMMU_SIZE_BITS)] ALIGN(BIT(SMMU_SIZE_BITS));

struct smmu_entry
{
   iopde_t* iopd;
   uint16_t sid;
   bool_t in_use;
};

static struct smmu_entry smmu_entries[ARM_PLAT_NUM_CB];

static void plat_smmu_tlb_sync(void)
{
   uint32_t *gr0 = (uint32_t*)ARM_SMMU_GR0;
   uint32_t reg;

   int count = 0;

   ARM_SMMU_WR(0, gr0, ARM_SMMU_GR0_sTLBGSYNC);
   reg = ARM_SMMU_RD(gr0, ARM_SMMU_GR0_sTLBGSTATUS);
   while ((reg & sTLBGSTATUS_GSACTIVE) != 0)
   {
      if (count == TLB_LOOP_TIMEOUT)
      {
         printf("TLB sync timed out.\n");
         return;
      }

      ++count;
      reg = ARM_SMMU_RD(gr0, ARM_SMMU_GR0_sTLBGSTATUS);
   }
}

static void plat_smmu_tlb_inval_all(void)
{
   uint32_t *gr0 = ARM_SMMU_GR0;

   ARM_SMMU_WR(0, gr0, ARM_SMMU_GR0_STLBIALL);

   ARM_SMMU_WR(0, gr0, ARM_SMMU_GR0_TLBIALLNSNH);

   plat_smmu_tlb_sync();
}

void plat_smmu_tlb_flush_all(void)
{
   plat_smmu_tlb_inval_all();

   dsb();
}

static void smmu_clear_cb(uint8_t cbidx)
{
   uint32_t* gr0 = ARM_SMMU_GR0;
   uint32_t *cb = ARM_SMMU_CB(cbidx);
   uint32_t reg;

   /* Clear stream match id */
   ARM_SMMU_WR(0, gr0, ARM_SMMU_GR0_SMR(cbidx));

   /* Set default match behavior */
#ifdef CONFIG_ARM_SMMU_UNKNOWN_STREAM_FAULT
   reg = S2CR_TYPE_FAULT;
#else
   reg = S2CR_TYPE_BYPASS;
#endif

   /* Clear stream to context bank mapping */
   ARM_SMMU_WR(reg, gr0, ARM_SMMU_GR0_S2CR(cbidx));

   /* Clear control register */
   ARM_SMMU_WR(0, cb, ARM_SMMU_CB_SCTLR);

   /* Clear faults */
   reg = ARM_SMMU_RD(cb, ARM_SMMU_CB_FSR);
   ARM_SMMU_WR(reg, cb, ARM_SMMU_CB_FSR);

   /* Clear TTBR0 */
   ARM_SMMU_WR(0, cb, ARM_SMMU_CB_TTBR0_LO);
   ARM_SMMU_WR(0, cb, ARM_SMMU_CB_TTBR0_HI);

   plat_smmu_tlb_inval_all();
}

#ifdef DEBUG
static void print_smmu_id_regs(void)
{
   uint32_t *gr0 = ARM_SMMU_GR0;
   uint32_t id;
   uint32_t reg = ARM_SMMU_GR0_ID0;

   int i;

   for (i = 0; i < ARM_SMMU_ID_REGS; ++i)
   {
      id = ARM_SMMU_RD(gr0, reg);
      reg += sizeof(uint32_t);

      printf("SMMU ID%d:  0x%x\n", i, id);
   }
}
#endif

BOOT_CODE int plat_smmu_init(void)
{
   uint32_t *gr0 = ARM_SMMU_GR0;
   uint32_t reg;

   int i = 0;

#ifdef DEBUG
   print_smmu_id_regs();
#endif

   /* Allocate a page directories per context bank */
   for (i = 0; i < ARM_PLAT_NUM_CB; ++i)
   {
      iopde_t *pd = (iopde_t *)smmu_pds[i];
      if (!pd)
      {
         printf("Failed to allocate SMMU IOPageDirectory for ASID %d\n", i);
         return 0;
      }

      /* put the PD in the lookup table */
      smmu_entries[i].iopd   = pd;
      smmu_entries[i].sid    = 0xFF;
      smmu_entries[i].in_use = false;

      memset(pd, 0, BIT(SMMU_SIZE_BITS));
      cleanCacheRange_RAM((word_t)pd, ((word_t)pd + BIT(SMMU_SIZE_BITS)),
                          addrFromPPtr(pd));

      /* Don't add to context bank, yet. */
   }
   printf("Total %d IOASID set up\n", i);

   /* clear all faults */
   reg = ARM_SMMU_RD(gr0, ARM_SMMU_GR0_sGFSR);
   ARM_SMMU_WR(reg, gr0, ARM_SMMU_GR0_sGFSR);

   /* clear all context banks */
   for (i = 0; i < ARM_PLAT_NUM_CB; ++i)
   {
      smmu_clear_cb(i);
   }

   /* Configure SMMU */
   reg = ARM_SMMU_RD(gr0, ARM_SMMU_GR0_sCR0);

   /* Enable fault reporting */
   reg = (sCR0_GFRE | sCR0_GFIE | sCR0_GCFGFRE | sCR0_GCFGFIE);

   /* Disable TLB broadcasting. */
   reg |= sCR0_PTM;
   reg &= ~sCR0_FB;

   /* Default to either fault or bypass for unknown stream IDs depending on configuration */
#ifdef CONFIG_ARM_SMMU_UNKNOWN_STREAM_FAULT
   reg |= sCR0_USFCFG;
#else
   reg &= ~sCR0_USFCFG;

   /*
      Make sure barrier upgrages are disabled, because this register's reset value is unknown
      and it affects bypass transactions.
   */
   reg &= ~(sCR0_BSU_MASK << sCR0_BSU_SHIFT);
#endif

   /* Enable client access */
   reg &= ~sCR0_CLIENTPD;

   ARM_SMMU_WR(reg, gr0, ARM_SMMU_GR0_sCR0);

   return ARM_PLAT_NUM_CB;
}

static void smmu_alloc_cb(uint8_t cbidx, uint16_t sid, iopde_t* iopd)
{
   uint32_t* gr0 = ARM_SMMU_GR0;
   uint32_t* gr1 = ARM_SMMU_GR1;
   uint32_t *cb = ARM_SMMU_CB(cbidx);

   uint32_t asid = SMMU_IDX_ASID(cbidx);

   word_t addr;

   uint32_t reg;

   reg = ARM_SMMU_RD(gr1, ARM_SMMU_GR1_CBAR(cbidx));

#ifdef CONFIG_SMMU_S1_TRANS
   /* Weakest settings.  Will be overridden by the page-table entries */
   reg = ((CBAR_S1_BPSHCFG_NSH << CBAR_S1_BPSHCFG_SHIFT)
          | (CBAR_S1_MEMATTR_WB << CBAR_S1_MEMATTR_SHIFT)
          | CBAR_TYPE_S1_TRANS_S2_BYPASS);
#else
   /* We're not currently using VMID for anything, but we could in the future. */
   reg = CBAR_TYPE_S2_TRANS;
#endif

   ARM_SMMU_WR(reg, gr1, ARM_SMMU_GR1_CBAR(cbidx));

#ifdef CONFIG_ARCH_AARCH64
   reg = CBA2R_RW64_64BIT;
#else
   reg = CBA2R_RW64_32BIT;
#endif

   ARM_SMMU_WR(reg, gr1, ARM_SMMU_GR1_CBA2R(cbidx));

   /* TTBCR2 only exists for Stage 1 translations */
#ifdef CONFIG_SMMU_S1_TRANS
   /* Output Size = 49 bits.  Let the SMMU configuration dictate sign-extension */
   reg = (TTBCR2_ADDR_UBS << TTBCR2_SEP_SHIFT);

#ifdef CONFIG_ARCH_AARCH64
   /* Limit SMMU to 32 bits */
   reg |= TTBCR2_ADDR_32;
#elif defined(CONFIG_SMMU_S1_TRANS)
   reg |= TTBCR2_ADDR_48;
#endif

   ARM_SMMU_WR(reg, cb, ARM_SMMU_CB_TTBCR2);
#endif

   addr = pptr_to_paddr(iopd);
   reg = (uint32_t)(addr & 0xFFFFFFFF);

   /* TTBR0 */
   ARM_SMMU_WR(reg, cb, ARM_SMMU_CB_TTBR0_LO);

#ifdef CONFIG_ARCH_AARCH64
   reg = (uint32_t)(addr >> 32);
#else
   reg = 0;
#endif

   reg |= (asid << TTBRn_HI_ASID_SHIFT);

   ARM_SMMU_WR(reg, cb, ARM_SMMU_CB_TTBR0_HI);

#ifndef CONFIG_ARCH_AARCH64
   /*
      Trying to deal with short descriptors as well would complicate things,
      so just use long ones.
   */
   reg = TTBCR_EAE;
#else
   reg = 0;
#endif

   /* non-shareable WBWA tables */
   reg |= ((TTBCR_SH_NS << TTBCR_SH0_SHIFT)
           | (TTBCR_RGN_WBWA << TTBCR_ORGN0_SHIFT)
           | (TTBCR_RGN_WBWA << TTBCR_IRGN0_SHIFT));

#ifdef CONFIG_ARCH_AARCH64
   reg &= ~(TTBCR_TG0_MASK << TTBCR_TG0_SHIFT);
   reg |= TTBCR_TG0_4K;

#ifndef CONFIG_SMMU_S1_TRANS
   reg |= TTBCR_ADDR_48;
#endif
#endif

#ifdef CONFIG_ARCH_AARCH64
#ifdef CONFIG_SMMU_S1_TRANS
   /* Input = 48 bits.  Limit translation to this range with T0SZ. */
   reg |= 16;  /* 64 - 48 */
#else

#ifdef AARCH64_VSPACE_S2_START_L1
   reg |= (TTBCR_SL0_LVL_1 << TTBCR_SL0_SHIFT);

   /*
      We're only using 39 IPA bits, because we'd need concatenated tables with the full 40 bits.

      Limit translation to this range with T0SZ.
   */
   reg |= 25;  /* 64 - 39 */
#else
   reg |= (TTBCR_SL0_LVL_0 << TTBCR_SL0_SHIFT);
#endif /* CONFIG_START_L1 */

#endif /* CONFIG_SMMU_S1_TRANS */

#else
   /* Don't touch T0SZ for 32-bit, since we only use TTBR0. */
#endif /* CONFIG_ARCH_AARCH64 */

   ARM_SMMU_WR(reg, cb, ARM_SMMU_CB_TTBCR);

#ifdef CONFIG_SMMU_S1_TRANS
   /* MAIR0 (non-cacheable) */
   reg = (MAIR_ATTR_NC << MAIR_ATTR_SHIFT(MAIR_ATTR_IDX_NC));
   ARM_SMMU_WR(reg, cb, ARM_SMMU_CB_S1_MAIR0);
#endif

   /* SCTLR */
#ifdef CONFIG_SMMU_S1_TRANS
   reg = SCTLR_S1_ASIDPNE;
#else
   reg = 0;
#endif

   reg |= SCTLR_CFCFG | SCTLR_CFIE | SCTLR_CFRE | SCTLR_M | SCTLR_EAE_SBOP;

   ARM_SMMU_WR(reg, cb, ARM_SMMU_CB_SCTLR);


   /* Match on Stream ID */
   reg = SMR_VALID | (sid << SMR_ID_SHIFT);
   ARM_SMMU_WR(reg, gr0, ARM_SMMU_GR0_SMR(cbidx));

   /* Stream ID to CB ID */
   reg = S2CR_TYPE_TRANS | cbidx;
   ARM_SMMU_WR(reg, gr0, ARM_SMMU_GR0_S2CR(cbidx));
}

iopde_t* plat_smmu_lookup_iopd_by_asid(uint32_t asid)
{
   struct smmu_entry* fentry;
   int idx = SMMU_ASID_IDX(asid);

   if (idx > ARM_PLAT_NUM_CB)
   {
      return NULL;
   }

   fentry = &smmu_entries[idx];
   return fentry->iopd;
}

void plat_smmu_release_asid(uint32_t asid)
{
   struct smmu_entry* fentry;
   int idx = SMMU_ASID_IDX(asid);
   iopde_t* iopd;

   if (idx > ARM_PLAT_NUM_CB)
   {
      return;
   }

   fentry = &smmu_entries[idx];
   iopd = fentry->iopd;

   memset(iopd, 0, BIT(SMMU_SIZE_BITS));
   cleanCacheRange_RAM((word_t)iopd, ((word_t)iopd + BIT(SMMU_SIZE_BITS)),
                       addrFromPPtr(iopd));

   fentry->sid    = 0xFF;
   fentry->in_use = false;

   smmu_clear_cb(idx);

   return;
}

uint32_t plat_smmu_get_asid_by_stream_id(uint16_t sid)
{
   int idx;
   int eslot = -1;
   struct smmu_entry* pentry = smmu_entries;
   struct smmu_entry* fentry = NULL;

   for (idx = 0; idx < ARM_PLAT_NUM_CB; ++idx, ++pentry)
   {
      if (pentry->in_use)
      {
         if (pentry->sid == sid)
         {
            return SMMU_IDX_ASID(idx);
         }
      }
      else if (eslot < 0)
      {
         eslot = idx;
      }
   }

   /* Allocate new context bank */
   if (eslot >= 0)
   {
      fentry = &smmu_entries[eslot];

      fentry->sid    = sid;
      fentry->in_use = true;

      smmu_alloc_cb(eslot, sid, fentry->iopd);

      return SMMU_IDX_ASID(eslot);
   }

   printf("Ran out of SMMU context banks.\n");
   halt();

   return asidInvalid;
}

void plat_smmu_handle_interrupt(void)
{
   uint32_t *gr0 = ARM_SMMU_GR0;

   int i = 0;

   uint32_t *cb;
   uint32_t fsr;
   uint32_t fsynr;
   word_t far;

   /* Check for global faults */
   fsr = ARM_SMMU_RD(gr0, ARM_SMMU_GR0_sGFSR);

   if (fsr)
   {
      uint32_t gfsynr0, gfsynr1, gfsynr2;
      gfsynr0 = ARM_SMMU_RD(gr0, ARM_SMMU_GR0_sGFSYNR0);
      gfsynr1 = ARM_SMMU_RD(gr0, ARM_SMMU_GR0_sGFSYNR0);
      gfsynr2 = ARM_SMMU_RD(gr0, ARM_SMMU_GR0_sGFSYNR0);

      /* This is bad */
      printf("SMMU global fault:  GFSR 0x%08x, GFSYNR0 0x%08x, "
             "GFSYNR1 0x%08x, GFSYNR2 0x%08x\n",
             fsr, gfsynr0, gfsynr1, gfsynr2);

      ARM_SMMU_WR(fsr, gr0, ARM_SMMU_GR0_sGFSR);
   }

   /* Check if any of the individual contexts reported a fault */
   for (i = 0; i < ARM_PLAT_NUM_CB; ++i)
   {
      cb = ARM_SMMU_CB(i);
      fsr = ARM_SMMU_RD(cb, ARM_SMMU_CB_FSR);

      if (fsr & FSR_IGN)
      {
         printf("Strange SMMU context fault, Bank %d:  FSR 0x%08x\n", i, fsr);
      }
      else if (fsr & FSR_FAULT)
      {
         fsynr = ARM_SMMU_RD(cb, ARM_SMMU_CB_FSYNR0);

         far = ARM_SMMU_RD(cb, ARM_SMMU_CB_FAR_LO);
#ifdef CONFIG_ARCH_AARCH64
         far |= ((word_t)ARM_SMMU_RD(cb, ARM_SMMU_CB_FAR_HI) << 32);
#endif
         printf("SMMU context fault on %s, Bank %d:  FSYNR 0x%08x, FAR 0x%08lx\n",
                ((fsynr & FSYNR0_WNR) ? "write" : "read"),
                i, fsynr, far);
      }

      ARM_SMMU_WR(fsr, cb, ARM_SMMU_CB_FSR);

      /* Terminate stalled transactions. */
      if (fsr & FSR_SS)
      {
         ARM_SMMU_WR(RESUME_TERM, cb, ARM_SMMU_CB_RESUME);
      }
   }
}
#endif
