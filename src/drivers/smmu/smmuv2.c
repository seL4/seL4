/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */
#include <config.h>
#include <types.h>
#include <plat/machine/devices_gen.h>
#include <drivers/smmu/smmuv2.h>

/*supported stages of translations*/
#define STAGE1_TRANS           (1 << 0)
#define STAGE2_TRANS           (1 << 1)
#define NESTED_TRANS           (1 << 2)
/*supported translation table formats*/
#define AARCH32S_FMT           (1 << 0)
#define AARCH32L_FMT           (1 << 1)
#define NO_AARCH32_FMT         (1 << 2)
#define TRANS_PAGES_4KB        (1 << 3)
#define TRANS_PAGES_16KB       (1 << 4)
#define TRANS_PAGES_64KB       (1 << 5)

struct  smmu_feature {
	bool_t stream_match;              /*stream match register funtionality included*/
	bool_t trans_op;                  /*address translation operations supported*/
	bool_t cotable_walk;              /*coherent translation table walk*/
	bool_t broadcast_tlb;             /*broadcast TLB maintenance*/
	bool_t vmid16;                    /*16 bits VMIDs are supported*/
	uint32_t supported_trans;         /*supported translation stages*/
	uint32_t supported_fmt;           /*supported translation formats*/
	uint32_t num_cfault_ints;         /*supported number of context fault interrupts*/
	uint32_t num_stream_ids;          /*number of stream IDs*/
	uint32_t num_stream_map_groups;   /*num stream mapping register groups*/
	uint32_t smmu_page_size;          /*page size in SMMU register address space*/
	uint32_t smmu_num_pages;          /*number of pages in global or context bank address space*/
	uint32_t num_s2_cbanks;           /*cbanks that support stage 2 only*/
	uint32_t num_cbanks;              /*total number of context banks*/
	uint32_t pa_size;                 /*PA address size*/
	uint32_t ipa_size;                /*IPA address size*/
	pptr_t cb_base;                   /*base of context bank address space*/
};

static struct smmu_feature smmu_dev_knowledge;

static inline uint32_t smmu_read_reg32(pptr_t base, uint32_t index)
{
	return *(volatile uint32_t*)(base + index);
}

static inline void smmu_write_reg32(pptr_t base, uint32_t index, uint32_t val)
{
	*(volatile uint32_t*) (base + index) = val;
}

static inline uint64_t smmu_read_reg64(pptr_t base, uint32_t index)
{
	return *(volatile uint64_t*) (base + index);
}

static inline void smmu_write_reg64(pptr_t base, uint32_t index, uint64_t val)
{
	*(volatile uint64_t *) (base + index) = val;
}

static void smmu_tlb_sync(pptr_t base, uint32_t sync, uint32_t status)
{
	int count = 0;
	smmu_write_reg32(base, sync, SMMU_TLB_SYNC_MASK);
	while (count < TLBSYNC_LOOP) {
		/*pulling the active flag, reading the TLB command state.*/
		if (!(smmu_read_reg32(base, status) & TLBSTATUS_GSACTIVE))
			break;
		count++;
	}
}

BOOT_CODE static void smmu_mapping_init(void)
{
	/*Creating mapping for the rest of SMMU address space.
	 * the code assumes registers in each SMMU page are located in a 4K page
	 * even though the alignement of the (physical) pages can be 64K.
	 * We make this assumption to compact the SMMU virtual address window.*/

	/* This is a temporary solution. A correct solution should be adjust
	 * the virutal address space layout of the kernel, leaving enough virtual
	 * address space to SMMU windows. For example, SMMU on TX2 requires a 8M space
	 * in total, including those empty areas resulted from the 64K alignment.
	 * Also, kernel requires device space to be configured statically. To
	 * support populate device space using HW config, we need to modify
	 * kernel_frame_t and map_kernel_frame, allowing devices mapped in a
	 * seperate page table using HW config.*/

	/*the current implementation has been only tested on the TX2 platform*/

	/*init the GR1 region, start: smmu_pptr + 4K, size 4K*/
	map_kernel_frame(SMMU_GR1_PADDR(smmu_dev_knowledge.smmu_page_size),
                     SMMU_GR1_PPTR,
                     VMKernelOnly,
                     vm_attributes_new(true, false, false));
	/*GID registers*/
	map_kernel_frame(SMMU_GID_PADDR(smmu_dev_knowledge.smmu_page_size),
                     SMMU_GID_PPTR,
                     VMKernelOnly,
                     vm_attributes_new(true, false, false));
	/*PM registers*/
	map_kernel_frame(SMMU_PM_PADDR(smmu_dev_knowledge.smmu_page_size),
                     SMMU_PM_PPTR,
                     VMKernelOnly,
                     vm_attributes_new(true, false, false));
	/*SSD registers*/
	map_kernel_frame(SMMU_SSD_PADDR(smmu_dev_knowledge.smmu_page_size),
                     SMMU_SSD_PPTR,
                     VMKernelOnly,
                     vm_attributes_new(true, false, false));
	/*map the context banks, each bank maps to a 4K page*/
	for (int i = 0; i < smmu_dev_knowledge.num_cbanks; i++) {
           map_kernel_frame(SMMU_CBn_PADDR(smmu_dev_knowledge.cb_base, i, smmu_dev_knowledge.smmu_page_size),
                            SMMU_CBn_BASE_PPTR(i),
                            VMKernelOnly,
                            vm_attributes_new(true, false, false));
       }
}

BOOT_CODE static void smmu_config_prob(void)
{
	uint32_t reg, field;
	/*ID0*/
	reg = smmu_read_reg32(SMMU_GR0_PPTR, SMMU_IDR0);
	/*stages supported*/
	if (reg & IDR0_S1TS)
		smmu_dev_knowledge.supported_trans |= STAGE1_TRANS;
	if (reg & IDR0_S2TS)
		smmu_dev_knowledge.supported_trans |= STAGE2_TRANS;
	if (reg & IDR0_NTS)
		smmu_dev_knowledge.supported_trans |= NESTED_TRANS;
	/*stream matching register*/
	if (reg & IDR0_SMS)
		smmu_dev_knowledge.stream_match = true;
	/*address translation operation*/
	if ((reg & IDR0_ATOSNS) == 0)
		smmu_dev_knowledge.trans_op = true;
	/*AARCH32 translation format support*/
	field = IDR0_PTFS_VAL(reg & IDR0_PTFS);
	if (field == PTFS_AARCH32S_AARCH32L) {
		smmu_dev_knowledge.supported_fmt |= AARCH32L_FMT;
		smmu_dev_knowledge.supported_fmt |= AARCH32S_FMT;
	} else if (field == PTFS_AARCH32L_ONLY) {
		smmu_dev_knowledge.supported_fmt |= AARCH32L_FMT;
	} else {
		smmu_dev_knowledge.supported_fmt |= NO_AARCH32_FMT;
	}
	/*number of context fault intrrupts
	* However, in smmuv2, each context bank has dedicated interrupt pin
	* hence no requirement to specify implemented interrupts here.*/
	smmu_dev_knowledge.num_cfault_ints = IDR0_NUMIRPT_VAL(reg & IDR0_NUMIRPT);
	/*coherent translation table walk*/
	if (reg & IDR0_CTTW)
		smmu_dev_knowledge.cotable_walk = true;
	/*broadcast TLB maintenance*/
	if (reg & IDR0_BTM)
		smmu_dev_knowledge.broadcast_tlb = true;
	/*number of stream IDs*/
	smmu_dev_knowledge.num_stream_ids = (1 << IDR0_NUMSIDB_VAL(reg & IDR0_NUMSIDB)) - 1;
	/*number of stream mapping register groups*/
	smmu_dev_knowledge.num_stream_map_groups = reg & IDR0_NUMSMRG;

	/*ID1*/
	reg = smmu_read_reg32(SMMU_GR0_PPTR, SMMU_IDR1);
	/*smmu page size*/
	if (reg & IDR1_PAGESIZE )
		smmu_dev_knowledge.smmu_page_size = SMMU_PAGE_64KB;
	else
		smmu_dev_knowledge.smmu_page_size = SMMU_PAGE_4KB;
	/*smmu num pages, 2^(numdxb + 1)*/
	field = IDR1_NUMPAGENDXB_VAL(reg & IDR1_NUMPAGENDXB);
	smmu_dev_knowledge.smmu_num_pages = 1 << (field + 1);
	/*num of stage 2 context banks*/
	smmu_dev_knowledge.num_s2_cbanks = IDR1_NUMS2CB_VAL(reg & IDR1_NUMS2CB);
	/*total num of context banks*/
	smmu_dev_knowledge.num_cbanks = reg & IDR1_NUMCB;
	/*calcuate the context bank base*/
	smmu_dev_knowledge.cb_base = SMMU_CB_BASE_PADDR(
		SMMU_GLOBAL_SIZE(smmu_dev_knowledge.smmu_num_pages, smmu_dev_knowledge.smmu_page_size));

	/*ID2*/
	reg = smmu_read_reg32(SMMU_GR0_PPTR, SMMU_IDR2);
	/*VNID16S*/
	if (reg & IDR2_VMID16S)
		smmu_dev_knowledge.vmid16 = true;
	/*PTFSV8_64KB*/
	if (reg & IDR2_PTFSV8_64)
		smmu_dev_knowledge.supported_fmt |= TRANS_PAGES_64KB;
	/*PTFSV8_16KB*/
	if (reg & IDR2_PTFSV8_16)
		smmu_dev_knowledge.supported_fmt |= TRANS_PAGES_16KB;
	/*PTFSV8_64KB*/
	if (reg & IDR2_PTFSV8_4)
		smmu_dev_knowledge.supported_fmt |= TRANS_PAGES_4KB;
	/*OAS*/
	smmu_dev_knowledge.pa_size = IDR2_OAS_VAL(reg & IDR2_OAS);
	/*IAS*/
	smmu_dev_knowledge.ipa_size = reg & IDR2_IAS;
}


BOOT_CODE  static void smmu_dev_reset(void)
{
	uint32_t reg;
	pptr_t cb_bank_ptr;
	uint32_t major;

	/*clear the global FSR by writing back the read value*/
	reg = smmu_read_reg32(SMMU_GR0_PPTR, SMMU_sGFSR);
	smmu_write_reg32(SMMU_GR0_PPTR, SMMU_sGFSR, reg);

	/*reset stream to context config as using context banks*/
	reg = S2CR_PRIVCFG_SET(S2CR_PRIVCFG_DEFAULT);
	reg |= S2CR_TYPE_SET(S2CR_TYPE_CB);

	/*the number of stream-to-context is realted to the stream indexing method*/
	if (smmu_dev_knowledge.stream_match) {
		/*stream matching*/
		for (int i = 0; i < smmu_dev_knowledge.num_stream_map_groups; i++)
			smmu_write_reg32(SMMU_GR0_PPTR, SMMU_S2CRn(i), reg);
		/*reset the stream match registers as invalid*/
		reg = SMR_VALID_SET(SMR_VALID_DIS);
		for (int i = 0; i < smmu_dev_knowledge.num_stream_map_groups; i++)
			smmu_write_reg32(SMMU_GR0_PPTR, SMMU_SMRn(i), reg);
	} else {
		/*stream ID*/
		for (int i = 0; i < smmu_dev_knowledge.num_stream_ids; i++)
			smmu_write_reg32(SMMU_GR0_PPTR, SMMU_S2CRn(i), reg);
	}

	/*special init requested by the smmu-500: start*/
	reg = smmu_read_reg32(SMMU_GR0_PPTR, SMMU_IDR7);
	major = IDR7_MAJOR_VAL(reg & IDR7_MAJOR);
	/*init the auxiliary configuration register*/
	reg = smmu_read_reg32(SMMU_GR0_PPTR, SMMU_sACR);
	/*unlock the write access to SMMU_CBn_ACTLR,
	only provided in version 2 and above*/
	if (major >= 2)
		reg &= ~ACR_CACHE_LOCK;
	/*enable the TLB to cache bypassing*/
	reg |= ACR_S2CRB_TLBEN | ACR_SMTNMB_TLBEN;
	smmu_write_reg32(SMMU_GR0_PPTR, SMMU_sACR, reg);
	/*special init requested by the smmu-500: end*/

	for (int i = 0; i < smmu_dev_knowledge.num_cbanks; i++) {
		cb_bank_ptr = SMMU_CBn_BASE_PPTR(i);
		/*disable context banks and clear the context bank fault registers*/
		smmu_write_reg32(cb_bank_ptr, SMMU_CBn_SCTLR, 0);
		smmu_write_reg32(cb_bank_ptr, SMMU_CBn_FSR, CBn_FSR_CLEAR_ALL);

		/*special init requested by the smmu-500: start*/
		/*disable MMU-500's next page prefetch due to errata 841119 and 826419*/
		reg = smmu_read_reg32(cb_bank_ptr, SMMU_CBn_ACTLR);
		reg &= ~CBn_ACTLR_CPRE;
		smmu_write_reg32(cb_bank_ptr, SMMU_CBn_ACTLR, reg);
        /*special init requested by the smmu-500: end*/
	}

	/*invalidate TLB */
	smmu_write_reg32(SMMU_GR0_PPTR, SMMU_TLBIALLH, SMMU_TLB_INVALL_MASK);
	smmu_write_reg32(SMMU_GR0_PPTR, SMMU_TLBIALLNSNH, SMMU_TLB_INVALL_MASK);

	reg = smmu_read_reg32(SMMU_GR0_PPTR, SMMU_sCR0);
	/*enable global fault reporting*/
	reg |= CR0_GFRE | CR0_GFIE | CR0_GCFGFRE | CR0_GCFGFIE;
	/*raise fault for any transaction that does not match to
	any stream mapping table entires*/
	reg |= CR0_USFCFG;
	/*raise fault for stream match conflict*/
	reg |= CR0_SMCFCFG;
	/*disable the VMID private name space*/
	reg &= ~CR0_VMIDPNE;
	/*TLB is maintained together with the rest of the system*/
	reg &= ~CR0_PTM;
	/*enable force TLB broadcast on bypassing transactions*/
	reg |= CR0_FB;
	/*enable client access, ie transaction enforced by SMMU*/
	reg &= ~CR0_CLIENTPD;
	/*Not upgrade barrier*/
	reg &= ~CR0_BSU;
	/*syn above issued TLB operations*/
	smmu_tlb_sync(SMMU_GR0_PPTR, SMMU_sTLBGSYNC, SMMU_sTLBGSTATUS);
	/*enable the SMMU*/
	smmu_write_reg32(SMMU_GR0_PPTR, SMMU_sCR0, reg);
}

BOOT_CODE  void plat_smmu_init(void)
{
	smmu_config_prob();
	smmu_mapping_init();
	smmu_dev_reset();
}

