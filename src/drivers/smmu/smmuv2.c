/*
 * Copyright 2019, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_GPL)
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


/*the default vritual address bits for partition TTBR0 and TTBR1*/
#define SMMU_VA_DEFAULT_BITS      48 

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
	uint32_t smmu_page_size;          /*page size in smmu register address space*/
	uint32_t smmu_num_pages;          /*number of pages in global or context bank address space*/
	uint32_t num_s2_cbanks;           /*cbanks that support stage 2 only*/ 
	uint32_t num_cbanks;              /*total number of context banks*/
	uint32_t va_bits;                 /*upstream address size*/
	uint32_t pa_bits;                 /*PA address size*/
	uint32_t ipa_bits;                /*IPA address size*/
	pptr_t cb_base;                   /*base of context bank address space*/
}; 


struct smmu_table_config {

	uint32_t tcr[2];                    /*SMMU_CBn_TCRm*/
	uint32_t mair[2];                  /*SMMU_CBn_MAIRm*/
	uint64_t ttbr[2];                  /*SMMU_CBn_TTBRm*/

}; 

static struct smmu_feature smmu_dev_knowledge; 

static struct smmu_table_config smmu_stage_table_config; 

static inline uint32_t smmu_read_reg32(pptr_t base, uint32_t index) {

	return *(volatile uint32_t*)(base + index); 
}

static inline void smmu_write_reg32(pptr_t base, uint32_t index, uint32_t val) {

	*(volatile uint32_t*) (base + index) = val; 
}

static inline uint64_t smmu_read_reg64(pptr_t base, uint32_t index) {

	return *(volatile uint64_t*) (base + index); 
}

static inline void smmu_write_reg64(pptr_t base, uint32_t index, uint64_t val) {

	*(volatile uint64_t *) (base + index) = val; 
}

static void smmu_tlb_sync(pptr_t base, uint32_t sync, uint32_t status) {

	int count = 0; 

	smmu_write_reg32(base, sync, SMMU_TLB_SYNC_MASK); 

	while (count < TLBSYNC_LOOP) {

		/*pulling the active flag*/
		if (!(smmu_read_reg32(base, status) & TLBSTATUS_GSACTIVE)) 
			break; 

		count++; 
	}
}


static inline uint32_t smmu_obs_size_to_bits(uint32_t size) {

	/*coverting the output bus address size into address bit, defined in 
	IDx registers*/ 
	switch (size) {
		case 0: 
			return 32; 
		case 1: 
			return 36; 
		case 2: 
			return 40; 
		case 3:
			return 42; 
		case 4: 
			return 44; 
		case 5: 
		default:
			return 48; 
	}
}


static inline uint32_t smmu_ubs_size_to_bits(uint32_t size) {

	/*coverting the upstream address size into address bit, defined in 
	IDx registers*/ 
	switch (size) {
		case 0: 
			return 32; 
		case 1: 
			return 36; 
		case 2: 
			return 40; 
		case 3: 
			return 42; 
		case 4: 
			return 44; 
		case 5: 
			return 49;
		case 0xf: 	
		default:
			return 64; 
	}
}

BOOT_CODE static void smmu_mapping_init(void) {

	/*creating mapping for the rest of smmu address space. 
	the code assumes registers in each smmu page are witin a 4K page
	even though the alignement of the (physical) pages can be 64K, i.e., compacting the 
	smmu window with 4k pages*/

	/*This is a temporary solution. A correct solution should 
	be adjust the virutal address space layout of the kernel in order to have 
	enough space for the smmu. For example, smmu on TX2 requires a 8M space. 
	Also, the verification requires the space to be populated with a static knowledge, which 
	requires changes to the kernel_frame_t  and the map_kernel_frame, allowing 
	the devices can be mapped in a seperate page table with a described region.*/

	/*the current implementation only works on TX2*/

	/*init the GR1 region, start: smmu_pptr + 4K, size 4K*/ 
	map_kernel_frame(SMMU_GR1_PADDR(smmu_dev_knowledge.smmu_page_size),
		SMMU_GR1_PPTR, 
		VMKernelOnly,
		vm_attributes_new(true, false, false));

	printf("SMMU_GR1_PADDR 0x%x, SMMU_GR1_PPTR 0x%lx\n", SMMU_GR1_PADDR(smmu_dev_knowledge.smmu_page_size), SMMU_GR1_PPTR);

	/*GID registers*/
	map_kernel_frame(SMMU_GID_PADDR(smmu_dev_knowledge.smmu_page_size),
		SMMU_GID_PPTR, 
		VMKernelOnly,
		vm_attributes_new(true, false, false));

	printf("SMMU_GID_PADDR 0x%x, SMMU_GID_PPTR 0x%lx\n", SMMU_GID_PADDR(smmu_dev_knowledge.smmu_page_size), SMMU_GID_PPTR);
	/*PM registers*/
	map_kernel_frame(SMMU_PM_PADDR(smmu_dev_knowledge.smmu_page_size),
		SMMU_PM_PPTR, 
		VMKernelOnly,
		vm_attributes_new(true, false, false));
	printf("SMMU_PM_PADDR 0x%x, SMMU_PM_PPTR 0x%lx\n", SMMU_PM_PADDR(smmu_dev_knowledge.smmu_page_size), SMMU_PM_PPTR);
	
	/*SSD registers*/
	map_kernel_frame(SMMU_SSD_PADDR(smmu_dev_knowledge.smmu_page_size),
		SMMU_SSD_PPTR, 
		VMKernelOnly,
		vm_attributes_new(true, false, false));

	printf("SMMU_SSD_PADDR 0x%x, SMMU_SSD_PPTR 0x%lx\n", SMMU_SSD_PADDR(smmu_dev_knowledge.smmu_page_size), SMMU_SSD_PPTR);
	
	/*map the context banks, each 4K page*/
	for (int i = 0; i < smmu_dev_knowledge.num_cbanks; i++) {

           map_kernel_frame(SMMU_CBn_PADDR(smmu_dev_knowledge.cb_base, i, smmu_dev_knowledge.smmu_page_size), 
                         SMMU_CBn_BASE_PPTR(i), 
                         VMKernelOnly,
                         vm_attributes_new(true, false, false));
           	printf("SMMU_CBn_PADDR 0x%lx, SMMU_SSD_PPTR 0x%lx\n", SMMU_CBn_PADDR(smmu_dev_knowledge.cb_base, i, smmu_dev_knowledge.smmu_page_size),  SMMU_CBn_BASE_PPTR(i));
        }    
}



BOOT_CODE static void smmu_config_prob(void) {

	uint32_t reg, field;


	printf("smmu_config_prob, smmu_pptr 0x%lx\n",  SMMU_GR0_PPTR);

	/*ID0*/
	reg = smmu_read_reg32(SMMU_GR0_PPTR, SMMU_IDR0); 

	/*stages supported*/
	if (reg & IDR0_S1TS) 
		smmu_dev_knowledge.supported_trans |= STAGE1_TRANS; 
	if (reg & IDR0_S2TS) 
		smmu_dev_knowledge.supported_trans |= STAGE2_TRANS; 
	if (reg & IDR0_NTS) 
		smmu_dev_knowledge.supported_trans |= NESTED_TRANS; 

	printf("supporting stages: 1 %s 2 %s nested %s \n",  
		smmu_dev_knowledge.supported_trans & STAGE1_TRANS ? "true" : "false", 
		smmu_dev_knowledge.supported_trans & STAGE2_TRANS ? "true" : "false", 
		smmu_dev_knowledge.supported_trans & NESTED_TRANS ? "true" : "false");

	/*stream matching register*/
	if (reg & IDR0_SMS) 
		smmu_dev_knowledge.stream_match = true; 

	/*address translation operation*/
	if ((reg & IDR0_ATOSNS) == 0)  
		smmu_dev_knowledge.trans_op = true; 
	
	printf("supporting address translation %s \n",  
		smmu_dev_knowledge.trans_op ? "true" : "false"); 

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

		printf("supporting translation: 32L %s 32S %s no 32 %s \n",  
		smmu_dev_knowledge.supported_fmt & AARCH32L_FMT ? "true" : "false", 
		smmu_dev_knowledge.supported_fmt & AARCH32S_FMT ? "true" : "false", 
		smmu_dev_knowledge.supported_fmt & NO_AARCH32_FMT ? "true" : "false");

	/*number of context fault intrrupts 
	however, in smmuv2, each context bank has dedicated interrupt pin 
	hence no requirement to specify implemented interrupts here*/
	smmu_dev_knowledge.num_cfault_ints = IDR0_NUMIRPT_VAL(reg & IDR0_NUMIRPT); 

	printf("number context falut interrupts %d\n", smmu_dev_knowledge.num_cfault_ints);

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

	printf("num stream ids %d num stream map groups %d support stream match %s \n", smmu_dev_knowledge.num_stream_ids, smmu_dev_knowledge.num_stream_map_groups, 
		smmu_dev_knowledge.stream_match ? "true" : "false");

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

	printf("num of context banks %d\n", smmu_dev_knowledge.num_cbanks);

	/*calcuate the context bank base*/
	smmu_dev_knowledge.cb_base = SMMU_CB_BASE_PADDR(
		SMMU_GLOBAL_SIZE(smmu_dev_knowledge.smmu_num_pages, smmu_dev_knowledge.smmu_page_size)); 


	printf("smmu page size 0x%x num pages %d context bank base 0x%lx\n",  smmu_dev_knowledge.smmu_page_size, 
	smmu_dev_knowledge.smmu_num_pages, smmu_dev_knowledge.cb_base);	

	/*ID2*/
	reg = smmu_read_reg32(SMMU_GR0_PPTR, SMMU_IDR2); 

	/*VNID16S*/
	if (reg & IDR2_VMID16S) 
		smmu_dev_knowledge.vmid16 = true; 

	printf("support VMID 16 bits %s \n", smmu_dev_knowledge.vmid16 ? "true" : "false");

	/*PTFSV8_64KB*/
	if (reg & IDR2_PTFSV8_64) 
		smmu_dev_knowledge.supported_fmt |= TRANS_PAGES_64KB; 

	/*PTFSV8_16KB*/
	if (reg & IDR2_PTFSV8_16) 
		smmu_dev_knowledge.supported_fmt |= TRANS_PAGES_16KB; 
	
	/*PTFSV8_64KB*/
	if (reg & IDR2_PTFSV8_4) 
		smmu_dev_knowledge.supported_fmt |= TRANS_PAGES_4KB; 	

	printf("support page fmt 64 %s 16 %s 4 %s\n", 
		smmu_dev_knowledge.supported_fmt & TRANS_PAGES_64KB ? "true" : "flase", 
		smmu_dev_knowledge.supported_fmt & TRANS_PAGES_16KB ? "true" : "flase",
		smmu_dev_knowledge.supported_fmt & TRANS_PAGES_4KB ? "true" : "flase");

	/*UBS virtual address size*/ 
	smmu_dev_knowledge.va_bits = smmu_ubs_size_to_bits(IDR2_UBS_VAL(reg & IDR2_UBS)); 
	
	/*OAS*/
	smmu_dev_knowledge.pa_bits = smmu_obs_size_to_bits(IDR2_OAS_VAL(reg & IDR2_OAS)); 
	/*IAS*/ 
	smmu_dev_knowledge.ipa_bits = smmu_obs_size_to_bits(reg & IDR2_IAS); 


	printf("smmu upper stream (vritual) address bits  %d,  physical address bits %d, IPA bits  %d \n", 
		smmu_dev_knowledge.va_bits, smmu_dev_knowledge.pa_bits, smmu_dev_knowledge.ipa_bits); 

}


BOOT_CODE  static void smmu_dev_reset(void) {

	uint32_t reg = 0; 
	pptr_t cb_bank_ptr; 

//#ifdef CONFIG_SMMU_500
	uint32_t major;
//#endif

	/*clear the fault syndrom registers*/ 
	smmu_write_reg32(SMMU_GR0_PPTR, SMMU_sGFSYNR0, reg);
	smmu_write_reg32(SMMU_GR0_PPTR, SMMU_sGFSYNR1, reg);
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

	/*special init requested by the smmu-500*/ 
//#ifdef CONFIG_SMMU_500 
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
//#endif /*CONFIG_SMMU_500*/


	for (int i = 0; i < smmu_dev_knowledge.num_cbanks; i++) {

		cb_bank_ptr = SMMU_CBn_BASE_PPTR(i);
		/*disable context banks and clear the context bank fault registers*/
		smmu_write_reg32(cb_bank_ptr, SMMU_CBn_SCTLR, 0); 
		/*clear the syndrom register*/ 
		smmu_write_reg64(cb_bank_ptr, SMMU_CBn_FAR, 0ULL); 
		smmu_write_reg32(cb_bank_ptr, SMMU_CBn_FSR, CBn_FSR_CLEAR_ALL); 

//#ifdef CONFIG_SMMU_500 
		/*disable MMU-500's next page prefetch due to errata 841119 and 826419*/
		reg = smmu_read_reg32(cb_bank_ptr, SMMU_CBn_ACTLR); 
		reg &= ~CBn_ACTLR_CPRE; 
		smmu_write_reg32(cb_bank_ptr, SMMU_CBn_ACTLR, reg); 
//#endif /*CONFIG_SMMU_500*/
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
	/*TODO: using the private name space for now, 
	enable when the kernel provide support on allocating VMID*/
	reg &= ~CR0_VMIDPNE; 

	/*TLB is maintained together with the rest of the system*/
	reg &= ~CR0_PTM; 

	/*enable force TLB broadcast on bypassing transactions*/
	reg |= CR0_FB; 

	/*enable client access, ie transaction enforced by SMMU*/
	reg &= ~CR0_CLIENTPD; 

	/*TODO: checking the VMID bits used in the system, only need to 
	configure when the hypermode is enabled*/

	/*Not upgrade barrier*/
	reg &= ~CR0_BSU(CR0_BSU_ALL); 

	printf("SMMU_sCR0, val 0x%x\n", reg); 

	/*syn above issued TLB operations*/
	smmu_tlb_sync(SMMU_GR0_PPTR, SMMU_sTLBGSYNC, SMMU_sTLBGSTATUS); 

	/*enable the SMMU*/
	smmu_write_reg32(SMMU_GR0_PPTR, SMMU_sCR0, reg); 

}


BOOT_CODE void plat_smmu_init(void) {

	
	printf("plat_smmu_init....start\n");

	smmu_config_prob(); 


	smmu_mapping_init(); 
	
	smmu_dev_reset(); 

	printf("plat_smmu_init.....done.\n");

}

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
static void smmu_config_stage2 (struct smmu_table_config *cfg, 
	vspace_root_t *vspace) {

	uint32_t reg = 0; 

	/*SMMU_CBn_TCR*/
	reg |= CBn_TCR_SH0_SET(CBn_TCR_SH_INNER); 
	reg |= CBn_TCR_ORGN0_SET(CBn_TCR_GN_WB_WA_CACHE); 
	reg |= CBn_TCR_IRGN0_SET(CBn_TCR_GN_WB_WA_CACHE); 
	reg |= CBn_TCR_TG0_SET(CBn_TCR_TG_4K); 

	/*setting according to the  vcpu_init_vtcr in vcpu.h*/
#ifdef CONFIG_ARM_PA_SIZE_BITS_40    
	reg |= CBn_TCR_T0SZ_SET(24);                                                                                           
	reg |= CBn_TCR_PASize_SET(CBn_TCR2_PASize_40);        
	reg |= CBn_TCR_SL0_SET(CBn_TCR_SL0_4KB_L1);                                                                                               
#else 
	reg |= CBn_TCR_T0SZ_SET(20);     	
	reg |= CBn_TCR_PASize_SET(CBn_TCR2_PASize_44);     
	reg |= CBn_TCR_SL0_SET(CBn_TCR_SL0_4KB_L0);                                                                                                  
#endif
	/*reserved as 1*/
	reg |= BIT(31);       

	cfg->tcr[0] = reg; 

	/*vttbr*/ 
	cfg->ttbr[0] = ttbr_new(0, pptr_to_paddr(vspace)).words[0];                         

} 

#else 
static void smmu_config_stage1 (struct smmu_table_config *cfg, 
	bool_t coherence, uint32_t pa_bits,
	vspace_root_t *vspace, asid_t asid) {

	uint32_t reg = 0;  

	/*SMMU_CBn_TCR*/
	if (coherence) {

		reg |= CBn_TCR_SH0_SET(CBn_TCR_SH_INNER); 
		reg |= CBn_TCR_ORGN0_SET(CBn_TCR_GN_WB_WA_CACHE); 
		reg |= CBn_TCR_IRGN0_SET(CBn_TCR_GN_WB_WA_CACHE); 

	} else {

		reg |= CBn_TCR_SH0_SET(CBn_TCR_SH_OUTER); 
		reg |= CBn_TCR_ORGN0_SET(CBn_TCR_GN_NCACHE); 
		reg |= CBn_TCR_IRGN0_SET(CBn_TCR_GN_NCACHE); 

	}

	/*page size is configed as 4k*/
	reg |= CBn_TCR_TG0_SET(CBn_TCR_TG_4K); 
	
	/*the TTBR0 size, caculated according to the aarch64 formula*/
	reg |= CBn_TCR_T0SZ_SET(64 - SMMU_VA_DEFAULT_BITS); 

	/*disable (speculative) page table walks through TTBR1*/
	reg |= CBn_TCR_EPD1_DIS; 

	cfg->tcr[0] = reg; 

	/*TCR2*/
	reg = 0; 
	switch (pa_bits) {
		case 32: 
			reg |= CBn_TCR2_PASize_SET(CBn_TCR2_PASize_32); 
			break; 
		case 36: 
			reg |= CBn_TCR2_PASize_SET(CBn_TCR2_PASize_36); 
			break; 
		case 40: 
			reg |= CBn_TCR2_PASize_SET(CBn_TCR2_PASize_40); 
			break; 		
		case 42: 
			reg |= CBn_TCR2_PASize_SET(CBn_TCR2_PASize_42); 
			break; 		
		case 44: 
			reg |= CBn_TCR2_PASize_SET(CBn_TCR2_PASize_44); 
			break; 		
		case 48: 
		default: 
			reg |= CBn_TCR2_PASize_SET(CBn_TCR2_PASize_48); 
			break; 		
	}

	/*currently only support AArch64*/
	reg |= CBn_TCR2_SEP_SET(CBn_TCR2_SEP_UPSTREAM_SIZE) | CBn_TCR2_AS_SET(CBn_TCR2_AS_16); 
	cfg->tcr[1] = reg; 


	/*MAIR0*/
	reg = CBn_MAIRm_ATTR_DEVICE_nGnRnE << CBn_MAIRm_ATTR_SHIFT(CBn_MAIRm_ATTR_ID_DEVICE_nGnRnE); 
	reg |= CBn_MAIRm_ATTR_DEVICE_nGnRE << CBn_MAIRm_ATTR_SHIFT(CBn_MAIRm_ATTR_ID_DEVICE_nGnRE); 
	reg |= CBn_MAIRm_ATTR_DEVICE_GRE << CBn_MAIRm_ATTR_SHIFT(CBn_MAIRm_ATTR_ID_DEVICE_GRE);
	reg |= CBn_MAIRm_ATTR_NC << CBn_MAIRm_ATTR_SHIFT(CBn_MAIRm_ATTR_ID_NC);

	cfg->mair[0] = reg; 

	/*MAIR1*/
	reg = CBn_MAIRm_ATTR_CACHE << CBn_MAIRm_ATTR_SHIFT(CBn_MAIRm_ATTR_ID_CACHE); 
	cfg->mair[1] = reg; 
	
	/*TTBRs*/
	/*NOTE: the smmu only configured with user-level address space, TTBR0.*/
	cfg->ttbr[0] = ttbr_new(asid, pptr_to_paddr(vspace)).words[0]; 
	cfg->ttbr[1] = 0; 


}
#endif /*CONFIG_ARM_HYPERVISOR_SUPPORT*/

void smmu_cb_assgin_vspace(word_t cb, vspace_root_t *vspace, asid_t asid) {

	uint32_t reg = 0; 
	/*currently only support private vmid space, using cb index*/
	word_t vmid = cb; 


#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT

	smmu_config_stage2(&smmu_stage_table_config, 
		vspace); 

#else 

	smmu_config_stage1(&smmu_stage_table_config, 
		smmu_dev_knowledge.cotable_walk, 
		smmu_dev_knowledge.ipa_bits, 
		vspace, 
		asid); 

#endif /*CONFIG_ARM_HYPERVISOR_SUPPORT*/


	/*see table 2-6 for ttbr requirement 
	see tabel 1-7 for ttbr ragions on stage 1 */

	/*CBA2R*/ 
	/*currently only support aarch64*/
	reg = CBA2Rn_VA64_SET; 

	if (smmu_dev_knowledge.vmid16) 
		reg |= CBA2Rn_VMID_SET(vmid); 

	smmu_write_reg32(SMMU_GR1_PPTR, SMMU_CBA2Rn(cb), reg); 

	printf("GR1 SMMU_CBA2Rn 0x%x\n", reg);

	/*CBAR*/
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT

	/*stage 2 translation only, CBAR_TYPE_S2_TRANS*/
	/*for the stage 2 translation, the VMID not shared with the 
	system by using private vmid space, and the vmid number is the same 
	with the context bank number */

	reg = CBARn_TYPE_SET(CBARn_TYPE_STAGE2); 

	/*8 bit VMID*/
	if (!smmu_dev_knowledge.vmid16) 
		reg |= CBARn_VMID_SET(vmid); 

#else 
	/*stage 1 translation only, CBAR_TYPE_S1_TRANS_S2_BYPASS*/ 
	reg = CBARn_TYPE_SET(CBARn_TYPE_STAGE1); 

	/*the weakest shareability/memory types, so they can be overwritten 
	by ttbcr or pte*/
	reg |= CBARn_BPSHCFG_SET(CBARn_BPSHCFG_NONE); 
	reg |= CBARn_MemAttr_SET(MemAttr_OWB_IWB); 

#endif 	/*CONFIG_ARM_HYPERVISOR_SUPPORT*/

	smmu_write_reg32(SMMU_GR1_PPTR, SMMU_CBARn(cb), reg); 
	printf("GR1 SMMU_CBARn 0x%x\n", reg);
	
	/*TCR*/
	smmu_write_reg32(SMMU_CBn_BASE_PPTR(cb), SMMU_CBn_TCR, smmu_stage_table_config.tcr[0]); 

	printf("SMMU_CBn_TCR 0x%x\n", smmu_stage_table_config.tcr[0]);

#ifndef CONFIG_ARM_HYPERVISOR_SUPPORT
		/*TCR2 is used for stage 1 only*/
	smmu_write_reg32(SMMU_CBn_BASE_PPTR(cb), SMMU_CBn_TCR2, smmu_stage_table_config.tcr[1]); 

	printf("SMMU_CBn_TCR2 0x%x\n", smmu_stage_table_config.tcr[1]);
	/*stage 1 requires ttbr 1 and ttbr 0 
	stage 2 only require ttbr 0*/
	smmu_write_reg64(SMMU_CBn_BASE_PPTR(cb), SMMU_CBn_TTBR1, smmu_stage_table_config.ttbr[1]); 
#endif /*!CONFIG_ARM_HYPERVISOR_SUPPORT*/

	/*ttbr0 (user space), for both stage 1 and stage 2*/ 
	smmu_write_reg64(SMMU_CBn_BASE_PPTR(cb), SMMU_CBn_TTBR0, smmu_stage_table_config.ttbr[0]); 

#ifndef CONFIG_ARM_HYPERVISOR_SUPPORT

	/*MAIRs is used for stage 1 only*/
	smmu_write_reg32(SMMU_CBn_BASE_PPTR(cb), SMMU_CBn_MAIR0, smmu_stage_table_config.mair[0]); 

	smmu_write_reg32(SMMU_CBn_BASE_PPTR(cb), SMMU_CBn_MAIR1, smmu_stage_table_config.mair[1]);

	printf("SMMU_CBn_MAIR0 0x%x\n", smmu_stage_table_config.mair[0]);

	printf("SMMU_CBn_MAIR1 0x%x\n", smmu_stage_table_config.mair[1]);

#endif /*!CONFIG_ARM_HYPERVISOR_SUPPORT*/

	/*SCTLR, */
	reg = CBn_SCTLR_CFIE | CBn_SCTLR_CFRE | CBn_SCTLR_AFE | CBn_SCTLR_TRE | CBn_SCTLR_M; 
	
	smmu_write_reg32(SMMU_CBn_BASE_PPTR(cb), SMMU_CBn_SCTLR, reg);

	printf("SMMU_CBn_SCTLR 0x%x\n", reg);
	printf("smmu_cb_assgin_vspace done\n");
}

void smmu_cb_disable(word_t cb, asid_t asid) {

	uint32_t reg = smmu_read_reg32(SMMU_CBn_BASE_PPTR(cb), SMMU_CBn_SCTLR); 
	reg &= ~CBn_SCTLR_M; 	
	smmu_write_reg32(SMMU_CBn_BASE_PPTR(cb), SMMU_CBn_SCTLR, reg);

	smmu_tlb_invalidate_cb(cb, asid); 
	printf("smmu_cb_disable for cb %lu asid 0x%lx\n", cb, asid);
}

void smmu_sid_bind_cb(word_t sid, word_t cb) {

	uint32_t reg = 0; 

	reg = S2CR_PRIVCFG_SET(S2CR_PRIVCFG_DEFAULT); 
	reg |= S2CR_TYPE_SET(S2CR_TYPE_CB); 
	reg |= S2CR_CBNDX_SET(cb); 

	printf("SMMU_S2CRn 0x%x\n", reg);
	smmu_write_reg32(SMMU_GR0_PPTR, SMMU_S2CRn(sid), reg);  

	/*the number of stream-to-context is realted to the stream indexing method
	currently supports one to on mapping, one sid in on group, and never change*/
	if (smmu_dev_knowledge.stream_match) {

		reg = SMR_VALID_SET(SMR_VALID_EN) | SMR_ID_SET(sid); 
		smmu_write_reg32(SMMU_GR0_PPTR, SMMU_SMRn(sid), reg); 

		printf("SMMU_SMRn 0x%x\n", reg);
	}		
} 

void smmu_sid_unbind(word_t sid) {

	uint32_t reg = 	S2CR_TYPE_SET(S2CR_TYPE_FAULT);
	smmu_write_reg32(SMMU_GR0_PPTR, SMMU_S2CRn(sid), reg);  
	if (smmu_dev_knowledge.stream_match) {
		reg = SMR_VALID_SET(SMR_VALID_DIS); 
		smmu_write_reg32(SMMU_GR0_PPTR, SMMU_SMRn(sid), reg); 
	}		
	printf("smmu_sid_unbind sid number %lu\n", sid);
}


void smmu_tlb_invalidate_all(void) {

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
	/*for hyp entries*/
	smmu_write_reg32(SMMU_GR0_PPTR, SMMU_TLBIALLH, SMMU_TLB_INVALL_MASK); 
#else 
	/*for non-secure non-hyp entries*/
	smmu_write_reg32(SMMU_GR0_PPTR, SMMU_TLBIALLNSNH, SMMU_TLB_INVALL_MASK); 
#endif 
		/*syn above issued TLB operations*/
	smmu_tlb_sync(SMMU_GR0_PPTR, SMMU_sTLBGSYNC, SMMU_sTLBGSTATUS); 
}

void smmu_tlb_invalidate_cb(int cb, asid_t asid) { 

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
	/*stage 2*/
	/*vmid is currently a private space, equal with cb*/
	uint32_t reg = TLBIVMID_SET(cb); 

	smmu_write_reg32(SMMU_GR0_PPTR, SMMU_TLBIVMID, reg);
	printf("SMMU_TLBIVMID 0x%x\n", reg);	
	smmu_tlb_sync(SMMU_GR0_PPTR, SMMU_sTLBGSYNC, SMMU_sTLBGSTATUS); 
#else 	
	/*stage 1*/
	uint32_t reg = CBn_TLBIASID_SET(asid); 

	smmu_write_reg32(SMMU_CBn_BASE_PPTR(cb), SMMU_CBn_TLBIASID, reg);
	printf("SMMU_CBn_TLBIASID 0x%x\n", reg);		
	smmu_tlb_sync(SMMU_CBn_BASE_PPTR(cb), SMMU_CBn_TLBSYNC, SMMU_CBn_TLBSTATUS); 
#endif
}


void smmu_read_fault_state(uint32_t *status, uint32_t *syndrome_0, uint32_t *syndrome_1) {

	*status = smmu_read_reg32(SMMU_GR0_PPTR, SMMU_sGFSR);
	*syndrome_0 = smmu_read_reg32(SMMU_GR0_PPTR, SMMU_sGFSYNR0);
	*syndrome_1 = smmu_read_reg32(SMMU_GR0_PPTR, SMMU_sGFSYNR1);
}

void smmu_clear_fault_state(void) {

	uint32_t reg = smmu_read_reg32(SMMU_GR0_PPTR, SMMU_sGFSR);
	smmu_write_reg32(SMMU_GR0_PPTR, SMMU_sGFSR, reg);  
}


void smmu_cb_read_fault_state(int cb, uint32_t *status, word_t *address) {
	*status = smmu_read_reg32(SMMU_CBn_BASE_PPTR(cb), SMMU_CBn_FSR);
	*address = smmu_read_reg64(SMMU_CBn_BASE_PPTR(cb),SMMU_CBn_FAR); 
} 


void smmu_cb_clear_fault_state(int cb) {
	smmu_write_reg32(SMMU_CBn_BASE_PPTR(cb), SMMU_CBn_FSR, CBn_FSR_CLEAR_ALL); 
} 

