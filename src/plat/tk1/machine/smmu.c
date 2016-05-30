#include <types.h>
#include <config.h>

#ifdef CONFIG_ARM_SMMU

#include <plat/machine/smmu.h>
#include <arch/linker.h>
#include <plat/machine/devices.h>
#include <plat/machine/hardware.h>
#include <object/structures.h>


#define SMMU_CONFIG_OFFSET          0x10

#define PTB_DATA_BASE_SHIFT         12
#define PTB_DATA_READ               BIT(31)
#define PTB_DATA_WRITE              BIT(30)
#define PTB_DATA_NONSECURE          BIT(29)
#define PTB_DATA_BASE_PD_MASK       0x3fffff

#define MODULE_ASID_ENABLE          BIT(31)

#define PTC_FLUSH_ALL               0
#define PTC_FLUSH_ADR               1

#define TLB_ASID_MATCH              BIT(31)
#define TLB_FLUSH_ALL               (0)
#define TLB_FLUSH_SECTION           (2)
#define TLB_FLUSH_GROUP             (3)

#define MC_DECERR_MTS_BIT           16u
#define MC_SECERR_SEC_BIT           13u
#define MC_DECERR_VPR_BIT           12u
#define MC_APB_ASID_UPDATE_BIT      11u
#define MC_SMMU_PAGE_BIT            10u
#define MC_ARBITRATION_EMEM_BIT     9u
#define MC_SECURITY_BIT             8u
#define MC_DECERR_EMEM_BIT          6u


#define MC_ERR_ID_MASK              0x7f
#define MC_ERR_ADR_MASK             0x7000
#define MC_ERR_RW_MASK              0x10000
#define MC_ERR_SEC_MASK             0x20000
#define MC_ERR_SWAP_MASK            0x40000
#define MC_ERR_ADR_HI_MASK          0x300000
#define MC_ERR_INVALID_SMMU_PAGE_NONSECURE_MASK     0x2000000
#define MC_ERR_INVALID_SMMU_PAGE_WRITE_MASK         0x4000000
#define MC_ERR_INVALID_SMMU_PAGE_READ_MASK          0x8000000
#define MC_ERR_TYPE_MASK                            0x70000000
#define MC_ERR_TYPE_SHIFT                           28

#define MC_ERR_TYPE_RSVD                0
#define MC_ERR_TYPE_DECERR_EMEM         2
#define MC_ERR_TYPE_SECURITY            3
#define MC_ERR_TYPE_SECURITY_CARVEOUT   4
#define MC_ERR_TYPE_INVALID_SMMU_PAGE   6

#define IOPDE_4M_INDEX_SHIFT            22

static volatile tk1_mc_regs_t *smmu_regs = (volatile tk1_mc_regs_t *)(SMMU_PPTR);

static void
do_smmu_enable(void)
{
    volatile uint32_t *config = (volatile uint32_t *)(MC_PADDR + SMMU_CONFIG_OFFSET);
    *config = 1;
}

static void
do_smmu_disable(void)
{
    volatile uint32_t *config = (volatile uint32_t *)(MC_PADDR + SMMU_CONFIG_OFFSET);
    *config = 0;
}

static inline void
smmu_disable(void)
{
    if (config_set(ARM_HYP)) {
        /* in hyp mode, we need call the hook in monitor mode */
        /* we need physical address here */
        paddr_t addr = addrFromPPtr(&do_smmu_disable);
        asm (".arch_extension sec\n");
        asm volatile ("mov r0, %0\n\t"
                      "dsb\nisb\n"
                      "smc #0\n"
                      ::"r"(addr));
    } else {
        /* in secure mode, can enable it directly */
        smmu_regs->smmu_config = 0;
    }

    return;
}

static inline void
smmu_enable(void)
{
    if (config_set(ARM_HYP)) {
        paddr_t addr = addrFromPPtr(&do_smmu_enable);
        asm (".arch_extension sec\n");
        asm volatile ("mov r0, %0\n\t"
                      "dsb\nisb\n"
                      "smc #0\n"
                      ::"r"(addr));
    } else {
        smmu_regs->smmu_config = 1;
    }

    return;
}


static uint32_t
make_ptb_data(uint32_t pd_base, bool_t read, bool_t write, bool_t nonsecure)
{
    uint32_t ret = 0;
    ret = (pd_base >> PTB_DATA_BASE_SHIFT);

    if (read) {
        ret |= PTB_DATA_READ;
    }
    if (write) {
        ret |= PTB_DATA_WRITE;
    }
    if (nonsecure) {
        ret |= PTB_DATA_NONSECURE;
    }

    return ret;
}

static uint32_t
ptb_data_get_pd_base(uint32_t data)
{
    uint32_t ret = data;
    ret &= PTB_DATA_BASE_PD_MASK;
    ret <<= PTB_DATA_BASE_SHIFT;
    return ret;
}

void
plat_smmu_ptc_flush_all(void)
{
    uint32_t cmd = PTC_FLUSH_ALL;
    smmu_regs->smmu_ptc_flush = cmd;
}

void
plat_smmu_tlb_flush_all(void)
{
    uint32_t cmd = TLB_FLUSH_ALL;
    smmu_regs->smmu_tlb_flush = cmd;
}



/* Using 4 MiB mapping for the Linxu guest VM.
 * This is a temporary solution for enabling guest VM
 * devices that need DMA while still providing some
 * protections. Once the device untyped feature is done,
 * this code should be replaced with proper user-mode
 * VM initialisation code.
 */

static void
plat_smmu_vm_mapping(word_t iopd, word_t gpa, word_t pa, word_t size)
{
    iopde_t *iopde = (iopde_t *)iopd;
    while (size > 0) {
        word_t index = gpa >> IOPDE_4M_INDEX_SHIFT;
        iopde_iopde_4m_ptr_new(
            iopde + index,
            1,
            1,
            1,
            pa
        );
        gpa += BIT(IOPDE_4M_INDEX_SHIFT);
        pa += BIT(IOPDE_4M_INDEX_SHIFT);
        size -= BIT(IOPDE_4M_INDEX_SHIFT);
    }
}

BOOT_CODE int
plat_smmu_init(void)
{
    uint32_t asid = 1;
    int i = 0;

    smmu_disable();

    for (i = 0; i < ARM_PLAT_NUM_SMMU; i++) {
        iopde_t *pd = (iopde_t *)alloc_region(SMMU_PD_BITS);

        if (pd == 0) {
            printf("Failed to allocate SMMU IOPageDirectory for ASID %d\n", asid);
            return 0;
        }

        memset(pd, 0, BIT(SMMU_PD_BITS));
        if (config_set(CONFIG_ARM_SMMU_VM_DEFAULT_MAPPING)) {
            plat_smmu_vm_mapping((word_t)pd, VM_GUEST_PA_START, VM_HOST_PA_START, VM_HOST_PA_SIZE);
        }
        cleanCacheRange_RAM((word_t)pd, ((word_t)pd + BIT(SMMU_PD_BITS)),
                            addrFromPPtr(pd));

        smmu_regs->smmu_ptb_asid = asid;

        /* make it read/write/nonsecure but all translation entries are invalid */
        smmu_regs->smmu_ptb_data = make_ptb_data(pptr_to_paddr(pd), true, true, true);
        asid++;
    }
    printf("Total %d IOASID set up\n", (asid - 1));

    /* now assign IOASID to each module */
    smmu_regs->smmu_afi_asid = SMMU_AFI_ASID | MODULE_ASID_ENABLE;
    smmu_regs->smmu_avpc_asid = SMMU_AVPC_ASID | MODULE_ASID_ENABLE;
    smmu_regs->smmu_dc_asid = SMMU_DC_ASID | MODULE_ASID_ENABLE;
    smmu_regs->smmu_dcb_asid = SMMU_DCB_ASID | MODULE_ASID_ENABLE;
    smmu_regs->smmu_hc_asid = SMMU_HC_ASID | MODULE_ASID_ENABLE;
    smmu_regs->smmu_hda_asid = SMMU_HDA_ASID | MODULE_ASID_ENABLE;
    smmu_regs->smmu_isp2_asid = SMMU_ISP2_ASID | MODULE_ASID_ENABLE;
    smmu_regs->smmu_msenc_asid = SMMU_MSENC_ASID | MODULE_ASID_ENABLE;
    smmu_regs->smmu_nv_asid = SMMU_NV_ASID | MODULE_ASID_ENABLE;
    smmu_regs->smmu_nv2_asid = SMMU_NV2_ASID | MODULE_ASID_ENABLE;
    smmu_regs->smmu_ppcs_asid = SMMU_PPCS_ASID | MODULE_ASID_ENABLE;
    smmu_regs->smmu_sata_asid = SMMU_SATA_ASID | MODULE_ASID_ENABLE;
    smmu_regs->smmu_vde_asid = SMMU_VDE_ASID | MODULE_ASID_ENABLE;
    smmu_regs->smmu_vi_asid = SMMU_VI_ASID | MODULE_ASID_ENABLE;
    smmu_regs->smmu_vic_asid = SMMU_VIC_ASID | MODULE_ASID_ENABLE;
    smmu_regs->smmu_xusb_host_asid = SMMU_XUSB_HOST_ASID | MODULE_ASID_ENABLE;
    smmu_regs->smmu_xusb_dev_asid = SMMU_XUSB_DEV_ASID | MODULE_ASID_ENABLE;
    smmu_regs->smmu_tsec_asid = SMMU_TSEC_ASID | MODULE_ASID_ENABLE;
    smmu_regs->smmu_ppcs1_asid = SMMU_PPCS1_ASID | MODULE_ASID_ENABLE;
    smmu_regs->smmu_sdmmc1a_asid = SMMU_SDMMC1A_ASID | MODULE_ASID_ENABLE;
    smmu_regs->smmu_sdmmc2a_asid = SMMU_SDMMC2A_ASID | MODULE_ASID_ENABLE;
    smmu_regs->smmu_sdmmc3a_asid = SMMU_SDMMC3A_ASID | MODULE_ASID_ENABLE;
    smmu_regs->smmu_sdmmc4a_asid = SMMU_SDMMC4A_ASID | MODULE_ASID_ENABLE;
    smmu_regs->smmu_isp2b_asid = SMMU_ISP2B_ASID | MODULE_ASID_ENABLE;
    smmu_regs->smmu_gpu_asid = SMMU_GPU_ASID | MODULE_ASID_ENABLE;
    smmu_regs->smmu_gpub_asid = SMMU_GPUB_ASID | MODULE_ASID_ENABLE;
    smmu_regs->smmu_ppcs2_asid = SMMU_PPCS2_ASID | MODULE_ASID_ENABLE;

    /* flush page table cache */
    plat_smmu_ptc_flush_all();
    /* flush TLB              */
    plat_smmu_tlb_flush_all();
    smmu_enable();

    /* also need to unmask interrupts */
    smmu_regs->intmask = BIT(MC_APB_ASID_UPDATE_BIT) | BIT(MC_SMMU_PAGE_BIT) |
                         BIT(MC_DECERR_MTS_BIT) | BIT(MC_SECERR_SEC_BIT) |
                         BIT(MC_DECERR_VPR_BIT) | BIT(MC_ARBITRATION_EMEM_BIT) |
                         BIT(MC_SECURITY_BIT) | BIT(MC_DECERR_EMEM_BIT);
    return ARM_PLAT_NUM_SMMU;
}


iopde_t *
plat_smmu_lookup_iopd_by_asid(uint32_t asid)
{
    iopde_t *pd = 0;
    uint32_t data = 0;
    if (asid < SMMU_FIRST_ASID || asid > SMMU_LAST_ASID) {
        return 0;
    }

    smmu_regs->smmu_ptb_asid = asid;
    data = smmu_regs->smmu_ptb_data;
    pd = (iopde_t *)(paddr_to_pptr(ptb_data_get_pd_base(data)));
    return pd;
}

void
plat_smmu_handle_interrupt(void)
{
    uint32_t status = smmu_regs->intstatus;
    uint32_t clear_status = 0;

    if (status & BIT(MC_DECERR_MTS_BIT)) {
        clear_status |= BIT(MC_DECERR_MTS_BIT);
    }
    if (status & BIT(MC_SECERR_SEC_BIT)) {
        clear_status |= BIT(MC_SECERR_SEC_BIT);
    }
    if (status & BIT(MC_DECERR_VPR_BIT)) {
        clear_status |= BIT(MC_DECERR_VPR_BIT);
    }
    if (status & BIT(MC_ARBITRATION_EMEM_BIT)) {
        clear_status |= BIT(MC_ARBITRATION_EMEM_BIT);
    }
    if (status & BIT(MC_SECURITY_BIT)) {
        clear_status |= BIT(MC_SECURITY_BIT);
    }
    if (status & BIT(MC_DECERR_EMEM_BIT)) {
        clear_status |= BIT(MC_DECERR_EMEM_BIT);
    }
    if (status & BIT(MC_APB_ASID_UPDATE_BIT)) {
        clear_status |= BIT(MC_APB_ASID_UPDATE_BIT);
    }

    /* we only care about SMMU translation failures */
    if (status & BIT(MC_SMMU_PAGE_BIT)) {
        if (config_set(DEBUG)) {
            uint32_t err_status = smmu_regs->err_status;
            uint32_t UNUSED err_adr = smmu_regs->err_adr;
            uint32_t UNUSED id = err_status & MC_ERR_ID_MASK;
            uint32_t UNUSED rw = (err_status & MC_ERR_RW_MASK);
            uint32_t UNUSED read = (err_status & MC_ERR_INVALID_SMMU_PAGE_READ_MASK);
            uint32_t UNUSED write = (err_status & MC_ERR_INVALID_SMMU_PAGE_WRITE_MASK);
            uint32_t UNUSED nonsecure = (err_status & MC_ERR_INVALID_SMMU_PAGE_NONSECURE_MASK);
            uint32_t UNUSED type = (err_status & MC_ERR_TYPE_MASK) >> MC_ERR_TYPE_SHIFT;

            printf("SMMU Address translation error:\n");
            printf("ID: %d address: 0x%x type: %d direction: 0x%x\n", id, err_adr, type, rw);
            printf("IOPT permission: read 0x%x write 0x%x nonsecure 0x%x\n", read, write, nonsecure);
        }
        clear_status |= BIT(MC_SMMU_PAGE_BIT);
    }

    /* write 1 to clear the interrupt */
    smmu_regs->intstatus = clear_status;
}
#endif
