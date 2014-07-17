/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __ARCH_MACHINE_H
#define __ARCH_MACHINE_H

#include <arch/types.h>
#include <arch/object/structures.h>
#include <arch/machine/hardware.h>
#include <arch/machine/pat.h>
#include <arch/machine/cpu_registers.h>
#include <arch/model/statedata.h>

#define wordBits 32

#define IA32_APIC_BASE_MSR      0x01B
#define IA32_SYSENTER_CS_MSR    0x174
#define IA32_SYSENTER_ESP_MSR   0x175
#define IA32_SYSENTER_EIP_MSR   0x176

word_t PURE getRestartPC(tcb_t *thread);
void setNextPC(tcb_t *thread, word_t v);

/* Address space control */
static inline paddr_t getCurrentPD(void)
{
    return ia32KSCurrentPD;
}

static inline void setCurrentPD(paddr_t addr)
{
    ia32KSCurrentPD = addr;
    write_cr3(addr);
}

/* TLB control */
void invalidateTLB(void);
void invalidateTLBentry(vptr_t vptr);

/* Invalidates page structures cache */
void invalidatePageStructureCache(void);

/* Flushes entire CPU Cache */
void ia32_wbinvd(void);

/* GDT installation */
void ia32_install_gdt(gdt_idt_ptr_t* gdt_idt_ptr);

/* IDT installation */
void ia32_install_idt(gdt_idt_ptr_t* gdt_idt_ptr);

/* LDT installation */
void ia32_install_ldt(uint32_t ldt_sel);

/* TSS installation */
void ia32_install_tss(uint32_t tss_sel);

/* Get page fault address from CR2 register */
uint32_t getFaultAddr(void);

/* Get current stack pointer */
void* get_current_esp(void);

/* Cleaning memory before user-level access */
static inline void clearMemory(void* ptr, unsigned int bits)
{
    memzero(ptr, BIT(bits));
    /* no cleaning of caches necessary on IA-32 */
}

/* Initialises MSRs required to setup sysenter and sysexit */
void init_sysenter_msrs(void);

/* Read model specific register */
uint32_t ia32_rdmsr_low(const uint32_t reg);
uint32_t ia32_rdmsr_high(const uint32_t reg);

/* Write model specific register */
void ia32_wrmsr(const uint32_t reg, const uint32_t val_high, const uint32_t val_low);

/* Read different parts of CPUID */
uint32_t ia32_cpuid_edx(uint32_t eax, uint32_t ecx);

/* Read/write memory fence */
void ia32_mfence(void);

/* sysenter entry point */
void handle_syscall(void);

void int_00(void);
void int_01(void);
void int_02(void);
void int_03(void);
void int_04(void);
void int_05(void);
void int_06(void);
void int_07(void);
void int_08(void);
void int_09(void);
void int_0a(void);
void int_0b(void);
void int_0c(void);
void int_0d(void);
void int_0e(void);
void int_0f(void);

void int_10(void);
void int_11(void);
void int_12(void);
void int_13(void);
void int_14(void);
void int_15(void);
void int_16(void);
void int_17(void);
void int_18(void);
void int_19(void);
void int_1a(void);
void int_1b(void);
void int_1c(void);
void int_1d(void);
void int_1e(void);
void int_1f(void);

void int_20(void);
void int_21(void);
void int_22(void);
void int_23(void);
void int_24(void);
void int_25(void);
void int_26(void);
void int_27(void);
void int_28(void);
void int_29(void);
void int_2a(void);
void int_2b(void);
void int_2c(void);
void int_2d(void);
void int_2e(void);
void int_2f(void);

void int_30(void);
void int_31(void);
void int_32(void);
void int_33(void);
void int_34(void);
void int_35(void);
void int_36(void);
void int_37(void);
void int_38(void);
void int_39(void);
void int_3a(void);
void int_3b(void);
void int_3c(void);
void int_3d(void);
void int_3e(void);
void int_3f(void);

void int_40(void);
void int_41(void);
void int_42(void);
void int_43(void);
void int_44(void);
void int_45(void);
void int_46(void);
void int_47(void);
void int_48(void);
void int_49(void);
void int_4a(void);
void int_4b(void);
void int_4c(void);
void int_4d(void);
void int_4e(void);
void int_4f(void);

void int_50(void);
void int_51(void);
void int_52(void);
void int_53(void);
void int_54(void);
void int_55(void);
void int_56(void);
void int_57(void);
void int_58(void);
void int_59(void);
void int_5a(void);
void int_5b(void);
void int_5c(void);
void int_5d(void);
void int_5e(void);
void int_5f(void);

void int_60(void);
void int_61(void);
void int_62(void);
void int_63(void);
void int_64(void);
void int_65(void);
void int_66(void);
void int_67(void);
void int_68(void);
void int_69(void);
void int_6a(void);
void int_6b(void);
void int_6c(void);
void int_6d(void);
void int_6e(void);
void int_6f(void);

void int_70(void);
void int_71(void);
void int_72(void);
void int_73(void);
void int_74(void);
void int_75(void);
void int_76(void);
void int_77(void);
void int_78(void);
void int_79(void);
void int_7a(void);
void int_7b(void);
void int_7c(void);
void int_7d(void);
void int_7e(void);
void int_7f(void);

void int_80(void);
void int_81(void);
void int_82(void);
void int_83(void);
void int_84(void);
void int_85(void);
void int_86(void);
void int_87(void);
void int_88(void);
void int_89(void);
void int_8a(void);
void int_8b(void);
void int_8c(void);
void int_8d(void);
void int_8e(void);
void int_8f(void);

void int_90(void);
void int_91(void);
void int_92(void);
void int_93(void);
void int_94(void);
void int_95(void);
void int_96(void);
void int_97(void);
void int_98(void);
void int_99(void);
void int_9a(void);
void int_9b(void);
void int_9c(void);
void int_9d(void);
void int_9e(void);
void int_9f(void);

void int_a0(void);
void int_a1(void);
void int_a2(void);
void int_a3(void);
void int_a4(void);
void int_a5(void);
void int_a6(void);
void int_a7(void);
void int_a8(void);
void int_a9(void);
void int_aa(void);
void int_ab(void);
void int_ac(void);
void int_ad(void);
void int_ae(void);
void int_af(void);

void int_b0(void);
void int_b1(void);
void int_b2(void);
void int_b3(void);
void int_b4(void);
void int_b5(void);
void int_b6(void);
void int_b7(void);
void int_b8(void);
void int_b9(void);
void int_ba(void);
void int_bb(void);
void int_bc(void);
void int_bd(void);
void int_be(void);
void int_bf(void);

void int_c0(void);
void int_c1(void);
void int_c2(void);
void int_c3(void);
void int_c4(void);
void int_c5(void);
void int_c6(void);
void int_c7(void);
void int_c8(void);
void int_c9(void);
void int_ca(void);
void int_cb(void);
void int_cc(void);
void int_cd(void);
void int_ce(void);
void int_cf(void);

void int_d0(void);
void int_d1(void);
void int_d2(void);
void int_d3(void);
void int_d4(void);
void int_d5(void);
void int_d6(void);
void int_d7(void);
void int_d8(void);
void int_d9(void);
void int_da(void);
void int_db(void);
void int_dc(void);
void int_dd(void);
void int_de(void);
void int_df(void);

void int_e0(void);
void int_e1(void);
void int_e2(void);
void int_e3(void);
void int_e4(void);
void int_e5(void);
void int_e6(void);
void int_e7(void);
void int_e8(void);
void int_e9(void);
void int_ea(void);
void int_eb(void);
void int_ec(void);
void int_ed(void);
void int_ee(void);
void int_ef(void);

void int_f0(void);
void int_f1(void);
void int_f2(void);
void int_f3(void);
void int_f4(void);
void int_f5(void);
void int_f6(void);
void int_f7(void);
void int_f8(void);
void int_f9(void);
void int_fa(void);
void int_fb(void);
void int_fc(void);
void int_fd(void);
void int_fe(void);
void int_ff(void);

#endif
