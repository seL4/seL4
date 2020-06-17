/*
 * Copyright 2016, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <string.h>
#include <util.h>
#include <arch/machine.h>

/** @file Support routines for identifying the processor family, model, etc
 * on INTEL x86 processors, as well as attempting to determine the model string.
 *
 * AMD processors would be different.
 */

const char X86_CPUID_VENDOR_STRING_INTEL[] = {'G', 'e', 'n', 'u', 'i', 'n', 'e', 'I', 'n', 't', 'e', 'l', 0};
const char X86_CPUID_VENDOR_STRING_AMD_LEGACY[] = { 'A', 'M', 'D', 'i', 's', 'b', 'e', 't', 't', 'e', 'r', '!', 0};
const char X86_CPUID_VENDOR_STRING_AMD[] = {'A', 'u', 't', 'h', 'e', 'n', 't', 'i', 'c', 'A', 'M', 'D', 0};

BOOT_BSS static cpu_identity_t cpu_identity;

BOOT_CODE cpu_identity_t *x86_cpuid_get_identity(void)
{
    return &cpu_identity;
}

BOOT_CODE x86_cpu_identity_t *x86_cpuid_get_model_info(void)
{
    return &cpu_identity.display;
}

/** Extracts the vendor string from CPUID_000H.E[BCD]X.
 * Will be one of "GenuineIntel", "AMDisbetter!", "AuthenticAMD", "CentaurHauls"
 * etc. We don't support x86 CPUs from vendors other than AMD and Intel.
 */
BOOT_CODE static void x86_cpuid_fill_vendor_string(cpu_identity_t *ci)
{
    MAY_ALIAS uint32_t *vendor_string32 = (uint32_t *)ci->vendor_string;

    if (ci == NULL) {
        return;
    }

    vendor_string32[0] = x86_cpuid_ebx(0, 0);
    vendor_string32[1] = x86_cpuid_edx(0, 0);
    vendor_string32[2] = x86_cpuid_ecx(0, 0);

    ci->vendor_string[X86_CPUID_VENDOR_STRING_MAXLENGTH] = '\0';
}

struct family_model {
    uint8_t family, model;
};

BOOT_CODE static void x86_cpuid_intel_identity_initialize(cpu_identity_t *ci,
                                                          struct family_model original)
{
    /* Next, there are some values which require additional adjustment, and
     * require you to take into account an additional extended family and model
     * ID.
     *
     * See Intel manuals vol2, section 3.2 for the literal constants.
     */
    if (original.family != 0x0F) {
        ci->display.family = original.family;
    } else {
        ci->display.family = ci->display.extended_family + original.family;
    }

    /* The Intel manuals' wording would make you think you should use the
     * original family_ID value read from CPUID.EAX, like:
     *      if (original->family == 0x06 || original->family == 0x0F) {
     *
     * But Linux doesn't do that, Linux uses the family_ID value AFTER
     * adjustment, like:
     *      if (ci->display.family == 0x06 || ci->display.family == 0x0F) {
     *
     * Additionally, even though the Intel manuals say to adjust the model
     * number if the family number is 0x6 OR 0xF, Linux just adusts it as long
     * as the family number is GREATER THAN OR EQUAL to 0x6.
     *
     * I have followed Linux in the first case, where it could be a case of
     * them having the correct interpretation of the text, but in the second case
     * where they flagrantly disobey the manual, I have not followed them.
     *
     * See Linux source at: /arch/x86/lib/cpu.c:
     *      http://lxr.free-electrons.com/source/arch/x86/lib/cpu.c
     */
    if (ci->display.family == 0x06 || ci->display.family == 0x0F) {
        ci->display.model = (ci->display.extended_model << 4u) + original.model;
    } else {
        ci->display.model = original.model;
    }
}

BOOT_CODE static void x86_cpuid_amd_identity_initialize(cpu_identity_t *ci,
                                                        struct family_model original)
{
    /* Intel and AMD's specifications give slightly different ways to compose
     * the family and model IDs (AMD CPUID manual, section 2.)
     *
     * AMD says that if family is LESS THAN 0xF, then adjustments are needed.
     * Intel says that if family == 0xF || family == 0x6, then adjustments are
     * needed.
     */
    if (original.family < 0xF) {
        ci->display.family = original.family;
        ci->display.model = original.model;
    } else {
        ci->display.family = original.family + ci->display.extended_family;
        ci->display.family = (ci->display.extended_model << 4u) + original.model;
    }
}

bool_t x86_cpuid_initialize(void)
{
    cpu_identity_t *ci = x86_cpuid_get_identity();
    struct family_model original;
    cpuid_001h_eax_t eax;
    cpuid_001h_ebx_t ebx;

    memset(ci, 0, sizeof(*ci));

    /* First determine which vendor manufactured the CPU. */
    x86_cpuid_fill_vendor_string(ci);

    /* Need both eax and ebx ouput values. */
    eax.words[0] = x86_cpuid_eax(1, 0);
    ebx.words[0] = x86_cpuid_ebx(1, 0);

    /* We now use EAX for the family, model, stepping values, and EBX for the
     * brand index. Store the original values from CPUID_001H.EAX.
     */
    original.family = cpuid_001h_eax_get_family(eax);
    original.model = cpuid_001h_eax_get_model(eax);
    ci->display.stepping = cpuid_001h_eax_get_stepping(eax);

    /* Also store extended family and model values used for adjustment */
    ci->display.extended_family = cpuid_001h_eax_get_extended_family(eax);
    ci->display.extended_model = cpuid_001h_eax_get_extended_model(eax);

    /* Also store the brand index value given in EBX */
    ci->display.brand = cpuid_001h_ebx_get_brand(ebx);

    if (strncmp(ci->vendor_string, X86_CPUID_VENDOR_STRING_INTEL,
                X86_CPUID_VENDOR_STRING_MAXLENGTH) == 0) {
        ci->vendor = X86_VENDOR_INTEL;
        x86_cpuid_intel_identity_initialize(ci, original);
        return true;
    } else if (strncmp(ci->vendor_string, X86_CPUID_VENDOR_STRING_AMD_LEGACY,
                       X86_CPUID_VENDOR_STRING_MAXLENGTH) == 0
               || strncmp(ci->vendor_string, X86_CPUID_VENDOR_STRING_AMD,
                          X86_CPUID_VENDOR_STRING_MAXLENGTH) == 0) {
        ci->vendor = X86_VENDOR_AMD;
        x86_cpuid_amd_identity_initialize(ci, original);
        return true;
    } else {
        /* CPU from unsupported vendor. Examples could be Cyrix, Centaur, etc.
         * The old time x86 clones. Return false to the boot and let the upper
         * level caller decide what to do.
         */
        ci->vendor = X86_VENDOR_OTHER;
        return false;
    }
}
