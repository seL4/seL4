/*
 * Copyright (c) 2025, Capabilities Limited
 * CHERI support contributed by Capabilities Limited was developed by Hesham Almatary
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#if defined(CONFIG_HAVE_CHERI)
#include <cheri/cheri.h>

void *__capability RootCheriCap;

inline void *__capability cheri_sel4_build_cap(void *__capability src, word_t base, word_t address, word_t size,
                                               word_t perms, word_t flags, int sentry, int user)
{
    void *__capability returned_cap = (src == NULL) ? RootCheriCap : src;

    returned_cap = __builtin_cheri_perms_and(returned_cap, perms);
    returned_cap = __builtin_cheri_address_set(returned_cap, base);
    returned_cap = __builtin_cheri_bounds_set(returned_cap, size);
    returned_cap = __builtin_cheri_address_set(returned_cap, address);
    returned_cap = __builtin_cheri_flags_set(returned_cap, flags);

    if (user) {
        returned_cap = __builtin_cheri_perms_and(returned_cap, ~__CHERI_CAP_PERMISSION_ACCESS_SYSTEM_REGISTERS__);
    }

    if (sentry) {
        returned_cap = __builtin_cheri_seal_entry(returned_cap);
    }

    return returned_cap;
}

exception_t handle_SysCheriWriteRegister(cap_t tcb_cap, word_t *ipc_buffer)
{
    cap_t vRootCap;
    void *__capability constructed_cap;

    word_t reg_idx = getSyscallArg(0, ipc_buffer);
    word_t cheri_base = getSyscallArg(1, ipc_buffer);
    word_t cheri_addr = getSyscallArg(2, ipc_buffer);
    word_t cheri_size = getSyscallArg(3, ipc_buffer);
    CheriCapMeta_t cheri_meta = {.words[0] = getSyscallArg(4, ipc_buffer)};

    tcb_t *tcb = TCB_PTR(cap_thread_cap_get_capTCBPtr(tcb_cap));
    vRootCap   = current_extra_caps.excaprefs[0]->cap;
    register_t tcb_reg_idx;

    if (reg_idx < n_frameRegisters) {
        tcb_reg_idx = frameRegisters[reg_idx];
    } else if (reg_idx < n_frameRegisters + n_gpRegisters) {
        tcb_reg_idx = gpRegisters[reg_idx - n_frameRegisters];
    } else if (reg_idx == DDC) {
        tcb_reg_idx = DDC;
    } else {
        userError("SysCheriWriteRegister: Wrong reg_idx number");
        return EXCEPTION_NONE;
    }

    /* TCB is valid, try to construct a valid CHERI cap */
    if (CheriCapMeta_get_V(cheri_meta)) {
        /* If the user passed a valid VSpace cap, construct a tagged CHERI cap
         * off RootCheriCap.
         */
        if (isValidVTableRoot(vRootCap)) {
            constructed_cap = NULL;
        } else {
            /* If the user didn't pass a valid VSpace cap, construct a CHERI cap
             * off the requested reg_idx. It may or may not be tagged, we don't
             * care.
             */
            constructed_cap = (void *__capability) getRegister(tcb, tcb_reg_idx);
        }
    } else {
        /* The user requested to write an untagged CHERI cap, so just set the source
         * capability to an untagged CHERI cap with the address.
         */
        constructed_cap = (void *__capability) cheri_addr;
    }

    constructed_cap = CheriArch_BuildCap(constructed_cap,                  /* src */
                                         cheri_base,                       /* base */
                                         cheri_addr,                       /* address */
                                         cheri_size,                       /* size */
                                         cheri_meta,                       /* meta */
                                         1);                               /* user */

    setRegister(tcb, tcb_reg_idx, (rword_t)constructed_cap);
    setRegister(NODE_STATE(ksCurThread), msgInfoRegister, wordFromMessageInfo(
                    seL4_MessageInfo_new(0, 0, 0, 0)));

    return EXCEPTION_NONE;
}

exception_t handle_SysCheriReadRegister(cap_t tcb_cap, word_t *ipc_buffer)
{
    word_t reg_idx = getSyscallArg(0, ipc_buffer);
    register_t tcb_reg_idx;
    CheriCapMeta_t cheri_meta;
    void *__capability ret_reg;

    tcb_t *tcb = TCB_PTR(cap_thread_cap_get_capTCBPtr(tcb_cap));

    if (reg_idx < n_frameRegisters) {
        tcb_reg_idx = frameRegisters[reg_idx];
    } else if (reg_idx < n_frameRegisters + n_gpRegisters) {
        tcb_reg_idx = gpRegisters[reg_idx - n_frameRegisters];
    } else if (reg_idx == DDC) {
        tcb_reg_idx = DDC;
    } else {
        userError("SysCheriReadRegister: Wrong reg_idx number");
        return EXCEPTION_NONE;
    }

    ret_reg = (void *__capability) getRegister(tcb, tcb_reg_idx);
    cheri_meta = CheriArch_GetCapMeta(ret_reg);

    setMR(NODE_STATE(ksCurThread), ipc_buffer, 0, __builtin_cheri_base_get(ret_reg));
    setMR(NODE_STATE(ksCurThread), ipc_buffer, 1, __builtin_cheri_address_get(ret_reg));
    setMR(NODE_STATE(ksCurThread), ipc_buffer, 2, __builtin_cheri_length_get(ret_reg));
    setMR(NODE_STATE(ksCurThread), ipc_buffer, 3, cheri_meta.words[0]);

    return EXCEPTION_NONE;
}
#endif
