/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#ifdef CONFIG_VTX
typedef enum kernel_fault_context {
    /*
     * When performing a seL4_SysVMEnter seL4 expects certain guest state to be
     * placed in the message registers. These defines indicates which MRs hold
     * which values. Whenever a VMEnter returns seL4 will also fill these registers
     * with the corresponding guest state
     */
    SEL4_VMENTER_CALL_EIP_MR,
    SEL4_VMENTER_CALL_CONTROL_PPC_MR,
    SEL4_VMENTER_CALL_CONTROL_ENTRY_MR,

    /*
     * In addition to the above message registers, if a VMEnter results in
     * a fault the following constants describe the contents of the message
     * registers that contain fault specific information
     */
    SEL4_VMENTER_FAULT_REASON_MR,
    SEL4_VMENTER_FAULT_QUALIFICATION_MR,
    SEL4_VMENTER_FAULT_INSTRUCTION_LEN_MR,
    SEL4_VMENTER_FAULT_GUEST_PHYSICAL_MR,
    SEL4_VMENTER_FAULT_RFLAGS_MR,
    SEL4_VMENTER_FAULT_GUEST_INT_MR,
    SEL4_VMENTER_FAULT_CR3_MR,
    SEL4_VMENTER_FAULT_EAX,
    SEL4_VMENTER_FAULT_EBX,
    SEL4_VMENTER_FAULT_ECX,
    SEL4_VMENTER_FAULT_EDX,
    SEL4_VMENTER_FAULT_ESI,
    SEL4_VMENTER_FAULT_EDI,
    SEL4_VMENTER_FAULT_EBP,
#ifdef CONFIG_X86_64_VTX_64BIT_GUESTS
    SEL4_VMENTER_FAULT_R8,
    SEL4_VMENTER_FAULT_R9,
    SEL4_VMENTER_FAULT_R10,
    SEL4_VMENTER_FAULT_R11,
    SEL4_VMENTER_FAULT_R12,
    SEL4_VMENTER_FAULT_R13,
    SEL4_VMENTER_FAULT_R14,
    SEL4_VMENTER_FAULT_R15,
#endif
    SEL4_VMENTER_NUM_FAULT_MSGS,
} kernel_fault_context_t;
#endif
/*
 * After performing a seL4_SysVMEnter the msgInfo register is set to indicate
 * whether a return back to this thread happened due to a fault in the associated
 * VCPU, or a notification was received on the bound notification object.
 * If using the seL4_VMEnter wrapper function, then this is the return value
 *
 * In the case of a notification the badge register is the received notification
 * and the message registers are set in the same format as we passed them to
 * seL4_SysVMEnter
 *
 * If a fault is returned then the badge register is empty and the message
 * format is a combination of the format we passed to seL4_SysVMEnter with
 * additional registers described with the SEL4_VMENTER_FAULT_ constants
 */
#define SEL4_VMENTER_RESULT_FAULT 1
#define SEL4_VMENTER_RESULT_NOTIF 0

/*
 * Constants describing the number of message registers returned by the
 * kernel for each of the return cases of VMEnter
 */
#define SEL4_VMENTER_RESULT_FAULT_LEN SEL4_VMENTER_NUM_FAULT_MSGS
#define SEL4_VMENTER_RESULT_NOTIF_LEN 3

