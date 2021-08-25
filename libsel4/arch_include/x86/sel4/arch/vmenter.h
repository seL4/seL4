/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

/*
 * When performing a seL4_SysVMEnter seL4 expects certain guest state to be
 * placed in the message registers. These defines indicates which MRs hold
 * which values. Whenever a VMEnter returns seL4 will also fill these registers
 * with the corresponding guest state
 */
#define SEL4_VMENTER_CALL_EIP_MR 0
#define SEL4_VMENTER_CALL_CONTROL_PPC_MR 1
#define SEL4_VMENTER_CALL_CONTROL_ENTRY_MR 2

/*
 * In addition to the above message registers, if a VMEnter results in
 * a fault the following constants describe the contents of the message
 * registers that contain fault specific information
 */
#define SEL4_VMENTER_FAULT_REASON_MR 3
#define SEL4_VMENTER_FAULT_QUALIFICATION_MR 4
#define SEL4_VMENTER_FAULT_INSTRUCTION_LEN_MR 5
#define SEL4_VMENTER_FAULT_GUEST_PHYSICAL_MR 6
#define SEL4_VMENTER_FAULT_RFLAGS_MR 7
#define SEL4_VMENTER_FAULT_GUEST_INT_MR 8
#define SEL4_VMENTER_FAULT_CR3_MR 9
#define SEL4_VMENTER_FAULT_EAX 10
#define SEL4_VMENTER_FAULT_EBX 11
#define SEL4_VMENTER_FAULT_ECX 12
#define SEL4_VMENTER_FAULT_EDX 13
#define SEL4_VMENTER_FAULT_ESI 14
#define SEL4_VMENTER_FAULT_EDI 15
#define SEL4_VMENTER_FAULT_EBP 16

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
#define SEL4_VMENTER_RESULT_FAULT_LEN 17
#define SEL4_VMENTER_RESULT_NOTIF_LEN 3

