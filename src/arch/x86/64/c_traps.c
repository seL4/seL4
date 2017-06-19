/*
 * Copyright 2017, Data61
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
#include <model/statedata.h>
#include <machine/fpu.h>
#include <kernel/traps.h>
#include <arch/machine/debug.h>

#include <api/syscall.h>

#ifdef CONFIG_VTX
static void NORETURN vmlaunch_failed(word_t failInvalid, word_t failValid)
{
    NODE_LOCK_SYS;

    c_entry_hook();

    if (failInvalid) {
        userError("current VMCS pointer is not valid");
    }
    if (failValid) {
        userError("vmlaunch/vmresume error %d", (int)vmread(VMX_DATA_INSTRUCTION_ERROR));
    }

    handleVmEntryFail();
    restore_user_context();
}

static void NORETURN restore_vmx(void)
{
    restoreVMCS();
    tcb_t *cur_thread = NODE_STATE(ksCurThread);
#ifdef CONFIG_HARDWARE_DEBUG_API
    /* Do not support breakpoints in VMs, so just disable all breakpoints */
    loadAllDisabledBreakpointState(cur_thread);
#endif
    if (cur_thread->tcbArch.tcbVCPU->launched) {
        /* attempt to do a vmresume */
        asm volatile(
            // Set our stack pointer to the top of the tcb so we can efficiently pop
            "movq %[reg], %%rsp\n"
            "popq %%rax\n"
            "popq %%rbx\n"
            "popq %%rcx\n"
            "popq %%rdx\n"
            "popq %%rsi\n"
            "popq %%rdi\n"
            "popq %%rbp\n"
#if CONFIG_MAX_NUM_NODES > 1
            "swapgs\n"
#endif
            // Now do the vmresume
            "vmresume\n"
            "setb %%al\n"
            "sete %%bl\n"
            "movzx %%al, %%rdi\n"
            "movzx %%bl, %%rsi\n"
            // if we get here we failed
#if CONFIG_MAX_NUM_NODES > 1
            "swapgs\n"
            "movq %%gs:%c[stack_offset], %%rsp\n"
#else
            "leaq kernel_stack_alloc + %c[stack_size], %%rsp\n"
#endif
            "leaq %[failed], %%rax\n"
            "jmp *%%rax\n"
            :
            : [reg]"r"(&cur_thread->tcbArch.tcbVCPU->gp_registers[VCPU_EAX]),
            [failed]"m"(vmlaunch_failed),
            [stack_size]"i"(BIT(CONFIG_KERNEL_STACK_BITS))
#if CONFIG_MAX_NUM_NODES > 1
            , [stack_offset]"i"(OFFSETOF(nodeInfo_t, stackTop))
#endif
            // Clobber memory so the compiler is forced to complete all stores
            // before running this assembler
            : "memory"
        );
    } else {
        /* attempt to do a vmlaunch */
        asm volatile(
            // Set our stack pointer to the top of the tcb so we can efficiently pop
            "movq %[reg], %%rsp\n"
            "popq %%rax\n"
            "popq %%rbx\n"
            "popq %%rcx\n"
            "popq %%rdx\n"
            "popq %%rsi\n"
            "popq %%rdi\n"
            "popq %%rbp\n"
#if CONFIG_MAX_NUM_NODES > 1
            "swapgs\n"
#endif
            // Now do the vmresume
            "vmlaunch\n"
            // if we get here we failed
            "setb %%al\n"
            "sete %%bl\n"
            "movzx %%al, %%rdi\n"
            "movzx %%bl, %%rsi\n"
#if CONFIG_MAX_NUM_NODES > 1
            "swapgs\n"
            "movq %%gs:%c[stack_offset], %%rsp\n"
#else
            "leaq kernel_stack_alloc + %c[stack_size], %%rsp\n"
#endif
            "leaq %[failed], %%rax\n"
            "jmp *%%rax\n"
            :
            : [reg]"r"(&cur_thread->tcbArch.tcbVCPU->gp_registers[VCPU_EAX]),
            [failed]"m"(vmlaunch_failed),
            [stack_size]"i"(BIT(CONFIG_KERNEL_STACK_BITS))
#if CONFIG_MAX_NUM_NODES > 1
            , [stack_offset]"i"(OFFSETOF(nodeInfo_t, stackTop))
#endif
            // Clobber memory so the compiler is forced to complete all stores
            // before running this assembler
            : "memory"
        );
    }
    UNREACHABLE();
}
#endif

void VISIBLE NORETURN restore_user_context(void)
{
    NODE_UNLOCK_IF_HELD;
    c_exit_hook();

    /* we've now 'exited' the kernel. If we have a pending interrupt
     * we should 'enter' it again */
    if (ARCH_NODE_STATE(x86KSPendingInterrupt) != int_invalid) {
        irq_t irq = servicePendingIRQ();
        /* reset our stack and jmp to the IRQ entry point */
        asm volatile(
            /* round our stack back to the top to reset it */
            "andq %[stack_mask], %%rsp\n"
            "addq %[stack_size], %%rsp\n"
            "movq %[syscall], %%rsi\n"
            "movq %[irq], %%rdi\n"
            "call c_handle_interrupt"
            :
            : [stack_mask] "i"(~MASK(CONFIG_KERNEL_STACK_BITS)),
            [stack_size] "i"(BIT(CONFIG_KERNEL_STACK_BITS)),
            [syscall] "i"(0), /* syscall is unused for irq path */
            [irq] "r"((seL4_Word)irq)
            : "memory");
        UNREACHABLE();
    }

    tcb_t *cur_thread = NODE_STATE(ksCurThread);
    word_t *irqstack = MODE_NODE_STATE(x64KSIRQStack);
#ifdef CONFIG_VTX
    if (thread_state_ptr_get_tsType(&cur_thread->tcbState) == ThreadState_RunningVM) {
        restore_vmx();
    }
#endif
    lazyFPURestore(cur_thread);

#ifdef CONFIG_HARDWARE_DEBUG_API
    restore_user_debug_context(cur_thread);
#endif

#if CONFIG_MAX_NUM_NODES > 1
    cpu_id_t cpu = getCurrentCPUIndex();
    swapgs();
#endif

    /* Now that we have swapped back to the user gs we can safely
     * update the GS base. We must *not* use any kernel functions
     * that rely on having a kernel GS though. Most notably uses
     * of NODE_STATE etc cannot be used beyond this point */
    word_t base = getRegister(cur_thread, TLS_BASE);
    x86_write_fs_base(base, SMP_TERNARY(cpu, 0));

    base = cur_thread->tcbIPCBuffer;
    x86_write_gs_base(base, SMP_TERNARY(cpu, 0));

    // Check if we are returning from a syscall/sysenter or from an interrupt
    // There is a special case where if we would be returning from a sysenter,
    // but are current singlestepping, do a full return like an interrupt
    if (likely(cur_thread->tcbArch.tcbContext.registers[Error] == -1) &&
            (!config_set(CONFIG_SYSENTER) || !config_set(CONFIG_HARDWARE_DEBUG_API) || ((cur_thread->tcbArch.tcbContext.registers[FLAGS] & FLAGS_TF) == 0))) {
        if (config_set(CONFIG_SYSENTER)) {
            cur_thread->tcbArch.tcbContext.registers[FLAGS] &= ~FLAGS_IF;
            asm volatile(
                // Set our stack pointer to the top of the tcb so we can efficiently pop
                "movq %0, %%rsp\n"
                "popq %%rdi\n"
                "popq %%rsi\n"
                "popq %%rax\n"
                "popq %%rbx\n"
                "popq %%rbp\n"
                "popq %%r12\n"
                "popq %%r13\n"
                "popq %%r14\n"
                // skip RDX
                "addq $8, %%rsp\n"
                "popq %%r10\n"
                "popq %%r8\n"
                "popq %%r9\n"
                "popq %%r15\n"
                //restore RFLAGS
                "popfq\n"
                // reset interrupt bit
                "orq %[IF], -8(%%rsp)\n"
                // Restore NextIP
                "popq %%rdx\n"
                // Skip ERROR
                "addq $8, %%rsp\n"
                // Restore RSP
                "popq %%rcx\n"
                // Skip TLS_BASE, FaultIP
                "addq $16, %%rsp\n"
                "popq %%r11\n"
                // More register but we can ignore and are done restoring
                // enable interrupt disabled by sysenter
                "sti\n"
                /* return to user
                 * sysexit with rex.w user code = cs + 32, user data = cs + 40.
                 * without rex.w user code = cs + 16, user data = cs + 24
                 * */
                "rex.w sysexit\n"
                :
                : "r"(&cur_thread->tcbArch.tcbContext.registers[RDI]),
                [IF] "i" (FLAGS_IF)
                // Clobber memory so the compiler is forced to complete all stores
                // before running this assembler
                : "memory"
            );
        } else {
            asm volatile(
                // Set our stack pointer to the top of the tcb so we can efficiently pop
                "movq %0, %%rsp\n"
                "popq %%rdi\n"
                "popq %%rsi\n"
                "popq %%rax\n"
                "popq %%rbx\n"
                "popq %%rbp\n"
                "popq %%r12\n"
                "popq %%r13\n"
                "popq %%r14\n"
                "popq %%rdx\n"
                "popq %%r10\n"
                "popq %%r8\n"
                "popq %%r9\n"
                "popq %%r15\n"
                //restore RFLAGS
                "popq %%r11\n"
                // Restore NextIP
                "popq %%rcx\n"
                // clear RSP to not leak information to the user
                "xor %%rsp, %%rsp\n"
                // More register but we can ignore and are done restoring
                // enable interrupt disabled by sysenter
                "rex.w sysret\n"
                :
                : "r"(&cur_thread->tcbArch.tcbContext.registers[RDI])
                // Clobber memory so the compiler is forced to complete all stores
                // before running this assembler
                : "memory"
            );
        }
    } else {
        /* construct our return from interrupt frame */
        irqstack[1] = getRegister(cur_thread, NextIP);
        irqstack[2] = getRegister(cur_thread, CS);
        irqstack[3] = getRegister(cur_thread, FLAGS);
        irqstack[4] = getRegister(cur_thread, RSP);
        irqstack[5] = getRegister(cur_thread, SS);
        asm volatile(
            // Set our stack pointer to the top of the tcb so we can efficiently pop
            "movq %0, %%rsp\n"
            "popq %%rdi\n"
            "popq %%rsi\n"
            "popq %%rax\n"
            "popq %%rbx\n"
            "popq %%rbp\n"
            "popq %%r12\n"
            "popq %%r13\n"
            "popq %%r14\n"
            "popq %%rdx\n"
            "popq %%r10\n"
            "popq %%r8\n"
            "popq %%r9\n"
            "popq %%r15\n"
            /* skip RFLAGS, Error NextIP RSP, TLS_BASE, FaultIP */
            "addq $48, %%rsp\n"
            "popq %%r11\n"
            "popq %%rcx\n"
#if CONFIG_MAX_NUM_NODES > 1
            // Swapping gs twice here is worth it as it allows us to efficiently
            // set the user gs base previously
            "swapgs\n"
            "movq %%gs:8, %%rsp\n"
            "addq $8, %%rsp\n"
            // Switch to the user GS value
            "swapgs\n"
#else
            "leaq x64KSIRQStack + 8, %%rsp\n"
#endif
            "iretq\n"
            :
            : "r"(&cur_thread->tcbArch.tcbContext.registers[RDI])
            // Clobber memory so the compiler is forced to complete all stores
            // before running this assembler
            : "memory"
        );
    }
    UNREACHABLE();
}

void VISIBLE NORETURN c_x64_handle_interrupt(int irq, int syscall);
void VISIBLE NORETURN c_x64_handle_interrupt(int irq, int syscall)
{
    setRegister(NODE_STATE(ksCurThread), Error, MODE_NODE_STATE(x64KSIRQStack)[0]);
    /* In the case of an interrupt the NextIP and the FaultIP should be the same value,
     * i.e. the address of the instruction the CPU was about to execute before the
     * interrupt. This is the 5th value pushed on by the hardware, so indexing from
     * the bottom is x64KSIRQStack[1] */
    setRegister(NODE_STATE(ksCurThread), NextIP, MODE_NODE_STATE(x64KSIRQStack)[1]);
    setRegister(NODE_STATE(ksCurThread), FaultIP, MODE_NODE_STATE(x64KSIRQStack)[1]);
    setRegister(NODE_STATE(ksCurThread), FLAGS, MODE_NODE_STATE(x64KSIRQStack)[3]);
    setRegister(NODE_STATE(ksCurThread), RSP, MODE_NODE_STATE(x64KSIRQStack)[4]);
    c_handle_interrupt(irq, syscall);
    UNREACHABLE();
}
