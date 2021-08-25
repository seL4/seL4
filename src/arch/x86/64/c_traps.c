/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>
#include <model/statedata.h>
#include <machine/fpu.h>
#include <kernel/traps.h>
#include <arch/machine/debug.h>
#include <kernel/stack.h>

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
#ifdef ENABLE_SMP_SUPPORT
            "swapgs\n"
#endif
            // Now do the vmresume
            "vmresume\n"
            "setb %%al\n"
            "sete %%bl\n"
            "movzx %%al, %%rdi\n"
            "movzx %%bl, %%rsi\n"
            // if we get here we failed
#ifdef ENABLE_SMP_SUPPORT
            "swapgs\n"
            "movq %%gs:%c[stack_offset], %%rsp\n"
#else
            "leaq kernel_stack_alloc + %c[stack_size], %%rsp\n"
#endif
            "movq %[failed], %%rax\n"
            "jmp *%%rax\n"
            :
            : [reg]"r"(&cur_thread->tcbArch.tcbVCPU->gp_registers[VCPU_EAX]),
            [failed]"i"(&vmlaunch_failed),
            [stack_size]"i"(BIT(CONFIG_KERNEL_STACK_BITS))
#ifdef ENABLE_SMP_SUPPORT
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
#ifdef ENABLE_SMP_SUPPORT
            "swapgs\n"
#endif
            // Now do the vmresume
            "vmlaunch\n"
            // if we get here we failed
            "setb %%al\n"
            "sete %%bl\n"
            "movzx %%al, %%rdi\n"
            "movzx %%bl, %%rsi\n"
#ifdef ENABLE_SMP_SUPPORT
            "swapgs\n"
            "movq %%gs:%c[stack_offset], %%rsp\n"
#else
            "leaq kernel_stack_alloc + %c[stack_size], %%rsp\n"
#endif
            "movq %[failed], %%rax\n"
            "jmp *%%rax\n"
            :
            : [reg]"r"(&cur_thread->tcbArch.tcbVCPU->gp_registers[VCPU_EAX]),
            [failed]"i"(&vmlaunch_failed),
            [stack_size]"i"(BIT(CONFIG_KERNEL_STACK_BITS))
#ifdef ENABLE_SMP_SUPPORT
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
        interrupt_t irq = servicePendingIRQ();
        /* reset our stack and jmp to the IRQ entry point */
        asm volatile(
            "movq %[stack_top], %%rsp\n"
            "movq %[syscall], %%rsi\n"
            "movq %[irq], %%rdi\n"
            "call c_handle_interrupt"
            :
            : [stack_top] "r"(&(kernel_stack_alloc[CURRENT_CPU_INDEX()][BIT(CONFIG_KERNEL_STACK_BITS)])),
            [syscall] "i"(0), /* syscall is unused for irq path */
            [irq] "r"((seL4_Word)irq)
            : "memory");
        UNREACHABLE();
    }

    tcb_t *cur_thread = NODE_STATE(ksCurThread);
    word_t *irqstack = x64KSIRQStack[CURRENT_CPU_INDEX()];
#ifdef CONFIG_VTX
    if (thread_state_ptr_get_tsType(&cur_thread->tcbState) == ThreadState_RunningVM) {
        restore_vmx();
    }
#endif
    lazyFPURestore(cur_thread);

#ifdef CONFIG_HARDWARE_DEBUG_API
    restore_user_debug_context(cur_thread);
#endif

#ifdef ENABLE_SMP_SUPPORT
#ifdef CONFIG_KERNEL_SKIM_WINDOW
    word_t user_cr3 = MODE_NODE_STATE(x64KSCurrentUserCR3);
#endif /* CONFIG_KERNEL_SKIM_WINDOW */
    swapgs();
#endif /* ENABLE_SMP_SUPPORT */

    if (config_set(CONFIG_KERNEL_X86_IBRS_BASIC)) {
        x86_disable_ibrs();
    }

    // Check if we are returning from a syscall/sysenter or from an interrupt
    // There is a special case where if we would be returning from a sysenter,
    // but are current singlestepping, do a full return like an interrupt
    if (likely(cur_thread->tcbArch.tcbContext.registers[Error] == -1) &&
        (!config_set(CONFIG_SYSENTER) || !config_set(CONFIG_HARDWARE_DEBUG_API)
         || ((cur_thread->tcbArch.tcbContext.registers[FLAGS] & FLAGS_TF) == 0))) {
        if (config_set(CONFIG_KERNEL_SKIM_WINDOW)) {
            /* if we are using the SKIM window then we are trying to hide kernel state from
             * the user in the case of Meltdown where the kernel region is effectively readable
             * by the user. To prevent a storage channel across threads through the irq stack,
             * which is idirectly controlled by the user, we need to clear the stack. We perform
             * this here since when we return *from* an interrupt we must use this stack and
             * cannot clear it. This means if we restore from interrupt, then enter from a syscall
             * and switch to a different thread we must either on syscall entry, or before leaving
             * the kernel, clear the irq stack. */
            irqstack[0] = 0;
            irqstack[1] = 0;
            irqstack[2] = 0;
            irqstack[3] = 0;
            irqstack[4] = 0;
            irqstack[5] = 0;
        }
        if (config_set(CONFIG_SYSENTER)) {
            cur_thread->tcbArch.tcbContext.registers[FLAGS] &= ~FLAGS_IF;
#if defined(ENABLE_SMP_SUPPORT) && defined(CONFIG_KERNEL_SKIM_WINDOW)
            register word_t user_cr3_r11 asm("r11") = user_cr3;
#endif
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
                // Skip FaultIP
                "addq $8, %%rsp\n"
#if defined(ENABLE_SMP_SUPPORT) && defined(CONFIG_KERNEL_SKIM_WINDOW)
                "popq %%rsp\n"
                "movq %%r11, %%cr3\n"
                "movq %%rsp, %%r11\n"
#else
                "popq %%r11\n"
#ifdef CONFIG_KERNEL_SKIM_WINDOW
                "movq (x64KSCurrentUserCR3), %%rsp\n"
                "movq %%rsp, %%cr3\n"
#endif /* CONFIG_KERNEL_SKIM_WINDOW */
#endif /* defined(ENABLE_SMP_SUPPORT) && defined(CONFIG_KERNEL_SKIM_WINDOW) */
                // More register but we can ignore and are done restoring
                // enable interrupt disabled by sysenter
                "sti\n"
                /* Return to user.
                 *
                 * SYSEXIT  0F 35     ; Return to compatibility mode from fast system call.
                 * SYSEXITQ 48 0F 35  ; Return to 64-bit mode from fast system call.
                 * */
                "sysexitq\n"
                :
                : "r"(&cur_thread->tcbArch.tcbContext.registers[RDI]),
#if defined(ENABLE_SMP_SUPPORT) && defined(CONFIG_KERNEL_SKIM_WINDOW)
                "r"(user_cr3_r11),
#endif
                [IF] "i"(FLAGS_IF)
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
#if defined(ENABLE_SMP_SUPPORT) && defined(CONFIG_KERNEL_SKIM_WINDOW)
                "popq %%rsp\n"
                "movq %%rcx, %%cr3\n"
                "movq %%rsp, %%rcx\n"
#else
                "popq %%rcx\n"
#ifdef CONFIG_KERNEL_SKIM_WINDOW
                "movq (x64KSCurrentUserCR3), %%rsp\n"
                "movq %%rsp, %%cr3\n"
#endif /* CONFIG_KERNEL_SKIM_WINDOW */
#endif /* defined(ENABLE_SMP_SUPPORT) && defined(CONFIG_KERNEL_SKIM_WINDOW) */
                // clear RSP to not leak information to the user
                "xor %%rsp, %%rsp\n"
                // More register but we can ignore and are done restoring
                // enable interrupt disabled by sysenter
                "sysretq\n"
                :
                : "r"(&cur_thread->tcbArch.tcbContext.registers[RDI])
#if defined(ENABLE_SMP_SUPPORT) && defined(CONFIG_KERNEL_SKIM_WINDOW)
                , "c"(user_cr3)
#endif
                // Clobber memory so the compiler is forced to complete all stores
                // before running this assembler
                : "memory"
            );
        }
    } else {
        /* construct our return from interrupt frame */
#ifdef CONFIG_KERNEL_SKIM_WINDOW
        /* Have to zero this to prevent storage channel */
        irqstack[0] = 0;
#endif
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
            /* skip RFLAGS, Error, NextIP, RSP, and FaultIP */
            "addq $40, %%rsp\n"
            "popq %%r11\n"

#if defined(ENABLE_SMP_SUPPORT) && defined(CONFIG_KERNEL_SKIM_WINDOW)
            /* pop into rsp as we're done with the stack for now and we need to
             * preserve our rcx value as it has our next cr3 value */
            "popq %%rsp\n"
#else
            "popq %%rcx\n"
#endif /* defined(ENABLE_SMP_SUPPORT) && defined(CONFIG_KERNEL_SKIM_WINDOW) */

#ifdef ENABLE_SMP_SUPPORT
            // Swapping gs twice here is worth it as it allows us to efficiently
            // set the user gs base previously
            "swapgs\n"
#ifdef CONFIG_KERNEL_SKIM_WINDOW
            /* now we stash rcx in the scratch space that we can access once we've
             * loaded the user cr3 */
            "movq %%rsp, %%gs:%c[scratch_offset]\n"
#endif /* CONFIG_KERNEL_SKIM_WINDOW */
            "movq %%gs:8, %%rsp\n"
#ifdef CONFIG_KERNEL_SKIM_WINDOW
            /* change to the user address space and then load the value of rcx that
             * we stashed */
            "movq %%rcx, %%cr3\n"
            "movq %%gs:%c[scratch_offset], %%rcx\n"
#endif /* CONFIG_KERNEL_SKIM_WINDOW */
            "addq $8, %%rsp\n"
            // Switch to the user GS value
            "swapgs\n"
#else /* !ENABLE_SMP_SUPPORT */
#ifdef CONFIG_KERNEL_SKIM_WINDOW
            "movq (x64KSCurrentUserCR3), %%rsp\n"
            "movq %%rsp, %%cr3\n"
#endif /* CONFIG_KERNEL_SKIM_WINDOW */
            "leaq x64KSIRQStack + 8, %%rsp\n"
#endif /* ENABLE_SMP_SUPPORT */
            "iretq\n"
            :
            : "r"(&cur_thread->tcbArch.tcbContext.registers[RDI])
#if defined(ENABLE_SMP_SUPPORT) && defined(CONFIG_KERNEL_SKIM_WINDOW)
            , "c"(user_cr3)
            , [scratch_offset] "i"(nodeSkimScratchOffset)
#endif
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
    if (config_set(CONFIG_KERNEL_X86_IBRS_BASIC)) {
        x86_enable_ibrs();
    }
    word_t *irq_stack = x64KSIRQStack[CURRENT_CPU_INDEX()];
    setRegister(NODE_STATE(ksCurThread), Error, irq_stack[0]);
    /* In the case of an interrupt the NextIP and the FaultIP should be the same value,
     * i.e. the address of the instruction the CPU was about to execute before the
     * interrupt. This is the 5th value pushed on by the hardware, so indexing from
     * the bottom is x64KSIRQStack[1] */
    setRegister(NODE_STATE(ksCurThread), NextIP, irq_stack[1]);
    setRegister(NODE_STATE(ksCurThread), FaultIP, irq_stack[1]);
    setRegister(NODE_STATE(ksCurThread), FLAGS, irq_stack[3]);
    setRegister(NODE_STATE(ksCurThread), RSP, irq_stack[4]);
    c_handle_interrupt(irq, syscall);
    UNREACHABLE();
}
