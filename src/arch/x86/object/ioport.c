/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <kernel/thread.h>
#include <api/failures.h>
#include <api/syscall.h>
#include <machine/io.h>
#include <arch/object/ioport.h>
#include <arch/api/invocation.h>
#include <plat/machine/io.h>

static inline void
apply_pattern(word_t_may_alias *w, word_t pattern, bool_t set)
{
    if (set) {
        *w |= pattern;
    } else {
        *w &= ~pattern;
    }
}

static inline word_t
make_pattern(int start, int end)
{
    // number of bits we want to have set
    int num_bits = end - start;
    // shift down to cut off the bits we don't want, then shift up to put the
    // bits into position
    return (~(word_t)0) >> (CONFIG_WORD_SIZE - num_bits) << start;
}

static exception_t
ensurePortOperationAllowed(cap_t cap, uint32_t start_port, uint32_t size)
{
    uint32_t first_allowed = cap_io_port_cap_get_capIOPortFirstPort(cap);
    uint32_t last_allowed = cap_io_port_cap_get_capIOPortLastPort(cap);
    uint32_t end_port = start_port + size - 1;
    assert(first_allowed <= last_allowed);
    assert(start_port <= end_port);

    if ((start_port < first_allowed) || (end_port > last_allowed)) {
        userError("IOPort: Ports %d--%d fall outside permitted range %d--%d.",
                  (int)start_port, (int)end_port,
                  (int)first_allowed, (int)last_allowed);
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    return EXCEPTION_NONE;
}

exception_t
decodeX86PortInvocation(
    word_t invLabel,
    word_t length,
    cptr_t cptr,
    cte_t* slot,
    cap_t cap,
    extra_caps_t excaps,
    word_t* buffer
)
{
    uint32_t res;
    uint32_t len;
    uint16_t port;
    exception_t ret;

    /* Ensure user specified at very least a port. */
    if (length < 1) {
        userError("IOPort: Truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (invLabel == X86IOPortOut8 || invLabel == X86IOPortOut16 || invLabel == X86IOPortOut32) {
        /* Ensure the incoming message is long enough for the write. */
        if (length < 2) {
            userError("IOPort Out32: Truncated message.");
            current_syscall_error.type = seL4_TruncatedMessage;
            return EXCEPTION_SYSCALL_ERROR;
        }
    }

    /* Get the port the user is trying to write to. */
    port = getSyscallArg(0, buffer) & 0xffff;

    switch (invLabel) {
    case X86IOPortIn8: { /* inport 8 bits */

        /* Check we are allowed to perform the operation. */
        ret = ensurePortOperationAllowed(cap, port, 1);
        if (ret != EXCEPTION_NONE) {
            return ret;
        }

        /* Perform the read. */
        res = in8(port);
        len = 1;
        break;
    }

    case X86IOPortIn16: { /* inport 16 bits */

        /* Check we are allowed to perform the operation. */
        ret = ensurePortOperationAllowed(cap, port, 2);
        if (ret != EXCEPTION_NONE) {
            return ret;
        }

        /* Perform the read. */
        res = in16(port);
        len = 1;
        break;
    }

    case X86IOPortIn32: { /* inport 32 bits */

        /* Check we are allowed to perform the operation. */
        ret = ensurePortOperationAllowed(cap, port, 4);
        if (ret != EXCEPTION_NONE) {
            return ret;
        }

        /* Perform the read. */
        res = in32(port);
        len = 1;
        break;
    }

    case X86IOPortOut8: { /* outport 8 bits */
        uint8_t data;

        /* Check we are allowed to perform the operation. */
        ret = ensurePortOperationAllowed(cap, port, 1);
        if (ret != EXCEPTION_NONE) {
            return ret;
        }

        /* Perform the write. */
        data = (getSyscallArg(1, buffer)) & 0xff;
        out8(port, data);
        len = 0;
        break;
    }

    case X86IOPortOut16: { /* outport 16 bits */
        uint16_t data;

        /* Check we are allowed to perform the operation. */
        ret = ensurePortOperationAllowed(cap, port, 2);
        if (ret != EXCEPTION_NONE) {
            return ret;
        }

        /* Perform the write. */
        data = (getSyscallArg(1, buffer)) & 0xffff;
        out16(port, data);
        len = 0;
        break;
    }

    case X86IOPortOut32: { /* outport 32 bits */
        uint32_t data;

        /* Check we are allowed to perform the operation. */
        ret = ensurePortOperationAllowed(cap, port, 4);
        if (ret != EXCEPTION_NONE) {
            return ret;
        }

        /* Perform the write. */
        data = getSyscallArg(1, buffer) & 0xffffffff;
        out32(port, data);
        len = 0;
        break;
    }

    default:
        userError("IOPort: Unknown operation.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (len > 0) {
        /* return the value read from the port */
        setRegister(NODE_STATE(ksCurThread), badgeRegister, 0);
        if (n_msgRegisters < 1) {
            word_t* ipcBuffer;
            ipcBuffer = lookupIPCBuffer(true, NODE_STATE(ksCurThread));
            if (ipcBuffer != NULL) {
                ipcBuffer[1] = res;
                len = 1;
            } else {
                len = 0;
            }
        } else {
            setRegister(NODE_STATE(ksCurThread), msgRegisters[0], res);
            len = 1;
        }
    }
    setRegister(NODE_STATE(ksCurThread), msgInfoRegister,
                wordFromMessageInfo(seL4_MessageInfo_new(0, 0, 0, len)));

    setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
    return EXCEPTION_NONE;
}

void
setIOPortMask(void *ioport_bitmap, uint16_t low, uint16_t high, bool_t set)
{
    //get an aliasing pointer
    word_t_may_alias *bitmap = ioport_bitmap;

    word_t low_word = low / CONFIG_WORD_SIZE;
    word_t high_word = high / CONFIG_WORD_SIZE;

    // see if we are just manipulating bits inside a single word. handling this
    // specially makes reasoning easier
    if (low_word == high_word) {
        apply_pattern(bitmap + low_word, make_pattern(low, high + 1), set);
    } else {
        word_t low_index = low % CONFIG_WORD_SIZE;
        // operate on the potentially partial first word
        apply_pattern(bitmap + low_word, make_pattern(low_index, CONFIG_WORD_SIZE), set);
        low_word++;
        // iterate over the whole words
        while (low_word < high_word) {
            apply_pattern(bitmap + low_word, ~(word_t)0, set);
            low_word++;
        }
        // apply to any remaining bits
        word_t high_index = high % CONFIG_WORD_SIZE;
        apply_pattern(bitmap + low_word, make_pattern(0, high_index + 1), set);
    }
}
