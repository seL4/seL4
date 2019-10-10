/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */
#include <config.h>

#ifdef CONFIG_ARM_SMMU
#include <arch/object/smmu.h>

exception_t decodeARMSIDControlInvocation(word_t label, unsigned int length, cptr_t cptr,
	cte_t *srcSlot, cap_t cap, extra_caps_t extraCaps,
	bool_t call, word_t *buffer) {

	word_t index, depth, sid;
	cte_t *destSlot;
	cap_t cnodeCap;
	lookupSlot_ret_t lu_ret;
	exception_t status;

	if (label != ARMSIDIssueSIDManager) {
		userError("SIDControl: Illegal operation.");
		current_syscall_error.type = seL4_IllegalOperation;
		return EXCEPTION_SYSCALL_ERROR;
	}
	if (length < 3 || extraCaps.excaprefs[0] == NULL) {
		current_syscall_error.type = seL4_TruncatedMessage;
		return EXCEPTION_SYSCALL_ERROR;
	}

	sid = getSyscallArg(0, buffer);
	index = getSyscallArg(1, buffer);
	depth = getSyscallArg(2, buffer);
	cnodeCap = extraCaps.excaprefs[0]->cap;

    if (sid >= SMMU_MAX_SID) {
        current_syscall_error.type = seL4_RangeError;
        current_syscall_error.rangeErrorMin = 0;
        current_syscall_error.rangeErrorMax = SMMU_MAX_SID - 1;
        userError("Rejecting request for SID %u. SID is greater than or equal to SMMU_MAX_SID.", (int)sid);
        return EXCEPTION_SYSCALL_ERROR;
    }
	if (smmuStateSIDTable[sid]) {
		current_syscall_error.type = seL4_RevokeFirst;
		userError("Rejecting request for SID %u. Already active.", (int)sid);
		return EXCEPTION_SYSCALL_ERROR;
	}

	lu_ret = lookupTargetSlot(cnodeCap, index, depth);
	if (lu_ret.status != EXCEPTION_NONE) {
		userError("Target slot for new SID Handler cap invalid: cap %lu, SID %u.",
			getExtraCPtr(buffer, 0), (int)sid);
		return lu_ret.status;
	}
	destSlot = lu_ret.slot;
	status = ensureEmptySlot(destSlot);
	if (status != EXCEPTION_NONE) {
		userError("Target slot for new SID Handler cap not empty: cap %lu, SID %u.",
			getExtraCPtr(buffer, 0), (int)sid);
		return status;
	}
	setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
	smmuStateSIDTable[sid] = true;
    cteInsert(cap_sid_cap_new(sid), srcSlot, destSlot);
	return EXCEPTION_NONE;
}

exception_t decodeARMSIDInvocation(word_t label, unsigned int length, cptr_t cptr,
	cte_t *srcSlot, cap_t cap, extra_caps_t extraCaps,
	bool_t call, word_t *buffer) {

	userError("SID: Illegal operation.");
	current_syscall_error.type = seL4_IllegalOperation;
	return EXCEPTION_SYSCALL_ERROR;
}

exception_t decodeARMCBControlInvocation(word_t label, unsigned int length, cptr_t cptr,
	cte_t *srcSlot, cap_t cap, extra_caps_t extraCaps,
	bool_t call, word_t *buffer) {

	word_t index, depth, cb;
	cte_t *destSlot;
	cap_t cnodeCap;
	lookupSlot_ret_t lu_ret;
	exception_t status;

	if (label != ARMCBIssueCBManager) {
		userError("CBControl: Illegal operation.");
		current_syscall_error.type = seL4_IllegalOperation;
		return EXCEPTION_SYSCALL_ERROR;
	}
	if (length < 3 || extraCaps.excaprefs[0] == NULL) {
		current_syscall_error.type = seL4_TruncatedMessage;
		return EXCEPTION_SYSCALL_ERROR;
	}

	cb = getSyscallArg(0, buffer);
	index = getSyscallArg(1, buffer);
	depth = getSyscallArg(2, buffer);
	cnodeCap = extraCaps.excaprefs[0]->cap;

    if (cb >= SMMU_MAX_CB) {
        current_syscall_error.type = seL4_RangeError;
        current_syscall_error.rangeErrorMin = 0;
        current_syscall_error.rangeErrorMax = SMMU_MAX_CB - 1;
        userError("Rejecting request for CB %u. CB is greater than or equal to SMMU_MAX_CB.", (int)cb);
        return EXCEPTION_SYSCALL_ERROR;
    }
	if (smmuStateCBTable[cb]) {
		current_syscall_error.type = seL4_RevokeFirst;
		userError("Rejecting request for CB %u. Already active.", (int)cb);
		return EXCEPTION_SYSCALL_ERROR;
	}

	lu_ret = lookupTargetSlot(cnodeCap, index, depth);
	if (lu_ret.status != EXCEPTION_NONE) {
		userError("Target slot for new SID Handler cap invalid: cap %lu, CB %u.",
			getExtraCPtr(buffer, 0), (int)cb);
		return lu_ret.status;
	}
	destSlot = lu_ret.slot;
	status = ensureEmptySlot(destSlot);
	if (status != EXCEPTION_NONE) {
		userError("Target slot for new CB Handler cap not empty: cap %lu, CB %u.",
			getExtraCPtr(buffer, 0), (int)cb);
		return status;
	}

	setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
	smmuStateCBTable[cb] = true;
	cteInsert(cap_cb_cap_new(cb), srcSlot, destSlot);
	return EXCEPTION_NONE;
}

exception_t decodeARMCBInvocation(word_t label, unsigned int length, cptr_t cptr,
	cte_t *srcSlot, cap_t cap, extra_caps_t extraCaps,
	bool_t call, word_t *buffer) {

	userError("CB: Illegal operation.");
	current_syscall_error.type = seL4_IllegalOperation;
	return EXCEPTION_SYSCALL_ERROR;

}

#endif

