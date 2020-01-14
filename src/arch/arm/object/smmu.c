/*
 * Copyright 2019, Data61
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

#ifdef CONFIG_ARM_SMMU

#include <arch/object/smmu.h>

static exception_t checkARMCBVspace(cap_t cap) {
	word_t cb = cap_cb_cap_get_capCB(cap); 
	cte_t *cbSlot = smmuStateCBNode + cb;

	if (unlikely(!isVTableRoot(cbSlot->cap))) {
		return EXCEPTION_SYSCALL_ERROR;
	}
	return EXCEPTION_NONE; 
}

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

	cap_t cbCap;
	cte_t *cbCapSlot;
	cte_t *cbAssignSlot;
	exception_t status;
	word_t sid; 

	if (unlikely(label != ARMSIDBindCB)) {
		userError("ARMSID: Illegal operation.");
		current_syscall_error.type = seL4_IllegalOperation;
		return EXCEPTION_SYSCALL_ERROR;
	}

	if (unlikely(extraCaps.excaprefs[0] == NULL)) {
		userError("ARMSIDBindCB: Invalid CB cap.");
		current_syscall_error.type = seL4_TruncatedMessage;
		return EXCEPTION_SYSCALL_ERROR;
	}

	cbCapSlot = extraCaps.excaprefs[0];
	cbCap = cbCapSlot->cap;

	if (unlikely(cap_get_capType(cbCap) != cap_cb_cap)) {
		userError("ARMSIDBindCB: Invalid CB cap.");
		current_syscall_error.type = seL4_InvalidCapability;
		current_syscall_error.invalidCapNumber = 1;
		return EXCEPTION_SYSCALL_ERROR;
	}

	if (unlikely(checkARMCBVspace(cbCap) != EXCEPTION_NONE)) {
		userError("ARMSIDBindCB: Invalid CB cap.");
		current_syscall_error.type = seL4_InvalidCapability;
		current_syscall_error.invalidCapNumber = 1;
		return EXCEPTION_SYSCALL_ERROR;
	}

	/*the SID number must be valid as assigned by the ARMSIDIssueSIDManager*/
	sid = cap_sid_cap_get_capSID(cap); 
	cbAssignSlot = smmuStateSIDNode + sid;

	printf("ARMSIDBindCB: Binding cb %d to sid %d\n", (int)cap_cb_cap_get_capCB(cbCap), (int)sid); 

	status = ensureEmptySlot(cbAssignSlot);
	if (status != EXCEPTION_NONE) {
		userError("ARMSIDBindCB: The SID is already bound with a context bank."); 
		return status;
	}

	setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
	/*binding the sid with cb in smmu driver*/
	smmu_sid_bind_cb(sid, cap_cb_cap_get_capCB(cbCap)); 
	/*building the connection by creating a copy of the orginial cb cap 
	in sid cnode*/
	cteInsert(cbCap, cbCapSlot, cbAssignSlot);
	/*recording SID number therefore can trigger a 
	cleanup/unbinding if the cb cap is deleted. 
	Because a cb can bound to multiple SIDs, 
	the sid number is only recored in the copy.*/
	cap_cb_cap_ptr_set_capBindSID(&(cbAssignSlot->cap), sid); 
	return EXCEPTION_NONE;
}

exception_t decodeARMCBControlInvocation(word_t label, unsigned int length, cptr_t cptr,
	cte_t *srcSlot, cap_t cap, extra_caps_t extraCaps,
	bool_t call, word_t *buffer) {

	word_t index, depth, cb;
	cte_t *destSlot;
	cap_t cnodeCap;
	lookupSlot_ret_t lu_ret;
	exception_t status;

	/*TLB invalidation for all entries*/ 
	if (label == ARMCBTLBInvalidateAll) { 
		setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);		
		smmu_tlb_invalidate_all(); 
		return EXCEPTION_NONE; 		
	}

	if (label != ARMCBIssueCBManager) {
		userError("ARMCBControl: Illegal operation.");
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
		userError("Target slot for new CB Handler cap invalid: cap %lu, CB %u.",
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
	cteInsert(cap_cb_cap_new(SID_INVALID, cb), srcSlot, destSlot);
	return EXCEPTION_NONE;
}

exception_t decodeARMCBInvocation(word_t label, unsigned int length, cptr_t cptr,
	cte_t *srcSlot, cap_t cap, extra_caps_t extraCaps,
	bool_t call, word_t *buffer) {

	cap_t vspaceCap;
	cte_t *vspaceCapSlot;
	cte_t *cbSlot;
	exception_t status;
	word_t cb; 

	switch (label) {
		case ARMCBTLBInvalidate: 
			if (unlikely(checkARMCBVspace(cap) != EXCEPTION_NONE)) {
				userError("ARMCBTLBInvalidate: the CB does not have a vspace root."); 
				current_syscall_error.type = seL4_IllegalOperation;
				return EXCEPTION_SYSCALL_ERROR;
			}
			cb = cap_cb_cap_get_capCB(cap); 
			cbSlot = smmuStateCBNode + cb;
			setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
			smmu_tlb_invalidate_cb(cb, cap_vtable_root_get_mappedASID(cbSlot->cap)); 
			return EXCEPTION_NONE; 
		
		case ARMCBAssignVspace: 
			if (unlikely(extraCaps.excaprefs[0] == NULL)) {
				current_syscall_error.type = seL4_TruncatedMessage;
				return EXCEPTION_SYSCALL_ERROR;
			}

			vspaceCapSlot = extraCaps.excaprefs[0];
			vspaceCap = vspaceCapSlot->cap;

			if (unlikely(!isVTableRoot(vspaceCap) || !cap_vtable_root_isMapped(vspaceCap))) {
				userError("ARMCBAssignVspace: the vspace is invalid"); 
				current_syscall_error.type = seL4_InvalidCapability;
				current_syscall_error.invalidCapNumber = 1;
				return EXCEPTION_SYSCALL_ERROR;
			}

			/*the cb number must be valid as assigned by the ARMCBIssueCBManager*/
			cb = cap_cb_cap_get_capCB(cap); 
			cbSlot = smmuStateCBNode + cb;
			status = ensureEmptySlot(cbSlot);
			if (status != EXCEPTION_NONE) {
				userError("ARMCBAssignVspace: the CB already assigned with a vspace root."); 
				return status;
			}

			setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
			/*setting up cb in smmu*/
			smmu_cb_assgin_vspace(cb, cap_vtable_root_get_basePtr(vspaceCap), 
			cap_vtable_root_get_mappedASID(vspaceCap)); 
			/*building connection between the vspace cap and  cb*/
			cteInsert(vspaceCap, vspaceCapSlot, cbSlot);
			cap_vtable_root_ptr_set_mappedCB(&(cbSlot->cap), cb); 
			return EXCEPTION_NONE;
		
		case ARMCBUnassignVspace: 
			if (unlikely(checkARMCBVspace(cap) != EXCEPTION_NONE)) {
				userError("ARMCBUnassignVspace: the CB does not have an assigned VSpace.");
				current_syscall_error.type = seL4_IllegalOperation;
				return EXCEPTION_SYSCALL_ERROR;
			}
			cb = cap_cb_cap_get_capCB(cap); 
			cbSlot = smmuStateCBNode + cb;
			status = cteDelete(cbSlot, true); 
			if (unlikely(status != EXCEPTION_NONE)) {
				userError("ARMCBUnassignVspace: the Assigned VSpace cannot be deleted."); 				
				return status;
			}
			setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
			return EXCEPTION_NONE;		
		default: 
			userError("ARMCBInvocation: Illegal operation.");
			current_syscall_error.type = seL4_IllegalOperation;
			return EXCEPTION_SYSCALL_ERROR;

	}
}

#endif 

