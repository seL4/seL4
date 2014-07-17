/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

.section .text

.global setCurrentPD
.global setHardwareASID
.global invalidateTLB
.global invalidateHWASID
.global invalidateMVA
.global lockTLBEntry
.global cleanCacheMVA
.global cleanCacheRange
.global cleanCache
.global invalidateCacheRange
.global invalidateCache
.global getIFSR
.global getDFSR
.global getFAR

/* From faster-fastpath */
.global clearExMonitor

.macro cpwait
#ifdef XSCALE

/*
 * specifically for XSCALE clean cache operations
 * the macro is guarantees the operations to cp15 is done
 */

    str r1, r12_var 
    mrc p15, 0, r1, c2, c0, 0
    mov r1, r1
    ldr r1, r12_var
    sub pc, pc, #4
#else
    /* Do nothing! */
#endif
.endm

#ifdef XSCALE
/* the dedicated area used to clean XScale dcache, 32KB */
.data
xscale_dclean:
.align 5
.zero 32768
.data
xscale_mc:
.align 5
.zero 4096 

r12_var:
.word 0
#endif

clearExMonitor:
    bx lr


/* Address space control */
setCurrentPD:
    /* Write TTBR0 */
    mcr p15, 0, r0, c2, c0, 0

    mov pc, lr

.ltorg

setHardwareASID:
    /* Data Synchronisation Barrier. */
    mov r2, #0
    mcr p15, 0, r2, c7, c10, 4
    
    /* Read context ID. */
    mrc p15, 0, r1, c13, c0, 1

    /* Mask and update ASID. */
    bic r1, r1, #0xff
    orr r1, r1, r0

    /* Write context ID. */
    mcr p15, 0, r1, c13, c0, 1

    mov pc, lr


/* TLB control. */
invalidateTLB:
    mov r0, #0
    mcr p15, 0, r0, c8, c7, 0
    mov pc, lr

invalidateHWASID:
    mcr p15, 0, r0, c8, c7, 2
    mov pc, lr

invalidateMVA:
    /* Do the flush the TLB */
#ifdef XSCALE
    mcr p15, 0, r0, c8, c6, 1
#else
    mcr p15, 0, r0, c8, c7, 1
#endif
    mov pc, lr

lockTLBEntry:
    /* Set the lockdown register's preserve bit. */
    mrc p15, 0, r1, c10, c0, 0
    orr r1, r1, #1
    mcr p15, 0, r1, c10, c0, 0

    /* Flush any existing entry in the TLB. Note that this assumes that
     * there is no page boundary here (i.e., that the next two instruction
     * accesses don't cause TLB misses unless they are in the page being
     * locked); given that the kernel has a 16MB mapping for about 16k of
     * code, this is a reasonably safe assumption.
     */
    mcr p15, 0, r0, c8, c7, 1
    /* Now cause a TLB miss, to load the entry into the TLB locked region. */
    ldr r2, [r0]
    
    /* Clear the lockdown register's preserve bit. */
    mrc p15, 0, r1, c10, c0, 0
    bic r1, r1, #1
    mcr p15, 0, r1, c10, c0, 0
    
    mov pc, lr

/* Cache control. */
cleanCacheMVA:
    /* Clean a line of the D-Cache with a given virtual address. */
    mcr p15, 0, r0, c7, c10, 1
    mov r0, #0
    mcr p15, 0, r0, c7, c10, 4
#ifdef IXP420
    /* Flush the branch target buffer as well */
    mcr p15, 0, r0, c7, c5, 6
#endif
#ifdef XSCALE
    cpwait
#endif
    mov pc, lr

cleanCacheRange:
    /* Clean the D-Cache for a range of virtual addresses. */
	/* r0 start addr, r1 end addr
	 * have to simulate the range clean
     */
ccr_rep:
    mcr p15, 0, r0, c7, c10, 1
    add r0, r0, #32
    cmp	r1, r0
    bhi ccr_rep
    mov r0, #0
    mcr p15, 0, r0, c7, c5, 0
    mcr p15, 0, r0, c7, c10, 4
#ifdef XSCALE
    cpwait
#endif
	bx lr

cleanCache:
#ifdef XSCALE

#define CACHE_LINE_COUNT        1024
#define CACHE_LINE_SIZE         32
#define MINICACHE_LINE_COUNT    64	

    ldr r0, =xscale_dclean
    mov r1, #CACHE_LINE_COUNT
xccr_rep:
    mcr p15, 0, r0, c7, c2, 5
    add	r0, r0, #CACHE_LINE_SIZE
    subs r1, r1, #1
    bne	xccr_rep

    ldr r0, =xscale_mc
    mov r1, #MINICACHE_LINE_COUNT
lmc:
    ldr r2, [r0], #CACHE_LINE_SIZE
    subs r1, r1, #1
    bne lmc
    mov r0, #0
    mcr p15, 0, r0, c7, c6, 0
    mcr p15, 0, r0, c7, c5, 0
    mcr p15, 0, r0, c7, c10, 4
    cpwait
#else
    mov r0, #0
    mcr p15, 0, r0, c7, c10, 0
    mcr p15, 0, r0, c7, c10, 4
#endif
    mov pc, lr

invalidateCache:
    mov r0, #0
    mcr p15, 0, r0, c7, c6, 0
    mcr p15, 0, r0, c7, c5, 0
    mcr p15, 0, r0, c7, c10, 4
#ifdef XSCALE
    cpwait
#endif
    bx  lr


invalidateCacheRange:
    /* Invalidate the I-cache and D-cache for a range of virtual addresses. */
icr_rep:
    mcr p15, 0, r0, c7, c6, 1
    mcr p15, 0, r0, c7, c5, 1
    add r0, r0, #32
    cmp r1, r0
    bhi icr_rep

    mcr p15, 0, r0, c7, c5, 0
    mcr p15, 0, r0, c7, c10, 4
#ifdef XSCALE
    cpwait
#endif
	bx lr

/* Fault status. */
getIFSR:
    mrc p15, 0, r0, c5, c0, 0
    mov pc, lr

getDFSR:
    mrc p15, 0, r0, c5, c0, 0
    mov pc, lr

getFAR:
    mrc p15, 0, r0, c6, c0, 0
#ifdef XSCALE
    cpwait
#endif
    mov pc, lr
