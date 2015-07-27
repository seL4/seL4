/*
 * Copyright 2014, NICTA
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(NICTA_BSD)
 */

#ifndef __LIBSEL4_BOOTINFO_H
#define __LIBSEL4_BOOTINFO_H

#include <sel4/types.h>

/* caps with fixed slot potitions in the root CNode */

enum {
    seL4_CapNull                =  0, /* null cap */
    seL4_CapInitThreadTCB       =  1, /* initial thread's TCB cap */
    seL4_CapInitThreadCNode     =  2, /* initial thread's root CNode cap */
    seL4_CapInitThreadVSpace    =  3, /* initial thread's VSpace cap */
    seL4_CapIRQControl          =  4, /* global IRQ controller cap */
    seL4_CapIOPort              =  5, /* global IO port cap (null cap if not supported) */
    seL4_CapIOSpace             =  6, /* global IO space cap (null cap if no IOMMU support) */
    seL4_CapBootInfoFrame       =  7, /* bootinfo frame cap */
    seL4_CapArchBootInfoFrame   =  8, /* arch bootinfo frame cap */
    seL4_CapInitThreadIPCBuffer =  9, /* initial thread's IPC buffer frame cap */
    seL4_CapIPI                 = 10, /* IPI cap */
    seL4_CapDomain              = 11  /* global domain controller cap */
};

/* Legacy code will have assumptions on the vspace root being a Page Directory
 * type, so for now we define one to the other */
#define seL4_CapInitThreadPD seL4_CapInitThreadVSpace

/* types */

typedef struct {
    seL4_Word start; /* first CNode slot position OF region */
    seL4_Word end;   /* first CNode slot position AFTER region */
} seL4_SlotRegion;

typedef struct {
    seL4_Word         nodeID;          /* ID [0..numNodes-1] of the seL4 node (0 if uniprocessor) */
    seL4_Word         numNodes;        /* number of seL4 nodes (1 if uniprocessor) */
    seL4_Word         numIOPTLevels;   /* number of IOMMU PT levels (0 if no IOMMU support) */
    seL4_IPCBuffer*   ipcBuffer;       /* pointer to initial thread's IPC buffer */
    seL4_SlotRegion   empty;           /* empty slots (null caps) */
    seL4_SlotRegion   sharedFrames;    /* shared-frame caps (shared between seL4 nodes) */
    seL4_SlotRegion   userImageFrames; /* userland-image frame caps */
    seL4_SlotRegion   userImagePDs;    /* userland-image PD caps */
    seL4_SlotRegion   userImagePTs;    /* userland-image PT caps */
    seL4_SlotRegion   untyped;         /* untyped-object caps (untyped caps) */
    seL4_SlotRegion   deviceUntyped;   /* untyped-object caps for device regions */
    seL4_Word         untypedPaddrList   [CONFIG_MAX_NUM_BOOTINFO_UNTYPED_CAPS]; /* physical address of each untyped cap */
    seL4_Uint8        untypedSizeBitsList[CONFIG_MAX_NUM_BOOTINFO_UNTYPED_CAPS]; /* size (2^n) bytes of each untyped cap */
    seL4_Uint8        initThreadCNodeSizeBits; /* initial thread's root CNode size (2^n slots) */
    seL4_Uint32       initThreadDomain; /* Initial thread's domain ID */
} seL4_BootInfo;

/* function declarations */

void seL4_InitBootInfo(seL4_BootInfo* bi);
seL4_BootInfo* seL4_GetBootInfo(void);

#endif // __LIBSEL4_BOOTINFO_H
