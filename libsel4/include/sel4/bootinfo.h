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

#include <autoconf.h>
#include <sel4/types.h>
#include <stdint.h>

/* caps with fixed slot potitions in the root CNode */

enum {
    seL4_CapNull                =  0, /* null cap */
    seL4_CapInitThreadTCB       =  1, /* initial thread's TCB cap */
    seL4_CapInitThreadCNode     =  2, /* initial thread's root CNode cap */
    seL4_CapInitThreadPD        =  3, /* initial thread's PD cap */
    seL4_CapIRQControl          =  4, /* global IRQ controller cap */
    seL4_CapASIDControl         =  5, /* global ASID controller cap */
    seL4_CapInitThreadASIDPool  =  6, /* initial thread's ASID pool cap */
    seL4_CapIOPort              =  7, /* global IO port cap (null cap if not supported) */
    seL4_CapIOSpace             =  8, /* global IO space cap (null cap if no IOMMU support) */
    seL4_CapBootInfoFrame       =  9, /* bootinfo frame cap */
    seL4_CapInitThreadIPCBuffer = 10, /* initial thread's IPC buffer frame cap */
    seL4_CapDomain              = 11  /* global domain controller cap */
};

/* types */

typedef struct {
    seL4_Word start; /* first CNode slot position OF region */
    seL4_Word end;   /* first CNode slot position AFTER region */
} seL4_SlotRegion;

typedef struct {
    seL4_Word       basePaddr;     /* base physical address of device region */
    seL4_Word       frameSizeBits; /* size (2^n bytes) of a device-region frame */
    seL4_SlotRegion frames;        /* device-region frame caps */
} seL4_DeviceRegion;

typedef struct {
    seL4_Word         nodeID;          /* ID [0..numNodes-1] of the seL4 node (0 if uniprocessor) */
    seL4_Word         numNodes;        /* number of seL4 nodes (1 if uniprocessor) */
    seL4_Word         numIOPTLevels;   /* number of IOMMU PT levels (0 if no IOMMU support) */
    seL4_IPCBuffer*   ipcBuffer;       /* pointer to initial thread's IPC buffer */
    seL4_SlotRegion   empty;           /* empty slots (null caps) */
    seL4_SlotRegion   sharedFrames;    /* shared-frame caps (shared between seL4 nodes) */
    seL4_SlotRegion   userImageFrames; /* userland-image frame caps */
    seL4_SlotRegion   userImagePTs;    /* userland-image PT caps */
    seL4_SlotRegion   untyped;         /* untyped-object caps (untyped caps) */
    seL4_Word         untypedPaddrList   [CONFIG_MAX_NUM_BOOTINFO_UNTYPED_CAPS]; /* physical address of each untyped cap */
    uint8_t           untypedSizeBitsList[CONFIG_MAX_NUM_BOOTINFO_UNTYPED_CAPS]; /* size (2^n) bytes of each untyped cap */
    uint8_t           initThreadCNodeSizeBits; /* initial thread's root CNode size (2^n slots) */
    seL4_Word         numDeviceRegions;        /* number of device regions */
    seL4_DeviceRegion deviceRegions[CONFIG_MAX_NUM_BOOTINFO_DEVICE_REGIONS]; /* device regions */
    uint8_t           initThreadDomain; /* Initial thread's domain ID */
} seL4_BootInfo;

/* function declarations */

void seL4_InitBootInfo(seL4_BootInfo* bi);
seL4_BootInfo* seL4_GetBootInfo(void);

#endif
