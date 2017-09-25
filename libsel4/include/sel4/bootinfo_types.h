/*
 * Copyright 2017, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(DATA61_BSD)
 */

#ifndef __LIBSEL4_BOOTINFO_TYPES_H
#define __LIBSEL4_BOOTINFO_TYPES_H

/* caps with fixed slot positions in the root CNode */

enum {
    seL4_CapNull                =  0, /* null cap */
    seL4_CapInitThreadTCB       =  1, /* initial thread's TCB cap */
    seL4_CapInitThreadCNode     =  2, /* initial thread's root CNode cap */
    seL4_CapInitThreadVSpace    =  3, /* initial thread's VSpace cap */
    seL4_CapIRQControl          =  4, /* global IRQ controller cap */
    seL4_CapASIDControl         =  5, /* global ASID controller cap */
    seL4_CapInitThreadASIDPool  =  6, /* initial thread's ASID pool cap */
    seL4_CapIOPort              =  7, /* global IO port cap (null cap if not supported) */
    seL4_CapIOSpace             =  8, /* global IO space cap (null cap if no IOMMU support) */
    seL4_CapBootInfoFrame       =  9, /* bootinfo frame cap */
    seL4_CapInitThreadIPCBuffer = 10, /* initial thread's IPC buffer frame cap */
    seL4_CapDomain              = 11, /* global domain controller cap */
    seL4_NumInitialCaps         = 12
};

/* Legacy code will have assumptions on the vspace root being a Page Directory
 * type, so for now we define one to the other */
#define seL4_CapInitThreadPD seL4_CapInitThreadVSpace

/* types */
typedef seL4_Word seL4_SlotPos;

typedef struct {
    seL4_SlotPos start; /* first CNode slot position OF region */
    seL4_SlotPos end;   /* first CNode slot position AFTER region */
} SEL4_PACKED seL4_SlotRegion;

typedef struct {
    seL4_Word  paddr;   /* physical address of untyped cap  */
    seL4_Uint8 padding1;
    seL4_Uint8 padding2;
    seL4_Uint8 sizeBits;/* size (2^n) bytes of each untyped */
    seL4_Uint8 isDevice;/* whether the untyped is a device  */
} SEL4_PACKED seL4_UntypedDesc;

typedef struct {
    seL4_Word         extraLen;        /* length of any additional bootinfo information */
    seL4_NodeId       nodeID;          /* ID [0..numNodes-1] of the seL4 node (0 if uniprocessor) */
    seL4_Word         numNodes;        /* number of seL4 nodes (1 if uniprocessor) */
    seL4_Word         numIOPTLevels;   /* number of IOMMU PT levels (0 if no IOMMU support) */
    seL4_IPCBuffer*   ipcBuffer;       /* pointer to initial thread's IPC buffer */
    seL4_SlotRegion   empty;           /* empty slots (null caps) */
    seL4_SlotRegion   sharedFrames;    /* shared-frame caps (shared between seL4 nodes) */
    seL4_SlotRegion   userImageFrames; /* userland-image frame caps */
    seL4_SlotRegion   userImagePaging; /* userland-image paging structure caps */
    seL4_SlotRegion   ioSpaceCaps;     /* IOSpace caps for ARM SMMU */
    seL4_SlotRegion   extraBIPages;    /* caps for any pages used to back the additional bootinfo information */
    seL4_Uint8        initThreadCNodeSizeBits; /* initial thread's root CNode size (2^n slots) */
    seL4_Domain       initThreadDomain; /* Initial thread's domain ID */
    seL4_Word         archInfo;        /* tsc freq on x86, unused on arm */
    seL4_SlotRegion   untyped;         /* untyped-object caps (untyped caps) */
    seL4_UntypedDesc  untypedList[CONFIG_MAX_NUM_BOOTINFO_UNTYPED_CAPS]; /* information about each untyped */
    /* the untypedList should be the last entry in this struct, in order
     * to make this struct easier to represent in other languages */
} SEL4_PACKED seL4_BootInfo;

/* If extraLen > 0 then 4K after the start of bootinfo is a region of extraLen additional
 * bootinfo structures. Bootinfo structures are arch/platform specific and may or may not
 * exist in any given execution. */
typedef struct {
    /* identifier of the following chunk. IDs are arch/platform specific */
    seL4_Word id;
    /* length of the chunk, including this header */
    seL4_Word len;
} SEL4_PACKED seL4_BootInfoHeader;

/* Bootinfo identifiers share a global namespace, even if they are arch or platform specific
 * and are enumerated here */
#define SEL4_BOOTINFO_HEADER_PADDING 0
#define SEL4_BOOTINFO_HEADER_X86_VBE 1
#define SEL4_BOOTINFO_HEADER_X86_MBMMAP 2
#define SEL4_BOOTINFO_HEADER_X86_ACPI_RSDP 3

#endif // __LIBSEL4_BOOTINFO_TYPES_H
