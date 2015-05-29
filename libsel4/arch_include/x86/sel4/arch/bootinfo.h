/*
 * Copyright 2014, NICTA
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(NICTA_BSD)
 */

#ifndef __LIBSEL4_ARCH_BOOTINFO_H
#define __LIBSEL4_ARCH_BOOTINFO_H

#include <sel4/bootinfo.h>

typedef struct _seL4_VBEInfoBlock {
    char     signature[4];
    uint16_t version;
    uint32_t oemStringPtr;
    uint32_t capabilities;
    uint32_t modeListPtr;
    uint16_t totalMemory;
    uint16_t oemSoftwareRev;
    uint32_t oemVendorNamePtr;
    uint32_t oemProductNamePtr;
    uint32_t oemProductRevPtr;
    uint8_t  reserved[222];
    uint8_t  oemData[256];
} __attribute__ ((packed)) seL4_VBEInfoBlock;

typedef struct _seL4_VBEModeInfoBlock {
    /* All VBE revisions */
    uint16_t modeAttr;
    uint8_t  winAAttr;
    uint8_t  winBAttr;
    uint16_t winGranularity;
    uint16_t winSize;
    uint16_t winASeg;
    uint16_t winBSeg;
    uint32_t winFuncPtr;
    uint16_t bytesPerScanLine;

    /* VBE 1.2+ */
    uint16_t xRes;
    uint16_t yRes;
    uint8_t  xCharSize;
    uint8_t  yCharSize;
    uint8_t  planes;
    uint8_t  bitsPerPixel;
    uint8_t  banks;
    uint8_t  memoryModel;
    uint8_t  bankSize;
    uint8_t  imagePages;
    uint8_t  reserved1;

    uint8_t  redLen;
    uint8_t  redOff;
    uint8_t  greenLen;
    uint8_t  greenOff;
    uint8_t  blueLen;
    uint8_t  blueOff;
    uint8_t  rsvdLen;
    uint8_t  rsvdOff;
    uint8_t  directColorInfo;  /* direct color mode attributes */

    /* VBE 2.0+ */
    uint32_t physBasePtr;
    uint8_t  reserved2[6];

    /* VBE 3.0+ */
    uint16_t linBytesPerScanLine;
    uint8_t  bnkImagePages;
    uint8_t  linImagePages;
    uint8_t  linRedLen;
    uint8_t  linRedOff;
    uint8_t  linGreenLen;
    uint8_t  linGreenOff;
    uint8_t  linBlueLen;
    uint8_t  linBlueOff;
    uint8_t  linRsvdLen;
    uint8_t  linRsvdOff;
    uint32_t maxPixelClock;
    uint16_t modeId;
    uint8_t  depth;

    uint8_t reserved3[187];
} __attribute__ ((packed)) seL4_VBEModeInfoBlock;

typedef struct seL4_IA32_Mem_Region {
    uint64_t paddr;
    uint64_t len;
} seL4_IA32_Mem_Region_t;

typedef struct _seL4_IA32_Bootinfo {
    seL4_VBEInfoBlock vbeInfoBlock;
    seL4_VBEModeInfoBlock vbeModeInfoBlock;
    uint32_t vbeMode;
    uint32_t vbeInterfaceSeg;
    uint32_t vbeInterfaceOff;
    uint32_t vbeInterfaceLen;
    seL4_IA32_Mem_Region_t memRegions[CONFIG_MAX_MEM_REGIONS];
} __attribute__ ((packed)) seL4_IA32_BootInfo;

static inline seL4_IA32_BootInfo* seL4_IA32_GetBootInfo()
{
    return (seL4_IA32_BootInfo*)(((uint32_t)seL4_GetBootInfo()) + 4096);
}

#endif
