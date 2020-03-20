/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#define SEL4_MULTIBOOT_MAX_MMAP_ENTRIES 50
#define SEL4_MULTIBOOT_RAM_REGION_TYPE 1

typedef struct seL4_VBEInfoBlock {
    char        signature[4];
    seL4_Uint16 version;
    seL4_Uint32 oemStringPtr;
    seL4_Uint32 capabilities;
    seL4_Uint32 modeListPtr;
    seL4_Uint16 totalMemory;
    seL4_Uint16 oemSoftwareRev;
    seL4_Uint32 oemVendorNamePtr;
    seL4_Uint32 oemProductNamePtr;
    seL4_Uint32 oemProductRevPtr;
    seL4_Uint8  reserved[222];
    seL4_Uint8  oemData[256];
} SEL4_PACKED seL4_VBEInfoBlock_t;

/* the seL4_VBEModeInfoBlock struct is split into multiple parts to aid the C parser */
typedef struct seL4_VBEModeInfoCommon {
    seL4_Uint16 modeAttr;
    seL4_Uint8  winAAttr;
    seL4_Uint8  winBAttr;
    seL4_Uint16 winGranularity;
    seL4_Uint16 winSize;
    seL4_Uint16 winASeg;
    seL4_Uint16 winBSeg;
    seL4_Uint32 winFuncPtr;
    seL4_Uint16 bytesPerScanLine;
} SEL4_PACKED seL4_VBEModeInfoCommon_t;

typedef struct  seL4_VBEInfo12Part1 {
    seL4_Uint16 xRes;
    seL4_Uint16 yRes;
    seL4_Uint8  xCharSize;
    seL4_Uint8  yCharSize;
    seL4_Uint8  planes;
    seL4_Uint8  bitsPerPixel;
    seL4_Uint8  banks;
    seL4_Uint8  memoryModel;
    seL4_Uint8  bankSize;
    seL4_Uint8  imagePages;
    seL4_Uint8  reserved1;
} SEL4_PACKED seL4_VBEInfo12Part1_t;

typedef struct seL4_VBEInfo12Part2 {
    seL4_Uint8  redLen;
    seL4_Uint8  redOff;
    seL4_Uint8  greenLen;
    seL4_Uint8  greenOff;
    seL4_Uint8  blueLen;
    seL4_Uint8  blueOff;
    seL4_Uint8  rsvdLen;
    seL4_Uint8  rsvdOff;
    seL4_Uint8  directColorInfo;  /* direct color mode attributes */
} SEL4_PACKED seL4_VBEInfo12Part2_t;

typedef struct  seL4_VBEInfo20 {
    seL4_Uint32 physBasePtr;
    seL4_Uint8  reserved2[6];
} SEL4_PACKED seL4_VBEInfo20_t;

typedef struct seL4_VBEInfo30 {
    seL4_Uint16 linBytesPerScanLine;
    seL4_Uint8  bnkImagePages;
    seL4_Uint8  linImagePages;
    seL4_Uint8  linRedLen;
    seL4_Uint8  linRedOff;
    seL4_Uint8  linGreenLen;
    seL4_Uint8  linGreenOff;
    seL4_Uint8  linBlueLen;
    seL4_Uint8  linBlueOff;
    seL4_Uint8  linRsvdLen;
    seL4_Uint8  linRsvdOff;
    seL4_Uint32 maxPixelClock;
    seL4_Uint16 modeId;
    seL4_Uint8  depth;
} SEL4_PACKED seL4_VBEInfo30_t;

typedef struct seL4_VBEModeInfoBlock {
    /* All VBE revisions */
    seL4_VBEModeInfoCommon_t vbe_common;
    /* VBE 1.2+ */
    seL4_VBEInfo12Part1_t vbe12_part1;
    seL4_VBEInfo12Part2_t vbe12_part2;

    /* VBE 2.0+ */
    seL4_VBEInfo20_t vbe20;

    /* VBE 3.0+ */
    seL4_VBEInfo30_t vbe30;

    seL4_Uint8 reserved3[187];
} SEL4_PACKED seL4_VBEModeInfoBlock_t;

typedef struct _seL4_X86_BootInfo_VBE {
    seL4_BootInfoHeader header;
    seL4_VBEInfoBlock_t vbeInfoBlock;
    seL4_VBEModeInfoBlock_t vbeModeInfoBlock;
    seL4_Uint32 vbeMode;
    seL4_Uint32 vbeInterfaceSeg;
    seL4_Uint32 vbeInterfaceOff;
    seL4_Uint32 vbeInterfaceLen;
} SEL4_PACKED seL4_X86_BootInfo_VBE;

/**
 * Copy of multiboot mmap fields.
 * https://www.gnu.org/software/grub/manual/multiboot/multiboot.html
 */
typedef struct seL4_X86_mb_mmap {
    uint32_t size; // size of this struct in bytes
    uint64_t base_addr; // physical address of start of this region
    uint64_t length; // length of the region this struct represents in bytes
    uint32_t type; // memory type of region. Type 1 corresponds to RAM.
} SEL4_PACKED seL4_X86_mb_mmap_t;

typedef struct seL4_X86_BootInfo_mmap {
    seL4_BootInfoHeader header;
    seL4_Uint32 mmap_length;
    seL4_X86_mb_mmap_t mmap[SEL4_MULTIBOOT_MAX_MMAP_ENTRIES];
} SEL4_PACKED seL4_X86_BootInfo_mmap_t;

typedef struct multiboot2_fb seL4_X86_BootInfo_fb_t;

