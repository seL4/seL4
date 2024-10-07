/*
 * Copyright (c) 2024, Capabilities Limited
 * Copyright (c) 2017 Alex Richardson
 * Copyright (c) 2014 Robert N. M. Watson
 *
 * CHERI support contributed by Capabilities Limited was developed by Hesham Almatary
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

/*
 * This file is copied from CHERI-LLVM's cheri_init_globals.h and modified to
 * to work for seL4.
 */
#if defined(CONFIG_HAVE_CHERI)

#include <types.h>
#include <string.h>

/* Bump this on every incompatible change */
#define CHERI_INIT_GLOBALS_VERSION 5
#define CHERI_INIT_GLOBALS_NUM_ARGS 7
#define CHERI_INIT_GLOBALS_SUPPORTS_CONSTANT_FLAG 1

typedef uint32_t  Elf32_Addr;
typedef uint32_t  Elf32_Off;
typedef uint16_t  Elf32_Half;
typedef uint32_t  Elf32_Word;
typedef uint8_t   Elf32_Char;
typedef int32_t   Elf32_Xword;

typedef uint64_t  Elf64_Addr;
typedef uint64_t  Elf64_Off;
typedef uint64_t  Elf64_Xword;
typedef uint16_t  Elf64_Half;
typedef uint32_t  Elf64_Word;
typedef uint8_t   Elf64_Char;
typedef int64_t   Elf64_Sxword;

struct capreloc {
  __SIZE_TYPE__ capability_location;
  __SIZE_TYPE__ object;
  __SIZE_TYPE__ offset;
  __SIZE_TYPE__ size;
  __SIZE_TYPE__ permissions;
};

typedef struct {
  Elf64_Addr      r_offset;
  Elf64_Xword     r_info;
  Elf64_Sxword    r_addend;
} Elf64_Rela;

#define Elf_Addr Elf64_Addr

static const __SIZE_TYPE__ function_reloc_flag = (__SIZE_TYPE__)1
                                                 << (__SIZE_WIDTH__ - 1);
static const __SIZE_TYPE__ function_pointer_permissions_mask =
    ~(__SIZE_TYPE__)(__CHERI_CAP_PERMISSION_PERMIT_SEAL__ |
                     __CHERI_CAP_PERMISSION_PERMIT_STORE_CAPABILITY__ |
                     __CHERI_CAP_PERMISSION_PERMIT_STORE__);
#ifdef __aarch64__
/*
 * Morello capreloc permission encoding (inverse of capability
 * permisisons for use with clearperm):
 *   Executable       0x8000000000013DBCULL
 *   Read-Write Data  0x8FBEULL
 *   Read-Only Data   0x1BFBEULL
 */
static const __SIZE_TYPE__ constant_reloc_flag =
    __CHERI_CAP_PERMISSION_PERMIT_STORE__;
#else
static const __SIZE_TYPE__ constant_reloc_flag = (__SIZE_TYPE__)1
                                                 << (__SIZE_WIDTH__ - 2);
#endif
static const __SIZE_TYPE__ constant_pointer_permissions_mask =
    ~(__SIZE_TYPE__)(__CHERI_CAP_PERMISSION_PERMIT_SEAL__ |
                     __CHERI_CAP_PERMISSION_PERMIT_STORE_CAPABILITY__ |
                     __CHERI_CAP_PERMISSION_PERMIT_STORE_LOCAL__ |
                     __CHERI_CAP_PERMISSION_PERMIT_STORE__ |
                     __CHERI_CAP_PERMISSION_PERMIT_EXECUTE__);
static const __SIZE_TYPE__ global_pointer_permissions_mask =
    ~(__SIZE_TYPE__)(__CHERI_CAP_PERMISSION_PERMIT_SEAL__ |
                     __CHERI_CAP_PERMISSION_PERMIT_EXECUTE__);

__attribute__((weak)) extern struct capreloc __start___cap_relocs;
__attribute__((weak)) extern struct capreloc __stop___cap_relocs;

__attribute__((weak)) extern void *__capability __cap_table_start;
__attribute__((weak)) extern void *__capability __cap_table_end;

#define ELF64_R_SYM(info)             ((info)>>32)
#define ELF64_R_TYPE(info)            ((Elf64_Word)(info))
#define ELF64_R_INFO(sym, type)       (((Elf64_Xword)(sym)<<32)+ \
                                        (Elf64_Xword)(type))
#define MORELLO_FRAG_EXECUTABLE 0x4
#define MORELLO_FRAG_RWDATA 0x2
#define MORELLO_FRAG_RODATA 0x1

#define R_MORELLO_RELATIVE  59395

/*
 * Sandbox data segments are relocated by moving DDC, since they're compiled as
 * position-dependent executables.
 */
#ifdef CHERI_INIT_GLOBALS_USE_OFFSET
#ifdef __aarch64__
#define cgetaddr_or_offset "gcoff"
#define csetaddr_or_offset "scoff"
#else
#define cgetaddr_or_offset "cgetoffset"
#define csetaddr_or_offset "csetoffset"
#endif
#define cheri_address_or_offset_set(_cap, _val)                                \
  __builtin_cheri_offset_set((_cap), (_val))
#else
#ifdef __aarch64__
#define cgetaddr_or_offset "gcvalue"
#define csetaddr_or_offset "scvalue"
#else
#define cgetaddr_or_offset "cgetaddr"
#define csetaddr_or_offset "csetaddr"
#endif
#define cheri_address_or_offset_set(_cap, _val)                                \
  __builtin_cheri_address_set((_cap), (_val))
#endif

#define __STRINGIFY2(x) #x
#define __STRINGIFY(x) __STRINGIFY2(x)
#define CGP_PERMISSIONS                                                        \
  __STRINGIFY((__CHERI_CAP_PERMISSION_PERMIT_LOAD_CAPABILITY__ |               \
               __CHERI_CAP_PERMISSION_PERMIT_LOAD__))

/* By default derive $cgp from $pcc on startup */
#ifndef GET_GCP_BASE_CAPABILITY
/* The initial PCC should have load+load_cap and span the current binary */
#define GET_GCP_BASE_CAPABILITY "cgetpcc $cgp\n\t"
#endif

#if defined(__mips__)

#if !defined(__CHERI_PURE_CAPABILITY__) || __CHERI_CAPABILITY_TABLE__ == 3
/* No need to setup $cgp for pc-relative or hybrid ABI: */
#define INIT_CGP_REGISTER_ASM
#else
#define INIT_CGP_REGISTER_ASM                                                  \
  ".option pic0\n\t"                                                           \
  /* If we already have a tagged $cgp skip the setup (e.g. called by RTLD) */  \
  "cbts $cgp, .Lskip_cgp_setup\n\t"                                            \
  "nop\n\t"                                                                    \
  GET_GCP_BASE_CAPABILITY                                                      \
  "dla $2, __cap_table_start\n\t"                                              \
  "dla $3, __cap_table_end\n\t"                                                \
  "dsubu $1, $3, $2\n\t"                                                       \
  csetaddr_or_offset " $cgp, $cgp, $2\n\t"                                     \
  "csetbounds $cgp, $cgp, $1\n\t"                                              \
  /* Clear all permissions except LOAD+LOAD_CAP */                             \
  "dli $1, " CGP_PERMISSIONS "\n\t"                                            \
  "candperm $cgp, $cgp, $1\n\t"                                                \
  ".Lskip_cgp_setup: \n\t"
#endif

/*
 * Defines a __start function that sets up $cgp and then branches to
 * c_startup_fn which does the real startup
 */
#define DEFINE_CHERI_START_FUNCTION(c_startup_fn)                              \
  __asm__(".text\n\t"                                                          \
          ".set noreorder\n\t"                                                 \
          ".global __start\n\t"                                                \
          ".ent __start\n\t"                                                   \
          "__start:\n\t"                                                       \
          INIT_CGP_REGISTER_ASM                                                \
          ".set noat\n\t"                                                      \
          /* Setup $c12 correctly in case we are inferring $cgp from $c12 */   \
          ".protected "  #c_startup_fn "\n\t"                                  \
          "lui $1, %pcrel_hi("  #c_startup_fn " - 8)\n\t"                      \
          "daddiu $1, $1, %pcrel_lo("  #c_startup_fn " - 4)\n\t"               \
          "cgetpcc $c12\n\t"                                                   \
          "cincoffset $c12, $c12, $1\n\t"                                      \
          "cjr $c12\n\t"                                                       \
          "nop\n\t"                                                            \
          ".end __start");

#elif defined(__riscv)
/* No DEFINE_CHERI_START_FUNCTION needed; everything is currently PC-relative
 * using AUIPCC. */
#elif defined(__aarch64__)
/* No DEFINE_CHERI_START_FUNCTION needed. */
#else
#error Unknown architecture
#endif

static void *RootKernelCap;
static void *RootUserCap;
static void *RootKernelDeviceCap;
__uintcap_t KernelVirtOffsetCap;

inline void *cheri_seal_cap(void *unsealed_cap, size_t otype) {
    void *sealer = __builtin_cheri_address_set(RootKernelCap, otype);
    return __builtin_cheri_seal(unsealed_cap, sealer);
}

inline void *cheri_unseal_cap(void *sealed_cap) {
    size_t otype = __builtin_cheri_type_get(sealed_cap);
    void *unsealer = __builtin_cheri_address_set(RootKernelCap, otype);
    return __builtin_cheri_unseal(sealed_cap, unsealer);
}

inline void *cheri_build_data_cap(ptraddr_t address, size_t size, size_t perms) {
    void *returned_cap = RootKernelCap;
    returned_cap = __builtin_cheri_perms_and(returned_cap, perms);
    returned_cap = __builtin_cheri_address_set(returned_cap, address);
    returned_cap = __builtin_cheri_bounds_set(returned_cap, size);
    return returned_cap;
}

inline void *cheri_build_code_cap( ptraddr_t address, size_t size, size_t perms) {
    void *returned_cap = RootKernelCap;
    returned_cap = __builtin_cheri_perms_and(returned_cap, perms);
    returned_cap = __builtin_cheri_address_set(returned_cap, address);
    returned_cap = __builtin_cheri_bounds_set(returned_cap, size);
    return returned_cap;
}

inline void *cheri_build_device_cap(ptraddr_t address, size_t size) {
    void *returned_cap = RootKernelDeviceCap;
    returned_cap = __builtin_cheri_address_set(returned_cap, address);
    returned_cap = __builtin_cheri_bounds_set(returned_cap, size);
    return returned_cap;
}

inline void *cheri_build_user_cap(ptraddr_t address, size_t size, size_t perms) {
    void *returned_cap = RootUserCap;
    returned_cap = __builtin_cheri_perms_and(returned_cap, perms);
    returned_cap = __builtin_cheri_address_set(returned_cap, address);
    returned_cap = __builtin_cheri_bounds_set(returned_cap, size);
    return returned_cap;
}

inline void *cheri_build_code_cap_unbounded(ptraddr_t address, size_t perms) {
    void *returned_cap = RootKernelCap;
    returned_cap = __builtin_cheri_perms_and(returned_cap, perms);
    returned_cap = __builtin_cheri_address_set(returned_cap, address);
    return returned_cap;
}

inline void *cheri_derive_data_cap(void *src, ptraddr_t address, size_t size, size_t perms) {
    void *returned_cap = src;

    if ((address + size) < address)
      return (void *)(uintptr_t) address;

    returned_cap = __builtin_cheri_perms_and(returned_cap, perms);
    returned_cap = __builtin_cheri_address_set(returned_cap, address);
    returned_cap = __builtin_cheri_bounds_set(returned_cap, size);
    return returned_cap;
}

inline void *cheri_derive_code_cap(void *src, ptraddr_t address, size_t size, size_t perms) {
    void *returned_cap = src;
    returned_cap = __builtin_cheri_perms_and(returned_cap, perms);
    returned_cap = __builtin_cheri_address_set(returned_cap, address);
    returned_cap = __builtin_cheri_bounds_set(returned_cap, size);
    return returned_cap;
}

void cheri_print_cap(const void *cap) {
    printf("cap: [v: %u | f: %u | sealed: %u | addr: %zx \
            | base: %zx | length: %zx | offset: %zx \
            | perms: %zx | otype: %zx \n",
            (uint8_t) __builtin_cheri_tag_get(cap),
            (uint8_t) __builtin_cheri_flags_get(cap),
            (uint8_t) __builtin_cheri_sealed_get(cap),
            __builtin_cheri_address_get(cap),
            __builtin_cheri_base_get(cap),
            __builtin_cheri_length_get(cap),
            __builtin_cheri_offset_get(cap),
            __builtin_cheri_perms_get(cap),
            __builtin_cheri_type_get(cap)
            );
}

static __attribute__((always_inline)) void
cheri_init_globals_impl(const struct capreloc *start_relocs,
                        const struct capreloc *stop_relocs,
                        void *__capability data_cap,
                        const void *__capability code_cap,
                        const void *__capability rodata_cap,
                        int tight_code_bounds, __SIZE_TYPE__ base_addr) {
  data_cap =
      __builtin_cheri_perms_and(data_cap, global_pointer_permissions_mask);
  code_cap =
      __builtin_cheri_perms_and(code_cap, function_pointer_permissions_mask);
  rodata_cap =
      __builtin_cheri_perms_and(rodata_cap, constant_pointer_permissions_mask);
  for (const struct capreloc *reloc = start_relocs; reloc < stop_relocs;
       reloc++) {
    const void *__capability *__capability dest =
        (const void *__capability *__capability)cheri_address_or_offset_set(
            data_cap, reloc->capability_location + base_addr);
#ifndef __aarch64__
    /* Morello uses object == 0 for functions */
    if (reloc->object == 0) {
      /* XXXAR: clang fills uninitialized capabilities with 0xcacaca..., so we
       * we need to explicitly write NULL here */
      *dest = (void *__capability)0;
      continue;
    }
#endif
    const void *__capability base_cap;
    int can_set_bounds = true;
    if ((reloc->permissions & function_reloc_flag) == function_reloc_flag) {
      base_cap = code_cap; /* code pointer */
      /* Do not set tight bounds for functions (unless we are in the plt ABI) */
      can_set_bounds = tight_code_bounds;
    } else if ((reloc->permissions & constant_reloc_flag) ==
               constant_reloc_flag) {
      base_cap = rodata_cap; /* read-only data pointer */
    } else {
      base_cap = data_cap; /* read-write data */
    }
    const void *__capability src =
        cheri_address_or_offset_set(base_cap, reloc->object + base_addr);
    if (can_set_bounds && (reloc->size != 0)) {
      src = __builtin_cheri_bounds_set(src, reloc->size);
    }
    src = __builtin_cheri_offset_increment(src, reloc->offset);
    if ((reloc->permissions & function_reloc_flag) == function_reloc_flag) {
      /* Convert function pointers to sentries: */
      src = __builtin_cheri_seal_entry(src);
    }
    *dest = src;
  }
}

static __attribute__((always_inline)) void
cheri_init_globals_3(void *__capability data_cap,
                     const void *__capability code_cap,
                     const void *__capability rodata_cap) {
  const struct capreloc *start_relocs;
  const struct capreloc *stop_relocs;
  __SIZE_TYPE__ start_addr, stop_addr;
#if defined(__mips__)
  __asm__(".option pic0\n\t"
          "dla %0, __start___cap_relocs\n\t"
          "dla %1, __stop___cap_relocs\n\t"
          : "=r"(start_addr), "=r"(stop_addr));
#elif defined(__riscv)
#if !defined(__CHERI_PURE_CAPABILITY__)
  __asm__("lla %0, __start___cap_relocs\n\t"
          "lla %1, __stop___cap_relocs\n\t"
          : "=r"(start_addr), "=r"(stop_addr));
#else
  void *__capability tmp;
  __asm__ (
       "cllc %2, __start___cap_relocs\n\t"
       cgetaddr_or_offset " %0, %2\n\t"
       "cllc %2, __stop___cap_relocs\n\t"
       cgetaddr_or_offset " %1, %2\n\t"
       :"=r"(start_addr), "=r"(stop_addr), "=&C"(tmp));
#endif
#elif defined(__aarch64__)
#if !defined(__CHERI_PURE_CAPABILITY__)
  __asm__ (
       "adrp %0, __start___cap_relocs\n\t"
       "add %0, %0, :lo12:__start___cap_relocs\n\t"
       "adrp %1, __stop___cap_relocs\n\t"
       "add %1, %1, :lo12:__stop___cap_relocs\n\t"
       : "=r"(start_addr), "=r"(stop_addr));
#else
  void *__capability tmp;
  __asm__ (
       "adrp %2, __start___cap_relocs\n\t"
       "add %2, %2, :lo12:__start___cap_relocs\n\t"
       cgetaddr_or_offset " %0, %2\n\t"
       "adrp %2, __stop___cap_relocs\n\t"
       "add %2, %2, :lo12:__stop___cap_relocs\n\t"
       cgetaddr_or_offset " %1, %2\n\t"
       : "=r"(start_addr), "=r"(stop_addr), "=&C"(tmp));
#endif
#else
#error Unknown architecture
#endif

#if !defined(__CHERI_PURE_CAPABILITY__)
  start_relocs = (const struct capreloc *)(__UINTPTR_TYPE__)start_addr;
  stop_relocs = (const struct capreloc *)(__UINTPTR_TYPE__)stop_addr;
#else
  __SIZE_TYPE__ relocs_size = stop_addr - start_addr;
  /*
   * Always get __cap_relocs relative to the initial $pcc. This should span
   * rodata and rw data, too so we can access __cap_relocs, no matter where it
   * was placed.
   */
  start_relocs = (const struct capreloc *)cheri_address_or_offset_set(
      __builtin_cheri_program_counter_get(), start_addr);
  start_relocs = __builtin_cheri_bounds_set(start_relocs, relocs_size);
  /*
   * Note: with imprecise capabilities start_relocs could have a non-zero offset
   * so we must not use setoffset!
   * TODO: use csetboundsexact and teach the linker to align __cap_relocs.
   */
  stop_relocs =
      (const struct capreloc *)(const void *)((const char *)start_relocs +
                                              relocs_size);
#endif

#if !defined(__CHERI_PURE_CAPABILITY__) || __CHERI_CAPABILITY_TABLE__ == 3
  /* pc-relative or hybrid ABI -> need large bounds on $pcc */
  int can_set_code_bounds = false;
#else
  int can_set_code_bounds = true; /* fn-desc/plt ABI -> tight bounds okay */
#endif
  /*
   * We can assume that all relocations in the __cap_relocs section have already
   * been processed so we don't need to add a relocation base address to the
   * location of the capreloc.
   */
  cheri_init_globals_impl(start_relocs, stop_relocs, data_cap, code_cap,
                          rodata_cap, can_set_code_bounds, /*relocbase=*/0);
}

static __attribute__((always_inline, unused)) void
cheri_init_globals_gdc(void *__capability gdc) {
  cheri_init_globals_3(gdc, __builtin_cheri_program_counter_get(), gdc);
}

#ifndef CHERI_INIT_GLOBALS_GDC_ONLY
static __attribute__((always_inline, unused)) void cheri_init_globals(void) {
  cheri_init_globals_gdc(__builtin_cheri_global_data_get());
}
#endif

#if defined(__aarch64__)
static __uintcap_t UNUSED
build_cap_from_fragment(Elf_Addr *fragment, Elf_Addr relocbase,
    Elf_Addr offset, void * __capability data_cap,
    const void * __capability code_cap)
{
  Elf_Addr addr, size;
  uint8_t perms;
  __uintcap_t cap;

  addr = fragment[0];
  size = fragment[1] & ((1UL << (8 * sizeof(Elf_Addr) - 8)) - 1);
  perms = fragment[1] >> (8 * sizeof(Elf_Addr) - 8);

  cap = perms == MORELLO_FRAG_EXECUTABLE ?
      (__uintcap_t)code_cap : (__uintcap_t)data_cap;
  cap = __builtin_cheri_address_set(cap, relocbase + addr);

  if (perms == MORELLO_FRAG_EXECUTABLE ||
      perms == MORELLO_FRAG_RODATA) {
    cap = __builtin_cheri_perms_and(cap, ~(__CHERI_CAP_PERMISSION_PERMIT_SEAL__ |
        __CHERI_CAP_PERMISSION_PERMIT_STORE__ | __CHERI_CAP_PERMISSION_PERMIT_STORE_CAPABILITY__ |
        __CHERI_CAP_PERMISSION_PERMIT_STORE_LOCAL__));
  }
  if (perms == MORELLO_FRAG_RWDATA ||
      perms == MORELLO_FRAG_RODATA) {
    cap = __builtin_cheri_perms_and(cap, ~(__CHERI_CAP_PERMISSION_PERMIT_SEAL__ |
        __CHERI_CAP_PERMISSION_PERMIT_EXECUTE__));
    cap = __builtin_cheri_bounds_set(cap, size);
  }
  cap += offset;
  if (perms == MORELLO_FRAG_EXECUTABLE) {
    cap = __builtin_cheri_seal_entry(cap);
  }

  return (cap);
}
#endif

BOOT_CODE UNUSED static void cheri_init_dyn_relocs(void) {
#if defined(__aarch64__)
    Elf64_Rela* rela;
    Elf64_Rela* rela_start;
    Elf64_Rela* rela_end;
    Elf64_Addr *fragment;
    uintptr_t cap;

  __asm__ (
       "adrp %0, __rela_dyn_start\n\t"
       "add %0, %0, :lo12:__rela_dyn_start\n\t"
       "adrp %1, __rela_dyn_end\n\t"
       "add %1, %1, :lo12:__rela_dyn_end\n\t"
       : "=C"(rela_start), "=C"(rela_end));

    for(rela = rela_start; rela < rela_end; rela++) {
        if (ELF64_R_TYPE(rela->r_info) != R_MORELLO_RELATIVE)
            continue;

        fragment = (Elf_Addr *)((char *)__builtin_cheri_global_data_get() + rela->r_offset);
        cap = build_cap_from_fragment(fragment, 0, rela->r_addend,
            __builtin_cheri_global_data_get(), __builtin_cheri_program_counter_get());
        *((uintptr_t *)fragment) = cap;
    }
#endif
}

BOOT_CODE void _start_purecap(void);
BOOT_CODE void _start_purecap(void)
{
    /* Use legacy cap relocs for now (which works for all CHERI architecures),
     * but in the future make this optional for CHERI archiectures like Morello
     * that have the capability table in the GOT with dynamic relocs.
     */
#if 1
    /* Init CHERI capability table using legacy caprelocs */
		cheri_init_globals_3(__builtin_cheri_global_data_get(),
													__builtin_cheri_program_counter_get(),
													__builtin_cheri_global_data_get());
#else
    /* Init CHERI capability table using dynamic relocations */
    cheri_init_dyn_relocs();
#endif
    /* We assume data/DDC is an almighty capability that will have all permissions, including
     * execute. Change this if needed.
     */
    RootKernelCap = __builtin_cheri_global_data_get();
    size_t max_length = (uintptr_t) __builtin_cheri_length_get(__builtin_cheri_global_data_get());

    /* seL4's kernel devices are mapped from KERNEL_PT_BASE to the most top address */
    RootKernelDeviceCap = cheri_derive_data_cap(RootKernelCap, KDEV_BASE, ((size_t) (max_length)) - KDEV_BASE,
        __CHERI_CAP_PERMISSION_PERMIT_LOAD__ |
        __CHERI_CAP_PERMISSION_PERMIT_STORE__);

    /* This is used by the kernel when converting from physical address to virtual kernel
     * addresses in limited scenarios eg iterating page tables
     */
    KernelVirtOffsetCap = (__uintcap_t) cheri_derive_data_cap(RootKernelCap, PPTR_BASE - PADDR_BASE, PPTR_TOP - PPTR_BASE - PADDR_BASE, -1);

    /* Root user capability for the root task. seL4 gives the user a range from 0 to
     * USER_TOP.
     * cheriTODO: invalidate DDC for the user and pass this cap instead, and the
     * executive permission.
     */
    RootUserCap = cheri_derive_data_cap(RootKernelCap, 0, (size_t) USER_TOP + 1,
        __CHERI_CAP_PERMISSION_GLOBAL__ |
        __CHERI_CAP_PERMISSION_PERMIT_LOAD__ |
#if defined(__aarch64__)
        __ARM_CAP_PERMISSION_MUTABLE_LOAD__ |
        __ARM_CAP_PERMISSION_EXECUTIVE__ |
#endif
        __CHERI_CAP_PERMISSION_PERMIT_LOAD_CAPABILITY__ |
        __CHERI_CAP_PERMISSION_PERMIT_SEAL__ |
        __CHERI_CAP_PERMISSION_PERMIT_STORE_CAPABILITY__ |
        __CHERI_CAP_PERMISSION_PERMIT_STORE_LOCAL__ |
        __CHERI_CAP_PERMISSION_PERMIT_STORE__ |
        __CHERI_CAP_PERMISSION_PERMIT_EXECUTE__);

    /* Root kernel cap from PPTR_BASE to the highest address, used to create/derive caps for the kernel mappings only */
    RootKernelCap = cheri_derive_data_cap(RootKernelCap, PPTR_BASE, KDEV_BASE - PPTR_BASE,
        __CHERI_CAP_PERMISSION_GLOBAL__ |
        __CHERI_CAP_PERMISSION_ACCESS_SYSTEM_REGISTERS__ |
        __CHERI_CAP_PERMISSION_PERMIT_LOAD__ |
        __CHERI_CAP_PERMISSION_PERMIT_LOAD_CAPABILITY__ |
#if defined(__aarch64__)
        __ARM_CAP_PERMISSION_EXECUTIVE__ |
        __ARM_CAP_PERMISSION_MUTABLE_LOAD__ |
#endif
        __CHERI_CAP_PERMISSION_PERMIT_SEAL__ |
        __CHERI_CAP_PERMISSION_PERMIT_STORE_CAPABILITY__ |
        __CHERI_CAP_PERMISSION_PERMIT_STORE_LOCAL__ |
        __CHERI_CAP_PERMISSION_PERMIT_STORE__ |
        __CHERI_CAP_PERMISSION_PERMIT_EXECUTE__);
}
#endif
