#
# Copyright 2014, General Dynamics C4 Systems
#
# This software may be distributed and modified according to the terms of
# the GNU General Public License version 2. Note that NO WARRANTY is provided.
# See "LICENSE_GPLv2.txt" for details.
#
# @TAG(GD_GPL)
#

# Disable built-in rules.
.SUFFIXES:

############################################################
### Build parameters
############################################################

SEL4_ARCH_LIST:=aarch32 aarch64 ia32 x86_64 riscv32 riscv64
ARCH_LIST:=arm x86 riscv
CPU_LIST:=arm1136jf-s ixp420 cortex-a7 cortex-a8 cortex-a9 cortex-a15 cortex-a53 cortex-a57
PLAT_LIST:=imx31 pc99 ixp420 omap3 am335x exynos4 exynos5 imx6 imx7 apq8064 zynq7000 zynqmp allwinnerA20 tk1 hikey bcm2837 tx1 spike
ARMV_LIST:=armv6 armv7-a armv8-a

ifndef SOURCE_ROOT
    # Assume we're in the source directory if not specified.
    SOURCE_ROOT=.
    export SOURCE_ROOT
endif

# The if statements below check to see if the make target doesn't require
# values for the ARCH, PLAT, or SEL4_ARCH variables.
ifneq (${MAKECMDGOALS},)
ifeq (${MAKECMDGOALS}, $(filter ${MAKECMDGOALS}, style astyle xmllint pylint))
# Pick an arbitrary value for ARCH, PLAT, and SEL4_ARCH to placate
# other rules that depend on them being defined.
ARCH:=x86
PLAT:=pc99
SEL4_ARCH:=ia32
endif
endif

# we do need them if we want to build anything else
$(if $(filter ${ARCH},${ARCH_LIST}),, \
	$(error ARCH ${ARCH} invalid or undefined, should be one of [${ARCH_LIST}]))

$(if $(filter ${PLAT},${PLAT_LIST}),, \
	$(error PLAT ${PLAT} invalid or undefined, should be one of [${PLAT_LIST}]))

ifeq (${ARCH}, riscv)
ifeq (${KERNEL_32}, y)
TYPE_SUFFIX:=32
SEL4_ARCH:=riscv32
else
TYPE_SUFFIX:=64
SEL4_ARCH:=riscv64
endif
endif

ifeq (${ARCH}, arm)
$(if $(filter ${CPU},${CPU_LIST}),, \
	$(error CPU ${CPU} invalid or undefined, should be one of [${CPU_LIST}]))

$(if $(filter ${ARMV},${ARMV_LIST}),, \
	$(error ARMV ${ARMV} invalid or undefined, should be one of [${ARMV_LIST}]))
ifneq (${SEL4_ARCH}, aarch64)
SEL4_ARCH:=aarch32
endif
endif

$(if $(filter ${SEL4_ARCH},${SEL4_ARCH_LIST}),, \
    $(error SEL4_ARCH ${SEL4_ARCH} invalid or undefined, should be one of [${SEL4_ARCH_LIST}]))

# If no domain configuration file was specified, use a default
# configuration of just a single domain.
ifeq (${CONFIG_DOMAIN_SCHEDULE},)
DOMAIN_CONFIG_FILE=${SOURCE_ROOT}/src/config/default_domain.c
else
DOMAIN_CONFIG_FILE=$(wildcard ${CONFIG_DOMAIN_SCHEDULE})
ifeq ($(DOMAIN_CONFIG_FILE),)
$(error Domain schedule, ${CONFIG_DOMAIN_SCHEDULE}, does not exist)
endif
endif

### Verbose building
########################################

# Set V=1 for verbose building, this can be passed in on the command line
# Set V=2 to have make echo out commands before executing them

ifeq ($V, 1)
	BUILD_VERBOSE = 1
	MAKE_SILENT = -s
	quiet = 
	Q =
else
ifeq ($V, 2)
	BUILD_VERBOSE = 1
	MAKE_SILENT =
	quiet =
	Q =
else
ifeq ($V, 3)
	BUILD_VERBOSE = 1
	MAKE_SILENT =
	quiet =
	Q =
else
	MAKE_SILENT = -s
	quiet = quiet_
	Q = @
endif
endif
endif

### Benchmarking parameters
########################################

CONFIG_DEFS=

ifdef BENCHMARK_PROFILER
CONFIG_DEFS += PROFILER
ifdef CHECKPOINT_PROFILER
CONFIG_DEFS += CHECKPOINT_PROFILER
endif # CHECKPOINT_PROFILER
else
CONFIG_DEFS = CYCLE_COUNTER
endif # BENCHMARK_PROFILER

ifdef BENCHMARK_ICACHE
CONFIG_DEFS += PERF_COUNTER=ARM_INSTRUCTION_CACHE_MISS
else
ifdef BENCHMARK_DCACHE
CONFIG_DEFS += PERF_COUNTER=ARM_DATA_CACHE_MISS
else
CONFIG_DEFS += PERF_COUNTER=CYCLE_COUNTER
endif # BENCHMARK_DCACHE
endif # BENCHMARK_ICACHE

ifdef IOMMU
CONFIG_DEFS += CONFIG_IOMMU
endif

CONFIG_DEFS += $(strip $(foreach var, \
  CONFIG_ROOT_CNODE_SIZE_BITS \
  CONFIG_TIME_SLICE \
  CONFIG_NUM_DOMAINS \
  CONFIG_NUM_PRIORITIES \
  CONFIG_RETYPE_FAN_OUT_LIMIT \
  CONFIG_MAX_NUM_WORK_UNITS_PER_PREEMPTION \
  CONFIG_MAX_NUM_BOOTINFO_DEVICE_REGIONS \
  CONFIG_MAX_NUM_BOOTINFO_UNTYPED_CAPS \
  CONFIG_TIMER_TICK_MS, \
  $(if $(value ${var}), ${var}=$(value ${var}), )))

ifdef BUILD_VERBOSE
$(info seL4 build options:)
$(info ===================)
$(info ARCH          = ${ARCH})
$(info PLAT          = ${PLAT})
$(info CPU           = ${CPU})
$(info TOOLPREFIX    = ${TOOLPREFIX})
$(info PATH          = ${PATH})
$(info DEBUG         = ${DEBUG})
$(info ASSERT        = ${ASSERT})
$(info CONFIG_DEFS   = ${CONFIG_DEFS})
$(info DANGEROUS_CODE_INJECTION = ${DANGEROUS_CODE_INJECTION})
endif

.PHONY: all default clean preprocess validate

default: all

############################################################
### Tool setup
############################################################

PATH := ${SOURCE_ROOT}/tools:${PATH}
export PATH

PARSER = c-parser

CC =
# Allow manually specifying a compiler.
ifdef CONFIG_KERNEL_COMPILER
ifneq (${CONFIG_KERNEL_COMPILER},)
ifneq (${CONFIG_KERNEL_COMPILER},"")
CC = ${CONFIG_KERNEL_COMPILER}
# Assume that this is a non-GNU compiler.
CPPFLAGS += -U__GNUC__
endif
endif
endif
ifeq (${CC},)
CC = ${TOOLPREFIX}gcc
endif

ifeq (${PYTHON},)
PYTHON = python
# Suppress python bytecode (pyc) files for build thread safety
export PYTHONDONTWRITEBYTECODE = true
endif

# Allow manually appending CPP flags.
ifneq (${CONFIG_KERNEL_EXTRA_CPPFLAGS},)
ifneq (${CONFIG_KERNEL_EXTRA_CPPFLAGS},"")
CPPFLAGS += ${CONFIG_KERNEL_EXTRA_CPPFLAGS}
endif
endif

CPP = ${TOOLPREFIX}cpp
AS = ${TOOLPREFIX}as
LD = ${TOOLPREFIX}ld
STRIP = ${TOOLPREFIX}strip
BF_GEN_PATH = ${SOURCE_ROOT}/tools/bitfield_gen.py
CHANGED = ${SOURCE_ROOT}/tools/changed.sh
CPP_GEN = ${SOURCE_ROOT}/tools/cpp_gen.sh
SYSCALL_ID_GEN_PATH = ${SOURCE_ROOT}/tools/syscall_header_gen.py
INVOCATION_ID_GEN_PATH = ${SOURCE_ROOT}/tools/invocation_header_gen.py
XMLLINT = xmllint.sh
CIRCULAR_INCLUDES = ${SOURCE_ROOT}/tools/circular_includes.py

########################################
## Check tools
########################################

ifdef BUILD_VERBOSE
$(info CC            = ${CC})
$(info CC_PATH       = ${CC_PATH})
$(info CPP           = ${CPP})
$(info CPP_PATH      = ${CPP_PATH})
$(info AS            = ${AS})
$(info AS_PATH       = ${AS_PATH})
$(info LD            = ${LD})
$(info LD_PATH       = ${LD_PATH})
$(info PARSER        = ${PARSER})
$(info PARSER_PATH   = ${PARSER_PATH})
$(info BF_GEN_PATH   = ${BF_GEN_PATH})
$(info SYSCALL_ID_GEN_PATH = ${SYSCALL_ID_GEN_PATH})
$(info INVOCATION_ID_GEN_PATH = ${INVOCATION_ID_GEN_PATH})
$(info XMLLINT_PATH  = ${XMLLINT_PATH})
$(info XMLLINT       = ${XMLLINT})
endif

ifndef SKIP_PATH_CHECKS

CC_PATH = $(shell PATH=${PATH} sh -c "which ${CC}")
$(if ${CC_PATH},,$(error ${CC} not in PATH or not executable))

CPP_PATH = $(shell PATH=${PATH} sh -c "which ${CPP}")
$(if ${CPP_PATH},,$(error ${CPP} not in PATH or not executable))

AS_PATH = $(shell PATH=${PATH} sh -c "which ${AS}")
$(if ${AS_PATH},,$(error ${AS} not in PATH or not executable))

LD_PATH = $(shell PATH=${PATH} sh -c "which ${LD}")
$(if ${LD_PATH},,$(error ${LD} not in PATH or not executable))

STRIP_PATH = $(shell PATH=${PATH} sh -c "which ${STRIP}")
$(if ${STRIP_PATH},,$(error ${STRIP} not in PATH or not executable))

PYTHON_PATH = $(shell PATH=${PATH} sh -c "which ${PYTHON}")
$(if ${PYTHON_PATH},,$(error ${PYTHON} not in PATH or not executable))

XMLLINT_PATH = $(shell PATH=${PATH} sh -c "which ${XMLLINT}")
$(if ${XMLLINT_PATH},,$(error ${XMLLINT} not in PATH or not executable))
endif

ifeq ($(SORRY_BITFIELD_PROOFS),1)
    SORRY_ARG:=--sorry_lemmas
else
    SORRY_ARG:=
endif

# If a parent Makefile has passed us DEFINES, assume they will be missing -D.
DEFINES := ${DEFINES:%=-D%}

INCLUDES = ${INCLUDE_DIRS:%=-I%}
DEFINES += ${CONFIG_DEFS:%=-D%}

ifdef DEBUG
DEFINES += -DDEBUG
CFLAGS  += -ggdb -g3
endif

ifdef DANGEROUS_CODE_INJECTION
DEFINES += -DDANGEROUS_CODE_INJECTION
endif

ifdef ASSERT
DEFINES += -DASSERT
endif

ifdef FASTPATH
DEFINES += -DFASTPATH
endif

# Only set CFLAGS if we're building standalone.
# common/Makefile.Flags sets NK_CFLAGS  in Kbuild environments.
ifndef NK_CFLAGS
STATICHEADERS += ${SOURCE_ROOT}/configs/$(PLAT)/autoconf.h
INCLUDES += "-I${SOURCE_ROOT}/configs/$(PLAT)"
DEFINES += -DHAVE_AUTOCONF
ifdef DEBUG
DEFINES += -DCONFIG_DEBUG_BUILD
DEFINE  += -DCONFIG_PRINTING
DEFINES += -DCONFIG_USER_STACK_TRACE_LENGTH=1
endif
ifeq (${ARCH}, arm)
CFLAGS += -mtune=${CPU} -marm -march=${ARMV}
ASFLAGS += -Wa,-mcpu=${CPU} -Wa,-march=${ARMV}
DEFINES += -D$(shell echo ${ARMV}|tr [:lower:] [:upper:]|tr - _)
DEFINES += -DARCH_ARM
ifeq (${SEL4_ARCH}, aarch32)
DEFINES += -D__KERNEL_32__ -DAARCH32
TYPE_SUFFIX:=32
export __ARM_32__ = y
ifeq (${CPU},cortex-a7)
DEFINES += -DARM_CORTEX_A7
endif
ifeq (${CPU},cortex-a8)
DEFINES += -DARM_CORTEX_A8
endif
ifeq (${CPU},cortex-a9)
DEFINES += -DARM_CORTEX_A9
endif
ifeq (${CPU},cortex-a15)
DEFINES += -DARM_CORTEX_A15
endif
ifeq ($(PLAT),imx6)
DEFINES += -DIMX6
endif
ifeq ($(PLAT),imx7)
DEFINES += -DIMX7
endif
ifeq ($(PLAT),imx31)
DEFINES += -DIMX31
endif
ifeq ($(PLAT),pc99)
DEFINES += -DPC99
endif
ifeq ($(PLAT),ixp420)
DEFINES += -DIXP420
endif
ifeq ($(PLAT),omap3)
DEFINES += -DOMAP3
endif
ifeq ($(PLAT),am335x)
DEFINES += -DAM335X
endif
ifeq ($(PLAT),exynos4)
DEFINES += -DEXYNOS4
endif
ifeq ($(PLAT),exynos5)
DEFINES += -DEXYNOS5
endif
ifeq ($(PLAT),apq8064)
DEFINES += -DAPQ8064
endif
ifeq ($(PLAT),zynq7000)
DEFINES += -DZYNQ7000
endif
ifeq ($(PLAT),zynqmp)
DEFINES += -DZYNQMP
endif
ifeq ($(PLAT),allwinnerA20)
DEFINES += -DALLWINNERA20
endif
ifeq ($(PLAT),bcm2837)
DEFINES += -DBCM2837
endif
endif # SEL4_ARCH=aarch32
ifeq (${SEL4_ARCH}, aarch64)
DEFINES += -D__KERNEL_64__ -DAARCH64
TYPE_SUFFIX:=64
export __ARM_64__ = y
endif # SEL4_ARCH=aarch64
ifeq (${CPU},cortex-a53)
DEFINES += -DARM_CORTEX_A53
endif
ifeq ($(PLAT),hikey)
DEFINES += -DHIKEY
endif
endif # ARCH=arm
ifeq (${SEL4_ARCH}, x86_64)
CFLAGS += -m64 -fno-asynchronous-unwind-tables
ASFLAGS += -Wa,--64
DEFINES += -DARCH_X86 -DX86_64 -DCONFIG_X86_64=y -D__KERNEL_64__ -DKERNEL_64=y -D__X86_64__=y
export __X86_64__ = y
TYPE_SUFFIX:=64
endif
ifeq (${SEL4_ARCH}, ia32)
CFLAGS += -m32
ASFLAGS += -Wa,--32
DEFINES += -DARCH_IA32 -DARCH_X86 -DX86_32 -D__KERNEL_32__
LDFLAGS += -Wl,-m,elf_i386
TYPE_SUFFIX:=32
export __X86_32__ = y
endif
else # NK_CFLAGS
# Require autoconf to be provided if larger build
$(if ${HAVE_AUTOCONF},,$(error autoconf.h not provided))
STATICHEADERS += $(srctree)/include/generated/autoconf.h
endif # NK_CFLAGS

ifeq (${SEL4_ARCH}, ia32)
INCLUDES += "-I${SOURCE_ROOT}/include/arch/$(ARCH)/arch/32"
INCLUDES += "-I${SOURCE_ROOT}/include/plat/$(PLAT)/plat/32"
else
ifeq ($(SEL4_ARCH), x86_64)
INCLUDES += "-I${SOURCE_ROOT}/include/arch/$(ARCH)/arch/64"
INCLUDES += "-I${SOURCE_ROOT}/include/plat/$(PLAT)/plat/64"
endif
endif

ifeq ($(SEL4_ARCH), aarch32)
INCLUDES += "-I${SOURCE_ROOT}/include/arch/$(ARCH)/arch/32"
INCLUDES += "-I${SOURCE_ROOT}/include/plat/$(PLAT)/plat/32"
else
ifeq ($(SEL4_ARCH), aarch64)
INCLUDES += "-I${SOURCE_ROOT}/include/arch/$(ARCH)/arch/64"
INCLUDES += "-I${SOURCE_ROOT}/include/plat/$(PLAT)/plat/64"
endif
ifeq ($(ARCH), riscv)
INCLUDES += "-I${SOURCE_ROOT}/include/arch/$(ARCH)/arch/$(TYPE_SUFFIX)"
INCLUDES += "-I${SOURCE_ROOT}/include/plat/$(PLAT)/plat/$(TYPE_SUFFIX)"
endif
endif

ifeq (${CPU}, arm1136jf-s)
DEFINES += -DARM1136_WORKAROUND
# Add definition for verified platform to support standalone kernel builds
DEFINES += -DCONFIG_ARM1136JF_S
endif

WARNINGS = all error strict-prototypes missing-prototypes nested-externs \
	missing-declarations undef pointer-arith no-nonnull

CFLAGS += --std=c99 -nostdlib -nostdinc -ffreestanding \
	${WARNINGS:%=-W%} ${INCLUDES} -fno-pic
CPPFLAGS += -nostdinc
LDFLAGS += -nostdlib -nostdinc -static
LDFLAGS += -Wl,--build-id=none
ASFLAGS += ${INCLUDES}

# As the kernel has no function pointers or other indirect jumps except those
# as generated by the compiler through switch statements we can disable jump
# tables in order to prevent Spectre Variant 2 style attacks. Note that some
# architectures treat certain kinds of function returns as indirect jumps and
# this needs other mitigations
CFLAGS += -fno-jump-tables

# Compiler optimisation level. Note that you can't build the kernel with
# GCC -Os without linking against libgcc.
ifeq (${CONFIG_OPTIMISATION_Os},y)
CFLAGS += -Os
LDFLAGS += -Os
else
ifeq (${CONFIG_OPTIMISATION_O0},y)
CFLAGS += -O0
LDFLAGS += -O0
else
ifeq (${CONFIG_OPTIMISATION_O1},y)
CFLAGS += -O1
LDFLAGS += -O1
else
ifeq (${CONFIG_OPTIMISATION_O3},y)
CFLAGS += -O3
LDFLAGS += -O3
else # Make -O2 the default for kernel builds.
CFLAGS += -O2
LDFLAGS += -O2
endif # CONFIG_OPTIMISATION_O3
endif # CONFIG_OPTIMISATION_O1
endif # CONFIG_OPTIMISATION_O0
endif # CONFIG_OPTIMISATION_Os

# Attempt to enable -fwhole-program if it was requested. Considered harmful
ifeq (${CONFIG_WHOLE_PROGRAM_OPTIMISATIONS_KERNEL}, y)
    CFLAGS += -fwhole-program
endif

# Set kernel build specific flags for the different x86 variants
# These are set here and not by the common build system as they
# only apply to building the kernel, and nothing else
ifeq (${ARCH}, x86)
CFLAGS += -mno-mmx -mno-sse -mno-sse2 -mno-3dnow
endif
ifeq (${SEL4_ARCH}, x86_64)
CFLAGS += -mcmodel=kernel
endif
ifeq (${SEL4_ARCH}, aarch64)
CFLAGS += -mgeneral-regs-only
endif
ifeq (${SEL4_ARCH}, aarch32)
CFLAGS += -mfloat-abi=soft
endif
ifeq (${ARCH}, riscv)
CFLAGS += -mcmodel=medany
endif

# Allow overriding of the CFLAGS. Use with caution.
ifdef CONFIG_KERNEL_CFLAGS
ifneq (${CONFIG_KERNEL_CFLAGS},)
ifneq (${CONFIG_KERNEL_CFLAGS},"")
CFLAGS := ${CONFIG_KERNEL_CFLAGS}
endif
endif
endif

CFLAGS += ${DEFINES}
CPPFLAGS += ${DEFINES} ${INCLUDES}

ifdef BUILD_VERBOSE
$(info CFLAGS   = ${CFLAGS})
$(info ASFLAGS  = ${ASFLAGS})
$(info CPPFLAGS = ${CPPFLAGS})
$(info LDFLAGS  = ${LDFLAGS})
endif

############################################################
### Paths
############################################################

PYTHONPATH := ${PYTHONPATH}:${SOURCE_ROOT}/tools
export PYTHONPATH

vpath %.c   ${SOURCE_ROOT}
vpath %.S   ${SOURCE_ROOT}
vpath %.h   ${SOURCE_ROOT}
vpath %.bf  ${SOURCE_ROOT}/include \
            ${SOURCE_ROOT}/include/arch/${ARCH} \
            ${SOURCE_ROOT}/include/plat/${PLAT}
vpath %.lds ${SOURCE_ROOT}

INCLUDE_DIRS = ${SOURCE_ROOT}/include ${SOURCE_ROOT}/include/arch/${ARCH} \
               ${SOURCE_ROOT}/include/plat/${PLAT} .

############################################################
### Sub-makefiles
############################################################

include ${SOURCE_ROOT}/include/arch/${ARCH}/arch/Makefile
include ${SOURCE_ROOT}/include/plat/${PLAT}/plat/Makefile
include ${SOURCE_ROOT}/src/arch/${ARCH}/Makefile
include ${SOURCE_ROOT}/src/plat/${PLAT}/Makefile
include ${SOURCE_ROOT}/include/Makefile
include ${SOURCE_ROOT}/src/Makefile

DIRECTORIES += arch plat src arch/api

############################################################
### Sources and targets
############################################################

C_SOURCES += $(patsubst %, src/arch/${ARCH}/%, ${ARCH_C_SOURCES})
ASM_SOURCES += $(patsubst %, src/arch/${ARCH}/%, ${ARCH_ASM_SOURCES})

C_SOURCES += $(patsubst %, src/plat/${PLAT}/%, ${PLAT_C_SOURCES})
ASM_SOURCES += $(patsubst %, src/plat/${PLAT}/%, ${PLAT_ASM_SOURCES})

GENHEADERS = $(patsubst %.bf, %.pbf, ${BF_SOURCES}) \
  $(patsubst %.bf, %_gen.h, ${BF_SOURCES})

GENHEADERS += arch/api/invocation.h arch/api/sel4_invocation.h api/invocation.h arch/api/syscall.h

DEFTHEORIES = $(patsubst %.bf, %_defs.thy, ${BF_SOURCES})
PROOFTHEORIES = $(patsubst %.bf, %_proofs.thy, ${BF_SOURCES})
THEORIES = ${DEFTHEORIES} ${PROOFTHEORIES}

C_SOURCES_WITH_PARSE = $(sort ${C_SOURCES})

STATICHEADERS := $(shell find ${SOURCE_ROOT}/include/ -name "*.h" \
                              ! -regex ".*include/arch.*" \
                              ! -regex ".*include/plat.*") \
                 $(shell find ${SOURCE_ROOT}/include/arch/${ARCH} -name "*.h") \
                 $(shell find ${SOURCE_ROOT}/include/plat/${PLAT} -name "*.h")

ifeq (${HAVE_AUTOCONF}, 1)
	STATICHEADERS += $(srctree)/include/generated/autoconf.h
endif

STATICSOURCES = $(foreach file,${C_SOURCES_WITH_PARSE} ${ASM_SOURCES}, \
                          ${SOURCE_ROOT}/${file})

OBJECTS = ${ASM_SOURCES:.S=.o} kernel.o

MAKEFILES := $(shell find ${SOURCE_ROOT} -name "Makefile")

############################################################
### Top-level targets
############################################################

all: kernel.elf kernel.elf.strip

theories: ${THEORIES}

preprocess: ${PPFILES}

.PHONY: style astyle xmllint pylint

# Combine style-checking rules into a single rule
style: astyle xmllint pylint

astyle: $(shell find ${SOURCE_ROOT}/src ${SOURCE_ROOT}/include ${SOURCE_ROOT}/libsel4 -name '*.[ch]')
	@echo " [Checking C style]"
	$(Q)astyle --max-instatement-indent=120 --style=otbs --pad-header --indent=spaces=4 --pad-oper $^

API_DTD = libsel4/tools/sel4_idl.dtd

xmllint: libsel4/include/interfaces/sel4.xml $(wildcard ${SOURCE_ROOT}/libsel4/arch_include/*/interfaces/sel4arch.xml) \
                                             $(wildcard ${SOURCE_ROOT}/libsel4/sel4_arch_include/*/interfaces/sel4arch.xml)
	@echo " [Checking XML API descriptions against $(API_DTD)]"
	$(Q)xmllint --dtdvalid $(API_DTD) --noout $^

pylint: $(shell find ${SOURCE_ROOT}/tools ${SOURCE_ROOT}/manual/tools ${SOURCE_ROOT}/libsel4/tools -name '*.py')
	@echo " [Checking for errors in python scripts]"
	$(Q)pylint --errors-only --rcfile=tools/pylintrc $^

validate: c-parser.log

c-parser.log: kernel_all.c_pp
	@echo " [VALIDATE] $<"
	$(Q)${PARSER} $^ 2>&1 | tee $@.errors
	$(Q)mv $@.errors $@

# The list of sources is a variable, not a file, so there is no sane way to ask make to depend
# upon it. This rule exists to touch a file to force the regeneration of kernel_all.c if we
# suspect that anything has changed that may have caused the sources list to change.
# Depend upon STATICHEADERS for autoconf/config related changes. Then try and depend upon
# any Makefiles. If you change environment variables (such as debug or fastpath in cases where
# not using autoconf) then you should make clean yourself. These dependencies really should
# just be on the kernel_all.c rule, but there is some magic in the usage of the $^ variable
# which has had the prerequsites located in the VPATH, and doing this ourselves from the
# C_SOURCES_WITH_PARSE variable is more trouble than this horrible intermediate file.
sources_list_updated: ${STATICHEADERS} ${MAKEFILES}
	@echo " [TOUCH] $@"
	$(Q)touch $@

kernel_all.c: sources_list_updated ${C_SOURCES_WITH_PARSE} ${DOMAIN_CONFIG_FILE}
	@echo " [CPP_GEN] $@"
	$(Q)${CPP_GEN} $(wordlist 2, $(words $^), $^) > $@

kernel.o: kernel_final.s
	@echo " [AS] $@"
	# Clear any .arch directive from the .s file in case we are assembling
	# for a different architecture. This happens when the compiler supports
	# a subset architecture compared to the assembler. For example where the
	# compiler only supports armv7-a, where the assembler supports armv7ve
	$(Q)sed -i 's/^[ \t]*\.arch .*$$//' $<
	$(Q)${CC} ${ASFLAGS} -o $@ -c $<

kernel_final.s: kernel_final.c
	@echo " [CC] $@"
	$(Q)${CC} ${CFLAGS} -S -o $@ $<

# Awkward rule to get around passing -x to CC and having a .c input file.
kernel_final.c: kernel_all.c_pp
	@echo " [Circular includes] $<"
	$(Q)${CIRCULAR_INCLUDES} < $<
	@echo " [CP] $@"
	$(Q)cp -a $< $@

LINKER_SCRIPT = src/plat/${PLAT}/linker.lds

linker.lds_pp: ${LINKER_SCRIPT}
	@echo " [CPP] $@"
	$(Q)${CPP} ${CPPFLAGS} -P -E -o $@ -x c $<

kernel.elf: ${OBJECTS} linker.lds_pp
	@echo " [LD] $@"
	$(Q)${CHANGED} $@ ${CC} ${LDFLAGS} -T linker.lds_pp -Wl,-n \
	     -o $@ ${OBJECTS}

############################################################
### Pattern rules
############################################################

%.elf.strip: %.elf | ${DIRECTORIES}
	@echo " [STRIP] $@"
	$(Q)${STRIP} -o $@ $<

%.o: %.s_pp | ${DIRECTORIES}
	@echo " [AS] $@"
	$(Q)${CC} ${ASFLAGS} -x assembler -c $< -o $@

###################
# Header generation
###################

arch/api/invocation.h: ${SOURCE_ROOT}/libsel4/arch_include/${ARCH}/interfaces/sel4arch.xml ${INVOCATION_ID_GEN_PATH} | ${DIRECTORIES}
	$(Q)rm -f ${SOURCE_ROOT}/include/arch/${ARCH}/arch/api/invocation.h
	$(Q)${INVOCATION_ID_GEN_PATH} --arch --xml $< \
		--dest $@

arch/api/sel4_invocation.h: ${SOURCE_ROOT}/libsel4/sel4_arch_include/${SEL4_ARCH}/interfaces/sel4arch.xml ${INVOCATION_ID_GEN_PATH} | ${DIRECTORIES}
	$(Q)rm -f ${SOURCE_ROOT}/include/arch/${ARCH}/arch/api/sel4_invocation.h
	$(Q)${INVOCATION_ID_GEN_PATH} --sel4_arch --xml $< \
		--dest $@


api/invocation.h: ${SOURCE_ROOT}/libsel4/include/interfaces/sel4.xml ${INVOCATION_ID_GEN_PATH} | ${DIRECTORIES}
	$(Q)rm -f ${SOURCE_ROOT}/include/api/invocation.h
	$(Q)${INVOCATION_ID_GEN_PATH} --xml $< \
		--dest $@

arch/api/syscall.h: ${SOURCE_ROOT}/include/api/syscall.xsd ${SOURCE_ROOT}/include/api/syscall.xml | ${DIRECTORIES} ${SYSCALL_ID_GEN_PATH}
	$(Q)${XMLLINT_PATH} --noout --schema $^
	$(Q)rm -f ${SOURCE_ROOT}/include/arch/${ARCH}/arch/api/syscall.h
	$(Q)${SYSCALL_ID_GEN_PATH} --xml $(word 2, $^) \
		--kernel_header $@

####################
# Bitfield generation
####################

PRUNES = $(foreach file,${STATICSOURCES} ${STATICHEADERS}, \
           --prune ${file} )

TOPLEVELTYPES=cte_C tcb_C endpoint_C notification_C asid_pool_C pte_C \
              pde_C user_data_C user_data_device_C
TOPTYPES = $(foreach tp,${TOPLEVELTYPES}, \
           --toplevel ${tp} )

%.pbf: %.bf ${STATICHEADERS} | ${DIRECTORIES}
	@echo " [PBF_GEN] $@"
	$(Q)${CPP} ${CPPFLAGS} -P $< > $@

%_gen.h: %.pbf ${STATICSOURCES} ${STATICHEADERS} ${BF_GEN_PATH} | ${DIRECTORIES}
	@echo " [BF_GEN] $@"
	$(Q)${PYTHON} ${BF_GEN_PATH} $< $@ ${PRUNES}

ifdef SKIP_MODIFIES
  SM_ARG=--skip_modifies
else
  SM_ARG=
endif

${DEFTHEORIES}: %_defs.thy: %.pbf ${BF_GEN_PATH} ${STATICSOURCES} \
                ${STATICHEADERS} ${SOURCE_ROOT}/Makefile | ${DIRECTORIES}
	@echo " [BF_DEFS] $@"
	$(Q)${PYTHON} ${BF_GEN_PATH} --cspec-dir ${CSPEC_DIR} --hol_defs $< $@ ${PRUNES} ${SM_ARG}

${PROOFTHEORIES}: %_proofs.thy: %.pbf ${BF_GEN_PATH} ${STATICSOURCES} \
                  ${STATICHEADERS} ${SOURCE_ROOT}/Makefile ${UMM_TYPES} | ${DIRECTORIES}
	@echo " [BF_PROOFS] $@"
	@$(if ${UMM_TYPES}, , echo "UMM_TYPES unset" ; false)
	$(Q)${PYTHON} ${BF_GEN_PATH} --cspec-dir ${CSPEC_DIR} --hol_proofs ${SORRY_ARG} $< $@ ${TOPTYPES} \
	           --umm_types ${UMM_TYPES} ${PRUNES} ${SM_ARG}

###########################
# Preprocessed source files
###########################

%.s_pp: %.S ${GENHEADERS} ${STATICHEADERS} | ${DIRECTORIES}
	@echo " [CPP] $@"
	$(Q)${CPP} ${CPPFLAGS} -CC -E -o $@ $<

%.c_pp: %.c ${GENHEADERS} ${STATICHEADERS} | ${DIRECTORIES}
	@echo " [CPP] $@"
ifdef NO_PRESERVE_TIMESTAMP
    # Preserving the timestamp can cause repeated rebuilding if a file is touched
    # with trivial modifications such that it has a future timestamp, but every time
    # we rebuild we restore this timestamp back into the past. This results in partial
    # rebuilds all the time. This flag allows you to stop this happening if you find it upsetting.
	$(Q)${CPP} ${CPPFLAGS} -CC -E -o $@ $<
else
	$(Q)${CHANGED} $@ ${CPP} ${CPPFLAGS} -CC -E -o $@ $<
endif

%.h_pp: %.h ${GENHEADERS} ${STATICHEADERS} | ${DIRECTORIES}
	@echo " [CPP] $@"
	$(Q)${CPP} ${CPPFLAGS} -CC -E -o $@ $<

############################################################
### Utility targets
############################################################

CLEANTARGETS = kernel.elf kernel.elf.strip ${GENHEADERS} ${OBJECTS} autoconf.h \
  parser.out parsetab.py \
  kernel_final.s kernel_final.c kernel_all.c kernel_all.c_pp \
  ${PPFILES} ${THEORIES} c-parser.log c-parser-all.log \
  arch api plat ${ASM_SOURCES:.S=.s_pp} linker.lds_pp

clean:
	@echo " [CLEAN]"
	rm -f ${SOURCE_ROOT}/include/arch/${ARCH}/arch/api/invocation.h
	rm -f ${SOURCE_ROOT}/include/api/invocation.h
	rm -f ${SOURCE_ROOT}/include/arch/${ARCH}/arch/api/syscall.h
	rm -Rf ${CLEANTARGETS}

.PHONY: distclean
distclean: clean
	@echo " [CLEAN]"
	$(Q)rm -f tools/*.pyc

${DIRECTORIES}:
	@echo " [MKDIR] $@"
	$(Q)mkdir -p ${DIRECTORIES}

.FORCE:

.PHONY: .FORCE
