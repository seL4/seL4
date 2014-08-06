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

ARCH_LIST:=arm ia32
CPU_LIST:=arm1136jf-s ixp420 cortex-a8 cortex-a9 cortex-a15
PLAT_LIST:=imx31 pc99 ixp420 omap3 am335x exynos4 exynos5 imx6 apq8064
ARMV_LIST:=armv6 armv7-a

ifndef SOURCE_ROOT
    # Assume we're in the source directory if not specified.
    SOURCE_ROOT=.
    export SOURCE_ROOT
endif

# we don't ARCH or PLAT for the style target, but other 
# targets fail -- so just bluff.
ifeq (${MAKECMDGOALS}, style)
ARCH:=ia32
PLAT:=pc99
endif

# we do need them if we want to build anything else
$(if $(filter ${ARCH},${ARCH_LIST}),, \
	$(error ARCH ${ARCH} invalid or undefined, should be one of [${ARCH_LIST}]))

$(if $(filter ${PLAT},${PLAT_LIST}),, \
	$(error PLAT ${PLAT} invalid or undefined, should be one of [${PLAT_LIST}]))

ifeq (${ARCH}, arm)
$(if $(filter ${CPU},${CPU_LIST}),, \
	$(error CPU ${CPU} invalid or undefined, should be one of [${CPU_LIST}]))

$(if $(filter ${ARMV},${ARMV_LIST}),, \
	$(error ARMV ${ARMV} invalid or undefined, should be one of [${ARMV_LIST}]))
endif

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
	MAKE_SILENT = -s
	quiet = quiet_
	Q = @
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
  CONFIG_TIMER_TICK_MS \
  CONFIG_MAX_NUM_NODES \
  CONFIG_MAX_NUM_PASSTHROUGH_DEVICES, \
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

GENHEADERS += arch/api/invocation.h api/invocation.h arch/api/syscall.h

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
BF_GEN = bitfield_gen.py
CHANGED = ${SOURCE_ROOT}/tools/changed.sh
CPP_GEN = ${SOURCE_ROOT}/tools/cpp_gen.sh
SYSCALL_ID_GEN = syscall_header_gen.py
INVOCATION_ID_GEN = invocation_header_gen.py
XMLLINT = xmllint.sh

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
$(info BF_GEN        = ${BF_GEN})
$(info BF_GEN_PATH   = ${BF_GEN_PATH})
$(info SYSCALL_ID_GEN = ${SYSCALL_ID_GEN})
$(info SYSCALL_ID_GEN_PATH = ${SYSCALL_ID_GEN_PATH})
$(info INVOCATION_ID_GEN = ${INVOCATION_ID_GEN})
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

BF_GEN_PATH = $(shell PATH=${PATH} sh -c "which ${BF_GEN}")
$(if ${BF_GEN_PATH},,$(error ${BF_GEN} not in PATH or not executable))

SYSCALL_ID_GEN_PATH = $(shell PATH=${PATH} sh -c "which ${SYSCALL_ID_GEN}")
$(if ${SYSCALL_ID_GEN_PATH},,$(error ${SYSCALL_ID_GEN} not in PATH or not executable))

INVOCATION_ID_GEN_PATH = $(shell PATH=${PATH} sh -c "which ${INVOCATION_ID_GEN}")
$(if ${INVOCATION_ID_GEN_PATH},,$(error ${INVOCATION_ID_GEN} not in PATH or not executable))

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
ifeq (${ARCH}, arm)
CFLAGS += -mtune=${CPU} -marm -march=${ARMV}
ASFLAGS += -mcpu=${CPU} -march=${ARMV}
DEFINES += -D$(shell echo ${ARMV}|tr [:lower:] [:upper:]|tr - _)
ifeq (${CPU},cortex-a8)
DEFINES += -DARM_CORTEX_A8
else
ifeq (${CPU},cortex-a9)
DEFINES += -DARM_CORTEX_A9
endif
endif
endif

ifeq (${ARCH}, ia32)
CFLAGS += -m32 -mno-mmx -mno-sse
ASFLAGS += --32
endif
endif

ifeq (${CPU}, arm1136jf-s)
DEFINES += -DARM1136_WORKAROUND
# Add definition for verified platform to support standalone kernel builds
DEFINES += -DCONFIG_ARM1136JF_S
endif

WARNINGS = all error strict-prototypes missing-prototypes nested-externs \
	missing-declarations undef pointer-arith no-nonnull declaration-after-statement

CFLAGS += --std=c99 -nostdlib -nostdinc -ffreestanding \
	${WARNINGS:%=-W%} ${INCLUDES}
LDFLAGS += -nostdlib -nostdinc
ASFLAGS += ${INCLUDES}

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
### Top-level targets
############################################################

all: kernel.elf kernel.elf.strip

theories: ${THEORIES}

preprocess: ${PPFILES}

style:
	astyle --max-instatement-indent=120 --style=otbs --pad-header --recursive --indent=spaces=4 --pad-oper "src/*.c"
	astyle --max-instatement-indent=120 --style=otbs --pad-header --recursive --indent=spaces=4 --pad-oper "include/*.h"
	astyle --max-instatement-indent=120 --style=otbs --pad-header --recursive --indent=spaces=4 --pad-oper "libsel4/*.h"
	astyle --max-instatement-indent=120 --style=otbs --pad-header --recursive --indent=spaces=4 --pad-oper "libsel4/*.c"

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
	$(Q)${AS} ${ASFLAGS} -o $@ $<

kernel_final.s: kernel_final.c
	@echo " [CC] $@"
	$(Q)${CC} ${CFLAGS} -S -o $@ $<

# Awkward rule to get around passing -x to CC and having a .c input file.
kernel_final.c: kernel_all.c_pp
	@echo " [CP] $@"
	$(Q)cp -a $< $@

LINKER_SCRIPT = src/plat/${PLAT}/linker.lds

kernel.elf: ${OBJECTS} ${LINKER_SCRIPT}
	@echo " [LD] $@"
	$(Q)${CHANGED} $@ ${LD} ${LDFLAGS} -T ${SOURCE_ROOT}/${LINKER_SCRIPT} \
		-o $@ ${OBJECTS}

############################################################
### Pattern rules
############################################################

%.elf.strip: %.elf | ${DIRECTORIES}
	@echo " [STRIP] $@"
	$(Q)${STRIP} -o $@ $<

%.o: %.s | ${DIRECTORIES}
	@echo " [AS] $@"
	$(Q)${AS} ${ASFLAGS} $< -o $@


###################
# Header generation
###################

arch/api/invocation.h: ${SOURCE_ROOT}/libsel4/arch_include/${ARCH}/interfaces/sel4arch.xml | ${DIRECTORIES}
	$(Q)rm -f ${SOURCE_ROOT}/include/arch/${ARCH}/arch/api/invocation.h
	$(Q)${INVOCATION_ID_GEN_PATH} --arch --xml $< \
		--dest $@


api/invocation.h: ${SOURCE_ROOT}/libsel4/include/interfaces/sel4.xml | ${DIRECTORIES}
	$(Q)rm -f ${SOURCE_ROOT}/include/api/invocation.h
	$(Q)${INVOCATION_ID_GEN_PATH} --xml $< \
		--dest $@

arch/api/syscall.h: ${SOURCE_ROOT}/include/api/syscall.xsd ${SOURCE_ROOT}/include/api/syscall.xml | ${DIRECTORIES}
	$(Q)${XMLLINT_PATH} --noout --schema $^
	$(Q)rm -f ${SOURCE_ROOT}/include/arch/${ARCH}/arch/api/syscall.h
	$(Q)${SYSCALL_ID_GEN_PATH} --xml $(word 2, $^) \
		--kernel_header $@


####################
# Bitfield generation
####################

PRUNES = $(foreach file,${STATICSOURCES} ${STATICHEADERS}, \
           --prune ${file} )

TOPLEVELTYPES=cte_C tcb_C endpoint_C async_endpoint_C asid_pool_C pte_C \
              pde_C adglobs_struct user_data_C
TOPTYPES = $(foreach tp,${TOPLEVELTYPES}, \
           --toplevel ${tp} )

%.pbf: %.bf ${STATICHEADERS} | ${DIRECTORIES}
	@echo " [PBF_GEN] $@"
	$(Q)${CPP} ${CPPFLAGS} -P $< > $@

%_gen.h: %.pbf ${STATICSOURCES} ${STATICHEADERS} | ${DIRECTORIES}
	@echo " [BF_GEN] $@"
	$(Q)${BF_GEN_PATH} $< $@ ${PRUNES}

ifdef SKIP_MODIFIES
  SM_ARG=--skip_modifies
else
  SM_ARG=
endif

${DEFTHEORIES}: %_defs.thy: %.bf ${BF_GEN_PATH} ${STATICSOURCES} \
                ${STATICHEADERS} ${SOURCE_ROOT}/Makefile | ${DIRECTORIES}
	@echo " [BF_DEFS] $@"
	$(Q)${BF_GEN_PATH} --cspec-dir ${CSPEC_DIR} --hol_defs $< $@ ${PRUNES} ${SM_ARG}

${PROOFTHEORIES}: %_proofs.thy: %.bf ${BF_GEN_PATH} ${STATICSOURCES} \
                  ${STATICHEADERS} ${SOURCE_ROOT}/Makefile ${UMM_TYPES} | ${DIRECTORIES}
	@echo " [BF_PROOFS] $@"
	@$(if ${UMM_TYPES}, , echo "UMM_TYPES unset" ; false)
	$(Q)${BF_GEN_PATH} --cspec-dir ${CSPEC_DIR} --hol_proofs ${SORRY_ARG} $< $@ ${TOPTYPES} \
	           --umm_types ${UMM_TYPES} ${PRUNES} ${SM_ARG}

###########################
# Preprocessed source files
###########################

%.s: %.S ${GENHEADERS} ${STATICHEADERS} | ${DIRECTORIES}
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

CLEANTARGETS = kernel.elf kernel.elf.strip ${GENHEADERS} ${OBJECTS} \
  parser.out parsetab.py \
  kernel_final.s kernel_final.c kernel_all.c kernel_all.c_pp \
  ${PPFILES} ${THEORIES} c-parser.log c-parser-all.log \
  arch api plat ${ASM_SOURCES:.S=.s}

clean:
	@echo " [CLEAN]"
	rm -f ${SOURCE_ROOT}/include/arch/${ARCH}/arch/api/invocation.h
	rm -f ${SOURCE_ROOT}/include/api/invocation.h
	rm -f ${SOURCE_ROOT}/include/arch/${ARCH}/arch/api/syscall.h
	$(Q)rm -Rf ${CLEANTARGETS}

.PHONY: distclean
distclean: clean
	@echo " [CLEAN]"
	$(Q)rm -f tools/*.pyc

${DIRECTORIES}:
	@echo " [MKDIR] $@"
	$(Q)mkdir -p ${DIRECTORIES}

.FORCE:

.PHONY: .FORCE
