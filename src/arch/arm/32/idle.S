/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <autoconf.h>
#include <machine/assembler.h>

.code 32
.section .text, "ax"

BEGIN_FUNC(idle_thread)
    mov r0, #0
1:
    /*
     * Idling should bring the the processor into a low power mode
     * to be re-awakened by an interrupt.
     * Most ARM architectures support this, but the necessary steps
     * depend on the actual architecture: The ARMv7 architectures
     * generally support a wfi (wait for interrupt) instruction, whereas
     * on most ARMv6 and some ARMv5, a similar function can be achieved
     * by writing to the coprocessor.
     *
     * The following contraption has been adapted from an ARM Knowledge
     * article at:
     * http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.faqs/ka13332.html
     * It uses macros from autoconf.h to decide which architecture.
     *
     * Note: The coprocessor method doesn't work on the KZM board,
     *       although, according to the article, it should.
     *       With the KZM currently being the only supported ARMv6
     *       platform, it is unclear at this time wether it works for
     *       other SoCs (e.g. BCM2835), so we explicitly disable only
     *       the KZM here.
     *
     */
#if defined(CONFIG_ARCH_ARM_V6) && !defined(CONFIG_PLAT_KZM)
	mcr p15, 0, r0, c7, c0, 4
#elif defined(CONFIG_ARCH_ARM_V7A)
	wfi
#else
	nop
#endif
    b 1b
END_FUNC(idle_thread)
