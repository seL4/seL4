#
# Copyright 2019, Data61
# Commonwealth Scientific and Industrial Research Organisation (CSIRO)
# ABN 41 687 119 230.
#
# This software may be distributed and modified according to the terms of
# the GNU General Public License version 2. Note that NO WARRANTY is provided.
# See "LICENSE_GPLv2.txt" for details.
#
# @TAG(DATA61_GPL)
#

cmake_minimum_required(VERSION 3.7.2)

macro(RegisterDriver compatibility_strings match_strings)
    foreach(match_string IN ITEMS ${match_strings})
        list(FIND ${compatibility_strings} ${match_string} res)
        if(NOT (res EQUAL -1))
            add_sources(${ARGN})
            break()
        endif()
    endforeach()
endmacro()

RegisterDriver(compatibility_strings "nvidia,tegra20-uart;ti,omap3-uart;snps,dw-apb-uart" PREFIX src/drivers/serial CFILES "tegra_omap3_dwapb.c")
RegisterDriver(compatibility_strings "brcm,bcm2835-aux-uart" PREFIX src/drivers/serial CFILES "bcm2835-aux-uart.c")
RegisterDriver(compatibility_strings "fsl,imx31-uart" PREFIX src/drivers/serial CFILES "imx31-uart.c")
RegisterDriver(compatibility_strings "arm,pl011" PREFIX src/drivers/serial CFILES "pl011.c")
RegisterDriver(compatibility_strings "samsung,exynos4210-uart" PREFIX src/drivers/serial CFILES "exynos4210-uart.c")
RegisterDriver(compatibility_strings "fsl,imx6q-uart" PREFIX src/drivers/serial CFILES "imx6q-uart.c")
RegisterDriver(compatibility_strings "qcom,msm-uartdm" PREFIX src/drivers/serial CFILES "msm-uartdm.c")
RegisterDriver(compatibility_strings "xlnx,xuartps" PREFIX src/drivers/serial CFILES "xuartps.c")