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

register_driver(compatibility_strings "ti,am335x-timer" PREFIX src/drivers/timer CFILES "am335x-timer.c")
register_driver(compatibility_strings "qcom,kpss-timer" PREFIX src/drivers/timer CFILES "kpss-timer.c")
register_driver(compatibility_strings "samsung,exynos4210-mct" PREFIX src/drivers/timer CFILES "exynos4210-mct.c")
register_driver(compatibility_strings "samsung,exynos4412-mct" PREFIX src/drivers/timer CFILES "exynos4412-mct.c")
register_driver(compatibility_strings "fsl,imx31-epit" PREFIX src/drivers/timer CFILES "imx31-epit.c")
register_driver(compatibility_strings "ti,omap3430-timer" PREFIX src/drivers/timer CFILES "omap3430-timer.c")
register_driver(compatibility_strings "arm,cortex-a9-twd-timer" PREFIX src/drivers/timer CFILES "priv_timer.c")
register_driver(compatibility_strings "arm,armv7-timer" PREFIX src/drivers/timer CFILES "generic_timer.c")
register_driver(compatibility_strings "arm,armv8-timer" PREFIX src/drivers/timer CFILES "generic_timer.c")
register_driver(compatibility_strings "allwinner,sun4i-a10-timer" PREFIX src/drivers/timer CFILES "allwinner-timer.c")
