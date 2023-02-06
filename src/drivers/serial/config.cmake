#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
# Copyright 2020, HENSOLDT Cyber GmbH
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

register_driver(
    compatibility_strings "brcm,bcm2835-aux-uart"
    PREFIX src/drivers/serial
    CFILES "bcm2835-aux-uart.c"
)
register_driver(compatibility_strings "arm,pl011" PREFIX src/drivers/serial CFILES "pl011.c")
register_driver(
    compatibility_strings "nvidia,tegra20-uart;ti,omap3-uart;snps,dw-apb-uart"
    PREFIX src/drivers/serial
    CFILES "tegra_omap3_dwapb.c"
)
register_driver(
    compatibility_strings "fsl,imx6q-uart;fsl,imx6sx-uart"
    PREFIX src/drivers/serial
    CFILES "imx.c"
)
register_driver(
    compatibility_strings "fsl,imx8qxp-lpuart"
    PREFIX src/drivers/serial
    CFILES "imx-lpuart.c"
)
register_driver(
    compatibility_strings "samsung,exynos4210-uart"
    PREFIX src/drivers/serial
    CFILES "exynos4210-uart.c"
)
register_driver(
    compatibility_strings "qcom,msm-uartdm"
    PREFIX src/drivers/serial
    CFILES "msm-uartdm.c"
)
register_driver(compatibility_strings "xlnx,xuartps" PREFIX src/drivers/serial CFILES "xuartps.c")
register_driver(
    compatibility_strings "amlogic,meson-gx-uart"
    PREFIX src/drivers/serial
    CFILES "meson-gx-uart.c"
)
register_driver(
    compatibility_strings "3A5000,loongson3A5000-uart"
    PREFIX src/drivers/serial
    CFILES "loongson3A5000-uart.c"
)

