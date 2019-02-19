
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

set(serial_list brcm,bcm2835-aux-uart fsl,imx31-uart arm,pl011 samsung,exynos4210-uart fsl,imx6q-uart qcom,msm-uartdm xlnx,xuartps)    
foreach(c IN ITEMS ${serial_list})
    RegisterDriver(compatibility_strings "${c}" PREFIX src/drivers/serial CFILES "${c}.c")
endforeach()