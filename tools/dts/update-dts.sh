#!/usr/bin/env sh
#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
# Copyright 2022, Capgemini Engineering
#
# SPDX-License-Identifier: GPL-2.0-only
#

if [ -z "$1" ]; then
    echo "Usage: $0 </path/to/linux/kernel>"
    exit 1
fi

destdir=`pwd`

cd "$1" || exit 1
echo Using DTS from Linux $(make -s kernelversion)
make ARCH=arm multi_v7_defconfig
make ARCH=arm -j4 dtbs

make ARCH=arm64 defconfig
make ARCH=arm64 -j4 dtbs

LICENSE="/*
 * Copyright Linux Kernel Team
 *
 * SPDX-License-Identifier: GPL-2.0-only
 *
 * This file is derived from an intermediate build stage of the
 * Linux kernel. The licenses of all input files to this process
 * are compatible with GPL-2.0-only.
 */
"

ARM_DTBS="
am335x-bone=am335x-bone
am335x-boneblack=am335x-boneblack
am335x-boneblue=am335x-boneblue
bcm2837-rpi-3-b=rpi3
exynos4412-odroidx=exynos4
exynos5250-arndale=exynos5250
exynos5410-odroidxu=exynos5410
exynos5422-odroidxu4=exynos5422
imx6q-sabrelite=sabre
imx6q-wandboard-revd1=wandq
imx7d-sdb=imx7sabre
omap3-beagle=omap3
qcom-apq8064-ifc6410=apq8064
sun7i-a20-cubietruck=allwinnera20
tegra124-jetson-tk1=tk1
zynq-zc706=zynq7000
"

ARM64_DTBS="
amlogic/meson-sm1-odroid-c4=odroidc4
amlogic/meson-gxbb-odroidc2=odroidc2
hisilicon/hi6220-hikey=hikey
nvidia/tegra210-p2371-2180=tx1
xilinx/avnet-ultra96-rev1=ultra96
xilinx/zynqmp-zcu102-rev1.0=zynqmp
freescale/fsl-imx8mq-evk=imx8mq-evk
freescale/fsl-imx8mm-evk=imx8mm-evk
rockchip/rk3399-rockpro64=rockpro64
rockchip/rk3566-quartz64-a=quartz64
broadcom/bcm2711-rpi-4-b=rpi4
avnet/maaxboard=maaxboard
"

extract_dts() {
    dtb=`echo $1| sed 's/=.*$//'`
    platform=`echo $1| sed 's/^.*=//'`
    echo "'$dtb'" = "'$platform'"
    dtc -I dtb -O dts -o $2/$platform.dts.tmp $3/$dtb.dtb
    echo "$LICENSE" > $2/$platform.dts
    cat $2/$platform.dts.tmp >> $2/$platform.dts
    rm $2/$platform.dts.tmp
}

for entry in $ARM_DTBS; do
    extract_dts $entry $destdir arch/arm/boot/dts
done

for entry in $ARM64_DTBS; do
    extract_dts $entry $destdir arch/arm64/boot/dts
done
