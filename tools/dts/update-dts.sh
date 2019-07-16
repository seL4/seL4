#!/bin/sh
#
# Copyright 2018, Data61
# Commonwealth Scientific and Industrial Research Organisation (CSIRO)
# ABN 41 687 119 230.
#
# This software may be distributed and modified according to the terms of
# the GNU General Public License version 2. Note that NO WARRANTY is provided.
# See "LICENSE_GPLv2.txt" for details.
#
# @TAG(DATA61_GPL)
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

# for kzm
make ARCH=arm imx_v6_v7_defconfig
make ARCH=arm -j4 dtbs

make ARCH=arm64 defconfig
make ARCH=arm64 -j4 dtbs

LICENSE="/*
 * The code contained herein is licensed under the GNU General Public
 * License. You may obtain a copy of the GNU General Public License
 * Version 2 or later at the following locations:
 *
 * http://www.opensource.org/licenses/gpl-license.html
 * http://www.gnu.org/copyleft/gpl.html
 *
 * @TAG(OTHER_GPL)
 */
"

ARM_DTBS="
am335x-boneblack=am335x-boneblack
am335x-boneblue=am335x-boneblue
bcm2837-rpi-3-b=rpi3
exynos4412-odroidx=exynos4
exynos5250-arndale=exynos5250
exynos5410-odroidxu=exynos5410
exynos5422-odroidxu4=exynos5422
imx31-bug=kzm
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
amlogic/meson-gxbb-odroidc2=odroidc2
hisilicon/hi6220-hikey=hikey
nvidia/tegra210-p2371-2180=tx1
xilinx/avnet-ultra96-rev1=ultra96
xilinx/zynqmp-zcu102-rev1.0=zynqmp
freescale/fsl-imx8mq-evk=imx8mq-evk
freescale/fsl-imx8mm-evk=imx8mm-evk
rockchip/rk3399-rockpro64=rockpro64
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
