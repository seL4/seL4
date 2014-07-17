/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <machine/io.h>
#include <arch/kernel/boot_sys.h>
#include <arch/linker.h>
#include <plat/machine/pci.h>
#include <plat/machine/hardware.h>

#define PCI_CONF_PORT_ADDR     0x0CF8
#define PCI_CONF_PORT_DATA     0x0CFC

#define PCI_CONF_REG_VID       0x00
#define PCI_CONF_REG_DID       0x02
#define PCI_CONF_REG_HDR_TYPE  0x0E
#define PCI_CONF_REG_BAR       0x10

#define PCI_HDR_TYPE_NORMAL    0x00
#define PCI_HDR_TYPE_BRIDGE    0x01
#define PCI_HDR_TYPE_CARDBUS   0x02

BOOT_CODE static uint32_t
pci_read_reg32(uint8_t bus, uint8_t dev, uint8_t fun, uint8_t reg)
{
    reg &= ~MASK(2);
    out32(PCI_CONF_PORT_ADDR, 0x80000000 | bus << 16 | dev << 11 | fun << 8 | reg);
    return in32(PCI_CONF_PORT_DATA);
}

BOOT_CODE static void
pci_write_reg32(uint8_t bus, uint8_t dev, uint8_t fun, uint8_t reg, uint32_t val)
{
    reg &= ~MASK(2);
    out32(PCI_CONF_PORT_ADDR, 0x80000000 | bus << 16 | dev << 11 | fun << 8 | reg);
    out32(PCI_CONF_PORT_DATA, val);
}

BOOT_CODE static uint16_t
pci_read_reg16(uint8_t bus, uint8_t dev, uint8_t fun, uint8_t reg)
{
    reg &= ~MASK(1);
    out32(PCI_CONF_PORT_ADDR, 0x80000000 | bus << 16 | dev << 11 | fun << 8 | (reg & ~MASK(2)));
    return in32(PCI_CONF_PORT_DATA) >> ((reg & MASK(2)) * 8);
}

/* Not used yet, avoid compiler warnings.
 *
BOOT_CODE static void
pci_write_reg16(uint8_t bus, uint8_t dev, uint8_t fun, uint8_t reg, uint16_t val)
{
        reg &= ~MASK(1);
        out32(PCI_CONF_PORT_ADDR, 0x80000000 | bus << 16 | dev << 11 | fun << 8 | reg);
        out16(PCI_CONF_PORT_DATA, val);
}
*/

BOOT_CODE static uint8_t
pci_read_reg8(uint8_t bus, uint8_t dev, uint8_t fun, uint8_t reg)
{
    out32(PCI_CONF_PORT_ADDR, 0x80000000 | bus << 16 | dev << 11 | fun << 8 | (reg & ~MASK(2)));
    return in32(PCI_CONF_PORT_DATA) >> ((reg & MASK(2)) * 8);
}

/* Not used yet, avoid compiler warnings.
 *
BOOT_CODE static void
pci_write_reg8(uint8_t bus, uint8_t dev, uint8_t fun, uint8_t reg, uint8_t val)
{
        out32(PCI_CONF_PORT_ADDR, 0x80000000 | bus << 16 | dev << 11 | fun << 8 | reg);
        out8(PCI_CONF_PORT_DATA, val);
}
*/

BOOT_CODE static void
pci_scan_bars(uint8_t bus, uint8_t dev, uint8_t fun, uint8_t count)
{
    pci_bar_t bar;
    pci_bar_t bar_save;
    paddr_t   map_base;
    uint32_t  map_size;
    uint32_t  map_size_bits;
    uint32_t  i;
    uint8_t   reg;

    for (i = 0; i < count; i++) {
        reg = PCI_CONF_REG_BAR + (i << 2);
        /* save base address written by BIOS */
        bar_save.words[0] = pci_read_reg32(bus, dev, fun, reg);
        /* write 0xffffffff in order to read size etc. */
        pci_write_reg32(bus, dev, fun, reg, 0xffffffff);
        bar.words[0] = pci_read_reg32(bus, dev, fun, reg);
        if (bar.words[0] != 0) {
            /* BAR is in use */
            printf("PCI:     BAR[%d] ", i);
            if (pci_bar_get_pci_space(bar) == pci_bar_pci_bar_mem) {
                map_size = ~(pci_bar_pci_bar_mem_get_base_address(bar) - 1);
                if (map_size < BIT(PAGE_BITS)) {
                    map_size = BIT(PAGE_BITS);
                }
                map_base = pci_bar_pci_bar_mem_get_base_address(bar_save);
                if (IS_ALIGNED(map_base, PAGE_BITS)) {
                    /* calculate map_size_bits */
                    map_size_bits = 0;
                    while ((map_size & BIT(map_size_bits)) == 0) {
                        map_size_bits++;
                    }
                    /* check whether map_size is a power of two */
                    if (map_size - BIT(map_size_bits) == 0) {
                        /* we have base/size, now do the mapping */
                        if (pci_bar_pci_bar_mem_get_above_4GB(bar)) {
                            if (pci_read_reg32(bus, dev, fun, reg + 4) != 0) {
                                printf("ignored: 64 bit BAR address above 4 GB\n");
                            } else {
                                printf("address=0x%x size=0x%x", map_base, map_size);
                                insert_dev_p_reg(
                                (p_region_t) {
                                    .start = map_base, .end = map_base + map_size
                                }
                                );
                            }
                            i++;
                        } else {
                            printf("address=0x%x size=0x%x",  map_base, map_size);
                            insert_dev_p_reg(
                            (p_region_t) {
                                .start = map_base, .end = map_base + map_size
                            }
                            );
                        }
                    } else {
                        printf("ignored: size corrupted (not a power of two): 0x%x\n", map_size);
                    }
                } else {
                    printf("ignored: address=0x%x not 4K aligned (size=0x%x)\n", map_base, map_size);
                }
            } else {
                /* pci_bar_pci_bar_io */
                printf("ignored: PCI IO space not supported\n");
            }
            /* write back address set by BIOS */
            pci_write_reg32(bus, dev, fun, reg, bar_save.words[0]);
        }
    }
}

BOOT_CODE static int
pci_scan_fun(uint8_t bus, uint8_t dev, uint8_t fun)
{
    uint16_t  did;
    uint8_t   type;
    uint16_t  vid;

    vid = pci_read_reg16(bus, dev, fun, PCI_CONF_REG_VID);
    if (vid != 0xffff) {

        did = pci_read_reg16(bus, dev, fun, PCI_CONF_REG_DID);
        type = pci_read_reg8(bus, dev, fun, PCI_CONF_REG_HDR_TYPE) & 0x7f;
        printf(
            "PCI: Detected device @ bus=0x%x dev=0x%x fun=0x%x: vid=0x%x did=0x%x type=",
            bus, dev, fun, vid, did
        );
        (void)did;
        switch (type) {
        case PCI_HDR_TYPE_NORMAL:
            printf("normal\n");
            pci_scan_bars(bus, dev, fun, 6);
            break;

        case PCI_HDR_TYPE_BRIDGE:
            printf("bridge\n");
            pci_scan_bars(bus, dev, fun, 2);
            break;

        case PCI_HDR_TYPE_CARDBUS:
            printf("cardbus\n");
            pci_scan_bars(bus, dev, fun, 1);
            break;

        default:
            printf("unknown (0x%x)\n", type);
            /* don't scan BARs */
            break;
        }
        return 1;
    }
    return 0;
}

BOOT_CODE void
pci_scan(uint32_t* bus_used_bitmap)
{
    bool_t    bus_used;
    uint16_t  bus;
    uint8_t   dev;
    uint8_t   fun;
    bool_t    multifunction;

    for (bus = 0; bus < 256; bus++) {
        bus_used = false;
        for (dev = 0; dev < 32; dev++) {
            if (pci_scan_fun(bus, dev, 0)) {
                multifunction = !!(pci_read_reg8(bus, dev, 0, PCI_CONF_REG_HDR_TYPE) & 0x80);
                bus_used = true;
                if (!multifunction) {
                    continue;
                }
            } else {
                continue;
            }

            for (fun = 1; fun < 8; fun++) {
                pci_scan_fun(bus, dev, fun);
            }
        }
        if (bus_used_bitmap && bus_used) {
            bus_used_bitmap[bus >> 5] |= BIT(bus & MASK(5));
        }
    }
}
