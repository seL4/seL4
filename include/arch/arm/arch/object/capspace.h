/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __ARCH_OBJECT_CAPSPACE_H
#define __ARCH_OBJECT_CAPSPACE_H

#define FMAPPED_OBJECT_HIGH(a) ((uint32_t)(a) >> 12)

enum capSpaceType {
    capSpaceUntypedMemory,
    capSpaceTypedMemory,
    capSpaceIRQ,
    capSpaceDomain,
};

static inline int CONST
cap_get_capSpaceType(cap_t cap)
{
    cap_tag_t ctag;

    ctag = cap_get_capType(cap);

    switch (ctag) {
    case cap_endpoint_cap:
    case cap_notification_cap:
    case cap_cnode_cap:
    case cap_thread_cap:
    case cap_frame_cap:
    case cap_page_table_cap:
    case cap_page_directory_cap:
    case cap_zombie_cap:
        return capSpaceTypedMemory;

    case cap_untyped_cap:
        return capSpaceUntypedMemory;

    case cap_irq_control_cap:
        return capSpaceIRQ;
    case cap_irq_handler_cap:
        return capSpaceIRQ;

    case cap_domain_cap:
        return capSpaceDomain;

    default:
        assert(!"Unknown cap type");
        /* Unreachable, but GCC can't figure that out */
        return 0;
    }
}

static inline void * CONST
cap_get_capSpacePtr(cap_t cap)
{
    cap_tag_t ctag;

    ctag = cap_get_capType(cap);

    switch (ctag) {
    case cap_endpoint_cap:
    case cap_notification_cap:
    case cap_cnode_cap:
    case cap_thread_cap:
    case cap_frame_cap:
    case cap_page_table_cap:
    case cap_page_directory_cap:
    case cap_zombie_cap:
    case cap_untyped_cap:
        return cap_get_capPtr(cap);

    case cap_reply_cap:
        return (void*)cap_reply_cap_get_capTCBPtr(cap);
    case cap_irq_control_cap:
        return (void*)0;
    case cap_irq_handler_cap:
        return (void*)cap_irq_handler_cap_get_capIRQ(cap);

    case cap_domain_cap:
        return (void*)0;

    default:
        assert(!"Unknown cap type");
        /* Unreachable, but GCC can't figure that out */
        return 0;
    }
}

static inline unsigned int CONST
cap_get_capSpaceSize(cap_t cap)
{
    cap_tag_t ctag;

    ctag = cap_get_capType(cap);

    switch (ctag) {
    case cap_endpoint_cap:
    case cap_notification_cap:
    case cap_cnode_cap:
    case cap_thread_cap:
    case cap_frame_cap:
    case cap_page_table_cap:
    case cap_page_directory_cap:
    case cap_zombie_cap:
    case cap_untyped_cap:
        return BIT(cap_get_capSizeBits(cap));

    case cap_reply_cap:
        return 1;
    case cap_irq_control_cap:
        return 0xff;
    case cap_irq_handler_cap:
        return 1;

    case cap_domain_cap:
        return 1;

    default:
        assert(!"Unknown cap type");
        /* Unreachable, but GCC can't figure that out */
        return 0;
    }
}

static inline unsigned int CONST
cap_get_capExtraComp(cap_t cap)
{
    switch (cap_get_capType(cap)) {
    case cap_frame_cap:
        if (!cap_frame_cap_get_capFMappedObject(cap)) {
            return 0;
        }
        switch (cap_frame_cap_get_capFSize(cap)) {
        case ARMSmallPage:
        case ARMLargePage:
            return PTE_REF(PTE_PTR(cap_frame_cap_get_capFMappedObject(cap)) + cap_frame_cap_get_capFMappedIndex(cap));
        case ARMSection:
        case ARMSuperSection:
            return PDE_REF(PDE_PTR(cap_frame_cap_get_capFMappedObject(cap)) + cap_frame_cap_get_capFMappedIndex(cap));
        default:
            fail ("Unknown frame size");
        }
    case cap_page_table_cap:
        if (!cap_page_table_cap_get_capPTMappedObject(cap)) {
            return 0;
        }
        return PDE_REF(PDE_PTR(cap_page_table_cap_get_capPTMappedObject(cap)) + cap_page_table_cap_get_capPTMappedIndex(cap));
    default:
        return 0;
    }
}

static inline unsigned int CONST
cte_depth_bits_type(cap_tag_t ctag)
{
    switch (ctag) {
    case cap_frame_cap:
    case cap_page_table_cap:
        return 1;
    default:
        return CTE_DEPTH_BITS;
    }
}

static inline unsigned int CONST
cte_depth_bits_cap(cap_t cap)
{
    return cte_depth_bits_type(cap_get_capType(cap));
}

#endif
