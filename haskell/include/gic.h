#include <stdint.h>
/* Memory map for GIC distributor */
typedef struct gic_dist_map_t{
    uint32_t enable;                /* 0x000 */
    uint32_t ic_type;               /* 0x004 */
    uint32_t dist_ident;            /* 0x008 */
    uint32_t res1[29];              /* [0x00C, 0x080) */

    uint32_t security[32];          /* [0x080, 0x100) */

    uint32_t enable_set[32];        /* [0x100, 0x180) */
    uint32_t enable_clr[32];        /* [0x180, 0x200) */
    uint32_t pending_set[32];       /* [0x200, 0x280) */
    uint32_t pending_clr[32];       /* [0x280, 0x300) */
    uint32_t active[32];            /* [0x300, 0x380) */
    uint32_t res2[32];              /* [0x380, 0x400) */

    uint32_t priority[255];         /* [0x400, 0x7FC) */
    uint32_t res3;                  /* 0x7FC */

    uint32_t targets[255];            /* [0x800, 0xBFC) */
    uint32_t res4;                  /* 0xBFC */

    uint32_t config[64];             /* [0xC00, 0xD00) */

    uint32_t spi[32];               /* [0xD00, 0xD80) */
    uint32_t res5[20];              /* [0xD80, 0xDD0) */
    uint32_t res6;                  /* 0xDD0 */
    uint32_t legacy_int;            /* 0xDD4 */
    uint32_t res7[2];               /* [0xDD8, 0xDE0) */
    uint32_t match_d;               /* 0xDE0 */
    uint32_t enable_d;              /* 0xDE4 */
    uint32_t res8[70];               /* [0xDE8, 0xF00) */

    uint32_t sgi_control;           /* 0xF00 */
    uint32_t res9[3];               /* [0xF04, 0xF10) */
    uint32_t sgi_pending_clr[4];    /* [0xF10, 0xF20) */
    uint32_t res10[40];             /* [0xF20, 0xFC0) */

    uint32_t periph_id[12];         /* [0xFC0, 0xFF0) */
    uint32_t component_id[4];       /* [0xFF0, 0xFFF] */
}gic_dist_map;

/* Memory map for GIC  cpu interface */
typedef struct gic_cpu_iface_map {
    uint32_t icontrol;              /*  0x000         */
    uint32_t pri_msk_c;             /*  0x004         */
    uint32_t pb_c;                  /*  0x008         */
    uint32_t int_ack;               /*  0x00C         */
    uint32_t eoi;                   /*  0x010         */
    uint32_t run_priority;          /*  0x014         */
    uint32_t hi_pend;               /*  0x018         */
    uint32_t ns_alias_bp_c;         /*  0x01C         */
    uint32_t ns_alias_ack;          /*  0x020 GIC400 only */
    uint32_t ns_alias_eoi;          /*  0x024 GIC400 only */
    uint32_t ns_alias_hi_pend;      /*  0x028 GIC400 only */

    uint32_t res1[5];               /* [0x02C, 0x040) */

    uint32_t integ_en_c;            /*  0x040 PL390 only */
    uint32_t interrupt_out;         /*  0x044 PL390 only */
    uint32_t res2[2];               /* [0x048, 0x050)    */

    uint32_t match_c;               /*  0x050 PL390 only */
    uint32_t enable_c;              /*  0x054 PL390 only */

    uint32_t res3[30];              /* [0x058, 0x0FC)  */
    uint32_t active_priority[4];    /* [0x0D0, 0xDC] GIC400 only */
    uint32_t ns_active_priority[4]; /* [0xE0,0xEC] GIC400 only */
    uint32_t res4[3];

    uint32_t cpu_if_ident;          /*  0x0FC         */
    uint32_t res5[948];             /* [0x100. 0xFC0) */

    uint32_t periph_id[8];          /* [0xFC0, 9xFF0) PL390 only */
    uint32_t component_id[4];       /* [0xFF0, 0xFFF] PL390 only */
}gic_cpu_iface_map;

