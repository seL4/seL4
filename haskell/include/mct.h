#include <stdint.h>
/* Memory map for MCT */
typedef struct mct_global_map {
    uint32_t reserved0[64];
    uint32_t cntl;           /* 0x100 Low word of count */
    uint32_t cnth;           /* 0x104 High word of count */
    uint32_t reserved1[1];
    uint32_t cnt_wstat;      /* 0x110 Write status for cnt */
    uint32_t reserved2[60];

    uint32_t comp0l;         /* 0x200 Low word of Compare value */
    uint32_t comp0h;         /* 0x204 High word of Compare value*/
    uint32_t comp0_add_inc;  /* 0x208 Low word of Automatic increment amount */
    uint32_t comp0_res;

    uint32_t comp1l;         /* 0x210 Low word of Compare value */
    uint32_t comp1h;         /* 0x214 High word of Compare value*/
    uint32_t comp1_add_inc;  /* 0x218 Low word of Automatic increment amount */
    uint32_t comp1_res;

    uint32_t comp2l;         /* 0x220 Low word of Compare value */
    uint32_t comp2h;         /* 0x224 High word of Compare value*/
    uint32_t comp2_add_inc;  /* 0x228 Low word of Automatic increment amount */
    uint32_t comp2_res;

    uint32_t comp3l;         /* 0x230 Low word of Compare value */
    uint32_t comp3h;         /* 0x234 High word of Compare value*/
    uint32_t comp3_add_inc;  /* 0x238 Low word of Automatic increment amount */
    uint32_t comp3_res;

    uint32_t tcon;           /* 0x240 Timer control */
    uint32_t int_stat;       /* 0x244 Interrupt pending status */
    uint32_t int_en;         /* 0x248 Interrupt enable */
    uint32_t wstat;          /* 0x24C  write status */
    uint32_t reserved3[44];
}mct_global_map;

struct mct_local_map {
    uint32_t tcompl;         /* 0x00 */
    uint32_t tcntl;          /* 0x04 */
    uint32_t tcomph;         /* 0x08 */
    uint32_t tcnth;          /* 0x0C */
    uint32_t reserved0[4];
    uint32_t tcon;           /* 0x20 Timer control */
    uint32_t int_stat;       /* 0x30 Interrupt status */
    uint32_t int_en;         /* 0x34 Interrupt enable */
    uint32_t reserved1[2];
    uint32_t wstat;          /* 0x40 Write status */
    uint32_t reserved2[50];
};

struct mct_map {
    struct mct_global_map global;
    struct mct_local_map local[4];
};

