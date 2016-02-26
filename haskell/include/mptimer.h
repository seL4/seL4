#include <stdint.h>
/* Memory map for MCT */
typedef struct mp_priv_timer {
    uint32_t load;
    uint32_t count;
    uint32_t ctrl;
    uint32_t ints;
}priv_timer;
