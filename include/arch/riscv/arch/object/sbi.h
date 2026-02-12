#pragma once

#define NUM_SBI_REGS 8

exception_t decodeRISCVSBIInvocation(word_t label, unsigned int length, cptr_t cptr,
                                    cte_t *srcSlot, cap_t cap, bool_t call, word_t *buffer);

