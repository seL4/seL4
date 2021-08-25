"
" Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
"
" SPDX-License-Identifier: BSD-2-Clause
"

" Vim syntax for .bf/.pbf files. Where relevant, comments below refer to the
" bitfield generator's source.

syn match   BFComment "\(#\( .*\)\?\|--.*\)$"
syn match   BFCPP     "[ \t]*#[^ ].*$"

" reserved_map.keys()
syn keyword BFKeyword base block field field_high mask padding tag tagged_union

" Tweaked t_INTLIT
syn match   BFLiteral "[ \t]\([1-9][0-9]*\|0[oO]\?[0-7]\+\|0[xX][0-9a-fA-F]\+\|0[bB][01]\+\|0\)[lL]\?"

hi def link BFComment Comment
hi def link BFCPP     PreProc
hi def link BFKeyword Type
hi def link BFLiteral Constant
