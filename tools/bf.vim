"
" Copyright 2014, NICTA
"
" This software may be distributed and modified according to the terms of
" the BSD 2-Clause license. Note that NO WARRANTY is provided.
" See "LICENSE_BSD2.txt" for details.
"
" @TAG(NICTA_BSD)
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
