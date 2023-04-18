#!/usr/bin/env python3
#
# Copyright 2022, Proofcraft Pty Ltd
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: BSD-2-Clause
#

##
# A tool for generating bifield structures with get/set/new methods
# including Isabelle/HOL specifications and correctness proofs.
##

from __future__ import print_function, division
import sys
import os.path
import optparse
import re
import itertools
import tempfile

from six.moves import range
from functools import reduce

import lex
from ply import yacc

import umm

# Whether debugging is enabled (turn on with command line option --debug).
DEBUG = False

# name of locale the bitfield proofs should be in
loc_name = 'kernel_all_substitute'

# The C parser emits variables with a suffix to indicate their types; this lets
# the C parser distinguish `char x` from `int x`.
#
# The suffix map should match the C parser types for `uint{8,16,32,64}_t` in
# `include/stdint.h`.
var_size_suffix_map = {k: 'unsigned' + v for (k, v) in
                       {8: '_char', 16: '_short', 32: '',
                        64: '_longlong'}.items()}


def return_name(base):
    # name of return value for standard word sizes
    return 'ret__' + var_size_suffix_map[base]


def var_name(name, base):
    # converts C variable name to Isabelle variable name via type mangling.
    return name + '___' + var_size_suffix_map[base]


# Headers to include depending on which environment we are generating code for.
INCLUDES = {
    'sel4': ['assert.h', 'config.h', 'stdint.h', 'util.h'],
    'libsel4': ['autoconf.h', 'sel4/simple_types.h', 'sel4/debug_assert.h'],
}

ASSERTS = {
    'sel4': 'assert',
    'libsel4': 'seL4_DebugAssert'
}

INLINE = {
    'sel4': 'static inline',
    'libsel4': 'LIBSEL4_INLINE_FUNC'
}

TYPES = {
    "sel4": {
        8:  "uint8_t",
        16: "uint16_t",
        32: "uint32_t",
        64: "uint64_t"
    },

    "libsel4": {
        8:  "seL4_Uint8",
        16: "seL4_Uint16",
        32: "seL4_Uint32",
        64: "seL4_Uint64"
    }
}

# Parser

reserved = ('BLOCK', 'BASE', 'FIELD', 'FIELD_HIGH', 'MASK', 'PADDING',
            'TAGGED_UNION', 'TAG')

tokens = reserved + ('IDENTIFIER', 'INTLIT', 'LBRACE', 'RBRACE',
                     'LPAREN', 'RPAREN', 'COMMA')

t_LBRACE = r'{'
t_RBRACE = r'}'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_COMMA = r','

reserved_map = dict((r.lower(), r) for r in reserved)


def t_IDENTIFIER(t):
    r'[A-Za-z_]\w+|[A-Za-z]'
    t.type = reserved_map.get(t.value, 'IDENTIFIER')
    return t


def t_INTLIT(t):
    r'([1-9][0-9]*|0[oO]?[0-7]+|0[xX][0-9a-fA-F]+|0[bB][01]+|0)[lL]?'
    t.value = int(t.value, 0)
    return t


def t_NEWLINE(t):
    r'\n+'
    t.lexer.lineno += len(t.value)


def t_comment(t):
    r'--.*|\#.*'


t_ignore = ' \t'


def t_error(t):
    print("%s: Unexpected character '%s'" % (sys.argv[0], t.value[0]),
          file=sys.stderr)
    if DEBUG:
        print('Token: %s' % str(t), file=sys.stderr)
    sys.exit(1)


def p_start(t):
    """start : entity_list"""
    t[0] = t[1]


def p_entity_list_empty(t):
    """entity_list : """
    t[0] = (None, {}, {})


def p_entity_list_base(t):
    """entity_list : entity_list base"""
    current_base, block_map, union_map = t[1]
    block_map.setdefault(t[2], {})
    union_map.setdefault(t[2], {})
    t[0] = (t[2], block_map, union_map)


def p_entity_list_block(t):
    """entity_list : entity_list block"""
    current_base, block_map, union_map = t[1]
    block_map[current_base][t[2].name] = t[2]
    t[0] = (current_base, block_map, union_map)


def p_entity_list_union(t):
    """entity_list : entity_list tagged_union"""
    current_base, block_map, union_map = t[1]
    union_map[current_base][t[2].name] = t[2]
    t[0] = (current_base, block_map, union_map)


def p_base_simple(t):
    """base : BASE INTLIT"""
    t[0] = (t[2], t[2], 0)


def p_base_mask(t):
    """base : BASE INTLIT LPAREN INTLIT COMMA INTLIT RPAREN"""
    t[0] = (t[2], t[4], t[6])


def p_block(t):
    """block : BLOCK IDENTIFIER opt_visible_order_spec""" \
        """ LBRACE fields RBRACE"""
    t[0] = Block(name=t[2], fields=t[5], visible_order=t[3])


def p_opt_visible_order_spec_empty(t):
    """opt_visible_order_spec : """
    t[0] = None


def p_opt_visible_order_spec(t):
    """opt_visible_order_spec : LPAREN visible_order_spec RPAREN"""
    t[0] = t[2]


def p_visible_order_spec_empty(t):
    """visible_order_spec : """
    t[0] = []


def p_visible_order_spec_single(t):
    """visible_order_spec : IDENTIFIER"""
    t[0] = [t[1]]


def p_visible_order_spec(t):
    """visible_order_spec : visible_order_spec COMMA IDENTIFIER"""
    t[0] = t[1] + [t[3]]


def p_fields_empty(t):
    """fields : """
    t[0] = []


def p_fields_field(t):
    """fields : fields FIELD IDENTIFIER INTLIT"""
    t[0] = t[1] + [(t[3], t[4], False)]


def p_fields_field_high(t):
    """fields : fields FIELD_HIGH IDENTIFIER INTLIT"""
    t[0] = t[1] + [(t[3], t[4], True)]


def p_fields_padding(t):
    """fields : fields PADDING INTLIT"""
    t[0] = t[1] + [(None, t[3], False)]


def p_tag_name_parts_one(t):
    """tag_name_parts : IDENTIFIER"""
    t[0] = [t[1]]


def p_tag_name_parts(t):
    """tag_name_parts : tag_name_parts COMMA IDENTIFIER"""
    t[0] = t[1] + [t[3]]


def p_tag_slices_empty(t):
    """tag_slices : """
    t[0] = []


def p_tag_slices(t):
    """tag_slices : LPAREN tag_name_parts RPAREN"""
    t[0] = t[2]


def p_tagged_union(t):
    """tagged_union : TAGGED_UNION IDENTIFIER IDENTIFIER tag_slices""" \
        """ LBRACE masks tags RBRACE"""
    t[0] = TaggedUnion(name=t[2], tagname=t[3], tag_slices=t[4], classes=t[6], tags=t[7])


def p_tags_empty(t):
    """tags :"""
    t[0] = []


def p_tag_values_one(t):
    """tag_values : INTLIT"""
    t[0] = [t[1]]


def p_tag_values(t):
    """tag_values : tag_values COMMA INTLIT"""
    t[0] = t[1] + [t[3]]


def p_tag_value(t):
    """tag_value : LPAREN tag_values RPAREN"""
    t[0] = t[2]


def p_tag_value_one(t):
    """tag_value : INTLIT"""
    t[0] = [t[1]]


def p_tags(t):
    """tags : tags TAG IDENTIFIER tag_value"""
    t[0] = t[1] + [(t[3], t[4])]


def p_masks_empty(t):
    """masks :"""
    t[0] = []


def p_masks(t):
    """masks : masks MASK INTLIT INTLIT"""
    t[0] = t[1] + [(t[3], t[4])]


def p_error(t):
    print("Syntax error at token '%s'" % t.value, file=sys.stderr)
    sys.exit(1)

# Templates

# C templates


typedef_template = \
    """struct %(name)s {
    %(type)s words[%(multiple)d];
};
typedef struct %(name)s %(name)s_t;"""

generator_template = \
    """%(inline)s %(block)s_t CONST
%(block)s_new(%(gen_params)s) {
    %(block)s_t %(block)s;

%(asserts)s

%(gen_inits)s

    return %(block)s;
}"""

ptr_generator_template = \
    """%(inline)s void
%(block)s_ptr_new(%(ptr_params)s) {
%(asserts)s

%(ptr_inits)s
}"""

reader_template = \
    """%(inline)s %(type)s CONST
%(block)s_get_%(field)s(%(block)s_t %(block)s) {
    %(type)s ret;
    ret = (%(block)s.words[%(index)d] & 0x%(mask)x%(suf)s) %(r_shift_op)s %(shift)d;
    /* Possibly sign extend */
    if (__builtin_expect(!!(%(sign_extend)d && (ret & (1%(suf)s << (%(extend_bit)d)))), %(sign_extend)d)) {
        ret |= 0x%(high_bits)x;
    }
    return ret;
}"""

ptr_reader_template = \
    """%(inline)s %(type)s PURE
%(block)s_ptr_get_%(field)s(%(block)s_t *%(block)s_ptr) {
    %(type)s ret;
    ret = (%(block)s_ptr->words[%(index)d] & 0x%(mask)x%(suf)s) """ \
    """%(r_shift_op)s %(shift)d;
    /* Possibly sign extend */
    if (__builtin_expect(!!(%(sign_extend)d && (ret & (1%(suf)s << (%(extend_bit)d)))), %(sign_extend)d)) {
        ret |= 0x%(high_bits)x;
    }
    return ret;
}"""

writer_template = \
    """%(inline)s %(block)s_t CONST
%(block)s_set_%(field)s(%(block)s_t %(block)s, %(type)s v%(base)d) {
    /* fail if user has passed bits that we will override */
    %(assert)s((((~0x%(mask)x%(suf)s %(r_shift_op)s %(shift)d ) | 0x%(high_bits)x) & v%(base)d) == ((%(sign_extend)d && (v%(base)d & (1%(suf)s << (%(extend_bit)d)))) ? 0x%(high_bits)x : 0));
    %(block)s.words[%(index)d] &= ~0x%(mask)x%(suf)s;
    %(block)s.words[%(index)d] |= (v%(base)d %(w_shift_op)s %(shift)d) & 0x%(mask)x%(suf)s;
    return %(block)s;
}"""

ptr_writer_template = \
    """%(inline)s void
%(block)s_ptr_set_%(field)s(%(block)s_t *%(block)s_ptr, %(type)s v%(base)d) {
    /* fail if user has passed bits that we will override */
    %(assert)s((((~0x%(mask)x%(suf)s %(r_shift_op)s %(shift)d) | 0x%(high_bits)x) & v%(base)d) == ((%(sign_extend)d && (v%(base)d & (1%(suf)s << (%(extend_bit)d)))) ? 0x%(high_bits)x : 0));
    %(block)s_ptr->words[%(index)d] &= ~0x%(mask)x%(suf)s;
    %(block)s_ptr->words[%(index)d] |= (v%(base)d %(w_shift_op)s """ \
    """%(shift)d) & 0x%(mask)x;
}"""

union_generator_template = \
    """%(inline)s %(union)s_t CONST
%(union)s_%(block)s_new(%(gen_params)s) {
    %(union)s_t %(union)s;

%(asserts)s

%(gen_inits)s

    return %(union)s;
}"""

ptr_union_generator_template = \
    """%(inline)s void
%(union)s_%(block)s_ptr_new(%(ptr_params)s) {
%(asserts)s

%(ptr_inits)s
}"""

union_reader_template = \
    """%(inline)s %(type)s CONST
%(union)s_%(block)s_get_%(field)s(%(union)s_t %(union)s) {
    %(type)s ret;
    %(assert)s(((%(union)s.words[%(tagindex)d] >> %(tagshift)d) & 0x%(tagmask)x) ==
           %(tagvalue)s);

    ret = (%(union)s.words[%(index)d] & 0x%(mask)x%(suf)s) %(r_shift_op)s %(shift)d;
    /* Possibly sign extend */
    if (__builtin_expect(!!(%(sign_extend)d && (ret & (1%(suf)s << (%(extend_bit)d)))), %(sign_extend)d)) {
        ret |= 0x%(high_bits)x;
    }
    return ret;
}"""

ptr_union_reader_template = \
    """%(inline)s %(type)s PURE
%(union)s_%(block)s_ptr_get_%(field)s(%(union)s_t *%(union)s_ptr) {
    %(type)s ret;
    %(assert)s(((%(union)s_ptr->words[%(tagindex)d] >> """ \
    """%(tagshift)d) & 0x%(tagmask)x) ==
           %(tagvalue)s);

    ret = (%(union)s_ptr->words[%(index)d] & 0x%(mask)x%(suf)s) """ \
    """%(r_shift_op)s %(shift)d;
    /* Possibly sign extend */
    if (__builtin_expect(!!(%(sign_extend)d && (ret & (1%(suf)s << (%(extend_bit)d)))), %(sign_extend)d)) {
        ret |= 0x%(high_bits)x;
    }
    return ret;
}"""

union_writer_template = \
    """%(inline)s %(union)s_t CONST
%(union)s_%(block)s_set_%(field)s(%(union)s_t %(union)s, %(type)s v%(base)d) {
    %(assert)s(((%(union)s.words[%(tagindex)d] >> %(tagshift)d) & 0x%(tagmask)x) ==
           %(tagvalue)s);
    /* fail if user has passed bits that we will override */
    %(assert)s((((~0x%(mask)x%(suf)s %(r_shift_op)s %(shift)d ) | 0x%(high_bits)x) & v%(base)d) == ((%(sign_extend)d && (v%(base)d & (1%(suf)s << (%(extend_bit)d)))) ? 0x%(high_bits)x : 0));

    %(union)s.words[%(index)d] &= ~0x%(mask)x%(suf)s;
    %(union)s.words[%(index)d] |= (v%(base)d %(w_shift_op)s %(shift)d) & 0x%(mask)x%(suf)s;
    return %(union)s;
}"""

ptr_union_writer_template = \
    """%(inline)s void
%(union)s_%(block)s_ptr_set_%(field)s(%(union)s_t *%(union)s_ptr,
                                      %(type)s v%(base)d) {
    %(assert)s(((%(union)s_ptr->words[%(tagindex)d] >> """ \
    """%(tagshift)d) & 0x%(tagmask)x) ==
           %(tagvalue)s);

    /* fail if user has passed bits that we will override */
    %(assert)s((((~0x%(mask)x%(suf)s %(r_shift_op)s %(shift)d) | 0x%(high_bits)x) & v%(base)d) == ((%(sign_extend)d && (v%(base)d & (1%(suf)s << (%(extend_bit)d)))) ? 0x%(high_bits)x : 0));

    %(union)s_ptr->words[%(index)d] &= ~0x%(mask)x%(suf)s;
    %(union)s_ptr->words[%(index)d] |= """ \
    """(v%(base)d %(w_shift_op)s %(shift)d) & 0x%(mask)x%(suf)s;
}"""

tag_reader_header_template = \
    """%(inline)s %(type)s CONST
%(union)s_get_%(tagname)s(%(union)s_t %(union)s) {
"""

tag_reader_entry_template = \
    """    if ((%(union)s.words[%(index)d] & 0x%(classmask)x) != 0x%(classmask)x)
        return (%(union)s.words[%(index)d] >> %(shift)d) & 0x%(mask)x%(suf)s;
"""

tag_reader_final_template = \
    """    return (%(union)s.words[%(index)d] >> %(shift)d) & 0x%(mask)x%(suf)s;"""

tag_reader_footer_template = \
    """
}"""

tag_eq_reader_header_template = \
    """%(inline)s int CONST
%(union)s_%(tagname)s_equals(%(union)s_t %(union)s, %(type)s %(union)s_type_tag) {
"""

tag_eq_reader_entry_template = \
    """    if ((%(union)s_type_tag & 0x%(classmask)x) != 0x%(classmask)x)
        return ((%(union)s.words[%(index)d] >> %(shift)d) & 0x%(mask)x%(suf)s) == %(union)s_type_tag;
"""

tag_eq_reader_final_template = \
    """    return ((%(union)s.words[%(index)d] >> %(shift)d) & 0x%(mask)x%(suf)s) == %(union)s_type_tag;"""

tag_eq_reader_footer_template = \
    """
}"""

ptr_tag_reader_header_template = \
    """%(inline)s %(type)s PURE
%(union)s_ptr_get_%(tagname)s(%(union)s_t *%(union)s_ptr) {
"""

ptr_tag_reader_entry_template = \
    """    if ((%(union)s_ptr->words[%(index)d] & 0x%(classmask)x) != 0x%(classmask)x)
        return (%(union)s_ptr->words[%(index)d] >> %(shift)d) & 0x%(mask)x%(suf)s;
"""

ptr_tag_reader_final_template = \
    """    return (%(union)s_ptr->words[%(index)d] >> %(shift)d) & 0x%(mask)x%(suf)s;"""

ptr_tag_reader_footer_template = \
    """
}"""

tag_writer_template = \
    """%(inline)s %(union)s_t CONST
%(union)s_set_%(tagname)s(%(union)s_t %(union)s, %(type)s v%(base)d) {
    /* fail if user has passed bits that we will override */
    %(assert)s((((~0x%(mask)x%(suf)s %(r_shift_op)s %(shift)d) | 0x%(high_bits)x) & v%(base)d) == ((%(sign_extend)d && (v%(base)d & (1%(suf)s << (%(extend_bit)d)))) ? 0x%(high_bits)x : 0));

    %(union)s.words[%(index)d] &= ~0x%(mask)x%(suf)s;
    %(union)s.words[%(index)d] |= (v%(base)d << %(shift)d) & 0x%(mask)x%(suf)s;
    return %(union)s;
}"""

ptr_tag_writer_template = \
    """%(inline)s void
%(union)s_ptr_set_%(tagname)s(%(union)s_t *%(union)s_ptr, %(type)s v%(base)d) {
    /* fail if user has passed bits that we will override */
    %(assert)s((((~0x%(mask)x%(suf)s %(r_shift_op)s %(shift)d) | 0x%(high_bits)x) & v%(base)d) == ((%(sign_extend)d && (v%(base)d & (1%(suf)s << (%(extend_bit)d)))) ? 0x%(high_bits)x : 0));

    %(union)s_ptr->words[%(index)d] &= ~0x%(mask)x%(suf)s;
    %(union)s_ptr->words[%(index)d] |= (v%(base)d << %(shift)d) & 0x%(mask)x%(suf)s;
}"""

# HOL definition templates

lift_def_template = \
    '''definition %(name)s_lift :: "%(name)s_C \<Rightarrow> %(name)s_CL" where
  "%(name)s_lift %(name)s \<equiv> \<lparr>
       %(fields)s \<rparr>"'''

block_lift_def_template = \
    '''definition %(union)s_%(block)s_lift :: ''' \
    '''"%(union)s_C \<Rightarrow> %(union)s_%(block)s_CL" where
  "%(union)s_%(block)s_lift %(union)s \<equiv>
    case (%(union)s_lift %(union)s) of ''' \
    '''Some (%(generator)s rec) \<Rightarrow> rec"'''

block_lift_lemma_template = \
    '''lemma %(union)s_%(block)s_lift:
  "(%(union)s_get_tag c = scast %(union)s_%(block)s) = ''' \
 '''(%(union)s_lift c = Some (%(generator)s (%(union)s_%(block)s_lift c)))"
  unfolding %(union)s_lift_def %(union)s_%(block)s_lift_def
  by (clarsimp simp: %(union)s_tag_defs Let_def)'''

union_get_tag_def_header_template = \
    '''definition %(name)s_get_tag :: "%(name)s_C \<Rightarrow> word%(base)d" where
  "%(name)s_get_tag %(name)s \<equiv>
     '''

union_get_tag_def_entry_template = \
    '''if ((index (%(name)s_C.words_C %(name)s) %(tag_index)d)''' \
    ''' AND 0x%(classmask)x \<noteq> 0x%(classmask)x)
      then ((index (%(name)s_C.words_C %(name)s) %(tag_index)d)'''\
''' >> %(tag_shift)d) AND mask %(tag_size)d
      else '''

union_get_tag_def_final_template = \
    '''((index (%(name)s_C.words_C %(name)s) %(tag_index)d)'''\
    ''' >> %(tag_shift)d) AND mask %(tag_size)d'''

union_get_tag_def_footer_template = '''"'''

union_get_tag_eq_x_def_header_template = \
    '''lemma %(name)s_get_tag_eq_x:
  "(%(name)s_get_tag c = x) = (('''

union_get_tag_eq_x_def_entry_template = \
    '''if ((x << %(tag_shift)d) AND 0x%(classmask)x \<noteq> 0x%(classmask)x)
      then ((index (%(name)s_C.words_C c) %(tag_index)d)''' \
''' >> %(tag_shift)d) AND mask %(tag_size)d
      else '''

union_get_tag_eq_x_def_final_template = \
    '''((index (%(name)s_C.words_C c) %(tag_index)d)''' \
    ''' >> %(tag_shift)d) AND mask %(tag_size)d'''

union_get_tag_eq_x_def_footer_template = ''') = x)"
  by (auto simp add: %(name)s_get_tag_def mask_def word_bw_assocs)'''

union_tag_mask_helpers_header_template = \
    '''lemma %(name)s_%(block)s_tag_mask_helpers:'''

union_tag_mask_helpers_entry_template = '''
  "w && %(full_mask)s = %(full_value)s \<Longrightarrow> w'''\
''' && %(part_mask)s = %(part_value)s"
'''

union_tag_mask_helpers_footer_template = \
    '''  by (auto elim: word_sub_mask simp: mask_def)'''

union_lift_def_template = \
    '''definition %(name)s_lift :: "%(name)s_C \<Rightarrow> %(name)s_CL option" where
  "%(name)s_lift %(name)s \<equiv>
    (let tag = %(name)s_get_tag %(name)s in
     %(tag_cases)s
     else None)"'''

union_access_def_template = \
    '''definition %(union)s_%(block)s_access ::
  "(%(union)s_%(block)s_CL \<Rightarrow> 'a) \<Rightarrow> %(union)s_CL \<Rightarrow> 'a" where
  "%(union)s_%(block)s_access f %(union)s \<equiv>
     (case %(union)s of %(generator)s rec \<Rightarrow> f rec)"'''

union_update_def_template = \
    '''definition %(union)s_%(block)s_update ::
  "(%(union)s_%(block)s_CL \<Rightarrow> %(union)s_%(block)s_CL) \<Rightarrow>'''\
  '''%(union)s_CL \<Rightarrow> %(union)s_CL" where
  "%(union)s_%(block)s_update f %(union)s \<equiv>
     (case %(union)s of %(generator)s rec \<Rightarrow>
        %(generator)s (f rec))"'''

# HOL proof templates

# FIXME: avoid [simp]
struct_lemmas_template = \
    '''
lemmas %(name)s_ptr_guards[simp] =
  %(name)s_ptr_words_NULL
  %(name)s_ptr_words_aligned
  %(name)s_ptr_words_ptr_safe'''

# FIXME: move to global theory
defs_global_lemmas = '''
lemma word_sub_mask:
  "\<lbrakk> w && m1 = v1; m1 && m2 = m2; v1 && m2 = v2 \<rbrakk>
     \<Longrightarrow> w && m2 = v2"
  by (clarsimp simp: word_bw_assocs)
'''

# Proof templates are stored as a list of
# (header, body, stuff to go afterwards).
# This makes it easy to replace the proof body with a sorry.

# ptrname should be a function of s


def ptr_basic_template(name, ptrname, retval, args, post):
    return ('''lemma (in ''' + loc_name + ''') %(name)s_ptr_''' + name + '''_spec:
           defines "ptrval s \<equiv> cslift s ''' + ptrname + '''"
           shows "\<forall>s. \<Gamma> \<turnstile> \<lbrace>s. s \<Turnstile>\<^sub>c ''' + ptrname + '''\<rbrace>
            ''' + retval + '''PROC %(name)s_ptr_''' + name + '''(\<acute>%(name)s_ptr''' + args + ''')
            ''' + post + ''' " ''')


def ptr_union_basic_template(name, ptrname, retval, args, pre, post):
    return ('''lemma (in ''' + loc_name + ''') %(name)s_%(block)s_ptr_''' + name + '''_spec:
    defines "ptrval s \<equiv> cslift s ''' + ptrname + '''"
    shows "\<forall>s. \<Gamma> \<turnstile> \<lbrace>s. s \<Turnstile>\<^sub>c ''' + ptrname + " " + pre + '''\<rbrace>
            ''' + retval + '''PROC %(name)s_%(block)s_ptr_''' + name + '''(\<acute>%(name)s_ptr''' + args + ''')
            ''' + post + ''' " ''')


direct_ptr_name = '\<^bsup>s\<^esup>%(name)s_ptr'
path_ptr_name = '(cparent \<^bsup>s\<^esup>%(name)s_ptr [%(path)s] :: %(toptp)s ptr)'


def ptr_get_template(ptrname):
    return ptr_basic_template('get_%(field)s', ptrname, '\<acute>%(ret_name)s :== ', '',
                              '''\<lbrace>\<acute>%(ret_name)s = '''
                              '''%(name)s_CL.%(field)s_CL '''
                              '''(%(name)s_lift (%(access_path)s))\<rbrace>''')


def ptr_set_template(name, ptrname):
    return ptr_basic_template(name, ptrname, '', ', \<acute>v%(base)d',
                              '''{t. \<exists>%(name)s.
                              %(name)s_lift %(name)s =
                              %(name)s_lift (%(access_path)s) \<lparr> %(name)s_CL.%(field)s_CL '''
                              ''':= %(sign_extend)s(\<^bsup>s\<^esup>v%(base)d AND %(mask)s) \<rparr> \<and>
                              t_hrs_' (globals t) = hrs_mem_update (heap_update
                                      (''' + ptrname + ''')
                                      %(update_path)s)
                                  (t_hrs_' (globals s))
                              }''')


def ptr_new_template(ptrname):
    return ptr_basic_template('new', ptrname, '', ', %(args)s',
                              '''{t. \<exists>%(name)s. %(name)s_lift %(name)s = \<lparr>
                              %(field_eqs)s \<rparr> \<and>
                              t_hrs_' (globals t) = hrs_mem_update (heap_update
                                      (''' + ptrname + ''')
                                      %(update_path)s)
                                  (t_hrs_' (globals s))
                              }''')


def ptr_get_tag_template(ptrname):
    return ptr_basic_template('get_%(tagname)s', ptrname, '\<acute>%(ret_name)s :== ', '',
                              '''\<lbrace>\<acute>%(ret_name)s = %(name)s_get_tag (%(access_path)s)\<rbrace>''')


def ptr_empty_union_new_template(ptrname):
    return ptr_union_basic_template('new', ptrname, '', '', '',
                                    '''{t. \<exists>%(name)s. '''
                                    '''%(name)s_get_tag %(name)s = scast %(name)s_%(block)s \<and>
                                    t_hrs_' (globals t) = hrs_mem_update (heap_update
                                            (''' + ptrname + ''')
                                            %(update_path)s)
                                        (t_hrs_' (globals s))
                                    }''')


def ptr_union_new_template(ptrname):
    return ptr_union_basic_template('new', ptrname, '', ', %(args)s', '',
                                    '''{t. \<exists>%(name)s. '''
                                    '''%(name)s_%(block)s_lift %(name)s = \<lparr>
                                    %(field_eqs)s \<rparr> \<and>
                                    %(name)s_get_tag %(name)s = scast %(name)s_%(block)s \<and>
                                    t_hrs_' (globals t) = hrs_mem_update (heap_update
                                            (''' + ptrname + ''')
                                            %(update_path)s)
                                        (t_hrs_' (globals s))
                                    }''')


def ptr_union_get_template(ptrname):
    return ptr_union_basic_template('get_%(field)s', ptrname,
                                    '\<acute>%(ret_name)s :== ', '',
                                    '\<and> %(name)s_get_tag %(access_path)s = scast %(name)s_%(block)s',
                                    '''\<lbrace>\<acute>%(ret_name)s = '''
                                    '''%(name)s_%(block)s_CL.%(field)s_CL '''
                                    '''(%(name)s_%(block)s_lift %(access_path)s)\<rbrace>''')


def ptr_union_set_template(ptrname):
    return ptr_union_basic_template('set_%(field)s', ptrname, '', ', \<acute>v%(base)d',
                                    '\<and> %(name)s_get_tag %(access_path)s = scast %(name)s_%(block)s',
                                    '''{t. \<exists>%(name)s. '''
                                    '''%(name)s_%(block)s_lift %(name)s =
                                    %(name)s_%(block)s_lift %(access_path)s '''
                                    '''\<lparr> %(name)s_%(block)s_CL.%(field)s_CL '''
                                    ''':= %(sign_extend)s(\<^bsup>s\<^esup>v%(base)d AND %(mask)s) \<rparr> \<and>
                                    %(name)s_get_tag %(name)s = scast %(name)s_%(block)s \<and>
                                    t_hrs_' (globals t) = hrs_mem_update (heap_update
                                            (''' + ptrname + ''')
                                            %(update_path)s)
                                        (t_hrs_' (globals s))
                                    }''')


proof_templates = {

    'lift_collapse_proof': [
        '''lemma %(name)s_lift_%(block)s:
  "%(name)s_get_tag %(name)s = scast %(name)s_%(block)s \<Longrightarrow>
  %(name)s_lift %(name)s =
  Some (%(value)s)"''',
        '''  by (simp add:%(name)s_lift_def %(name)s_tag_defs)'''],

    'words_NULL_proof': [
        '''lemma %(name)s_ptr_words_NULL:
  "c_guard (p::%(name)s_C ptr) \<Longrightarrow>
   0 < &(p\<rightarrow>[''words_C''])"''',
        '''  by (fastforce intro:c_guard_NULL_fl simp:typ_uinfo_t_def)'''],

    'words_aligned_proof': [
        '''lemma %(name)s_ptr_words_aligned:
  "c_guard (p::%(name)s_C ptr) \<Longrightarrow>
   ptr_aligned ((Ptr &(p\<rightarrow>[''words_C'']))::'''
        '''((word%(base)d[%(words)d]) ptr))"''',
        '''  by (fastforce intro:c_guard_ptr_aligned_fl simp:typ_uinfo_t_def)'''],

    'words_ptr_safe_proof': [
        '''lemma %(name)s_ptr_words_ptr_safe:
  "ptr_safe (p::%(name)s_C ptr) d \<Longrightarrow>
   ptr_safe (Ptr &(p\<rightarrow>[''words_C''])::'''
        '''((word%(base)d[%(words)d]) ptr)) d"''',
        '''  by (fastforce intro:ptr_safe_mono simp:typ_uinfo_t_def)'''],

    'get_tag_fun_spec_proof': [
        '''lemma (in ''' + loc_name + ''') fun_spec:
  "\<Gamma> \<turnstile> {\<sigma>}
       \<acute>ret__%(rtype)s :== PROC %(name)s_get_%(tag_name)s('''
        ''' \<acute>%(name))
       \<lbrace>\<acute>ret__%(rtype)s = %(name)s_get_tag'''
        '''\<^bsup>\<sigma>\<^esup>\<rbrace>"''',
        '''  apply(rule allI, rule conseqPre, vcg)
  apply (clarsimp)
  apply (simp add:$(name)s_get_tag_def word_sle_def mask_def ucast_def)
  done'''],

    'const_modifies_proof': [
        '''lemma (in ''' + loc_name + ''') %(fun_name)s_modifies:
  "\<forall> s. \<Gamma> \<turnstile>\<^bsub>/UNIV\<^esub> {s}
       PROC %(fun_name)s(%(args)s)
       {t. t may_not_modify_globals s}"''',
        '''  by (vcg spec=modifies strip_guards=true)'''],

    'ptr_set_modifies_proof': [
        '''lemma (in ''' + loc_name + ''') %(fun_name)s_modifies:
  "\<forall>s. \<Gamma> \<turnstile>\<^bsub>/UNIV\<^esub> {s}
       PROC %(fun_name)s(%(args)s)
       {t. t may_only_modify_globals s in [t_hrs]}"''',
        '''  by (vcg spec=modifies strip_guards=true)'''],


    'new_spec': [
        '''lemma (in ''' + loc_name + ''') %(name)s_new_spec:
  "\<forall> s. \<Gamma> \<turnstile> {s}
       \<acute>ret__struct_%(name)s_C :== PROC %(name)s_new(%(args)s)
       \<lbrace> %(name)s_lift \<acute>ret__struct_%(name)s_C = \<lparr>
          %(field_eqs)s \<rparr> \<rbrace>"''',
        '''  apply (rule allI, rule conseqPre, vcg)
  apply (clarsimp simp: guard_simps)
  apply (simp add: %(name)s_lift_def)
  apply ((intro conjI sign_extend_eq)?;
         (simp add: mask_def shift_over_ao_dists multi_shift_simps word_size
                    word_ao_dist word_bw_assocs word_and_max_simps)?)
  done'''],

    'ptr_new_spec_direct': [
        ptr_new_template(direct_ptr_name),
        '''sorry (* ptr_new_spec_direct *)'''],

    'ptr_new_spec_path': [
        ptr_new_template(path_ptr_name),
        '''sorry (* ptr_new_spec_path *)'''],


    'get_spec': [
        '''lemma (in ''' + loc_name + ''') %(name)s_get_%(field)s_spec:
  "\<forall>s. \<Gamma> \<turnstile> {s}
       \<acute>%(ret_name)s :== '''
        '''PROC %(name)s_get_%(field)s(\<acute>%(name)s)
       \<lbrace>\<acute>%(ret_name)s = '''
        '''%(name)s_CL.%(field)s_CL '''
        '''(%(name)s_lift \<^bsup>s\<^esup>%(name)s)\<rbrace>"''',
        '''  apply (rule allI, rule conseqPre, vcg)
  apply clarsimp
  apply (simp add: %(name)s_lift_def mask_shift_simps guard_simps)
  apply (simp add: sign_extend_def' mask_def nth_is_and_neq_0 word_bw_assocs
                   shift_over_ao_dists word_oa_dist word_and_max_simps)?
  done'''],

    'set_spec': [
        '''lemma (in ''' + loc_name + ''') %(name)s_set_%(field)s_spec:
  "\<forall>s. \<Gamma> \<turnstile> {s}
       \<acute>ret__struct_%(name)s_C :== '''
        '''PROC %(name)s_set_%(field)s(\<acute>%(name)s, \<acute>v%(base)d)
       \<lbrace>%(name)s_lift \<acute>ret__struct_%(name)s_C = '''
        '''%(name)s_lift \<^bsup>s\<^esup>%(name)s \<lparr> '''
        '''%(name)s_CL.%(field)s_CL '''
        ''':= %(sign_extend)s (\<^bsup>s\<^esup>v%(base)d AND %(mask)s) \<rparr>\<rbrace>"''',
        '''  apply(rule allI, rule conseqPre, vcg)
  apply (clarsimp simp: guard_simps ucast_id
                        %(name)s_lift_def
                        mask_def shift_over_ao_dists
                        multi_shift_simps word_size
                        word_ao_dist word_bw_assocs
                        NOT_eq)
  apply (simp add: sign_extend_def' mask_def nth_is_and_neq_0 word_bw_assocs
                   shift_over_ao_dists word_and_max_simps)?
  done'''],

    # where the top level type is the bitfield type --- these are split because they have different proofs
    'ptr_get_spec_direct': [
        ptr_get_template(direct_ptr_name),
        '''   unfolding ptrval_def
  apply (rule allI, rule conseqPre, vcg)
  apply (clarsimp simp: h_t_valid_clift_Some_iff)
  apply (simp add: %(name)s_lift_def guard_simps mask_def typ_heap_simps ucast_def)
  apply (simp add: sign_extend_def' mask_def nth_is_and_neq_0 word_bw_assocs
                   shift_over_ao_dists word_oa_dist word_and_max_simps)?
  done'''],

    'ptr_get_spec_path': [
        ptr_get_template(path_ptr_name),
        '''  unfolding ptrval_def
  apply (rule allI, rule conseqPre, vcg)
  apply (clarsimp simp: guard_simps)
  apply (frule iffD1[OF h_t_valid_clift_Some_iff], rule exE, assumption, simp)
  apply (frule clift_subtype, simp, simp, simp)
  apply (simp add: typ_heap_simps)
  apply (simp add: thread_state_lift_def)
  apply (simp add: sign_extend_def' mask_def nth_is_and_neq_0 word_bw_assocs
                   shift_over_ao_dists word_oa_dist word_and_max_simps)?
  apply (simp add: mask_shift_simps)?
  done'''],

    'ptr_set_spec_direct': [
        ptr_set_template('set_%(field)s', direct_ptr_name),
        '''  unfolding ptrval_def
  apply (rule allI, rule conseqPre, vcg)
  apply (clarsimp simp: guard_simps)
  apply (clarsimp simp add: packed_heap_update_collapse_hrs typ_heap_simps)?
  apply (rule exI, rule conjI[rotated], rule refl)
  apply (clarsimp simp: h_t_valid_clift_Some_iff %(name)s_lift_def typ_heap_simps)
  apply ((intro conjI sign_extend_eq)?;
         (simp add: mask_def shift_over_ao_dists multi_shift_simps word_size
                    word_ao_dist word_bw_assocs word_and_max_simps))?
  done'''],

    'ptr_set_spec_path': [
        ptr_set_template('set_%(field)s', path_ptr_name),
        '''  (* Invoke vcg *)
  unfolding ptrval_def
  apply (rule allI, rule conseqPre, vcg)
  apply (clarsimp)

  (* Infer h_t_valid for all three levels of indirection *)
  apply (frule h_t_valid_c_guard_cparent, simp, simp add: typ_uinfo_t_def)
  apply (frule h_t_valid_c_guard_field[where f="[''words_C'']"],
                                       simp, simp add: typ_uinfo_t_def)

  (* Discharge guards, including c_guard for pointers *)
  apply (simp add: h_t_valid_c_guard guard_simps)

  (* Lift field updates to bitfield struct updates *)
  apply (simp add: heap_update_field_hrs h_t_valid_c_guard typ_heap_simps)

  (* Collapse multiple updates *)
  apply (simp add: packed_heap_update_collapse_hrs)

  (* Instantiate the toplevel object *)
  apply (frule iffD1[OF h_t_valid_clift_Some_iff], rule exE, assumption, simp)

  (* Instantiate the next-level object in terms of the last *)
  apply (frule clift_subtype, simp+)

  (* Resolve pointer accesses *)
  apply (simp add: h_val_field_clift')

  (* Rewrite bitfield struct updates as enclosing struct updates *)
  apply (frule h_t_valid_c_guard)
  apply (simp add: parent_update_child)

  (* Equate the updated values *)
  apply (rule exI, rule conjI[rotated], simp add: h_val_clift')

  (* Rewrite struct updates *)
  apply (simp add: o_def %(name)s_lift_def)

  (* Solve bitwise arithmetic *)
  apply ((intro conjI sign_extend_eq)?;
         (simp add: mask_def shift_over_ao_dists multi_shift_simps word_size
                    word_ao_dist word_bw_assocs word_and_max_simps))?
  done'''],


    'get_tag_spec': [
        '''lemma (in ''' + loc_name + ''') %(name)s_get_%(tagname)s_spec:
  "\<forall>s. \<Gamma> \<turnstile> {s}
       \<acute>%(ret_name)s :== ''' \
    '''PROC %(name)s_get_%(tagname)s(\<acute>%(name)s)
       \<lbrace>\<acute>%(ret_name)s = ''' \
    '''%(name)s_get_tag \<^bsup>s\<^esup>%(name)s\<rbrace>"''',
        '''  apply(rule allI, rule conseqPre, vcg)
  apply (clarsimp)
  apply (simp add:%(name)s_get_tag_def mask_shift_simps guard_simps)
  done'''],

    'get_tag_equals_spec': [
        '''lemma (in ''' + loc_name + ''') %(name)s_%(tagname)s_equals_spec:
  "\<forall>s. \<Gamma> \<turnstile> {s}
       \<acute>ret__int :==
       PROC %(name)s_%(tagname)s_equals(\<acute>%(name)s, \<acute>%(name)s_type_tag)
       \<lbrace>\<acute>ret__int = of_bl [%(name)s_get_tag \<^bsup>s\<^esup>%(name)s = \<^bsup>s\<^esup>%(name)s_type_tag]\<rbrace>"''',
        '''  apply(rule allI, rule conseqPre, vcg)
  apply (clarsimp)
  apply (simp add:%(name)s_get_tag_eq_x mask_shift_simps guard_simps)
  done'''],

    'ptr_get_tag_spec_direct': [
        ptr_get_tag_template(direct_ptr_name),
        ''' unfolding ptrval_def
  apply (rule allI, rule conseqPre, vcg)
  apply (clarsimp simp:guard_simps)
  apply (frule h_t_valid_field[where f="[''words_C'']"], simp+)
  apply (frule iffD1[OF h_t_valid_clift_Some_iff], rule exE, assumption, simp)
  apply (simp add:h_val_clift' clift_field)
  apply (simp add:%(name)s_get_tag_def)
  apply (simp add:mask_shift_simps)?
  done'''],

    'ptr_get_tag_spec_path': [
        ptr_get_tag_template(path_ptr_name),
        ''' unfolding ptrval_def
  apply (rule allI, rule conseqPre, vcg)
  apply (clarsimp)
  apply (frule h_t_valid_c_guard_cparent, simp, simp add: typ_uinfo_t_def)
  apply (clarsimp simp: typ_heap_simps h_t_valid_clift_Some_iff)
  apply (frule clift_subtype, simp+)
  apply (simp add: %(name)s_get_tag_def mask_shift_simps guard_simps)
  done'''],


    'empty_union_new_spec': [
        '''lemma (in ''' + loc_name + ''') ''' \
        '''%(name)s_%(block)s_new_spec:
  "\<forall>s. \<Gamma> \<turnstile> {s}
       \<acute>ret__struct_%(name)s_C :== ''' \
    '''PROC %(name)s_%(block)s_new()
       \<lbrace>%(name)s_get_tag \<acute>ret__struct_%(name)s_C = ''' \
     '''scast %(name)s_%(block)s\<rbrace>"''',
        '''  apply(rule allI, rule conseqPre, vcg)
  by (clarsimp simp: guard_simps
                     %(name)s_lift_def
                     Let_def
                     %(name)s_get_tag_def
                     mask_shift_simps
                     %(name)s_tag_defs
                     word_of_int_hom_syms)'''],

    'union_new_spec': [
        '''lemma (in ''' + loc_name + ''') ''' \
        '''%(name)s_%(block)s_new_spec:
  "\<forall>s. \<Gamma> \<turnstile> {s}
       \<acute>ret__struct_%(name)s_C :== ''' \
    '''PROC %(name)s_%(block)s_new(%(args)s)
       \<lbrace>%(name)s_%(block)s_lift ''' \
    '''\<acute>ret__struct_%(name)s_C = \<lparr>
          %(field_eqs)s \<rparr> \<and>
        %(name)s_get_tag \<acute>ret__struct_%(name)s_C = ''' \
     '''scast %(name)s_%(block)s\<rbrace>"''',
        '''  apply (rule allI, rule conseqPre, vcg)
  apply (clarsimp simp: guard_simps o_def mask_def shift_over_ao_dists)
  apply (rule context_conjI[THEN iffD1[OF conj_commute]],
         fastforce simp: %(name)s_get_tag_eq_x %(name)s_%(block)s_def
                         mask_def shift_over_ao_dists word_bw_assocs word_ao_dist)
  apply (simp add: %(name)s_%(block)s_lift_def)
  apply (erule %(name)s_lift_%(block)s[THEN subst[OF sym]]; simp?)
  apply ((intro conjI sign_extend_eq)?;
         (simp add: mask_def shift_over_ao_dists multi_shift_simps word_size
                    word_ao_dist word_bw_assocs word_and_max_simps %(name)s_%(block)s_def))?
  done'''],

    'ptr_empty_union_new_spec_direct': [
        ptr_empty_union_new_template(direct_ptr_name),
        '''sorry (* ptr_empty_union_new_spec_direct *)'''],

    'ptr_empty_union_new_spec_path': [
        ptr_empty_union_new_template(path_ptr_name),
        '''  unfolding ptrval_def
   apply (rule allI, rule conseqPre, vcg)
   apply (clarsimp)
   apply (frule h_t_valid_c_guard_cparent, simp, simp add: typ_uinfo_t_def)
   apply (clarsimp simp: h_t_valid_clift_Some_iff)
   apply (frule clift_subtype, simp+)
   apply (clarsimp simp: typ_heap_simps c_guard_clift packed_heap_update_collapse_hrs)

   apply (simp add: parent_update_child[OF c_guard_clift] typ_heap_simps c_guard_clift)

   apply ((simp add: o_def)?, rule exI, rule conjI[OF _ refl])

   apply (simp add: %(name)s_get_tag_def %(name)s_tag_defs
                    guard_simps mask_shift_simps)
   done
'''],

    'ptr_union_new_spec_direct': [
        ptr_union_new_template(direct_ptr_name),
        '''sorry (* ptr_union_new_spec_direct *)'''],

    'ptr_union_new_spec_path': [
        ptr_union_new_template(path_ptr_name),
        '''  unfolding ptrval_def
  apply (rule allI, rule conseqPre, vcg)
  apply (clarsimp)
  apply (frule h_t_valid_c_guard_cparent, simp, simp add: typ_uinfo_t_def)
  apply (drule h_t_valid_clift_Some_iff[THEN iffD1], erule exE)
  apply (frule clift_subtype, simp, simp)
  apply (clarsimp simp: typ_heap_simps c_guard_clift
                        packed_heap_update_collapse_hrs)
  apply (simp add: guard_simps mask_shift_simps
                   %(name)s_tag_defs[THEN tag_eq_to_tag_masked_eq])?
  apply (simp add: parent_update_child[OF c_guard_clift]
                   typ_heap_simps c_guard_clift)
  apply (simp add: o_def %(name)s_%(block)s_lift_def)
  apply (simp only: %(name)s_lift_%(block)s cong: rev_conj_cong)
  apply (rule exI, rule conjI[rotated], rule conjI[OF _ refl])
   apply (simp_all add: %(name)s_get_tag_eq_x %(name)s_tag_defs mask_shift_simps)
  apply (intro conjI sign_extend_eq; simp add: mask_def word_ao_dist word_bw_assocs)?
  done'''],

    'union_get_spec': [
        '''lemma (in ''' + loc_name + ''') ''' \
        '''%(name)s_%(block)s_get_%(field)s_spec:
  "\<forall>s. \<Gamma> \<turnstile> ''' \
'''\<lbrace>s. %(name)s_get_tag \<acute>%(name)s = ''' \
        '''scast %(name)s_%(block)s\<rbrace>
       \<acute>%(ret_name)s :== ''' \
       '''PROC %(name)s_%(block)s_get_%(field)s(\<acute>%(name)s)
       \<lbrace>\<acute>%(ret_name)s = ''' \
       '''%(name)s_%(block)s_CL.%(field)s_CL ''' \
       '''(%(name)s_%(block)s_lift \<^bsup>s\<^esup>%(name)s)''' \
       '''\<rbrace>"''',
        '''  apply(rule allI, rule conseqPre, vcg)
  apply (clarsimp simp:guard_simps)
  apply (simp add:%(name)s_%(block)s_lift_def)
  apply (subst %(name)s_lift_%(block)s)
   apply (simp add: o_def
                    %(name)s_get_tag_def
                    %(name)s_%(block)s_def
                    mask_def word_size shift_over_ao_dists)
  apply (subst %(name)s_lift_%(block)s, simp)?
  apply (simp add: o_def
                   %(name)s_get_tag_def
                   %(name)s_%(block)s_def
                   mask_def word_size shift_over_ao_dists multi_shift_simps
                   word_bw_assocs word_oa_dist word_and_max_simps ucast_def
                   sign_extend_def' nth_is_and_neq_0)
  done'''],

    'union_set_spec': [
        '''lemma (in ''' + loc_name + ''') ''' \
        '''%(name)s_%(block)s_set_%(field)s_spec:
  "\<forall>s. \<Gamma> \<turnstile> ''' \
'''\<lbrace>s. %(name)s_get_tag \<acute>%(name)s = ''' \
        '''scast %(name)s_%(block)s\<rbrace>
       \<acute>ret__struct_%(name)s_C :== ''' \
    '''PROC %(name)s_%(block)s_set_%(field)s(\<acute>%(name)s, \<acute>v%(base)d)
       \<lbrace>%(name)s_%(block)s_lift \<acute>ret__struct_%(name)s_C = ''' \
    '''%(name)s_%(block)s_lift \<^bsup>s\<^esup>%(name)s \<lparr> ''' \
        '''%(name)s_%(block)s_CL.%(field)s_CL ''' \
        ''':= %(sign_extend)s (\<^bsup>s\<^esup>v%(base)d AND %(mask)s)\<rparr> \<and>
        %(name)s_get_tag \<acute>ret__struct_%(name)s_C = ''' \
     '''scast %(name)s_%(block)s\<rbrace>"''',
        '''  apply (rule allI, rule conseqPre, vcg)
  apply clarsimp
  apply (rule context_conjI[THEN iffD1[OF conj_commute]],
         fastforce simp: %(name)s_get_tag_eq_x %(name)s_lift_def %(name)s_tag_defs
                         mask_def shift_over_ao_dists multi_shift_simps word_size
                         word_ao_dist word_bw_assocs)
  apply (simp add: %(name)s_%(block)s_lift_def %(name)s_lift_def %(name)s_tag_defs)
  apply ((intro conjI sign_extend_eq)?;
         (simp add: mask_def shift_over_ao_dists multi_shift_simps word_size
                    word_ao_dist word_bw_assocs word_and_max_simps))?
  done'''],

    'ptr_union_get_spec_direct': [
        ptr_union_get_template(direct_ptr_name),
        ''' unfolding ptrval_def
  apply (rule allI, rule conseqPre, vcg)
  apply (clarsimp simp: typ_heap_simps h_t_valid_clift_Some_iff guard_simps
                        mask_shift_simps sign_extend_def' nth_is_and_neq_0
                        %(name)s_lift_%(block)s %(name)s_%(block)s_lift_def)
  done
'''],

    'ptr_union_get_spec_path': [
        ptr_union_get_template(path_ptr_name),
        '''unfolding ptrval_def
  apply (rule allI, rule conseqPre, vcg)
  apply (clarsimp)
  apply (frule h_t_valid_c_guard_cparent, simp, simp add: typ_uinfo_t_def)
  apply (drule h_t_valid_clift_Some_iff[THEN iffD1], erule exE)
  apply (frule clift_subtype, simp, simp)
  apply (clarsimp simp: typ_heap_simps c_guard_clift)
  apply (simp add: guard_simps mask_shift_simps)
  apply (simp add:%(name)s_%(block)s_lift_def)
  apply (subst %(name)s_lift_%(block)s)
  apply (simp add: mask_def)+
  done
  (* ptr_union_get_spec_path *)'''],

    'ptr_union_set_spec_direct': [
        ptr_union_set_template(direct_ptr_name),
        '''sorry (* ptr_union_set_spec_direct *)'''],


    'ptr_union_set_spec_path': [
        ptr_union_set_template(path_ptr_name),
        '''  unfolding ptrval_def
  apply (rule allI, rule conseqPre, vcg)
  apply (clarsimp)
  apply (frule h_t_valid_c_guard_cparent, simp, simp add: typ_uinfo_t_def)
  apply (drule h_t_valid_clift_Some_iff[THEN iffD1], erule exE)
  apply (frule clift_subtype, simp, simp)
  apply (clarsimp simp: typ_heap_simps c_guard_clift
                        packed_heap_update_collapse_hrs)
  apply (simp add: guard_simps mask_shift_simps
                   %(name)s_tag_defs[THEN tag_eq_to_tag_masked_eq])?
  apply (simp add: parent_update_child[OF c_guard_clift]
                   typ_heap_simps c_guard_clift)
  apply (simp add: o_def %(name)s_%(block)s_lift_def)
  apply (simp only: %(name)s_lift_%(block)s cong: rev_conj_cong)
  apply (rule exI, rule conjI[rotated], rule conjI[OF _ refl])
   apply (simp_all add: %(name)s_get_tag_eq_x %(name)s_tag_defs mask_shift_simps)
  apply (intro conjI sign_extend_eq; simp add: mask_def word_ao_dist word_bw_assocs)?
  done'''],

}


def make_proof(name, substs, sorry=False):
    result = proof_templates[name][0] % substs + '\n'

    if sorry:
        result += '\nsorry'
    else:
        result += proof_templates[name][1] % substs

    if len(proof_templates[name]) > 2:
        result += '\n' + '\n'.join(proof_templates[name][2:]) % substs

    return result

# AST objects


def emit_named(name, params, string):
    # Emit a named definition/proof, only when the given name is in
    # params.names

    if(name in params.names):
        print(string, file=params.output)
        print(file=params.output)

# This calculates substs for each proof, which is probably inefficient.  Meh


def emit_named_ptr_proof(fn_name, params, name, type_map, toptps, prf_prefix, substs):
    name_C = name + '_C'

    if name_C in type_map:
        toptp, path = type_map[name_C]

        substs['access_path'] = '(' + reduce(lambda x, y: y +
                                             ' (' + x + ')', ['the (ptrval s)'] + path) + ')'

        if len(path) == 0:
            substs['update_path'] = name
            emit_named(fn_name, params, make_proof(prf_prefix + '_direct', substs, params.sorry))
        else:
            substs['toptp'] = toptp
            # the split here gives us the field name (less any qualifiers) as the typ_heap
            # stuff doesn't use the qualifier
            substs['path'] = ', '.join(map(lambda x: "''%s''" % x.split('.')[-1], path))

            # The self.name here is the variable name (so not _C)
            path.reverse()
            substs['update_path'] = '(' + reduce(lambda x, y: y + '_update (' + x + ')',
                                                 ['\\<lambda>_. ' + name] + path) + '(the (ptrval s))' + ')'
            emit_named(fn_name, params, make_proof(prf_prefix + '_path', substs, params.sorry))


def field_mask_proof(base, base_bits, sign_extend, high, size):
    if high:
        if base_bits == base or sign_extend:
            # equivalent to below, but nicer in proofs
            return "NOT (mask %d)" % (base_bits - size)
        else:
            return "(mask %d << %d)" % (size, base_bits - size)
    else:
        return "mask %d" % size


def sign_extend_proof(high, base_bits, base_sign_extend):
    if high and base_sign_extend:
        return "sign_extend %d " % (base_bits - 1)
    else:
        return ""


def det_values(*dicts):
    """Deterministically iterate over the values of each dict in `dicts`."""
    def values(d):
        return (d[key] for key in sorted(d.keys()))
    return itertools.chain(*(values(d) for d in dicts))


def shiftr(n):
    """Shift right by possibly negative amount"""
    return f">> {n}" if n >= 0 else f"<< {-n}"


class TaggedUnion:
    def __init__(self, name, tagname, tag_slices, classes, tags):
        self.name = name
        if len(tag_slices) == 0:
            tag_slices = [tagname]
        self.tag_slices = tag_slices
        self.tagname = tagname
        self.constant_suffix = ''
        self.classes = dict(classes)
        self.tags = tags

    def resolve_tag_values(self):
        """Turn compound tag values into single tag values."""

        for name, value, ref in self.tags:
            if self.sliced_tag:
                if len(value) > 1:
                    if len(value) != len(self.tag_slices):
                        raise ValueError("Tag value for element %s of tagged union"
                                         "%s has incorrect number of parts" % (name, self.name))
                    compressed = 0
                    position = 0
                    for i, tag_slice in enumerate(self.tag_slices):
                        _, size, _ = ref.field_map[tag_slice]
                        if value[i] > 2 ** size - 1:
                            raise ValueError("Tag value %s for element %s of tagged union"
                                             "%s is too large for its field size" %
                                             (value[i], name, self.name))
                        compressed |= value[i] << position
                        position += size
                    value[0] = compressed
            else:
                if len(value) != 1:
                    raise ValueError("Tag value %s for element %s of tagged union"
                                     "%s must be a single value" % (value, name, self.name))

        self.tags = [(name, value[0], ref) for name, value, ref in self.tags]

        # Check for duplicate tags
        used_names = set()
        used_values = set()
        for name, value, _ in self.tags:
            if name in used_names:
                raise ValueError("Duplicate tag name %s" % name)
            if value in used_values:
                raise ValueError("Duplicate tag value %d" % value)

            used_names.add(name)
            used_values.add(value)

    def resolve(self, params, symtab):
        # Grab block references for tags
        self.tags = [(name, value, symtab[name]) for name, value in self.tags]

        self.sliced_tag = len(self.tag_slices) > 1

        if self.sliced_tag and self.classes:
            raise ValueError("Tagged union %s has both sliced tags and class masks." % self.name)

        if self.sliced_tag:
            self.record_tag_data()
        else:
            self.make_classes(params)

        self.resolve_tag_values()

        # Ensure that block sizes and tag size & position match for
        # all tags in the union
        union_base = None
        union_size = None
        for name, value, ref in self.tags:
            for tag_slice in self.tag_slices:
                _tag_offset, _tag_size, _tag_high = ref.field_map[tag_slice]

                if union_base is None:
                    union_base = ref.base
                elif union_base != ref.base:
                    raise ValueError("Base mismatch for element %s"
                                     " of tagged union %s" % (name, self.name))

                if union_size is None:
                    union_size = ref.size
                elif union_size != ref.size:
                    raise ValueError("Size mismatch for element %s"
                                     " of tagged union %s" % (name, self.name))

                if _tag_offset != self.tag_offset[tag_slice if self.sliced_tag else _tag_size]:
                    raise ValueError("Tag offset mismatch for element %s"
                                     " of tagged union %s" % (name, self.name))

                if self.sliced_tag:
                    if _tag_size != self.tag_size[tag_slice]:
                        raise ValueError("Tag size mismatch for element %s"
                                         " of tagged union %s" % (name, self.name))
                else:
                    self.assert_value_in_class(name, value, _tag_size)

                if _tag_high:
                    raise ValueError("Tag field is high-aligned for element %s"
                                     " of tagged union %s" % (name, self.name))

                # Flag block as belonging to a tagged union
                ref.tagged = True

        self.union_base = union_base
        self.union_size = union_size

    def set_base(self, base, base_bits, base_sign_extend, suffix):
        self.base = base
        self.multiple = self.union_size // base
        self.constant_suffix = suffix
        self.base_bits = base_bits
        self.base_sign_extend = base_sign_extend

        tag_index = None

        # This works for both tag classes and tag slices:
        for w in self.tag_offset:
            tag_offset = self.tag_offset[w]

            if tag_index is None:
                tag_index = tag_offset // base

            if (tag_offset // base) != tag_index:
                raise ValueError(
                    "The tag field of tagged union %s"
                    " is in a different word (%s) to the others (%s)."
                    % (self.name, hex(tag_offset // base), hex(tag_index)))

        self.tag_index = tag_index

        if self.sliced_tag:
            self.tag_mask = 0
            self.tag_offsets = []
            compressed_offset = 0
            for slice in self.tag_slices:
                size = self.tag_size[slice]
                offset = self.tag_offset[slice] % base
                self.tag_offsets += [(size, offset, compressed_offset)]
                compressed_offset += size
                self.tag_mask |= ((2 ** size) - 1) << offset
        else:
            self.tag_mask = None  # may depend on class

    def expanded_tag_val(self, compressed):
        """Expand a compressed tag value for use with the tag mask"""
        parts = [((compressed >> position) & ((1 << size) - 1)) << offset
                 for size, offset, position in self.tag_offsets]
        return reduce(lambda x, y: x | y, parts, 0)

    def generate_hol_proofs(self, params, type_map):
        output = params.output

        # Add fixed simp rule for struct
        print("lemmas %(name)s_C_words_C_fl_simp[simp] = "
              "%(name)s_C_words_C_fl[simplified]" %
              {"name": self.name}, file=output)
        print(file=output)

        # Generate struct field pointer proofs
        substs = {"name": self.name,
                  "words": self.multiple,
                  "base": self.base}

        print(make_proof('words_NULL_proof',
                         substs, params.sorry), file=output)
        print(file=output)

        print(make_proof('words_aligned_proof',
                         substs, params.sorry), file=output)
        print(file=output)

        print(make_proof('words_ptr_safe_proof',
                         substs, params.sorry), file=output)
        print(file=output)

        # Generate struct lemmas
        print(struct_lemmas_template % {"name": self.name},
              file=output)
        print(file=output)

        # Generate get_tag specs
        substs = {"name": self.name,
                  "tagname": self.tagname,
                  "ret_name": return_name(self.base)}

        if not params.skip_modifies:
            emit_named("%(name)s_get_%(tagname)s" % substs, params,
                       make_proof('const_modifies_proof',
                                  {"fun_name": "%(name)s_get_%(tagname)s" % substs,
                                   "args": ', '.join(["\<acute>ret__unsigned_long",
                                                      "\<acute>%(name)s" % substs])},
                                  params.sorry))
            emit_named("%(name)s_ptr_get_%(tagname)s" % substs, params,
                       make_proof('const_modifies_proof',
                                  {"fun_name": "%(name)s_ptr_get_%(tagname)s" % substs,
                                   "args": ', '.join(["\<acute>ret__unsigned_long",
                                                      "\<acute>%(name)s_ptr" % substs])},
                                  params.sorry))

        emit_named("%s_get_%s" % (self.name, self.tagname), params,
                   make_proof('get_tag_spec', substs, params.sorry))

        emit_named("%s_%s_equals" % (self.name, self.tagname), params,
                   make_proof('get_tag_equals_spec', substs, params.sorry))

        # Only generate ptr lemmas for those types reachable from top level types
        emit_named_ptr_proof("%s_ptr_get_%s" % (self.name, self.tagname), params, self.name,
                             type_map, params.toplevel_types,
                             'ptr_get_tag_spec', substs)

        for name, value, ref in self.tags:
            # Generate struct_new specs
            arg_list = ["\<acute>" + field
                        for field in ref.visible_order
                        if field not in self.tag_slices]

            # Generate modifies proof
            if not params.skip_modifies:
                emit_named("%s_%s_new" % (self.name, ref.name), params,
                           make_proof('const_modifies_proof',
                                      {"fun_name": "%s_%s_new" %
                                       (self.name, ref.name),
                                       "args": ', '.join([
                                           "\<acute>ret__struct_%(name)s_C" % substs] +
                                           arg_list)},
                                      params.sorry))

                emit_named("%s_%s_ptr_new" % (self.name, ref.name), params,
                           make_proof('ptr_set_modifies_proof',
                                      {"fun_name": "%s_%s_ptr_new" %
                                       (self.name, ref.name),
                                       "args": ', '.join([
                                           "\<acute>ret__struct_%(name)s_C" % substs] +
                                           arg_list)},
                                      params.sorry))

            if len(arg_list) == 0:
                # For an empty block:
                emit_named("%s_%s_new" % (self.name, ref.name), params,
                           make_proof('empty_union_new_spec',
                                      {"name": self.name,
                                       "block": ref.name},
                                      params.sorry))

                emit_named_ptr_proof("%s_%s_ptr_new" % (self.name, ref.name), params, self.name,
                                     type_map, params.toplevel_types,
                                     'ptr_empty_union_new_spec',
                                     {"name": self.name,
                                      "block": ref.name})
            else:
                field_eq_list = []
                for field in ref.visible_order:
                    _, size, high = ref.field_map[field]

                    if field in self.tag_slices:
                        continue

                    mask = field_mask_proof(self.base, self.base_bits,
                                            self.base_sign_extend, high, size)
                    sign_extend = sign_extend_proof(high, self.base_bits, self.base_sign_extend)
                    field_eq_list.append(
                        "%s_%s_CL.%s_CL = %s(\<^bsup>s\<^esup>%s AND %s)" %
                        (self.name, ref.name, field, sign_extend,
                            var_name(field, self.base), mask))
                field_eqs = ',\n          '.join(field_eq_list)

                emit_named("%s_%s_new" % (self.name, ref.name), params,
                           make_proof('union_new_spec',
                                      {"name": self.name,
                                       "block": ref.name,
                                       "args": ', '.join(arg_list),
                                       "field_eqs": field_eqs},
                                      params.sorry))

                emit_named_ptr_proof("%s_%s_ptr_new" % (self.name, ref.name), params, self.name,
                                     type_map, params.toplevel_types,
                                     'ptr_union_new_spec',
                                     {"name": self.name,
                                      "block": ref.name,
                                      "args": ', '.join(arg_list),
                                      "field_eqs": field_eqs})

            if self.sliced_tag:
                tag_mask_helpers = ""
            else:
                _, size, _ = ref.field_map[self.tagname]
                if any([w for w in self.widths if w < size]):
                    tag_mask_helpers = ("%s_%s_tag_mask_helpers"
                                        % (self.name, ref.name))
                else:
                    tag_mask_helpers = ""

            # Generate get/set specs
            for (field, _, size, high) in ref.fields:
                if field in self.tag_slices:
                    continue

                mask = field_mask_proof(self.base, self.base_bits,
                                        self.base_sign_extend, high, size)
                sign_extend = sign_extend_proof(high, self.base_bits, self.base_sign_extend)

                substs = {"name":  self.name,
                          "block": ref.name,
                          "field": field,
                          "mask":  mask,
                          "sign_extend": sign_extend,
                          "tag_mask_helpers": tag_mask_helpers,
                          "ret_name": return_name(self.base),
                          "base": self.base}

                # Get modifies spec
                if not params.skip_modifies:
                    emit_named("%s_%s_get_%s" % (self.name, ref.name, field),
                               params,
                               make_proof('const_modifies_proof',
                                          {"fun_name": "%s_%s_get_%s" %
                                           (self.name, ref.name, field),
                                              "args": ', '.join([
                                                  "\<acute>ret__unsigned_long",
                                                  "\<acute>%s" % self.name])},
                                          params.sorry))

                    emit_named("%s_%s_ptr_get_%s" % (self.name, ref.name, field),
                               params,
                               make_proof('const_modifies_proof',
                                          {"fun_name": "%s_%s_ptr_get_%s" %
                                           (self.name, ref.name, field),
                                              "args": ', '.join([
                                                  "\<acute>ret__unsigned_long",
                                                  "\<acute>%s_ptr" % self.name])},
                                          params.sorry))

                # Get spec
                emit_named("%s_%s_get_%s" % (self.name, ref.name, field),
                           params,
                           make_proof('union_get_spec', substs, params.sorry))

                # Set modifies spec
                if not params.skip_modifies:
                    emit_named("%s_%s_set_%s" % (self.name, ref.name, field),
                               params,
                               make_proof('const_modifies_proof',
                                          {"fun_name": "%s_%s_set_%s" %
                                           (self.name, ref.name, field),
                                              "args": ', '.join([
                                                  "\<acute>ret__struct_%s_C" % self.name,
                                                  "\<acute>%s" % self.name,
                                                  "\<acute>v%(base)d"])},
                                          params.sorry))

                    emit_named("%s_%s_ptr_set_%s" % (self.name, ref.name, field),
                               params,
                               make_proof('ptr_set_modifies_proof',
                                          {"fun_name": "%s_%s_ptr_set_%s" %
                                           (self.name, ref.name, field),
                                              "args": ', '.join([
                                                  "\<acute>%s_ptr" % self.name,
                                                  "\<acute>v%(base)d"])},
                                          params.sorry))

                # Set spec
                emit_named("%s_%s_set_%s" % (self.name, ref.name, field),
                           params,
                           make_proof('union_set_spec', substs, params.sorry))

                # Pointer get spec
                emit_named_ptr_proof("%s_%s_ptr_get_%s" % (self.name, ref.name, field),
                                     params, self.name, type_map, params.toplevel_types,
                                     'ptr_union_get_spec', substs)

                # Pointer set spec
                emit_named_ptr_proof("%s_%s_ptr_set_%s" % (self.name, ref.name, field),
                                     params, self.name, type_map, params.toplevel_types,
                                     'ptr_union_set_spec', substs)

    def generate_hol_defs(self, params):
        output = params.output

        empty_blocks = {}

        def gen_name(ref_name, capitalise=False):
            # Create datatype generator/type name for a block
            if capitalise:
                return "%s_%s" % \
                       (self.name[0].upper() + self.name[1:], ref_name)
            else:
                return "%s_%s" % (self.name, ref_name)

        # Generate block records with tag field removed
        for name, value, ref in self.tags:
            if ref.generate_hol_defs(params,
                                     suppressed_fields=self.tag_slices,
                                     prefix="%s_" % self.name,
                                     in_union=True):
                empty_blocks[ref] = True

        constructor_exprs = []
        for name, value, ref in self.tags:
            if ref in empty_blocks:
                constructor_exprs.append(gen_name(name, True))
            else:
                constructor_exprs.append("%s %s_CL" %
                                         (gen_name(name, True), gen_name(name)))

        print("datatype %s_CL =\n    %s\n" %
              (self.name, '\n  | '.join(constructor_exprs)),
              file=output)

        # Generate get_tag definition
        subs = {"name":      self.name,
                "base":      self.base}

        if self.sliced_tag:
            slice_subs = dict(subs, tag_mask=self.tag_mask, tag_index=self.tag_index)

        if self.sliced_tag:
            fs = (union_get_tag_def_header_template % subs
                  + self.compressed_tag_expr(
                      '(index (%(name)s_C.words_C %(name)s) %(tag_index)d)' % slice_subs,
                      code=False
                  )
                  + union_get_tag_def_footer_template % subs)
        else:
            templates = ([union_get_tag_def_entry_template] * (len(self.widths) - 1)
                         + [union_get_tag_def_final_template])

            fs = (union_get_tag_def_header_template % subs
                  + "".join([template %
                             dict(subs,
                                  tag_size=width,
                                  classmask=self.word_classmask(width),
                                  tag_index=self.tag_index,
                                  tag_shift=self.tag_offset[width] % self.base)
                             for template, width in zip(templates, self.widths)])
                  + union_get_tag_def_footer_template % subs)

        print(fs, file=output)
        print(file=output)

        # Generate get_tag_eq_x lemma
        if self.sliced_tag:
            fs = (union_get_tag_eq_x_def_header_template % subs
                  + self.compressed_tag_expr(
                      '(index (%(name)s_C.words_C c) %(tag_index)d)' % slice_subs,
                      code=False
                  )
                  + union_get_tag_eq_x_def_footer_template % subs)
        else:
            templates = ([union_get_tag_eq_x_def_entry_template]
                         * (len(self.widths) - 1)
                         + [union_get_tag_eq_x_def_final_template])

            fs = (union_get_tag_eq_x_def_header_template % subs
                  + "".join([template %
                             dict(subs,
                                  tag_size=width,
                                  classmask=self.word_classmask(width),
                                  tag_index=self.tag_offset[width] // self.base,
                                  tag_shift=self.tag_offset[width] % self.base)
                             for template, width in zip(templates, self.widths)])
                  + union_get_tag_eq_x_def_footer_template % subs)

        print(fs, file=output)
        print(file=output)

        # Generate mask helper lemmas

        if not self.sliced_tag:
            for name, value, ref in self.tags:
                offset, size, _ = ref.field_map[self.tagname]
                part_widths = [w for w in self.widths if w < size]
                if part_widths:
                    subs = {"name":         self.name,
                            "block":        name,
                            "full_mask":    hex(2 ** size - 1),
                            "full_value":   hex(value)}

                    fs = (union_tag_mask_helpers_header_template % subs
                          + "".join([union_tag_mask_helpers_entry_template %
                                     dict(subs, part_mask=hex(2 ** pw - 1),
                                          part_value=hex(value & (2 ** pw - 1)))
                                     for pw in part_widths])
                          + union_tag_mask_helpers_footer_template)

                    print(fs, file=output)
                    print(file=output)

        # Generate lift definition
        collapse_proofs = ""
        tag_cases = []
        for name, value, ref in self.tags:
            field_inits = []

            for field in ref.visible_order:
                offset, size, high = ref.field_map[field]

                if field in self.tag_slices:
                    continue

                index = offset // self.base
                sign_extend = ""

                if high:
                    shift_op = "<<"
                    shift = self.base_bits - size - (offset % self.base)
                    if shift < 0:
                        shift = -shift
                        shift_op = ">>"
                    if self.base_sign_extend:
                        sign_extend = "sign_extend %d " % (self.base_bits - 1)
                else:
                    shift_op = ">>"
                    shift = offset % self.base

                initialiser = \
                    "%s_CL.%s_CL = %s(((index (%s_C.words_C %s) %d) %s %d)" % \
                    (gen_name(name), field, sign_extend, self.name, self.name,
                     index, shift_op, shift)

                if size < self.base:
                    mask = field_mask_proof(self.base, self.base_bits,
                                            self.base_sign_extend, high, size)
                    initialiser += " AND " + mask

                field_inits.append("\n       " + initialiser + ")")

            if len(field_inits) == 0:
                value = gen_name(name, True)
            else:
                value = "%s \<lparr> %s \<rparr>" % \
                    (gen_name(name, True), ','.join(field_inits))

            tag_cases.append("if tag = scast %s then Some (%s)" %
                             (gen_name(name), value))

            collapse_proofs += \
                make_proof("lift_collapse_proof",
                           {"name": self.name,
                            "block": name,
                            "value": value},
                           params.sorry)
            collapse_proofs += "\n\n"

        print(union_lift_def_template %
              {"name": self.name,
               "tag_cases": '\n     else '.join(tag_cases)},
              file=output)
        print(file=output)

        print(collapse_proofs, file=output)

        block_lift_lemmas = "lemmas %s_lifts =\n" % self.name
        # Generate lifted access/update definitions, and abstract lifters
        for name, value, ref in self.tags:
            # Don't generate accessors if the block (minus tag) is empty
            if ref in empty_blocks:
                continue

            substs = {"union": self.name,
                      "block": name,
                      "generator": gen_name(name, True)}

            for t in [union_access_def_template, union_update_def_template]:
                print(t % substs, file=output)
                print(file=output)

            print(block_lift_def_template % substs, file=output)
            print(file=output)

            print(block_lift_lemma_template % substs, file=output)
            print(file=output)

            block_lift_lemmas += "\t%(union)s_%(block)s_lift\n" % substs

        print(block_lift_lemmas, file=output)
        print(file=output)

    def compressed_tag_expr(self, source, code=True):
        def mask(size):
            return f"0x{(1 << size) - 1:x}{self.constant_suffix}" if code else f"mask {size}"
        bit_or = "|" if code else "OR"
        bit_and = "&" if code else "AND"
        return f"\n        {bit_or} ".join([
            f"(({source} {bit_and} ({mask(size)} << {offset})) {shiftr(offset-position)})"
            for size, offset, position in self.tag_offsets])

    def generate(self, params):
        output = params.output

        # Generate typedef
        print(typedef_template %
              {"type": TYPES[options.environment][self.base],
               "name": self.name,
               "multiple": self.multiple}, file=output)
        print(file=output)

        # Generate tag enum
        print("enum %s_tag {" % self.name, file=output)
        if len(self.tags) > 0:
            for name, value, ref in self.tags[:-1]:
                print("    %s_%s = %d," % (self.name, name, value),
                      file=output)
            name, value, ref = self.tags[-1]
            print("    %s_%s = %d" % (self.name, name, value),
                  file=output)
        print("};", file=output)
        print("typedef enum %s_tag %s_tag_t;" %
              (self.name, self.name), file=output)
        print(file=output)

        subs = {
            'inline': INLINE[options.environment],
            'union': self.name,
            'type':  TYPES[options.environment][self.union_base],
            'tagname': self.tagname,
            'suf': self.constant_suffix}

        # Generate tag reader
        if self.sliced_tag:
            fs = (tag_reader_header_template % subs
                  + "    return "
                  + self.compressed_tag_expr(f"{self.name}.words[{self.tag_index}]")
                  + ";"
                  + tag_reader_footer_template % subs)
        else:
            templates = ([tag_reader_entry_template] * (len(self.widths) - 1)
                         + [tag_reader_final_template])

            fs = (tag_reader_header_template % subs
                  + "".join([template %
                             dict(subs,
                                  mask=2 ** width - 1,
                                  classmask=self.word_classmask(width),
                                  index=self.tag_offset[width] // self.base,
                                  shift=self.tag_offset[width] % self.base)
                             for template, width in zip(templates, self.widths)])
                  + tag_reader_footer_template % subs)

        emit_named("%s_get_%s" % (self.name, self.tagname), params, fs)

        # Generate tag eq reader
        if self.sliced_tag:
            fs = (tag_eq_reader_header_template % subs
                  + "    return ("
                  + self.compressed_tag_expr(f"{self.name}.words[{self.tag_index}]")
                  + ") == ex_type_tag;"
                  + tag_eq_reader_footer_template % subs)
        else:
            templates = ([tag_eq_reader_entry_template] * (len(self.widths) - 1)
                         + [tag_eq_reader_final_template])

            fs = (tag_eq_reader_header_template % subs
                  + "".join([template %
                             dict(subs,
                                  mask=2 ** width - 1,
                                  classmask=self.word_classmask(width),
                                  index=self.tag_offset[width] // self.base,
                                  shift=self.tag_offset[width] % self.base)
                             for template, width in zip(templates, self.widths)])
                  + tag_eq_reader_footer_template % subs)

        emit_named("%s_%s_equals" % (self.name, self.tagname), params, fs)

        # Generate pointer lifted tag reader
        if self.sliced_tag:
            fs = (ptr_tag_reader_header_template % subs
                  + "    return "
                  + self.compressed_tag_expr(f"{self.name}_ptr->words[{self.tag_index}]")
                  + ";"
                  + ptr_tag_reader_footer_template % subs)
        else:
            templates = ([ptr_tag_reader_entry_template] * (len(self.widths) - 1)
                         + [ptr_tag_reader_final_template])

            fs = (ptr_tag_reader_header_template % subs
                  + "".join([template %
                             dict(subs,
                                  mask=2 ** width - 1,
                                  classmask=self.word_classmask(width),
                                  index=self.tag_offset[width] // self.base,
                                  shift=self.tag_offset[width] % self.base)
                             for template, width in zip(templates, self.widths)])
                  + ptr_tag_reader_footer_template % subs)

        emit_named("%s_ptr_get_%s" % (self.name, self.tagname), params, fs)

        suf = self.constant_suffix

        for name, value, ref in self.tags:
            # Generate generators
            param_fields = [field for field in ref.visible_order if field not in self.tag_slices]
            param_list = ["%s %s" % (TYPES[options.environment][self.base], field)
                          for field in param_fields]

            if len(param_list) == 0:
                gen_params = 'void'
            else:
                gen_params = ', '.join(param_list)

            ptr_params = ', '.join(["%s_t *%s_ptr" % (self.name, self.name)] + param_list)

            field_updates = {word: [] for word in range(self.multiple)}
            field_asserts = ["    /* fail if user has passed bits that we will override */"]

            for field in ref.visible_order + ([self.tagname] if self.sliced_tag else []):

                if field == self.tagname and self.sliced_tag:
                    f_value = f"0x{self.expanded_tag_val(value):x}{suf}"
                    offset, size, high = 0, self.base, False
                elif field == self.tagname and not self.sliced_tag:
                    f_value = "(%s)%s_%s" % (TYPES[options.environment][self.base], self.name, name)
                    offset, size, high = ref.field_map[field]
                elif field not in self.tag_slices:
                    f_value = field
                    offset, size, high = ref.field_map[field]
                else:
                    continue

                index = offset // self.base
                if high:
                    shift_op = ">>"
                    shift = self.base_bits - size - (offset % self.base)
                    if self.base_sign_extend:
                        high_bits = ((self.base_sign_extend << (
                            self.base - self.base_bits)) - 1) << self.base_bits
                    else:
                        high_bits = 0
                    if shift < 0:
                        shift = -shift
                        shift_op = "<<"
                else:
                    shift_op = "<<"
                    shift = offset % self.base
                    high_bits = 0
                if size < self.base:
                    if high:
                        mask = ((1 << size) - 1) << (self.base_bits - size)
                    else:
                        mask = (1 << size) - 1

                    field_asserts.append(
                        "    %s((%s & ~0x%x%s) == ((%d && (%s & (1%s << %d))) ? 0x%x : 0));"
                        % (ASSERTS[options.environment], f_value, mask, suf, self.base_sign_extend,
                           f_value, suf, self.base_bits - 1, high_bits))

                    field_updates[index].append(
                        "(%s & 0x%x%s) %s %d" % (f_value, mask, suf, shift_op, shift))
                else:
                    field_updates[index].append("%s %s %d" % (f_value, shift_op, shift))

            word_inits = [
                ("words[%d] = 0" % index) + ''.join(["\n        | %s" % up for up in ups]) + ';'
                for (index, ups) in field_updates.items()]

            def mk_inits(prefix):
                return '\n'.join(["    %s%s" % (prefix, word_init) for word_init in word_inits])

            print_params = {
                "inline":     INLINE[options.environment],
                "block":      name,
                "union":      self.name,
                "gen_params": gen_params,
                "ptr_params": ptr_params,
                "gen_inits":  mk_inits("%s." % self.name),
                "ptr_inits":  mk_inits("%s_ptr->" % self.name),
                "asserts": '  \n'.join(field_asserts)
            }

            generator = union_generator_template % print_params
            ptr_generator = ptr_union_generator_template % print_params

            emit_named("%s_%s_new" % (self.name, name), params, generator)
            emit_named("%s_%s_ptr_new" % (self.name, name), params, ptr_generator)

            # Generate field readers/writers
            if self.sliced_tag:
                tagshift = 0
                tagindex = self.tag_index
                tagmask = self.tag_mask
                tagvalue = f"0x{self.expanded_tag_val(value):x}{suf}"
            else:
                tagnameoffset, tagnamesize, _ = ref.field_map[self.tagname]
                tagindex = self.tag_index
                tagshift = tagnameoffset % self.base
                tagmask = (2 ** tagnamesize) - 1
                tagvalue = self.name + "_" + ref.name

            for field, offset, size, high in ref.fields:
                # Don't duplicate tag accessors
                if field in self.tag_slices:
                    continue

                index = offset // self.base
                if high:
                    write_shift = ">>"
                    read_shift = "<<"
                    shift = self.base_bits - size - (offset % self.base)
                    if shift < 0:
                        shift = -shift
                        write_shift = "<<"
                        read_shift = ">>"
                    if self.base_sign_extend:
                        high_bits = ((self.base_sign_extend << (
                            self.base - self.base_bits)) - 1) << self.base_bits
                    else:
                        high_bits = 0
                else:
                    write_shift = "<<"
                    read_shift = ">>"
                    shift = offset % self.base
                    high_bits = 0
                mask = ((1 << size) - 1) << (offset % self.base)

                subs = {
                    "inline": INLINE[options.environment],
                    "block": ref.name,
                    "field": field,
                    "type": TYPES[options.environment][ref.base],
                    "assert": ASSERTS[options.environment],
                    "index": index,
                    "shift": shift,
                    "r_shift_op": read_shift,
                    "w_shift_op": write_shift,
                    "mask": mask,
                    "tagindex": tagindex,
                    "tagshift": tagshift,
                    "tagmask": tagmask,
                    "tagvalue": tagvalue,
                    "union": self.name,
                    "suf": self.constant_suffix,
                    "high_bits": high_bits,
                    "sign_extend": self.base_sign_extend and high,
                    "extend_bit": self.base_bits - 1,
                    "base": self.base}

                # Reader
                emit_named("%s_%s_get_%s" % (self.name, ref.name, field),
                           params, union_reader_template % subs)

                # Writer
                emit_named("%s_%s_set_%s" % (self.name, ref.name, field),
                           params, union_writer_template % subs)

                # Pointer lifted reader
                emit_named("%s_%s_ptr_get_%s" % (self.name, ref.name, field),
                           params, ptr_union_reader_template % subs)

                # Pointer lifted writer
                emit_named("%s_%s_ptr_set_%s" % (self.name, ref.name, field),
                           params, ptr_union_writer_template % subs)

    def make_names(self):
        "Return the set of candidate function names for a union"

        substs = {"union": self.name,
                  "tagname": self.tagname}
        names = [t % substs for t in [
            "%(union)s_get_%(tagname)s",
            "%(union)s_ptr_get_%(tagname)s",
            "%(union)s_%(tagname)s_equals"]]

        for name, value, ref in self.tags:
            names += ref.make_names(self)

        return names

    def represent_value(self, value, width):
        max_width = max(self.classes.keys())

        tail_str = ("{:0{}b}".format(value, width)
                    + "_" * (self.tag_offset[width] - self.class_offset))
        head_str = "_" * ((max_width + self.tag_offset[max_width]
                           - self.class_offset) - len(tail_str))

        return head_str + tail_str

    def represent_class(self, width):
        max_width = max(self.classes.keys())

        cmask = self.classes[width]
        return ("{:0{}b}".format(cmask, max_width).replace("0", "-")
                + " ({:#x})".format(cmask))

    def represent_field(self, width):
        max_width = max(self.classes.keys())
        offset = self.tag_offset[width] - self.class_offset

        return ("{:0{}b}".format((2 ** width - 1) << offset, max_width)
                .replace("0", "-").replace("1", "#"))

    def assert_value_in_class(self, name, value, width):
        max_width = max(self.classes.keys())
        ovalue = value << self.tag_offset[width]
        cvalue = value << (self.tag_offset[width] - self.class_offset)

        offset_field = (2 ** width - 1) << self.tag_offset[width]
        if (ovalue | offset_field) != offset_field:
            raise ValueError(
                "The value for element %s of tagged union %s,\n"
                "    %s\nexceeds the field bounds\n"
                "    %s."
                % (name, self.name,
                   self.represent_value(value, width),
                   self.represent_field(width)))

        for w, mask in [(lw, self.classes[lw])
                        for lw in self.widths if lw < width]:
            if (cvalue & mask) != mask:
                raise ValueError(
                    "The value for element %s of tagged union %s,\n"
                    "    %s\nis invalid: it has %d bits but fails"
                    " to match the earlier mask at %d bits,\n"
                    "    %s."
                    % (name, self.name,
                       self.represent_value(value, width),
                       width, w, self.represent_class(w)))

        if (self.widths.index(width) + 1 < len(self.widths) and
                (cvalue & self.classes[width]) == self.classes[width]):
            raise ValueError(
                "The value for element %s of tagged union %s,\n"
                "    %s (%d/%s)\nis invalid: it must not match the "
                "mask for %d bits,\n    %s."
                % (name, self.name,
                   ("{:0%db}" % width).format(cvalue),
                   value, hex(value),
                   width,
                   self.represent_class(width)))

    def word_classmask(self, width):
        "Return a class mask for testing a whole word, i.e., one."
        "that is positioned absolutely relative to the lsb of the"
        "relevant word."

        return (self.classes[width] << (self.class_offset % self.base))

    def record_tag_data(self):
        "Record size and offset of all tag slices"

        # Assuming tag slices are at same position and size in all blocks, we
        # can use any tag (e.g. the first one) to read out size and offset.
        # (This assumption is checked later).

        _, _, ref = self.tags[0]

        self.tag_offset = {}
        self.tag_size = {}

        for tag_slice in self.tag_slices:
            offset, size, _ = ref.field_map[tag_slice]
            self.tag_offset[tag_slice] = offset
            self.tag_size[tag_slice] = size

        self.widths = []

    def make_classes(self, params):
        "Calculate an encoding for variable width tagnames"

        # Check self.classes, which maps from the bit width of tagname in a
        # particular block to a classmask that identifies when a value belongs
        # to wider tagname.
        #
        # For example, given three possible field widths -- 4, 8, and 12 bits --
        # one possible encoding is:
        #
        #                       * * _ _     (** != 11)
        #             0 _ _ _   1 1 _ _
        #   _ _ _ _   1 _ _ _   1 1 _ _
        #
        # where the 3rd and 4th lsbs signify whether the field should be
        # interpreted using a 4-bit mask (if 00, 01, or 10) or as an 8 or 16 bit
        # mask (if 11). And, in the latter case, the 8th lsb signifies whether
        # to intrepret it as an 8 bit field (if 0) or a 16 bit field (if 1).
        #
        # In this example we have:
        #   4-bit class:  classmask = 0b00001100
        #   8-bit class:  classmask = 0b10001100
        #  16-bit class:  classmask = 0b10001100
        #
        # More generally, the fields need not all start at the same offset
        # (measured "left" from the lsb), for example:
        #
        #    ...# ###. .... ....       4-bit field at offset 9
        #    ..## #### ##.. ....       8-bit field at offset 6
        #    #### #### #### ....      12-bit field at offset 4
        #
        # In this case, the class_offset is the minimum offset (here 4).
        # Classmasks are declared relative to the field, but converted
        # internally to be relative to the class_offset; tag_offsets
        # are absolute (relative to the lsb); values are relative to
        # the tag_offset (i.e., within the field). for example:
        #
        #    ...1 100. ....    4-bit class: classmask=0xc   tag_offset=9
        #    ..01 1000 10..    8-bit class: classmask=0x62  tag_offset=6
        #    0001 1000 1000   16-bit class: classmask=0x188 tag_offset=4

        used = set()
        self.tag_offset = {}
        for name, _, ref in self.tags:
            offset, size, _ = ref.field_map[self.tagname]
            used.add(size)
            self.tag_offset[size] = offset

        self.class_offset = min(self.tag_offset.values())

        # internally, classmasks are relative to the class_offset, so
        # that we can compare them to each other.
        for w in self.classes:
            self.classes[w] <<= self.tag_offset[w] - self.class_offset

        used_widths = sorted(list(used))
        assert(len(used_widths) > 0)

        if not self.classes:
            self.classes = {used_widths[0]: 0}

        # sanity checks on classes
        classes = self.classes
        widths = sorted(self.classes.keys())
        context = "masks for %s.%s" % (self.name, self.tagname)
        class_offset = self.class_offset

        for uw in used_widths:
            if uw not in classes:
                raise ValueError("%s: none defined for a field of %d bits."
                                 % (context, uw))

        for mw in classes.keys():
            if mw not in used_widths:
                raise ValueError(
                    "%s: there is a mask with %d bits but no corresponding fields."
                    % (context, mw))

        for w in widths:
            offset_field = (2 ** w - 1) << self.tag_offset[w]
            if (classes[w] << class_offset) | offset_field != offset_field:
                raise ValueError(
                    "{:s}: the mask for {:d} bits:\n  {:s}\n"
                    "exceeds the field bounds:\n  {:s}."
                    .format(context, w, self.represent_class(w),
                            self.represent_field(w)))

        if len(widths) > 1 and classes[widths[0]] == 0:
            raise ValueError("%s: the first (width %d) is zero." % (
                context, widths[0]))

        if any([classes[widths[i-1]] == classes[widths[i]]
                for i in range(1, len(widths) - 1)]):
            raise ValueError("%s: there is a non-final duplicate!" % context)

        # smaller masks are included within larger masks
        pre_mask = None
        pre_width = None
        for w in widths:
            if pre_mask is not None and (classes[w] & pre_mask) != pre_mask:
                raise ValueError(
                    "{:s}: the mask\n  0b{:b} for width {:d} does not include "
                    "the mask\n  0b{:b} for width {:d}.".format(
                        context, classes[w], w, pre_mask, pre_width))
            pre_width = w
            pre_mask = classes[w]

        if params.showclasses:
            print("-----%s.%s" % (self.name, self.tagname), file=sys.stderr)
            for w in widths:
                print("{:2d} = {:s}".format(w, self.represent_class(w)),
                      file=sys.stderr)

        self.widths = widths


class Block:
    def __init__(self, name, fields, visible_order):
        offset = 0
        _fields = []
        self.size = sum(size for _name, size, _high in fields)
        offset = self.size
        self.constant_suffix = ''

        if visible_order is None:
            self.visible_order = []

        for _name, _size, _high in fields:
            offset -= _size
            if not _name is None:
                if visible_order is None:
                    self.visible_order.append(_name)
                _fields.append((_name, offset, _size, _high))

        self.name = name
        self.tagged = False
        self.fields = _fields
        self.field_map = dict((name, (offset, size, high))
                              for name, offset, size, high in _fields)

        if not visible_order is None:
            missed_fields = set(self.field_map.keys())

            for _name in visible_order:
                if _name not in self.field_map:
                    raise ValueError("Nonexistent field '%s' in visible_order"
                                     % _name)
                missed_fields.remove(_name)

            if len(missed_fields) > 0:
                raise ValueError("Fields %s missing from visible_order" %
                                 str([x for x in missed_fields]))

            self.visible_order = visible_order

    def set_base(self, base, base_bits, base_sign_extend, suffix):
        self.base = base
        self.constant_suffix = suffix
        self.base_bits = base_bits
        self.base_sign_extend = base_sign_extend
        if self.size % base != 0:
            raise ValueError("Size of block %s not a multiple of base"
                             % self.name)
        self.multiple = self.size // base
        for name, offset, size, high in self.fields:
            if offset // base != (offset+size-1) // base:
                raise ValueError("Field %s of block %s "
                                 "crosses a word boundary"
                                 % (name, self.name))

    def generate_hol_defs(self, params, suppressed_fields=[],
                          prefix="", in_union=False):
        output = params.output

        # Don't generate raw records for blocks in tagged unions
        if self.tagged and not in_union:
            return

        _name = prefix + self.name

        # Generate record def
        out = "record %s_CL =\n" % _name

        empty = True

        for field in self.visible_order:
            if field in suppressed_fields:
                continue

            empty = False

            out += '    %s_CL :: "word%d"\n' % (field, self.base)

        if not empty:
            print(out, file=output)

        # Generate lift definition
        if not in_union:
            field_inits = []

            for name in self.visible_order:
                offset, size, high = self.field_map[name]

                index = offset // self.base
                sign_extend = ""

                if high:
                    shift_op = "<<"
                    shift = self.base_bits - size - (offset % self.base)
                    if shift < 0:
                        shift = -shift
                        shift_op = ">>"
                    if self.base_sign_extend:
                        sign_extend = "sign_extend %d " % (self.base_bits - 1)
                else:
                    shift_op = ">>"
                    shift = offset % self.base

                initialiser = \
                    "%s_CL.%s_CL = %s(((index (%s_C.words_C %s) %d) %s %d)" % \
                    (self.name, name, sign_extend, self.name, self.name,
                     index, shift_op, shift)

                if size < self.base:
                    mask = field_mask_proof(self.base, self.base_bits,
                                            self.base_sign_extend, high, size)

                    initialiser += " AND " + mask

                field_inits.append(initialiser + ")")

            print(lift_def_template %
                  {"name": self.name,
                   "fields": ',\n       '.join(field_inits)},
                  file=output)
            print(file=output)

        return empty

    def generate_hol_proofs(self, params, type_map):
        output = params.output

        if self.tagged:
            return

        # Add fixed simp rule for struct
        print("lemmas %(name)s_C_words_C_fl_simp[simp] = "
              "%(name)s_C_words_C_fl[simplified]" %
              {"name": self.name}, file=output)
        print(file=output)

        # Generate struct field pointer proofs
        substs = {"name": self.name,
                  "words": self.multiple,
                  "base": self.base}

        print(make_proof('words_NULL_proof',
                         substs, params.sorry), file=output)
        print(file=output)

        print(make_proof('words_aligned_proof',
                         substs, params.sorry), file=output)
        print(file=output)

        print(make_proof('words_ptr_safe_proof',
                         substs, params.sorry), file=output)
        print(file=output)

        # Generate struct lemmas
        print(struct_lemmas_template % {"name": self.name},
              file=output)
        print(file=output)

        # Generate struct_new specs
        arg_list = ["\<acute>" + field for field in self.visible_order]

        if not params.skip_modifies:
            emit_named("%s_new" % self.name, params,
                       make_proof('const_modifies_proof',
                                  {"fun_name": "%s_new" % self.name,
                                   "args": ', '.join(["\<acute>ret__struct_%s_C" %
                                                      self.name] +
                                                     arg_list)},
                                  params.sorry))
            # FIXME: ptr_new (doesn't seem to be used)

        field_eq_list = []
        for field in self.visible_order:
            offset, size, high = self.field_map[field]
            mask = field_mask_proof(self.base, self.base_bits, self.base_sign_extend, high, size)
            sign_extend = sign_extend_proof(high, self.base_bits, self.base_sign_extend)

            field_eq_list.append("%s_CL.%s_CL = %s(\<^bsup>s\<^esup>%s AND %s)" %
                                 (self.name, field, sign_extend, field, mask))
        field_eqs = ',\n          '.join(field_eq_list)

        emit_named("%s_new" % self.name, params,
                   make_proof('new_spec',
                              {"name": self.name,
                               "args": ', '.join(arg_list),
                               "field_eqs": field_eqs},
                              params.sorry))

        emit_named_ptr_proof("%s_ptr_new" % self.name, params, self.name,
                             type_map, params.toplevel_types,
                             'ptr_new_spec',
                             {"name": self.name,
                              "args": ', '.join(arg_list),
                              "field_eqs": field_eqs})

        # Generate get/set specs
        for (field, offset, size, high) in self.fields:
            mask = field_mask_proof(self.base, self.base_bits, self.base_sign_extend, high, size)
            sign_extend = sign_extend_proof(high, self.base_bits, self.base_sign_extend)

            substs = {"name": self.name,
                      "field": field,
                      "mask": mask,
                      "sign_extend": sign_extend,
                      "ret_name": return_name(self.base),
                      "base": self.base}

            if not params.skip_modifies:
                # Get modifies spec
                emit_named("%s_get_%s" % (self.name, field), params,
                           make_proof('const_modifies_proof',
                                      {"fun_name": "%s_get_%s" % (self.name, field),
                                       "args": ', '.join([
                                           "\<acute>ret__unsigned_long",
                                           "\<acute>%s" % self.name])},
                                      params.sorry))

                # Ptr get modifies spec
                emit_named("%s_ptr_get_%s" % (self.name, field), params,
                           make_proof('const_modifies_proof',
                                      {"fun_name": "%s_ptr_get_%s" % (self.name, field),
                                       "args": ', '.join([
                                           "\<acute>ret__unsigned_long",
                                           "\<acute>%s_ptr" % self.name])},
                                      params.sorry))

            # Get spec
            emit_named("%s_get_%s" % (self.name, field), params,
                       make_proof('get_spec', substs, params.sorry))

            if not params.skip_modifies:
                # Set modifies spec
                emit_named("%s_set_%s" % (self.name, field), params,
                           make_proof('const_modifies_proof',
                                      {"fun_name": "%s_set_%s" % (self.name, field),
                                       "args": ', '.join([
                                           "\<acute>ret__struct_%s_C" % self.name,
                                           "\<acute>%s" % self.name,
                                           "\<acute>v%(base)d"])},
                                      params.sorry))

                emit_named("%s_ptr_set_%s" % (self.name, field), params,
                           make_proof('ptr_set_modifies_proof',
                                      {"fun_name": "%s_ptr_set_%s" % (self.name, field),
                                       "args": ', '.join([
                                           "\<acute>%s_ptr" % self.name,
                                           "\<acute>v%(base)d"])},
                                      params.sorry))

            # Set spec
            emit_named("%s_set_%s" % (self.name, field), params,
                       make_proof('set_spec', substs, params.sorry))

            emit_named_ptr_proof("%s_ptr_get_%s" % (self.name, field), params, self.name,
                                 type_map, params.toplevel_types,
                                 'ptr_get_spec', substs)
            emit_named_ptr_proof("%s_ptr_set_%s" % (self.name, field), params, self.name,
                                 type_map, params.toplevel_types,
                                 'ptr_set_spec', substs)

    def generate(self, params):
        output = params.output

        # Don't generate raw accessors for blocks in tagged unions
        if self.tagged:
            return

        # Type definition
        print(typedef_template %
              {"type": TYPES[options.environment][self.base],
               "name": self.name,
               "multiple": self.multiple}, file=output)
        print(file=output)

        # Generator
        param_fields = [field for field in self.visible_order]
        param_list = ["%s %s" % (TYPES[options.environment][self.base], field)
                      for field in param_fields]

        if len(param_list) == 0:
            gen_params = 'void'
        else:
            gen_params = ', '.join(param_list)

        ptr_params = ', '.join(["%s_t *%s_ptr" % (self.name, self.name)] + param_list)

        field_updates = {word: [] for word in range(self.multiple)}
        field_asserts = ["    /* fail if user has passed bits that we will override */"]

        for field, offset, size, high in self.fields:
            index = offset // self.base
            if high:
                shift_op = ">>"
                shift = self.base_bits - size - (offset % self.base)
                if self.base_sign_extend:
                    high_bits = ((self.base_sign_extend << (
                        self.base - self.base_bits)) - 1) << self.base_bits
                else:
                    high_bits = 0
                if shift < 0:
                    shift = -shift
                    shift_op = "<<"
            else:
                shift_op = "<<"
                shift = offset % self.base
                high_bits = 0
            if size < self.base:
                if high:
                    mask = ((1 << size) - 1) << (self.base_bits - size)
                else:
                    mask = (1 << size) - 1
                suf = self.constant_suffix

                field_asserts.append(
                    "    %s((%s & ~0x%x%s) == ((%d && (%s & (1%s << %d))) ? 0x%x : 0));"
                    % (ASSERTS[options.environment], field, mask, suf, self.base_sign_extend,
                       field, suf, self.base_bits - 1, high_bits))

                field_updates[index].append(
                    "(%s & 0x%x%s) %s %d" % (field, mask, suf, shift_op, shift))

            else:
                field_updates[index].append("%s %s %d;" % (field, shift_op, shift))

        word_inits = [
            ("words[%d] = 0" % index) + ''.join(["\n        | %s" % up for up in ups]) + ';'
            for (index, ups) in field_updates.items()]

        def mk_inits(prefix):
            return '\n'.join(["    %s%s" % (prefix, word_init) for word_init in word_inits])

        print_params = {
            "inline": INLINE[options.environment],
            "block": self.name,
            "gen_params": gen_params,
            "ptr_params": ptr_params,
            "gen_inits": mk_inits("%s." % self.name),
            "ptr_inits": mk_inits("%s_ptr->" % self.name),
            "asserts": '  \n'.join(field_asserts)
        }

        generator = generator_template % print_params
        ptr_generator = ptr_generator_template % print_params

        emit_named("%s_new" % self.name, params, generator)
        emit_named("%s_ptr_new" % self.name, params, ptr_generator)

        # Accessors
        for field, offset, size, high in self.fields:
            index = offset // self.base
            if high:
                write_shift = ">>"
                read_shift = "<<"
                shift = self.base_bits - size - (offset % self.base)
                if shift < 0:
                    shift = -shift
                    write_shift = "<<"
                    read_shift = ">>"
                if self.base_sign_extend:
                    high_bits = ((self.base_sign_extend << (
                        self.base - self.base_bits)) - 1) << self.base_bits
                else:
                    high_bits = 0
            else:
                write_shift = "<<"
                read_shift = ">>"
                shift = offset % self.base
                high_bits = 0
            mask = ((1 << size) - 1) << (offset % self.base)

            subs = {
                "inline": INLINE[options.environment],
                "block": self.name,
                "field": field,
                "type": TYPES[options.environment][self.base],
                "assert": ASSERTS[options.environment],
                "index": index,
                "shift": shift,
                "r_shift_op": read_shift,
                "w_shift_op": write_shift,
                "mask": mask,
                "suf": self.constant_suffix,
                "high_bits": high_bits,
                "sign_extend": self.base_sign_extend and high,
                "extend_bit": self.base_bits - 1,
                "base": self.base}

            # Reader
            emit_named("%s_get_%s" % (self.name, field), params,
                       reader_template % subs)

            # Writer
            emit_named("%s_set_%s" % (self.name, field), params,
                       writer_template % subs)

            # Pointer lifted reader
            emit_named("%s_ptr_get_%s" % (self.name, field), params,
                       ptr_reader_template % subs)

            # Pointer lifted writer
            emit_named("%s_ptr_set_%s" % (self.name, field), params,
                       ptr_writer_template % subs)

    def make_names(self, union=None):
        "Return the set of candidate function names for a block"

        if union is None:
            # Don't generate raw accessors for blocks in tagged unions
            if self.tagged:
                return []

            substs = {"block": self.name}

            # A standalone block
            field_templates = [
                "%(block)s_get_%(field)s",
                "%(block)s_set_%(field)s",
                "%(block)s_ptr_get_%(field)s",
                "%(block)s_ptr_set_%(field)s"]

            names = [t % substs for t in [
                "%(block)s_new",
                "%(block)s_ptr_new"]]
        else:
            substs = {"block": self.name,
                      "union": union.name}

            # A tagged union block
            field_templates = [
                "%(union)s_%(block)s_get_%(field)s",
                "%(union)s_%(block)s_set_%(field)s",
                "%(union)s_%(block)s_ptr_get_%(field)s",
                "%(union)s_%(block)s_ptr_set_%(field)s"]

            names = [t % substs for t in [
                "%(union)s_%(block)s_new",
                "%(union)s_%(block)s_ptr_new"]]

        for field, offset, size, high in self.fields:
            if not union is None and field == union.tagname:
                continue

            substs["field"] = field
            names += [t % substs for t in field_templates]

        return names


temp_output_files = []


class OutputFile(object):
    def __init__(self, filename, mode='w', atomic=True):
        """Open an output file for writing, recording its filename.
           If atomic is True, use a temporary file for writing.
           Call finish_output to finalise all temporary files."""
        self.filename = os.path.abspath(filename)
        if atomic:
            dirname, basename = os.path.split(self.filename)
            self.file = tempfile.NamedTemporaryFile(
                mode=mode, dir=dirname, prefix=basename + '.', delete=False)
            if DEBUG:
                print('Temp file: %r -> %r' % (self.file.name, self.filename), file=sys.stderr)
            global temp_output_files
            temp_output_files.append(self)
        else:
            self.file = open(filename, mode, encoding="utf-8")

    def write(self, *args, **kwargs):
        self.file.write(*args, **kwargs)


def finish_output():
    global temp_output_files
    for f in temp_output_files:
        os.rename(f.file.name, f.filename)
    temp_output_files = []


# Toplevel
if __name__ == '__main__':
    # Parse arguments to set mode and grab I/O filenames
    params = {}
    in_filename = None
    in_file = sys.stdin
    out_file = sys.stdout
    mode = 'c_defs'

    parser = optparse.OptionParser()
    parser.add_option('--c_defs', action='store_true', default=False)
    parser.add_option('--environment', action='store', default='sel4',
                      choices=list(INCLUDES.keys()))
    parser.add_option('--hol_defs', action='store_true', default=False)
    parser.add_option('--hol_proofs', action='store_true', default=False)
    parser.add_option('--sorry_lemmas', action='store_true',
                      dest='sorry', default=False)
    parser.add_option('--prune', action='append',
                      dest="prune_files", default=[])
    parser.add_option('--toplevel', action='append',
                      dest="toplevel_types", default=[])
    parser.add_option('--umm_types', action='store',
                      dest="umm_types_file", default=None)
    parser.add_option('--multifile_base', action='store', default=None)
    parser.add_option('--cspec-dir', action='store', default=None,
                      help="Location of the 'cspec' directory containing 'KernelState_C'.")
    parser.add_option('--thy-output-path', action='store', default=None,
                      help="Path that the output theory files will be located in.")
    parser.add_option('--skip_modifies', action='store_true', default=False)
    parser.add_option('--showclasses', action='store_true', default=False)
    parser.add_option('--debug', action='store_true', default=False)
    parser.add_option('--from_file', action='store', default=None,
                      help="Original source file before preprocessing.")

    options, args = parser.parse_args()
    DEBUG = options.debug

    if len(args) > 0:
        in_filename = args[0]
        in_file = open(in_filename, encoding="utf-8")

        if len(args) > 1:
            out_file = OutputFile(args[1])

    #
    # If generating Isabelle scripts, ensure we have enough information for
    # relative paths required by Isabelle.
    #
    if options.hol_defs or options.hol_proofs:
        # Ensure directory that we need to include is known.
        if options.cspec_dir is None:
            parser.error("'cspec_dir' not defined.")

        # Ensure that if an output file was not specified, an output path was.
        if len(args) <= 1:
            if options.thy_output_path is None:
                parser.error("Theory output path was not specified")
            if out_file == sys.stdout:
                parser.error(
                    'Output file name must be given when generating HOL definitions or proofs')
            out_file.filename = os.path.abspath(options.thy_output_path)

    if options.hol_proofs and not options.umm_types_file:
        parser.error('--umm_types must be specified when generating HOL proofs')

    del parser

    options.output = out_file

    # Parse the spec
    lexer = lex.lex()
    yacc.yacc(debug=0, write_tables=0)
    blocks = {}
    unions = {}
    _, block_map, union_map = yacc.parse(input=in_file.read(), lexer=lexer)
    base_list = [8, 16, 32, 64]
    # assumes that unsigned int = 32 bit on 32-bit and 64-bit platforms,
    # and that unsigned long long = 64 bit on 64-bit platforms.
    # Should still work fine if ull = 128 bit, but will not work
    # if unsigned int is less than 32 bit.
    suffix_map = {8: 'u', 16: 'u', 32: 'u', 64: 'ull'}
    for base_info, block_list in block_map.items():
        base, base_bits, base_sign_extend = base_info
        for name, b in block_list.items():
            if not base in base_list:
                raise ValueError("Invalid base size: %d" % base)
            suffix = suffix_map[base]
            b.set_base(base, base_bits, base_sign_extend, suffix)
            blocks[name] = b

    symtab = {}
    symtab.update(blocks)
    for base, union_list in union_map.items():
        unions.update(union_list)
    symtab.update(unions)
    for base_info, union_list in union_map.items():
        base, base_bits, base_sign_extend = base_info
        for u in union_list.values():
            if not base in base_list:
                raise ValueError("Invalid base size: %d" % base)
            suffix = suffix_map[base]
            u.resolve(options, symtab)
            u.set_base(base, base_bits, base_sign_extend, suffix)

    if not in_filename is None:
        base_filename = os.path.basename(in_filename).split('.')[0]

        # Generate the module name from the input filename
        module_name = base_filename

    # Prune list of names to generate
    name_list = []
    for e in det_values(blocks, unions):
        name_list += e.make_names()

    name_list = set(name_list)
    if len(options.prune_files) > 0:
        search_re = re.compile('[a-zA-Z0-9_]+')

        pruned_names = set()
        for filename in options.prune_files:
            f = open(filename, encoding="utf-8")
            string = f.read()

            matched_tokens = set(search_re.findall(string))
            pruned_names.update(matched_tokens & name_list)
    else:
        pruned_names = name_list

    options.names = pruned_names

    # Generate the output
    if options.hol_defs:
        # Fetch kernel
        if options.multifile_base is None:
            print("theory %s_defs" % module_name, file=out_file)
            print("imports \"%s/KernelState_C\"" % (
                os.path.relpath(options.cspec_dir,
                                os.path.dirname(out_file.filename))), file=out_file)
            print("begin", file=out_file)
            print(file=out_file)

            print(defs_global_lemmas, file=out_file)
            print(file=out_file)

            for e in det_values(blocks, unions):
                e.generate_hol_defs(options)

            print("end", file=out_file)
        else:
            print("theory %s_defs" % module_name, file=out_file)
            print("imports", file=out_file)
            print("  \"%s/KernelState_C\"" % (
                os.path.relpath(options.cspec_dir,
                                os.path.dirname(out_file.filename))), file=out_file)
            for e in det_values(blocks, unions):
                print("  %s_%s_defs" % (module_name, e.name),
                      file=out_file)
            print("begin", file=out_file)
            print("end", file=out_file)

            for e in det_values(blocks, unions):
                base_filename = \
                    os.path.basename(options.multifile_base).split('.')[0]
                submodule_name = base_filename + "_" + \
                    e.name + "_defs"
                out_file = OutputFile(options.multifile_base + "_" +
                                      e.name + "_defs" + ".thy")

                print("theory %s imports \"%s/KernelState_C\" begin" % (
                    submodule_name, os.path.relpath(options.cspec_dir,
                                                    os.path.dirname(out_file.filename))),
                      file=out_file)
                print(file=out_file)

                options.output = out_file
                e.generate_hol_defs(options)

                print("end", file=out_file)
    elif options.hol_proofs:
        def is_bit_type(tp):
            return umm.is_base(tp) & (umm.base_name(tp) in
                                      map(lambda e: e.name + '_C', det_values(blocks, unions)))

        tps = umm.build_types(options.umm_types_file)
        type_map = {}

        # invert type map
        for toptp in options.toplevel_types:
            paths = umm.paths_to_type(tps, is_bit_type, toptp)

            for path, tp in paths:
                tp = umm.base_name(tp)

                if tp in type_map:
                    raise ValueError("Type %s has multiple parents" % tp)

                type_map[tp] = (toptp, path)

        if options.multifile_base is None:
            print("theory %s_proofs" % module_name, file=out_file)
            print("imports %s_defs" % module_name, file=out_file)
            print("begin", file=out_file)
            print(file=out_file)
            print(file=out_file)

            for e in det_values(blocks, unions):
                e.generate_hol_proofs(options, type_map)

            print("end", file=out_file)
        else:
            # top types are broken here.
            print("theory %s_proofs" % module_name, file=out_file)
            print("imports", file=out_file)
            for e in det_values(blocks, unions):
                print("  %s_%s_proofs" % (module_name, e.name),
                      file=out_file)
            print("begin", file=out_file)
            print("end", file=out_file)

            for e in det_values(blocks, unions):
                base_filename = \
                    os.path.basename(options.multifile_base).split('.')[0]
                submodule_name = base_filename + "_" + \
                    e.name + "_proofs"
                out_file = OutputFile(options.multifile_base + "_" +
                                      e.name + "_proofs" + ".thy")

                print(("theory %s imports "
                       + "%s_%s_defs begin") % (
                    submodule_name, base_filename, e.name),
                    file=out_file)
                print(file=out_file)

                options.output = out_file
                e.generate_hol_proofs(options, type_map)

                print("end", file=out_file)
    else:
        if options.from_file:
            print(f"/* generated from {options.from_file} */\n", file=out_file)
        print("#pragma once\n", file=out_file)
        print('\n'.join(map(lambda x: '#include <%s>' % x,
                            INCLUDES[options.environment])), file=out_file)
        for e in det_values(blocks, unions):
            e.generate(options)

    finish_output()
