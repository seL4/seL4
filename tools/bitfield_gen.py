#!/usr/bin/env python

#
# Copyright 2014, NICTA
#
# This software may be distributed and modified according to the terms of
# the BSD 2-Clause license. Note that NO WARRANTY is provided.
# See "LICENSE_BSD2.txt" for details.
#
# @TAG(NICTA_BSD)
#

##
## A tool for generating bifield structures with get/set/new methods
## including Isabelle/HOL specifications and correctness proofs.
##

import sys
import os.path
import optparse
import re

import lex
import yacc

import umm

# Whether debugging is enabled (turn on with command line option --debug).
DEBUG = False

# Headers to include depending on which environment we are generating code for.
INCLUDES = {
    'sel4':['assert.h', 'config.h', 'stdint.h', 'util.h'],
    'libsel4':['assert.h', 'autoconf.h', 'stdint.h'],
}

### Parser

reserved = ('BLOCK', 'BASE', 'FIELD', 'FIELD_HIGH', 'MASK', 'PADDING', \
            'TAGGED_UNION', 'TAG')

tokens = reserved + ('IDENTIFIER', 'INTLIT', 'LBRACE', 'RBRACE', \
                     'LPAREN', 'RPAREN', 'COMMA')

t_LBRACE = r'{'
t_RBRACE = r'}'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_COMMA  = r','

reserved_map = dict((r.lower(), r) for r in reserved)

## FIXME this is config stuff just appearing in the file
loc_name = 'kernel_all_substitute'

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
    print >>sys.stderr, "%s: Unexpected character '%s'" % (sys.argv[0], t.value[0])
    if DEBUG:
        print >>sys.stderr, 'Token: %s' % str(t)
    sys.exit(1)

def p_start(t):
    """start : entity_list"""
    t[0] = t[1]

def p_entity_list_empty(t):
    """entity_list : """
    t[0] = (None,{},{})

def p_entity_list_base(t):
    """entity_list : entity_list base"""
    base, blocks, unions = t[1]
    t[0] = (t[2], blocks, unions)

def p_entity_list_block(t):
    """entity_list : entity_list block"""
    base, blocks, unions = t[1]
    blocks[t[2].name] = t[2]
    t[0] = (base, blocks, unions)

def p_entity_list_union(t):
    """entity_list : entity_list tagged_union"""
    base, blocks, unions = t[1]
    unions[t[2].name] = t[2]
    t[0] = (base, blocks, unions)

def p_base(t):
    """base : BASE INTLIT"""
    t[0] = t[2]

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

def p_tagged_union(t):
    """tagged_union : TAGGED_UNION IDENTIFIER IDENTIFIER""" \
                  """ LBRACE masks tags RBRACE"""
    t[0] = TaggedUnion(name=t[2], tagname=t[3], classes=t[5], tags=t[6])

def p_tags_empty(t):
    """tags :"""
    t[0] = []

def p_tags(t):
    """tags : tags TAG IDENTIFIER INTLIT"""
    t[0] = t[1] + [(t[3],t[4])]

def p_masks_empty(t):
    """masks :"""
    t[0] = []

def p_masks(t):
    """masks : masks MASK INTLIT INTLIT"""
    t[0] = t[1] + [(t[3],t[4])]

def p_error(t):
    print >>sys.stderr, "Syntax error at token '%s'" % t.value
    sys.exit(1)

### Templates

## C templates

typedef_template = \
"""struct %(name)s {
    uint%(base)d_t words[%(multiple)d];
};
typedef struct %(name)s %(name)s_t;"""

generator_template = \
"""static inline %(block)s_t CONST
%(block)s_new(%(args)s) {
    %(block)s_t %(block)s;

%(word_inits)s
    
%(field_inits)s

    return %(block)s;
}"""

ptr_generator_template = \
"""static inline void
%(block)s_ptr_new(%(args)s) {
%(word_inits)s

%(field_inits)s
}"""

reader_template = \
"""static inline uint%(base)d_t CONST
%(block)s_get_%(field)s(%(block)s_t %(block)s) {
    return (%(block)s.words[%(index)d] & 0x%(mask)x) %(r_shift_op)s %(shift)d;
}"""

ptr_reader_template = \
"""static inline uint%(base)d_t PURE
%(block)s_ptr_get_%(field)s(%(block)s_t *%(block)s_ptr) {
    return (%(block)s_ptr->words[%(index)d] & 0x%(mask)x) """ \
    """%(r_shift_op)s %(shift)d;
}"""

writer_template = \
"""static inline %(block)s_t CONST
%(block)s_set_%(field)s(%(block)s_t %(block)s, uint%(base)d_t v) {
    /* fail if user has passed bits that we will override */
    assert(((~0x%(mask)x %(r_shift_op)s %(shift)d) & v) == 0);
    %(block)s.words[%(index)d] &= ~0x%(mask)x;
    %(block)s.words[%(index)d] |= (v %(w_shift_op)s %(shift)d) & 0x%(mask)x;
    return %(block)s;
}"""

ptr_writer_template = \
"""static inline void
%(block)s_ptr_set_%(field)s(%(block)s_t *%(block)s_ptr, uint%(base)d_t v) {
    /* fail if user has passed bits that we will override */
    assert(((~0x%(mask)x %(r_shift_op)s %(shift)d) & v) == 0);
    %(block)s_ptr->words[%(index)d] &= ~0x%(mask)x;
    %(block)s_ptr->words[%(index)d] |= (v %(w_shift_op)s """ \
    """%(shift)d) & 0x%(mask)x;
}"""

union_generator_template = \
"""static inline %(union)s_t CONST
%(union)s_%(block)s_new(%(args)s) {
    %(union)s_t %(union)s;

%(word_inits)s
    
%(field_inits)s

    return %(union)s;
}"""

ptr_union_generator_template = \
"""static inline void
%(union)s_%(block)s_ptr_new(%(args)s) {
%(word_inits)s

%(field_inits)s
}"""

union_reader_template = \
"""static inline uint%(base)d_t CONST
%(union)s_%(block)s_get_%(field)s(%(union)s_t %(union)s) {
    assert(((%(union)s.words[%(tagindex)d] >> %(tagshift)d) & 0x%(tagmask)x) ==
           %(union)s_%(block)s);

    return (%(union)s.words[%(index)d] & 0x%(mask)x) %(r_shift_op)s %(shift)d;
}"""

ptr_union_reader_template = \
"""static inline uint%(base)d_t PURE
%(union)s_%(block)s_ptr_get_%(field)s(%(union)s_t *%(union)s_ptr) {
    assert(((%(union)s_ptr->words[%(tagindex)d] >> """ \
    """%(tagshift)d) & 0x%(tagmask)x) ==
           %(union)s_%(block)s);

    return (%(union)s_ptr->words[%(index)d] & 0x%(mask)x) """ \
    """%(r_shift_op)s %(shift)d;
}"""

union_writer_template = \
"""static inline %(union)s_t CONST
%(union)s_%(block)s_set_%(field)s(%(union)s_t %(union)s, uint%(base)d_t v) {
    assert(((%(union)s.words[%(tagindex)d] >> %(tagshift)d) & 0x%(tagmask)x) ==
           %(union)s_%(block)s);
    /* fail if user has passed bits that we will override */
    assert(((~0x%(mask)x %(r_shift_op)s %(shift)d) & v) == 0);

    %(union)s.words[%(index)d] &= ~0x%(mask)x;
    %(union)s.words[%(index)d] |= (v %(w_shift_op)s %(shift)d) & 0x%(mask)x;
    return %(union)s;
}"""

ptr_union_writer_template = \
"""static inline void
%(union)s_%(block)s_ptr_set_%(field)s(%(union)s_t *%(union)s_ptr,
                                      uint%(base)d_t v) {
    assert(((%(union)s_ptr->words[%(tagindex)d] >> """ \
    """%(tagshift)d) & 0x%(tagmask)x) ==
           %(union)s_%(block)s);

    /* fail if user has passed bits that we will override */
    assert(((~0x%(mask)x %(r_shift_op)s %(shift)d) & v) == 0);

    %(union)s_ptr->words[%(index)d] &= ~0x%(mask)x;
    %(union)s_ptr->words[%(index)d] |= """ \
    """(v %(w_shift_op)s %(shift)d) & 0x%(mask)x;
}"""

tag_reader_header_template = \
"""static inline uint%(base)d_t CONST
%(union)s_get_%(tagname)s(%(union)s_t %(union)s) {
"""

tag_reader_entry_template = \
"""    if ((%(union)s.words[%(index)d] & 0x%(classmask)x) != 0x%(classmask)x)
        return (%(union)s.words[%(index)d] >> %(shift)d) & 0x%(mask)x;
"""

tag_reader_final_template = \
"""    return (%(union)s.words[%(index)d] >> %(shift)d) & 0x%(mask)x;"""

tag_reader_footer_template = \
"""
}"""

tag_eq_reader_header_template = \
"""static inline int CONST
%(union)s_%(tagname)s_equals(%(union)s_t %(union)s, uint%(base)d_t %(union)s_type_tag) {
"""

tag_eq_reader_entry_template = \
"""    if ((%(union)s_type_tag & 0x%(classmask)x) != 0x%(classmask)x)
        return ((%(union)s.words[%(index)d] >> %(shift)d) & 0x%(mask)x) == %(union)s_type_tag;
"""

tag_eq_reader_final_template = \
"""    return ((%(union)s.words[%(index)d] >> %(shift)d) & 0x%(mask)x) == %(union)s_type_tag;"""

tag_eq_reader_footer_template = \
"""
}"""

ptr_tag_reader_header_template = \
"""static inline uint%(base)d_t PURE
%(union)s_ptr_get_%(tagname)s(%(union)s_t *%(union)s_ptr) {
"""

ptr_tag_reader_entry_template = \
"""    if ((%(union)s_ptr->words[%(index)d] & 0x%(classmask)x) != 0x%(classmask)x)
        return (%(union)s_ptr->words[%(index)d] >> %(shift)d) & 0x%(mask)x;
"""

ptr_tag_reader_final_template = \
"""    return (%(union)s_ptr->words[%(index)d] >> %(shift)d) & 0x%(mask)x;"""

ptr_tag_reader_footer_template = \
"""
}"""

tag_writer_template = \
"""static inline %(union)s_t CONST
%(union)s_set_%(tagname)s(%(union)s_t %(union)s, uint%(base)d_t v) {
    /* fail if user has passed bits that we will override */
    assert(((~0x%(mask)x %(r_shift_op)s %(shift)d) & v) == 0);

    %(union)s.words[%(index)d] &= ~0x%(mask)x;
    %(union)s.words[%(index)d] |= (v << %(shift)d) & 0x%(mask)x;
    return %(union)s;
}"""

ptr_tag_writer_template = \
"""static inline void
%(union)s_ptr_set_%(tagname)s(%(union)s_t *%(union)s_ptr, uint%(base)d_t v) {
    /* fail if user has passed bits that we will override */
    assert(((~0x%(mask)x %(r_shift_op)s %(shift)d) & v) == 0);

    %(union)s_ptr->words[%(index)d] &= ~0x%(mask)x;
    %(union)s_ptr->words[%(index)d] |= (v << %(shift)d) & 0x%(mask)x;
}"""

# HOL definition templates

lift_def_template = \
'''definition
  %(name)s_lift :: "%(name)s_C \<Rightarrow> %(name)s_CL"
where
  "%(name)s_lift %(name)s \<equiv> \<lparr>
       %(fields)s \<rparr>"'''

block_lift_def_template = \
'''definition %(union)s_%(block)s_lift :: ''' \
'''"%(union)s_C \<Rightarrow> %(union)s_%(block)s_CL"
where
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
'''definition
  %(name)s_get_tag :: "%(name)s_C \<Rightarrow> word%(base)d"
where
  "%(name)s_get_tag %(name)s \<equiv>
     '''

union_get_tag_def_entry_template = \
'''if ((index (%(name)s_C.words_C %(name)s) %(tag_index)d)''' \
''' AND %(classmask)d \<noteq> %(classmask)d)
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
'''if ((x << %(tag_shift)d) AND %(classmask)d \<noteq> %(classmask)d)
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
'''definition
  %(name)s_lift :: "%(name)s_C \<Rightarrow> %(name)s_CL option"
where
  "%(name)s_lift %(name)s \<equiv>
    (let tag = %(name)s_get_tag %(name)s in
     %(tag_cases)s
     else None)"'''
                 
union_access_def_template = \
'''definition
  %(union)s_%(block)s_access :: "(%(union)s_%(block)s_CL \<Rightarrow> 'a)
                                 \<Rightarrow> %(union)s_CL \<Rightarrow> 'a"
where
  "%(union)s_%(block)s_access f %(union)s \<equiv>
     (case %(union)s of %(generator)s rec \<Rightarrow> f rec)"'''

union_update_def_template = \
'''definition
  %(union)s_%(block)s_update :: "(%(union)s_%(block)s_CL \<Rightarrow>''' \
                                ''' %(union)s_%(block)s_CL) \<Rightarrow>
                                 %(union)s_CL \<Rightarrow> %(union)s_CL"
where
  "%(union)s_%(block)s_update f %(union)s \<equiv>
     (case %(union)s of %(generator)s rec \<Rightarrow>
        %(generator)s (f rec))"'''

# HOL proof templates

struct_lemmas_template = \
'''
lemmas %(name)s_ptr_guards[simp] =
  %(name)s_ptr_words_NULL
  %(name)s_ptr_words_aligned
  %(name)s_ptr_words_ptr_safe'''

global_lemmas = \
'''lemmas guard_simps =
  word_sle_def word_sless_def scast_id

lemmas mask_shift_simps =
  ucast_def shift_over_ao_dists word_bw_assocs
  word_size multi_shift_simps mask_def
  word_ao_dist NOT_eq scast_id
  word_and_max_word max_word_def

lemmas sep_heap_simps =
  sep_app_def hrs_mem_update_def
  hrs_htd_def split_def

lemma tag_eq_to_tag_masked_eq:
  "tag == v ==> tag && m = v && m"
  by simp
'''

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
    return ptr_basic_template('get_%(field)s', ptrname, '\<acute>ret__unsigned_long :== ', '',
                              '''\<lbrace>\<acute>ret__unsigned_long = ''' \
                              '''%(name)s_CL.%(field)s_CL ''' \
                              '''(%(name)s_lift (%(access_path)s))\<rbrace>''') #  AND %(mask)s

def ptr_set_template(name, ptrname):
    return ptr_basic_template(name, ptrname, '', ', \<acute>v',
                              '''{t. (\<exists>%(name)s.
                              %(name)s_lift %(name)s =
                              %(name)s_lift (%(access_path)s) \<lparr> %(name)s_CL.%(field)s_CL ''' \
                              ''':= \<^bsup>s\<^esup>v AND %(mask)s \<rparr> \<and> 
                              cslift t = (cslift s)(''' + ptrname + ''' \<mapsto> %(update_path)s)) \<and>
                              %(cslift_other)s \<and>
                              hrs_htd (t_hrs_' (globals t)) = hrs_htd (t_hrs_' (globals s))
                              }''')
                              
def ptr_new_template(ptrname):
    return ptr_basic_template('new', ptrname, '', ', %(args)s',
                              '''{t. (\<exists>%(name)s. %(name)s_lift %(name)s = \<lparr>
                              %(field_eqs)s \<rparr> \<and>
                              cslift t = (cslift s)(''' + ptrname + ''' \<mapsto> %(update_path)s)) \<and>
                              %(cslift_other)s \<and>
                              hrs_htd (t_hrs_' (globals t)) = hrs_htd (t_hrs_' (globals s))
                              }''')

def ptr_get_tag_template(ptrname):
    return ptr_basic_template('get_%(tagname)s', ptrname, '\<acute>ret__unsigned_long :== ', '', 
                              '''\<lbrace>\<acute>ret__unsigned_long = %(name)s_get_tag (%(access_path)s)\<rbrace>''')


def ptr_empty_union_new_template(ptrname):
    return ptr_union_basic_template('new', ptrname, '', '', '', 
                                    '''{t. (\<exists>%(name)s. ''' \
                                    '''%(name)s_get_tag %(name)s = scast %(name)s_%(block)s \<and>
                                    cslift t = (cslift s)(''' + ptrname + ''' \<mapsto> %(update_path)s)) \<and>
                                    %(cslift_other)s \<and>
                                    hrs_htd (t_hrs_' (globals t)) = hrs_htd (t_hrs_' (globals s))
                                    }''')

def ptr_union_new_template(ptrname):
    return ptr_union_basic_template('new', ptrname, '', ', %(args)s', '', 
                                    '''{t. (\<exists>%(name)s. ''' \
                                    '''%(name)s_%(block)s_lift %(name)s = \<lparr>
                                    %(field_eqs)s \<rparr> \<and>
                                    %(name)s_get_tag %(name)s = scast %(name)s_%(block)s \<and>
                                    cslift t = (cslift s)(''' + ptrname + ''' \<mapsto> %(update_path)s)) \<and>
                                    %(cslift_other)s \<and>
                                    hrs_htd (t_hrs_' (globals t)) = hrs_htd (t_hrs_' (globals s))
                                    }''')

def ptr_union_get_template(ptrname):
    return ptr_union_basic_template('get_%(field)s', ptrname,
                                    '\<acute>ret__unsigned_long :== ', '',
                                    '\<and> %(name)s_get_tag %(access_path)s = scast %(name)s_%(block)s',
                                    '''\<lbrace>\<acute>ret__unsigned_long = ''' \
                                    '''%(name)s_%(block)s_CL.%(field)s_CL ''' \
                                    '''(%(name)s_%(block)s_lift %(access_path)s)\<rbrace>''') #  AND %(mask)s --- given by _lift?

def ptr_union_set_template(ptrname):
    return ptr_union_basic_template('set_%(field)s', ptrname, '', ', \<acute>v',
                                    '\<and> %(name)s_get_tag %(access_path)s = scast %(name)s_%(block)s',
                                    '''{t. (\<exists>%(name)s. ''' \
                                    '''%(name)s_%(block)s_lift %(name)s =
                                    %(name)s_%(block)s_lift %(access_path)s ''' \
                                    '''\<lparr> %(name)s_%(block)s_CL.%(field)s_CL ''' \
                                    ''':= \<^bsup>s\<^esup>v AND %(mask)s \<rparr> \<and>
                                    %(name)s_get_tag %(name)s = scast %(name)s_%(block)s \<and>
                                    cslift t = (cslift s)(''' + ptrname + ''' \<mapsto> %(update_path)s)) \<and>
                                    %(cslift_other)s \<and>
                                    hrs_htd (t_hrs_' (globals t)) = hrs_htd (t_hrs_' (globals s))
                                    }''')

proof_templates = {

'lift_collapse_proof' : [
'''lemma %(name)s_lift_%(block)s:
  "%(name)s_get_tag %(name)s = scast %(name)s_%(block)s \<Longrightarrow>
  %(name)s_lift %(name)s =
  Some (%(value)s)"''',
''' apply(simp add:%(name)s_lift_def %(name)s_tag_defs)
done'''],

'words_NULL_proof' : [
'''lemma %(name)s_ptr_words_NULL:
  "c_guard (p::%(name)s_C ptr) \<Longrightarrow>
   0 < &(p\<rightarrow>[''words_C''])"''',
''' apply(fastforce intro:c_guard_NULL_fl simp:typ_uinfo_t_def)
done'''],

'words_aligned_proof' : [
'''lemma %(name)s_ptr_words_aligned:
  "c_guard (p::%(name)s_C ptr) \<Longrightarrow>
   ptr_aligned ((Ptr &(p\<rightarrow>[''words_C'']))::''' \
               '''((word32[%(words)d]) ptr))"''',
''' apply(fastforce intro:c_guard_ptr_aligned_fl simp:typ_uinfo_t_def)
done'''],

'words_ptr_safe_proof' : [
'''lemma %(name)s_ptr_words_ptr_safe:
  "ptr_safe (p::%(name)s_C ptr) d \<Longrightarrow>
   ptr_safe (Ptr &(p\<rightarrow>[''words_C''])::''' \
         '''((word32[%(words)d]) ptr)) d"''',
''' apply(fastforce intro:ptr_safe_mono simp:typ_uinfo_t_def)
done'''],

'get_tag_fun_spec_proof' : [
'''lemma (in ''' + loc_name + ''') fun_spec:
  "\<Gamma> \<turnstile> {\<sigma>}
       \<acute>ret__%(rtype)s :== PROC %(name)s_get_%(tag_name)s(''' \
                                             ''' \<acute>%(name))
       \<lbrace>\<acute>ret__%(rtype)s = %(name)s_get_tag''' \
                             '''\<^bsup>\<sigma>\<^esup>\<rbrace>"''',
''' apply(rule allI, rule conseqPre, vcg)
 apply(clarsimp)
 apply(simp add:$(name)s_get_tag_def word_sle_def
                mask_def ucast_def)
done'''],

'const_modifies_proof' : [
'''lemma (in ''' + loc_name + ''') %(fun_name)s_modifies:
  "\<forall> s. \<Gamma> \<turnstile>\<^bsub>/UNIV\<^esub> {s}
       PROC %(fun_name)s(%(args)s)
       {t. t may_not_modify_globals s}"''',
''' by (vcg spec=modifies strip_guards=true)'''],

'ptr_set_modifies_proof' : [
'''lemma (in ''' + loc_name + ''') %(fun_name)s_modifies:
  "\<forall>s. \<Gamma> \<turnstile>\<^bsub>/UNIV\<^esub> {s}
       PROC %(fun_name)s(%(args)s)
       {t. t may_only_modify_globals s in [t_hrs]}"''',
''' by (vcg spec=modifies strip_guards=true)'''],


'new_spec' : [
'''lemma (in ''' + loc_name + ''') %(name)s_new_spec:
  "\<forall> s. \<Gamma> \<turnstile> {s}
       \<acute>ret__struct_%(name)s_C :== PROC %(name)s_new(%(args)s)
       \<lbrace> %(name)s_lift \<acute>ret__struct_%(name)s_C = \<lparr>
          %(field_eqs)s \<rparr> \<rbrace>"''',
''' apply(rule allI, rule conseqPre, vcg)

 apply(clarsimp simp:guard_simps)
 apply(simp add:o_def)
 apply(simp add:shift_over_ao_dists mask_def)
 apply(unfold %(name)s_lift_def)
 apply(simp add:shift_over_ao_dists)
 apply(((simp add:word_ao_dist),
        (simp add:word_bw_assocs),
        (simp add:multi_shift_simps),
        (simp add:mask_def word_size))?)
 apply(simp add:word_bw_assocs)
done'''],

'ptr_new_spec_direct' : [
    ptr_new_template(direct_ptr_name),
'''sorry (* ptr_new_spec_direct *)'''],

'ptr_new_spec_path' : [
    ptr_new_template(path_ptr_name),
'''sorry (* ptr_new_spec_path *)'''],


'get_spec' : [
'''lemma (in ''' + loc_name + ''') %(name)s_get_%(field)s_spec:
  "\<forall>s. \<Gamma> \<turnstile> {s}
       \<acute>ret__unsigned_long :== ''' \
       '''PROC %(name)s_get_%(field)s(\<acute>%(name)s)
       \<lbrace>\<acute>ret__unsigned_long = ''' \
       '''%(name)s_CL.%(field)s_CL ''' \
       '''(%(name)s_lift \<^bsup>s\<^esup>%(name)s)\<rbrace>"''', #  AND %(mask)s
''' apply(rule allI, rule conseqPre, vcg)
 apply(clarsimp)
 apply(simp add:%(name)s_lift_def
                mask_shift_simps
                guard_simps)
done'''],

'set_spec' : [
'''lemma (in ''' + loc_name + ''') %(name)s_set_%(field)s_spec:
  "\<forall>s. \<Gamma> \<turnstile> {s}
       \<acute>ret__struct_%(name)s_C :== ''' \
       '''PROC %(name)s_set_%(field)s(\<acute>%(name)s, \<acute>v)
       \<lbrace>%(name)s_lift \<acute>ret__struct_%(name)s_C = ''' \
       '''%(name)s_lift \<^bsup>s\<^esup>%(name)s \<lparr> ''' \
       '''%(name)s_CL.%(field)s_CL ''' \
       ''':= \<^bsup>s\<^esup>v AND %(mask) s \<rparr>\<rbrace>"''',
''' apply(rule allI, rule conseqPre, vcg)
 apply(clarsimp simp:guard_simps
                     %(name)s_lift_def
                     mask_def shift_over_ao_dists
                     multi_shift_simps word_size
                     word_ao_dist word_bw_assocs
                     NOT_eq)
done'''],

# where the top level type is the bitfield type --- these are split because they have different proofs
'ptr_get_spec_direct' : [
    ptr_get_template(direct_ptr_name),
''' unfolding ptrval_def
 apply(rule allI, rule conseqPre, vcg)
 apply(clarsimp simp:h_t_valid_clift_Some_iff)
 apply(simp add:guard_simps
                %(name)s_lift_def
                typ_heap_simps
                ucast_def)
 apply(simp add:max_word_def word_and_max_word)?
done'''],

'ptr_get_spec_path' : [
    ptr_get_template(path_ptr_name),
''' unfolding ptrval_def
 apply(rule allI, rule conseqPre, vcg)
 apply(clarsimp simp:guard_simps)
 apply(frule iffD1[OF h_t_valid_clift_Some_iff], rule exE, assumption, simp)
 apply(frule clift_subtype, simp, simp, simp)
 apply(simp add:h_val_field_clift' typ_heap_simps)
 apply(simp add:thread_state_lift_def)
 apply(simp add:mask_shift_simps)?
done'''],

'ptr_set_spec_direct' : [
    ptr_set_template('set_%(field)s', direct_ptr_name),
''' unfolding ptrval_def
 apply(rule allI, rule conseqPre, vcg)
 apply(clarsimp simp add: packed_heap_update_collapse_hrs typ_heap_simps guard_simps)
 
 apply(rule exI, rule conjI[rotated], rule refl)
 apply(clarsimp simp:h_t_valid_clift_Some_iff
                     %(name)s_lift_def
                     typ_heap_simps)

 apply(simp add:mask_shift_simps)
done'''],

'ptr_set_spec_path' : [
    ptr_set_template('set_%(field)s', path_ptr_name),
'''(* Invoke vcg *)
 unfolding ptrval_def
 apply(rule allI, rule conseqPre, vcg)
 apply(clarsimp)

 (* Infer h_t_valid for all three levels of indirection *)
 apply(frule h_t_valid_c_guard_cparent, simp, simp add:typ_uinfo_t_def)
 apply(frule h_t_valid_c_guard_field[where f="[''words_C'']"],
                                     simp, simp add:typ_uinfo_t_def)

 (* Discharge guards, including c_guard for pointers *)
 apply(simp add:h_t_valid_c_guard guard_simps)

 (* Discharge heap-invariance conjuncts *)
 apply(rule conjI[rotated])
  apply(simp add:heap_update_field_hrs h_t_valid_c_guard typ_heap_simps)

 (* Lift field updates to bitfield struct updates *)
 apply(simp add:heap_update_field_hrs h_t_valid_c_guard typ_heap_simps)

 (* Collapse multiple updates *)
 apply(simp add:packed_heap_update_collapse_hrs)

 (* Instantiate the toplevel object *)
 apply(frule iffD1[OF h_t_valid_clift_Some_iff], rule exE, assumption, simp)

 (* Instantiate the next-level object in terms of the last *)
 apply(frule clift_subtype, simp+)

 (* Resolve pointer accesses *)
 apply(simp add:h_val_field_clift')

 (* Rewrite bitfield struct updates as enclosing struct updates *)
 apply(frule h_t_valid_c_guard)
 apply(simp add:parent_update_child)

 (* Rewrite the LHS of the equality to a function update *)
 apply(simp add:clift_heap_update)

 (* Equate the updated values *)
 apply(rule exI, rule conjI[rotated], simp add:h_val_clift')

 (* Rewrite struct updates *)
 apply(simp add:o_def %(name)s_lift_def)

 (* Solve bitwise arithmetic *)
 apply(simp add:mask_shift_simps)
 done'''],


'get_tag_spec' : [
'''lemma (in ''' + loc_name + ''') %(name)s_get_%(tagname)s_spec:
  "\<forall>s. \<Gamma> \<turnstile> {s}
       \<acute>ret__unsigned_long :== ''' \
    '''PROC %(name)s_get_%(tagname)s(\<acute>%(name)s)
       \<lbrace>\<acute>ret__unsigned_long = ''' \
    '''%(name)s_get_tag \<^bsup>s\<^esup>%(name)s\<rbrace>"''',
''' apply(rule allI, rule conseqPre, vcg)
 apply(clarsimp)
 apply(simp add:%(name)s_get_tag_def
                mask_shift_simps
                guard_simps)
done'''],

'get_tag_equals_spec' : [
'''lemma (in ''' + loc_name + ''') %(name)s_%(tagname)s_equals_spec:
  "\<forall>s. \<Gamma> \<turnstile> {s}
       \<acute>ret__int :==
       PROC %(name)s_%(tagname)s_equals(\<acute>%(name)s, \<acute>%(name)s_type_tag)
       \<lbrace>\<acute>ret__int = of_bl [%(name)s_get_tag \<^bsup>s\<^esup>%(name)s = \<^bsup>s\<^esup>%(name)s_type_tag]\<rbrace>"''',
''' apply(rule allI, rule conseqPre, vcg)
 apply(clarsimp)
 apply(simp add:%(name)s_get_tag_eq_x
                mask_shift_simps
                guard_simps)
done'''],

'ptr_get_tag_spec_direct' : [
    ptr_get_tag_template(direct_ptr_name),
''' unfolding ptrval_def
 apply(rule allI, rule conseqPre, vcg)
 apply(clarsimp simp:guard_simps)
 apply(frule h_t_valid_field[where f="[''words_C'']"], simp+)
 apply(frule iffD1[OF h_t_valid_clift_Some_iff], rule exE, assumption, simp)
 apply(simp add:h_val_clift' clift_field)
 apply(simp add:%(name)s_get_tag_def)
 apply(simp add:mask_shift_simps)
done'''],

'ptr_get_tag_spec_path' : [
    ptr_get_tag_template(path_ptr_name),
''' unfolding ptrval_def
 apply(rule allI, rule conseqPre, vcg)
 apply(clarsimp)
 apply(frule h_t_valid_c_guard_cparent, simp, simp add: typ_uinfo_t_def)
 apply(clarsimp simp: typ_heap_simps h_t_valid_clift_Some_iff)
 apply(frule clift_subtype, simp+)
 apply(simp add: %(name)s_get_tag_def mask_shift_simps guard_simps)
done'''],


'empty_union_new_spec' : [
'''lemma (in ''' + loc_name + ''') ''' \
      '''%(name)s_%(block)s_new_spec:
  "\<forall>s. \<Gamma> \<turnstile> {s}
       \<acute>ret__struct_%(name)s_C :== ''' \
    '''PROC %(name)s_%(block)s_new()
       \<lbrace>%(name)s_get_tag \<acute>ret__struct_%(name)s_C = ''' \
     '''scast %(name)s_%(block)s\<rbrace>"''',
''' apply(rule allI, rule conseqPre, vcg)
 apply(clarsimp simp:guard_simps
                     %(name)s_lift_def
                     Let_def
                     %(name)s_get_tag_def
                     mask_shift_simps
                     %(name)s_tag_defs
                     word_of_int_hom_syms)
done'''],

'union_new_spec' : [
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
''' apply(rule allI, rule conseqPre, vcg)
 apply(clarsimp simp:guard_simps o_def)
 apply(simp add:shift_over_ao_dists mask_def)
 apply(simp add:%(name)s_%(block)s_lift_def)
 apply(subst %(name)s_lift_%(block)s)
  apply(simp add:%(name)s_get_tag_def
                 mask_def
                 %(name)s_%(block)s_def
                 shift_over_ao_dists
                 word_ao_dist
                 word_bw_assocs
                 multi_shift_simps
                 word_and_max_word
                 max_word_def
                 word_size)+
done'''],

'ptr_empty_union_new_spec_direct' : [
    ptr_empty_union_new_template(direct_ptr_name),
'''sorry (* ptr_empty_union_new_spec_direct *)'''],

'ptr_empty_union_new_spec_path' : [
    ptr_empty_union_new_template(path_ptr_name),
''' unfolding ptrval_def
 apply(rule allI, rule conseqPre, vcg)
 apply(clarsimp)
 apply(frule h_t_valid_c_guard_cparent, simp, simp add: typ_uinfo_t_def)
 apply(clarsimp simp: h_t_valid_clift_Some_iff)
 apply(frule clift_subtype, simp+)
 apply(clarsimp simp: typ_heap_simps c_guard_clift)

 apply(simp add: guard_simps mask_shift_simps
                 %(name)s_tag_defs[THEN tag_eq_to_tag_masked_eq])

 apply(simp add: parent_update_child[OF c_guard_clift]
                 typ_heap_simps c_guard_clift)

 apply(simp add: o_def, rule exI, rule conjI[OF _ refl])

 apply(simp add: %(name)s_get_tag_def %(name)s_tag_defs
                 guard_simps mask_shift_simps)
done
'''],

'ptr_union_new_spec_direct' : [
    ptr_union_new_template(direct_ptr_name),
'''sorry (* ptr_union_new_spec_direct *)'''],

'ptr_union_new_spec_path' : [
    ptr_union_new_template(path_ptr_name),
''' unfolding ptrval_def
 apply(rule allI, rule conseqPre, vcg)
 apply(clarsimp)
 apply(frule h_t_valid_c_guard_cparent, simp, simp add: typ_uinfo_t_def)
 apply(drule h_t_valid_clift_Some_iff[THEN iffD1], erule exE)
 apply(frule clift_subtype, simp, simp)
 apply(clarsimp simp: typ_heap_simps c_guard_clift)

 apply(simp add: guard_simps mask_shift_simps
                 %(name)s_tag_defs[THEN tag_eq_to_tag_masked_eq])

 apply(simp add: parent_update_child[OF c_guard_clift]
                 typ_heap_simps c_guard_clift)

 apply(simp add: o_def %(name)s_%(block)s_lift_def)
 apply(simp only: %(name)s_lift_%(block)s cong: rev_conj_cong)
 apply(rule exI, rule conjI[rotated], rule conjI[OF _ refl])
  apply (simp_all add: %(name)s_get_tag_eq_x %(name)s_tag_defs mask_shift_simps)
done'''],

'union_get_spec' : [
'''lemma (in ''' + loc_name + ''') ''' \
'''%(name)s_%(block)s_get_%(field)s_spec:
  "\<forall>s. \<Gamma> \<turnstile> ''' \
'''\<lbrace>s. %(name)s_get_tag \<acute>%(name)s = ''' \
            '''scast %(name)s_%(block)s\<rbrace>
       \<acute>ret__unsigned_long :== ''' \
       '''PROC %(name)s_%(block)s_get_%(field)s(\<acute>%(name)s)
       \<lbrace>\<acute>ret__unsigned_long = ''' \
       '''%(name)s_%(block)s_CL.%(field)s_CL ''' \
       '''(%(name)s_%(block)s_lift \<^bsup>s\<^esup>%(name)s)''' \
       '''\<rbrace>"''', # AND %(mask)s
''' apply(rule allI, rule conseqPre, vcg)
 apply(clarsimp simp:guard_simps)
 apply(simp add:%(name)s_%(block)s_lift_def)
 apply(subst %(name)s_lift_%(block)s)
  apply(simp add:o_def
                 %(name)s_get_tag_def
                 %(name)s_%(block)s_def
                 mask_def
                 word_size
                 shift_over_ao_dists)
 apply(subst %(name)s_lift_%(block)s, simp)?
 apply(simp add:o_def
                %(name)s_get_tag_def
                %(name)s_%(block)s_def
                mask_def
                word_size
                shift_over_ao_dists
                multi_shift_simps
                word_bw_assocs
                word_ao_dist
                word_and_max_word
                max_word_def
                ucast_def)
done'''],

'union_set_spec' : [
'''lemma (in ''' + loc_name + ''') ''' \
       '''%(name)s_%(block)s_set_%(field)s_spec:
  "\<forall>s. \<Gamma> \<turnstile> ''' \
'''\<lbrace>s. %(name)s_get_tag \<acute>%(name)s = ''' \
            '''scast %(name)s_%(block)s\<rbrace>
       \<acute>ret__struct_%(name)s_C :== ''' \
    '''PROC %(name)s_%(block)s_set_%(field)s(\<acute>%(name)s, \<acute>v)
       \<lbrace>%(name)s_%(block)s_lift \<acute>ret__struct_%(name)s_C = ''' \
    '''%(name)s_%(block)s_lift \<^bsup>s\<^esup>%(name)s \<lparr> ''' \
    '''%(name)s_%(block)s_CL.%(field)s_CL ''' \
    ''':= \<^bsup>s\<^esup>v AND %(mask)s\<rparr> \<and>
        %(name)s_get_tag \<acute>ret__struct_%(name)s_C = ''' \
     '''scast %(name)s_%(block)s\<rbrace>"''',
''' apply(rule allI, rule conseqPre, vcg)
 apply(clarsimp)
 apply(simp add:%(name)s_lift_def
                %(name)s_%(block)s_lift_def
                mask_shift_simps
                guard_simps
                Let_def
                %(name)s_get_tag_eq_x
                %(tag_mask_helpers)s
                %(name)s_%(block)s_update_def
                %(name)s_tag_defs)
done'''],

'ptr_union_get_spec_direct' : [
    ptr_union_get_template(direct_ptr_name),
''' unfolding ptrval_def
 apply(rule allI, rule conseqPre, vcg)
 apply(clarsimp simp: typ_heap_simps h_t_valid_clift_Some_iff
                      guard_simps mask_shift_simps
                      %(name)s_lift_%(block)s %(name)s_%(block)s_lift_def)
done
'''],

'ptr_union_get_spec_path' : [
    ptr_union_get_template(path_ptr_name),
'''unfolding ptrval_def
  apply(rule allI, rule conseqPre, vcg)
  apply(clarsimp)
  apply(frule h_t_valid_c_guard_cparent, simp, simp add: typ_uinfo_t_def)
  apply(drule h_t_valid_clift_Some_iff[THEN iffD1], erule exE)
  apply(frule clift_subtype, simp, simp)
  apply(clarsimp simp: typ_heap_simps c_guard_clift)
  apply(simp add: guard_simps mask_shift_simps)
  apply(simp add:%(name)s_%(block)s_lift_def)
  apply(subst %(name)s_lift_%(block)s)
  apply simp+
  done
 (* ptr_union_get_spec_path *)'''],

'ptr_union_set_spec_direct' : [
        ptr_union_set_template(direct_ptr_name),
'''sorry (* ptr_union_set_spec_direct *)'''],


'ptr_union_set_spec_path' : [
        ptr_union_set_template(path_ptr_name),
''' unfolding ptrval_def
 apply(rule allI, rule conseqPre, vcg)
 apply(clarsimp)
 apply(frule h_t_valid_c_guard_cparent, simp, simp add: typ_uinfo_t_def)
 apply(drule h_t_valid_clift_Some_iff[THEN iffD1], erule exE)
 apply(frule clift_subtype, simp, simp)
 apply(clarsimp simp: typ_heap_simps c_guard_clift)

 apply(simp add: guard_simps mask_shift_simps
                 %(name)s_tag_defs[THEN tag_eq_to_tag_masked_eq])

 apply(simp add: parent_update_child[OF c_guard_clift]
                 typ_heap_simps c_guard_clift)

 apply(simp add: o_def %(name)s_%(block)s_lift_def)
 apply(simp only: %(name)s_lift_%(block)s cong: rev_conj_cong)
 apply(rule exI, rule conjI[rotated], rule conjI[OF _ refl])
  apply (simp_all add: %(name)s_get_tag_eq_x %(name)s_tag_defs mask_shift_simps)
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

## AST objects

def emit_named(name, params, string):
    # Emit a named definition/proof, only when the given name is in
    # params.names

     if(name in params.names):
        print >>params.output, string
        print >>params.output

# This calculates substs for each proof, which is probably inefficient.  Meh
def emit_named_ptr_proof(fn_name, params, name, type_map, toptps, prf_prefix, substs):
    name_C = name + '_C'

    if type_map.has_key(name_C):
        toptp, path = type_map[name_C]

        substs['cslift_other'] = ' \<and> '.join(
            [("(cslift t :: %s typ_heap) = (cslift s)" % x) for x in toptps if x != toptp])

        substs['access_path'] = '(' + reduce(lambda x, y: y + ' (' + x + ')', ['the (ptrval s)'] + path) + ')'

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

class TaggedUnion:
    def __init__(self, name, tagname, classes, tags):
        self.name = name
        self.tagname = tagname

        # Check for duplicate tags
        used_names = set()
        used_values = set()
        for name, value in tags:
            if name in used_names:
                raise ValueError("Duplicate tag name %s" % name)
            if value in used_values:
                raise ValueError("Duplicate tag value %d" % value)

            used_names.add(name)
            used_values.add(value)
        self.classes = dict(classes)
        self.tags = tags

    def resolve(self, params, symtab):
        # Grab block references for tags
        self.tags = [(name, value, symtab[name]) for name, value in self.tags]
        self.make_classes(params)

        # Ensure that block sizes and tag size & position match for
        # all tags in the union
        union_base = None
        union_size = None
        for name, value, ref in self.tags:
            _tag_offset, _tag_size, _tag_high = ref.field_map[self.tagname]

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

            if _tag_offset != self.tag_offset[_tag_size]:
                raise ValueError("Tag offset mismatch for element %s"
                                 " of tagged union %s" % (name, self.name))

            self.assert_value_in_class(name, value, _tag_size)

            if _tag_high:
                raise ValueError("Tag field is high-aligned for element %s"
                                 " of tagged union %s" % (name, self.name))

            # Flag block as belonging to a tagged union
            ref.tagged = True

        self.union_base = union_base
        self.union_size = union_size

    def set_base(self, base):
        self.base = base
        self.multiple = self.union_size / base

        tag_index = None
        for w in self.tag_offset:
            tag_offset = self.tag_offset[w]

            if tag_index is None:
                tag_index = tag_offset / base

            if (tag_offset / base) != tag_index:
                raise ValueError(
                    "The tag field of tagged union %s"
                    " is in a different word (%s) to the others (%s)."
                    % (self.name, hex(tag_offset / base), hex(tag_index)))

    def generate_hol_proofs(self, params, type_map):
        output = params.output

        # Add fixed simp rule for struct
        print >>output, "lemmas %(name)s_C_words_C_fl_simp[simp] = "\
                        "%(name)s_C_words_C_fl[simplified]" % \
                        {"name": self.name}
        print >>output

        # Generate struct field pointer proofs
        substs = {"name": self.name,
                  "words": self.multiple}

        print >>output, make_proof('words_NULL_proof',
                                   substs, params.sorry)
        print >>output

        print >>output, make_proof('words_aligned_proof',
                                   substs, params.sorry)
        print >>output

        print >>output, make_proof('words_ptr_safe_proof',
                                   substs, params.sorry)
        print >>output

        # Generate struct lemmas
        print >>output, struct_lemmas_template % {"name": self.name}
        print >>output

        # Generate get_tag specs
        substs = {"name": self.name,
                  "tagname": self.tagname}

        if not params.skip_modifies:
            emit_named("%(name)s_get_%(tagname)s" % substs, params,
                make_proof('const_modifies_proof',
                    {"fun_name": "%(name)s_get_%(tagname)s" % substs, \
                     "args": ', '.join(["\<acute>ret__unsigned_long", \
                                        "\<acute>%(name)s" % substs])},
                    params.sorry))
            emit_named("%(name)s_ptr_get_%(tagname)s" % substs, params,
                make_proof('const_modifies_proof',
                    {"fun_name": "%(name)s_ptr_get_%(tagname)s" % substs, \
                     "args": ', '.join(["\<acute>ret__unsigned_long", \
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
                        if field != self.tagname]

            # Generate modifies proof
            if not params.skip_modifies:
                emit_named("%s_%s_new" % (self.name, ref.name), params,
                           make_proof('const_modifies_proof',
                               {"fun_name": "%s_%s_new" % \
                                            (self.name, ref.name), \
                                "args": ', '.join([
                                "\<acute>ret__struct_%(name)s_C" % substs] + \
                                arg_list)},
                               params.sorry))

                emit_named("%s_%s_ptr_new" % (self.name, ref.name), params,
                           make_proof('ptr_set_modifies_proof',
                               {"fun_name": "%s_%s_ptr_new" % \
                                            (self.name, ref.name), \
                                "args": ', '.join([
                                "\<acute>ret__struct_%(name)s_C" % substs] + \
                                arg_list)},
                               params.sorry))

            if len(arg_list) == 0:
                # For an empty block:
                emit_named("%s_%s_new" % (self.name, ref.name), params,
                           make_proof('empty_union_new_spec',
                               {"name": self.name, \
                                "block": ref.name},
                               params.sorry))

                emit_named_ptr_proof("%s_%s_ptr_new" % (self.name, ref.name), params, self.name, 
                                     type_map, params.toplevel_types, 
                                     'ptr_empty_union_new_spec',
                                     {"name": self.name, \
                                      "block": ref.name})
            else:
                field_eq_list = []
                for field in ref.visible_order:
                    offset, size, high = ref.field_map[field]

                    if field == self.tagname:
                        continue

                    if high:
                        mask = "NOT (mask %d)" % (self.base - size)
                    else:
                        mask = "(mask %d)" % size

                    field_eq_list.append(
                        "%s_%s_CL.%s_CL = \<^bsup>s\<^esup>%s AND %s" % \
                        (self.name, ref.name, field, field, mask))
                field_eqs = ',\n          '.join(field_eq_list)

                emit_named("%s_%s_new" % (self.name, ref.name), params,
                           make_proof('union_new_spec',
                               {"name": self.name, \
                                "block": ref.name, \
                                "args": ', '.join(arg_list), \
                                "field_eqs": field_eqs},
                               params.sorry))

                emit_named_ptr_proof("%s_%s_ptr_new" % (self.name, ref.name), params, self.name, 
                                     type_map, params.toplevel_types, 
                                     'ptr_union_new_spec',
                                     {"name": self.name, \
                                      "block": ref.name, \
                                      "args": ', '.join(arg_list), \
                                      "field_eqs": field_eqs})
                
            _, size, _ = ref.field_map[self.tagname]
            if any([w for w in self.widths if w < size]):
                tag_mask_helpers = ("%s_%s_tag_mask_helpers"
                                        % (self.name, ref.name))
            else:
                tag_mask_helpers = ""

            # Generate get/set specs
            for (field, offset, size, high) in ref.fields:
                if field == self.tagname:
                    continue

                if high:
                    mask = "NOT (mask %d)" % (ref.base - size)
                else:
                    mask = "(mask %d)" % size

                substs = {"name":  self.name, \
                          "block": ref.name, \
                          "field": field, \
                          "mask":  mask, \
                          "tag_mask_helpers" : tag_mask_helpers}

                # Get modifies spec
                if not params.skip_modifies:
                    emit_named("%s_%s_get_%s" % (self.name, ref.name, field),
                               params,
                               make_proof('const_modifies_proof',
                                   {"fun_name": "%s_%s_get_%s" % \
                                        (self.name, ref.name, field), \
                                    "args": ', '.join([
                                    "\<acute>ret__unsigned_long",
                                    "\<acute>%s" % self.name] )},
                                   params.sorry))

                    emit_named("%s_%s_ptr_get_%s" % (self.name, ref.name, field),
                               params,
                               make_proof('const_modifies_proof',
                                   {"fun_name": "%s_%s_ptr_get_%s" % \
                                        (self.name, ref.name, field), \
                                    "args": ', '.join([
                                    "\<acute>ret__unsigned_long",
                                    "\<acute>%s_ptr" % self.name] )},
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
                                   {"fun_name": "%s_%s_set_%s" % \
                                        (self.name, ref.name, field), \
                                    "args": ', '.join([
                                    "\<acute>ret__struct_%s_C" % self.name, 
                                    "\<acute>%s" % self.name,
                                    "\<acute>v"] )},
                                   params.sorry))
                    
                    emit_named("%s_%s_ptr_set_%s" % (self.name, ref.name, field),
                               params,
                               make_proof('ptr_set_modifies_proof',
                                   {"fun_name": "%s_%s_ptr_set_%s" % \
                                        (self.name, ref.name, field), \
                                    "args": ', '.join([
                                    "\<acute>%s_ptr" % self.name,
                                    "\<acute>v"] )},
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

        def gen_name(ref_name, capitalise = False):
            # Create datatype generator/type name for a block
            if capitalise:
                return "%s_%s" % \
                       (self.name[0].upper() + self.name[1:], ref_name)
            else:
                return "%s_%s" % (self.name, ref_name)

        # Generate block records with tag field removed
        for name, value, ref in self.tags:
            if ref.generate_hol_defs(params, \
                                     suppressed_field = self.tagname, \
                                     prefix="%s_" % self.name, \
                                     in_union = True):
                empty_blocks[ref] = True

        constructor_exprs = []
        for name, value, ref in self.tags:
            if ref in empty_blocks:
                constructor_exprs.append(gen_name(name, True))
            else:
                constructor_exprs.append("%s %s_CL" % \
                    (gen_name(name, True), gen_name(name)))

        print >>output, "datatype %s_CL =\n    %s\n" % \
                        (self.name, '\n  | '.join(constructor_exprs))

        # Generate get_tag definition
        subs = {"name":      self.name,
                "base":      self.base }

        templates = ([union_get_tag_def_entry_template] * (len(self.widths) - 1)
                   + [union_get_tag_def_final_template])

        fs = (union_get_tag_def_header_template % subs
            + "".join([template %
                         dict(subs,
                              tag_size=width,
                              classmask=self.word_classmask(width),
                              tag_index=self.tag_offset[width] / self.base,
                              tag_shift=self.tag_offset[width] % self.base)
                       for template, width in zip(templates, self.widths)])
            + union_get_tag_def_footer_template % subs)

        print >>output, fs
        print >>output

        # Generate get_tag_eq_x lemma
        templates = ([union_get_tag_eq_x_def_entry_template]
                        * (len(self.widths) - 1)
                   + [union_get_tag_eq_x_def_final_template])

        fs = (union_get_tag_eq_x_def_header_template % subs
            + "".join([template %
                         dict(subs,
                              tag_size=width,
                              classmask=self.word_classmask(width),
                              tag_index=self.tag_offset[width] / self.base,
                              tag_shift=self.tag_offset[width] % self.base)
                       for template, width in zip(templates, self.widths)])
            + union_get_tag_eq_x_def_footer_template % subs)

        print >>output, fs
        print >>output

        # Generate mask helper lemmas

        for name, value, ref in self.tags:
            offset, size, _ = ref.field_map[self.tagname]
            part_widths = [w for w in self.widths if w < size]
            if part_widths:
                subs = {"name":         self.name,
                        "block":        name,
                        "full_mask":    hex(2 ** size - 1),
                        "full_value":   hex(value) }

                fs = (union_tag_mask_helpers_header_template % subs
                    + "".join([union_tag_mask_helpers_entry_template %
                               dict(subs, part_mask=hex(2 ** pw - 1),
                                          part_value=hex(value & (2 ** pw - 1)))
                               for pw in part_widths])
                    + union_tag_mask_helpers_footer_template)

                print >>output, fs
                print >>output

        # Generate lift definition
        collapse_proofs = ""
        tag_cases = []
        for name, value, ref in self.tags:
            field_inits = []

            for field in ref.visible_order:
                offset, size, high = ref.field_map[field]

                if field == self.tagname: continue

                index = offset / self.base

                if high:
                    shift_op = "<<"
                    shift = self.base - size - (offset % self.base)
                else:
                    shift_op = ">>"
                    shift = offset % self.base

                initialiser = \
                    "%s_CL.%s_CL = ((index (%s_C.words_C %s) %d) %s %d)" % \
                    (gen_name(name), field, self.name, self.name, \
                     index, shift_op, shift)

                if size < self.base:
                    if high:
                        mask = ((1 << size) - 1) << (self.base - size)
                    else:
                        mask = (1 << size) - 1

                    initialiser += " AND %d" % mask

                field_inits.append("\n       " + initialiser)

            if len(field_inits) == 0:
                value = gen_name(name, True)
            else:
                value = "%s \<lparr> %s \<rparr>" % \
                    (gen_name(name, True), ','.join(field_inits))

            tag_cases.append("if tag = scast %s then Some (%s)" % \
                             (gen_name(name), value))

            collapse_proofs += \
                make_proof("lift_collapse_proof",
                           {"name": self.name, \
                            "block": name, \
                            "value": value},
                           params.sorry)
            collapse_proofs += "\n\n"

        print >>output, union_lift_def_template % \
                        {"name": self.name, \
                         "tag_cases": '\n     else '.join(tag_cases)}
        print >>output

        print >>output, collapse_proofs

        block_lift_lemmas = "lemmas %s_lifts = \n" % self.name
        # Generate lifted access/update definitions, and abstract lifters
        for name, value, ref in self.tags:
            # Don't generate accessors if the block (minus tag) is empty
            if ref in empty_blocks: continue

            substs = {"union": self.name, \
                      "block": name, \
                      "generator": gen_name(name, True)}

            for t in [union_access_def_template, union_update_def_template]:
                print >>output, t % substs
                print >>output

            print >>output, block_lift_def_template % substs
            print >>output

            print >>output, block_lift_lemma_template % substs
            print >>output

            block_lift_lemmas += "\t%(union)s_%(block)s_lift\n" % substs
        
        print >>output, block_lift_lemmas
        print >>output

    def generate(self, params):
        output = params.output

        # Generate typedef
        print >>output, typedef_template % \
                        {"base": self.base, \
                         "name": self.name, \
                         "multiple": self.multiple}
        print >>output

        # Generate tag enum
        print >>output, "enum %s_tag {" % self.name
        if len(self.tags) > 0:
            for name, value, ref in self.tags[:-1]:
                print >>output, "    %s_%s = %d," % (self.name, name, value)
            name, value, ref = self.tags[-1];
            print >>output, "    %s_%s = %d" % (self.name, name, value)
        print >>output, "};"
        print >>output, "typedef enum %s_tag %s_tag_t;" % \
                        (self.name, self.name)
        print >>output
        
        subs = {\
            'union': self.name, \
            'base':  self.union_base, \
            'tagname': self.tagname}

        # Generate tag reader
        templates = ([tag_reader_entry_template] * (len(self.widths) - 1)
                   + [tag_reader_final_template])

        fs = (tag_reader_header_template % subs
            + "".join([template %
                         dict(subs,
                              mask=2 ** width - 1,
                              classmask=self.word_classmask(width),
                              index=self.tag_offset[width] / self.base,
                              shift=self.tag_offset[width] % self.base)
                       for template, width in zip(templates, self.widths)])
            + tag_reader_footer_template % subs)

        emit_named("%s_get_%s" % (self.name, self.tagname), params, fs)

	# Generate tag eq reader

        templates = ([tag_eq_reader_entry_template] * (len(self.widths) - 1)
                   + [tag_eq_reader_final_template])

        fs = (tag_eq_reader_header_template % subs
            + "".join([template %
                         dict(subs,
                              mask=2 ** width - 1,
                              classmask=self.word_classmask(width),
                              index=self.tag_offset[width] / self.base,
                              shift=self.tag_offset[width] % self.base)
                       for template, width in zip(templates, self.widths)])
            + tag_eq_reader_footer_template % subs)

        emit_named("%s_%s_equals" % (self.name, self.tagname), params, fs)

        # Generate pointer lifted tag reader
        templates = ([ptr_tag_reader_entry_template] * (len(self.widths) - 1)
                   + [ptr_tag_reader_final_template])

        fs = (ptr_tag_reader_header_template % subs
            + "".join([template %
                         dict(subs,
                              mask=2 ** width - 1,
                              classmask=self.word_classmask(width),
                              index=self.tag_offset[width] / self.base,
                              shift=self.tag_offset[width] % self.base)
                       for template, width in zip(templates, self.widths)])
            + ptr_tag_reader_footer_template % subs)

        emit_named("%s_ptr_get_%s" % (self.name, self.tagname), params, fs)
        
        for name, value, ref in self.tags:
            # Generate generators
            arg_list = ["uint%d_t %s" % (self.base, field) for \
                            field in ref.visible_order if
                            field != self.tagname]

            if len(arg_list) == 0:
                args = 'void'
            else:
                args = ', '.join(arg_list)

            ptr_args = ', '.join(["%s_t *%s_ptr" % (self.name, self.name)] + \
                                 arg_list)

            word_inits = ["    %s.words[%d] = 0;" % (self.name, i) \
                          for i in xrange(self.multiple)]

            ptr_word_inits = ["    %s_ptr->words[%d] = 0;" % (self.name, i) \
                              for i in xrange(self.multiple)]

            field_inits = []
            ptr_field_inits = []
            for field in ref.visible_order:
                offset, size, high = ref.field_map[field]

                if field == self.tagname:
                    f_value = "%s_%s" % (self.name, name)
                else:
                    f_value = field

                index = offset / self.base
                if high:
                    shift_op = ">>"
                    shift = self.base - size - (offset % self.base)
                else:
                    shift_op = "<<"
                    shift = offset % self.base
                if size < self.base:
                    if high:
                        mask = ((1 << size) - 1) << (self.base - size)
                    else:
                        mask = (1 << size) - 1

                    field_inits.append("/* fail if user has passed bits that we will override */")
                    field_inits.append(
                        "    assert((%s & ~0x%x) == 0);\n" % (f_value, mask))
                    field_inits.append(
                        "    %s.words[%d] |= (%s & 0x%x) %s %d;" % \
                         (self.name, index, f_value, mask, shift_op, shift))

                    ptr_field_inits.append("/* fail if user has passed bits that we will override */")
                    ptr_field_inits.append(
                        "    assert((%s & ~0x%x) == 0);\n" % (f_value, mask))
                    ptr_field_inits.append(
                        "    %s_ptr->words[%d] |= (%s & 0x%x) %s %d;" % \
                        (self.name, index, f_value, mask, shift_op, shift))
                else:
                    field_inits.append(
                            "       %s.words[%d] |= %s %s %d;" % \
                        (self.name, index, f_value, shift_op, shift))

                    ptr_field_inits.append(
                        "    %s_ptr->words[%d] |= %s %s %d;" % \
                        (self.name, index, f_value, shift_op, shift))

            generator = union_generator_template % \
                {"block":        name, \
                 "union":        self.name, \
                 "args":         args, \
                 "word_inits":   '\n'.join(word_inits), \
                 "field_inits":  '\n'.join(field_inits)}

            ptr_generator = ptr_union_generator_template % \
                {"block":        name, \
                 "union":        self.name, \
                 "args":         ptr_args, \
                 "word_inits":   '\n'.join(ptr_word_inits), \
                 "field_inits":  '\n'.join(ptr_field_inits)}

            emit_named("%s_%s_new" % (self.name, name), params, generator)
            emit_named("%s_%s_ptr_new" % (self.name, name), params,
                       ptr_generator)

            # Generate field readers/writers
            tagnameoffset, tagnamesize, _ = ref.field_map[self.tagname]
            tagmask = (2 ** tagnamesize) - 1
            for field, offset, size, high in ref.fields:
                # Don't duplicate tag accessors
                if field == self.tagname: continue

                index = offset / self.base
                if high:
                    write_shift = ">>"
                    read_shift = "<<"
                    shift = self.base - size - (offset % self.base)
                else:
                    write_shift = "<<"
                    read_shift = ">>"
                    shift = offset % self.base
                mask = ((1 << size) - 1) << (offset % self.base)
                
                subs = {\
                    "block": ref.name, \
                    "field": field, \
                    "base": ref.base, \
                    "index": index, \
                    "shift": shift, \
                    "r_shift_op": read_shift, \
                    "w_shift_op": write_shift, \
                    "mask": mask, \
                    "tagindex": tagnameoffset / self.base, \
                    "tagshift": tagnameoffset % self.base, \
                    "tagmask": tagmask, \
                    "union": self.name}

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

        substs = {"union" : self.name, \
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
            self.classes = { used_widths[0] : 0 }

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
            print >> sys.stderr, "-----%s.%s" % (self.name, self.tagname)
            for w in widths:
                print >> sys.stderr, "{:2d} = {:s}".format(
                                        w, self.represent_class(w))

        self.widths = widths

class Block:
    def __init__(self, name, fields, visible_order):
        offset = 0
        _fields = []
        self.size = sum(size for _name, size, _high in fields)
        offset = self.size

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
        self.field_map = dict((name, (offset, size, high)) \
                              for name, offset, size, high in _fields)

        if not visible_order is None:
            missed_fields = set(self.field_map.keys())

            for _name in visible_order:
                if not self.field_map.has_key(_name):
                    raise ValueError("Nonexistent field '%s' in visible_order"
                                     % _name)
                missed_fields.remove(_name)

            if len(missed_fields) > 0:
                raise ValueError("Fields %s missing from visible_order" % \
                                 str([x for x in missed_fields]))

            self.visible_order = visible_order

    def set_base(self, base):
        self.base = base
        if self.size % base != 0:
            raise ValueError("Size of block %s not a multiple of base" \
                             % self.name)
        self.multiple = self.size / base
        for name, offset, size, high in self.fields:
            if offset / base != (offset+size-1) / base:
                raise ValueError("Field %s of block %s " \
                                 "crosses a word boundary" \
                                 % (name, self.name))

    def generate_hol_defs(self, params, suppressed_field=None, \
                                prefix="", in_union = False):
        output = params.output

        # Don't generate raw records for blocks in tagged unions
        if self.tagged and not in_union: return

        _name = prefix + self.name

        # Generate record def
        out = "record %s_CL =\n" % _name

        empty = True

        for field in self.visible_order:
            if suppressed_field == field:
                continue

            empty = False

            out += '    %s_CL :: "word%d"\n' % (field, self.base)

        word_updates = ""

        if not empty:
            print >>output, out

        # Generate lift definition
        if not in_union:
            field_inits = []

            for name in self.visible_order:
                offset, size, high = self.field_map[name]

                index = offset / self.base

                if high:
                    shift_op = "<<"
                    shift = self.base - size - (offset % self.base)
                else:
                    shift_op = ">>"
                    shift = offset % self.base

                initialiser = \
                    "%s_CL.%s_CL = ((index (%s_C.words_C %s) %d) %s %d)" % \
                    (self.name, name, self.name, self.name, \
                     index, shift_op, shift)

                if size < self.base:
                    if high:
                        mask = ((1 << size) - 1) << (self.base - size)
                    else:
                        mask = (1 << size) - 1

                    initialiser += " AND %d" % mask

                field_inits.append(initialiser)

            print >>output, lift_def_template % \
                            {"name": self.name, \
                             "fields": ',\n       '.join(field_inits)}
            print >>output

        return empty

    def generate_hol_proofs(self, params, type_map):
        output = params.output

        if self.tagged: return

        # Add fixed simp rule for struct
        print >>output, "lemmas %(name)s_C_words_C_fl_simp[simp] = "\
                        "%(name)s_C_words_C_fl[simplified]" % \
                        {"name": self.name}
        print >>output

        # Generate struct field pointer proofs
        substs = {"name": self.name,
                  "words": self.multiple}

        print >>output, make_proof('words_NULL_proof',
                                   substs, params.sorry)
        print >>output

        print >>output, make_proof('words_aligned_proof',
                                   substs, params.sorry)
        print >>output

        print >>output, make_proof('words_ptr_safe_proof',
                                   substs, params.sorry)
        print >>output

        # Generate struct lemmas
        print >>output, struct_lemmas_template % {"name": self.name}
        print >>output

        # Generate struct_new specs
        arg_list = ["\<acute>" + field for
                    (field, offset, size, high) in self.fields]

        if not params.skip_modifies:
            emit_named("%s_new" % self.name, params,
                       make_proof('const_modifies_proof',
                           {"fun_name": "%s_new" % self.name, \
                            "args": ', '.join(["\<acute>ret__struct_%s_C" % \
                                               self.name] + \
                                              arg_list)},
                           params.sorry))
            # FIXME: ptr_new (doesn't seem to be used)

        field_eq_list = []
        for (field, offset, size, high) in self.fields:
            if high:
                mask = "NOT (mask %d)" % (self.base - size)
            else:
                mask = "(mask %d)" % size

            field_eq_list.append("%s_CL.%s_CL = \<^bsup>s\<^esup>%s AND %s" % \
                                 (self.name, field, field, mask))
        field_eqs = ',\n          '.join(field_eq_list)

        emit_named("%s_new" % self.name, params,
                   make_proof('new_spec',
                       {"name": self.name, \
                        "args": ', '.join(arg_list), \
                        "field_eqs": field_eqs},
                       params.sorry))

        emit_named_ptr_proof("%s_ptr_new" % self.name, params, self.name, 
                             type_map, params.toplevel_types, 
                             'ptr_new_spec',
                             {"name": self.name, \
                              "args": ', '.join(arg_list), \
                              "field_eqs": field_eqs})

        # Generate get/set specs
        for (field, offset, size, high) in self.fields:
            if high:
                mask = "NOT (mask %d)" % (self.base - size)
            else:
                mask = "(mask %d)" % size

            substs = {"name": self.name, \
                      "field": field, \
                      "mask": mask}

            if not params.skip_modifies:
                # Get modifies spec
                emit_named("%s_get_%s" % (self.name, field), params,
                           make_proof('const_modifies_proof',
                               {"fun_name": "%s_get_%s" % (self.name, field), \
                                "args": ', '.join([
                                "\<acute>ret__unsigned_long", 
                                "\<acute>%s" % self.name] )},
                               params.sorry))

                # Ptr get modifies spec
                emit_named("%s_ptr_get_%s" % (self.name, field), params,
                           make_proof('const_modifies_proof',
                               {"fun_name": "%s_ptr_get_%s" % (self.name, field), \
                                "args": ', '.join([
                                "\<acute>ret__unsigned_long", 
                                "\<acute>%s_ptr" % self.name] )},
                               params.sorry))


            # Get spec
            emit_named("%s_get_%s" % (self.name, field), params,
                        make_proof('get_spec', substs, params.sorry))

            if not params.skip_modifies:
                # Set modifies spec
                emit_named("%s_set_%s" % (self.name, field), params,
                           make_proof('const_modifies_proof',
                               {"fun_name": "%s_set_%s" % (self.name, field), \
                                "args": ', '.join([
                                "\<acute>ret__struct_%s_C" % self.name, 
                                "\<acute>%s" % self.name,
                                "\<acute>v"] )},
                               params.sorry))
                
                emit_named("%s_ptr_set_%s" % (self.name, field), params,
                           make_proof('ptr_set_modifies_proof',
                               {"fun_name": "%s_ptr_set_%s" % (self.name, field), \
                                "args": ', '.join([
                                "\<acute>%s_ptr" % self.name,
                                "\<acute>v"] )},
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
        if self.tagged: return

        # Type definition
        print >>output, typedef_template % \
                        {"base": self.base, \
                         "name": self.name, \
                         "multiple": self.multiple}
        print >>output

        # Generator
        arg_list = ["uint%d_t %s" % (self.base, field) for \
                        field in self.visible_order]
        if len(arg_list) == 0:
            args = 'void'
        else:
            args = ', '.join(arg_list)

        ptr_args = ', '.join(["%s_t *%s_ptr" % (self.name, self.name)] + \
                             arg_list)

        word_inits = ["    %s.words[%d] = 0;" % (self.name, i) \
                      for i in xrange(self.multiple)]

        ptr_word_inits = ["    %s_ptr->words[%d] = 0;" % (self.name, i) \
                          for i in xrange(self.multiple)]

        field_inits = []
        ptr_field_inits = []
        for field, offset, size, high in self.fields:
            index = offset / self.base
            if high:
                shift_op = ">>"
                shift = self.base - size - (offset % self.base)
            else:
                shift_op = "<<"
                shift = offset % self.base
            if size < self.base:
                if high:
                    mask = ((1 << size) - 1) << (self.base - size)
                else:
                    mask = (1 << size) - 1

                field_inits.append("/* fail if user has passed bits that we will override */")
                field_inits.append(
                    "    assert((%s & ~0x%x) == 0);\n" % (field, mask))
                field_inits.append(
                    "    %s.words[%d] |= (%s & 0x%x) %s %d;" % \
                    (self.name, index, field, mask, shift_op, shift))

                ptr_field_inits.append("/* fail if user has passed bits that we will override */")
                ptr_field_inits.append(
                    "    assert((%s & ~0x%x) == 0);\n" % (field, mask))
                ptr_field_inits.append(
                    "    %s_ptr->words[%d] |= (%s & 0x%x) %s %d;" % \
                    (self.name, index, field, mask, shift_op, shift))
            else:
                field_inits.append(
                    "    %s.words[%d] |= %s %s %d;" % \
                    (self.name, index, field, shift_op, shift))

                ptr_field_inits.append(
                    "    %s_ptr->words[%d] |= %s %s %d;" % \
                    (self.name, index, field, shift_op, shift))

        generator = generator_template % \
            {"block":        self.name, \
             "args":         args, \
             "word_inits":   '\n'.join(word_inits), \
             "field_inits":  '\n'.join(field_inits)}

        ptr_generator = ptr_generator_template % \
            {"block":        self.name, \
             "args":         ptr_args, \
             "word_inits":   '\n'.join(ptr_word_inits), \
             "field_inits":  '\n'.join(ptr_field_inits)}

        emit_named("%s_new" % self.name, params, generator)
        emit_named("%s_ptr_new" % self.name, params, ptr_generator)

        # Accessors
        for field, offset, size, high in self.fields:
            index = offset / self.base
            if high:
                write_shift = ">>"
                read_shift = "<<"
                shift = self.base - size - (offset % self.base)
            else:
                write_shift = "<<"
                read_shift = ">>"
                shift = offset % self.base
            mask = ((1 << size) - 1) << (offset % self.base)
            
            subs = {\
                "block": self.name, \
                "field": field, \
                "base": self.base, \
                "index": index, \
                "shift": shift, \
                "r_shift_op": read_shift, \
                "w_shift_op": write_shift, \
                "mask": mask}

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
            if self.tagged: return []

            substs = {"block" : self.name}

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
            substs = {"block" : self.name, \
                      "union" : union.name}

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

def open_output(filename):
    """Open an output file for writing, recording its filename."""
    class OutputFile(object):
        def __init__(self, filename, file):
            self.filename = os.path.abspath(filename)
            self.file = file
        def write(self, *args, **kwargs):
            self.file.write(*args, **kwargs)
    return OutputFile(filename, open(filename, "w"))

## Toplevel
if __name__ == '__main__':
    # Parse arguments to set mode and grab I/O filenames
    params = {}
    in_filename = None
    in_file  = sys.stdin
    out_file = sys.stdout
    mode = 'c_defs'

    parser = optparse.OptionParser()
    parser.add_option('--c_defs', action='store_true', default=False)
    parser.add_option('--environment', action='store', default='sel4',
                      choices=INCLUDES.keys())
    parser.add_option('--hol_defs', action='store_true', default=False)
    parser.add_option('--hol_proofs', action='store_true', default=False)
    parser.add_option('--sorry_lemmas', action='store_true',
                      dest='sorry', default=False)
    parser.add_option('--prune', action='append',
                      dest="prune_files", default = [])
    parser.add_option('--toplevel', action='append',
                      dest="toplevel_types", default = [])
    parser.add_option('--umm_types', action='store',
                      dest="umm_types_file", default = None)        
    parser.add_option('--multifile_base', action='store', default=None)
    parser.add_option('--cspec-dir', action='store', default=None,
            help="Location of the 'cspec' directory containing 'KernelState_C'.")
    parser.add_option('--thy-output-path', action='store', default=None,
            help="Path that the output theory files will be located in.")
    parser.add_option('--skip_modifies', action='store_true', default=False)
    parser.add_option('--showclasses', action='store_true', default=False)
    parser.add_option('--debug', action='store_true', default=False)

    options, args = parser.parse_args()
    DEBUG = options.debug

    if len(args) > 0:
        in_filename = args[0]
        in_file = open(in_filename)

        if len(args) > 1:
            out_file = open_output(args[1])

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
            out_file.filename = os.path.abspath(options.thy_output_path)

    del parser

    options.output = out_file

    # Parse the spec
    lex.lex()
    yacc.yacc(debug=0)
    base, blocks, unions = yacc.parse(in_file.read())
    if not base in [8,16,32,64]:
        raise ValueError("Invalid base size: %d" % base)
    symtab = {}
    symtab.update(blocks)
    symtab.update(unions)
    for b in blocks.values():
        b.set_base(base)
    for u in unions.values():
        u.resolve(options, symtab)
        u.set_base(base)

    if not in_filename is None:
        base_filename = os.path.basename(in_filename).split('.')[0]

        # Generate the module name from the input filename
        module_name = base_filename

    # Prune list of names to generate
    name_list = []
    for e in blocks.values() + unions.values():
        name_list += e.make_names()

    # Sort the list of names by decreasing length.  This should have the
    # effect of making the match greedy, as any string will appear before
    # its (initial) substrings
    name_list.sort(key=len, reverse=True)
    if len(options.prune_files) > 0:
        search_re = re.compile('|'.join(name_list))

        pruned_names = set()
        for filename in options.prune_files:
            f = open(filename)
            string = f.read()
            for match in search_re.finditer(string):
                pruned_names.add(string[match.start():match.end()])
    else:
        pruned_names = set(name_list)

    options.names = pruned_names

    # Generate the output
    if options.hol_defs:
        # Fetch kernel
        if options.multifile_base is None:
            print >>out_file, "theory %s_defs imports \"%s/KernelState_C\" begin" % (
                    module_name, os.path.relpath(options.cspec_dir,
                        os.path.dirname(out_file.filename)))
            print >>out_file

            print >>out_file, defs_global_lemmas
            print >>out_file

            for e in blocks.values() + unions.values():
                e.generate_hol_defs(options)

            print >>out_file, "end"
        else:
            print >>out_file, "theory %s_defs imports" % module_name
            print >>out_file, "\"%s/KernelState_C\"" % (
                    os.path.relpath(options.cspec_dir,
                        os.path.dirname(out_file.filename)))
            for e in blocks.values() + unions.values():
                print >>out_file, "  %s_%s_defs" % (module_name, e.name)
            print >>out_file, "begin"
            print >>out_file, "end"

            for e in blocks.values() + unions.values():
                base_filename = \
                    os.path.basename(options.multifile_base).split('.')[0]
                submodule_name = base_filename + "_" + \
                                 e.name + "_defs"
                out_file = open_output(options.multifile_base + "_" +
                                e.name + "_defs" + ".thy")

                print >>out_file, "theory %s imports \"%s/KernelState_C\" begin" % (
                        submodule_name, os.path.relpath(options.cspec_dir,
                            os.path.dirname(out_file.filename)))
                print >>out_file

                options.output = out_file
                e.generate_hol_defs(options)

                print >>out_file, "end"
    elif options.hol_proofs:
        def is_bit_type(tp):
            return (umm.is_base(tp) & (umm.base_name(tp) in map(lambda e: e.name + '_C', blocks.values() + unions.values())))
        
        tps = umm.build_types(options.umm_types_file)
        type_map = {}

        # invert type map
        for toptp in options.toplevel_types:
            paths = umm.paths_to_type(tps, is_bit_type, toptp)

            for path, tp in paths:
                tp = umm.base_name(tp)
                
                if type_map.has_key(tp):
                    raise ValueError("Type %s has multiple parents" % tp)

                type_map[tp] = (toptp, path)
        
        if options.multifile_base is None:
            print >>out_file, \
                "theory %s_proofs imports %s_defs \"%s/KernelState_C\" begin" % (
                    module_name, module_name,
                        os.path.relpath(options.cspec_dir,
                            os.path.dirname(out_file.filename)))
            print >>out_file

            for toptp in options.toplevel_types:
                print >>out_file, \
                    ('abbreviation\n\t"cslift_all_but_%s s t \<equiv> ' % toptp + 
                     ('\n\t\t\<and> '.join([('(cslift s :: %s typ_heap) = cslift t' % x)
                                       for x in options.toplevel_types if x != toptp]))
                     + '"')
            print >>out_file
                
            print >>out_file, global_lemmas
            print >>out_file

            for e in blocks.values() + unions.values():
                e.generate_hol_proofs(options, type_map)

            print >>out_file, "end"
        else:
            # top types are broken here.
            print >>out_file, "theory %s_proofs imports" % module_name
            print >>out_file, "  \"%s/KernelState_C\"" % (
                os.path.relpath(options.cspec_dir,
                    os.path.dirname(out_file.filename)))

            for e in blocks.values() + unions.values():
                print >>out_file, "  %s_%s_proofs" % (module_name, e.name)
            print >>out_file, "begin"
            print >>out_file, "end"
            
            for e in blocks.values() + unions.values():
                base_filename = \
                    os.path.basename(options.multifile_base).split('.')[0]
                submodule_name = base_filename + "_" + \
                                 e.name + "_proofs"
                out_file = open_output(options.multifile_base + "_" +
                                e.name + "_proofs" + ".thy")

                print >>out_file, ("theory %s imports "
                        + "%s_%s_defs \"%s/KernelState_C\" begin") % (
                            submodule_name, base_filename, e.name,
                                os.path.relpath(options.cspec_dir,
                                    os.path.dirname(out_file.filename)))
                print >>out_file

                print >>out_file, global_lemmas
                print >>out_file

                options.output = out_file
                e.generate_hol_proofs(options, type_map)

                print >>out_file, "end"
    else:
        guard = re.sub(r'[^a-zA-Z0-9_]', '_', out_file.filename.upper())
        print >>out_file, "#ifndef %(guard)s\n#define %(guard)s\n" % \
            {'guard':guard}
        print >>out_file, '\n'.join(map(lambda x: '#include <%s>' % x,
            INCLUDES[options.environment]))
        for e in blocks.values() + unions.values():
            e.generate(options)
        print >>out_file, "#endif"
