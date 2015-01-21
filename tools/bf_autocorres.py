'''
Functions related to generating AutoCorres-based definitions and proofs from
the bitfield generator.
'''

import sys

def is_tagged_union(item):
    '''Returns true if something is a TaggedUnion instance'''
    try:
        return item.tagname is not None
    except AttributeError:
        return False

def function_prefix(group):
    if group is None:
        return ''
    return '%s_' % group

def struct(group, name):
    if group is None:
        return '%s_C' % name
    return '%s_C' % group

def get_args(item):
    if item.tagged:
        return item.visible_order[1:]
    return item.visible_order

def generate_def(item, group=None):
    '''Generate a specification of the constructor and readers of a generated
    type'''

    assert not is_tagged_union(item), \
        'TaggedUnions do not have associated AutoCorres definitions'

    prefix = function_prefix(group)
    struct_type = struct(group, item.name)

    defs = []

    # Construct the type signature of the constructor
    argv = get_args(item)
    argc = len(argv)
    assert argc > 0, 'Structure definition contains no fields'
    typesig = '%s \\<Rightarrow> %s' % \
        (' \\<Rightarrow> '.join(['word%d' % item.base] * argc),
         struct_type)

    # Here we essentially mirror the logic used when generating the code for
    # these functions, but with a few extra niceties as humans will actually
    # need to read the Isabelle definitions. E.g. we omit shifts by 0.
    members = [[]] * item.multiple
    for field_index, (field, offset, size, high) in enumerate(item.fields):
        index = offset / item.base
        assert index < len(members), \
            'Member index greater than the total size of the type. Malformed ' \
            'structure definition?'
        # Mangle the field names with a trailing ' as some commonly used ones
        # conflict with Isabelle keywords
        if item.tagged and field_index == 0:
            # We're currently looking at the field that represents the tag. We
            # need to scast it because the C parser lifts it as a signed type
            # regardless of how the enum is defined.
            assert group is not None, \
                'Group not specified for block within a tagged union'
            val_new = '(scast %s_%s)' % (group, item.name)
        else:
            val_new = '%s\'' % field
        val_reader = 'index (%s.words_C x\') %d' % (struct_type, index)
        if size < item.base:
            # We need to mask the input value
            if high:
                mask = ((1 << size) - 1) << (item.base - size)
            else:
                mask = (1 << size) - 1
            val_new = '(%s && 0x%x)' % (val_new, mask)
            mask = ((1 << size) - 1) << (offset % item.base)
            val_reader = '(%s && 0x%x)' % (val_reader, mask)
        if high:
            shift_op = '>>'
            shift = item.base - size - (offset % item.base)
            read_shift = '<<'
        else:
            shift_op = '<<'
            shift = offset % item.base
            read_shift = '>>'
        if shift > 0:
            # We need to shift the input value
            val_new = '(%s %s %d)' % (val_new, shift_op, shift)
            val_reader = '(%s %s %d)' % (val_reader, read_shift, shift)

        # Collect this member for later use in the constructor definition
        members[index].append(val_new)

        # The remainder (reader and writer definition) is irrelevant if we are
        # currently looking at the field representing the tag
        if item.tagged and field_index == 0:
            continue

        # Create the reader definition for this member
        defs.append('definition\n' \
                    '  %(prefix)s%(name)s_get_%(field)s :: "%(struct_type)s \\<Rightarrow> ' \
                        'word%(base)d"\n' \
                    'where\n' \
                    '  "%(prefix)s%(name)s_get_%(field)s x\' = %(defn)s"' % {
                        'prefix':prefix,
                        'name':item.name,
                        'struct_type':struct_type,
                        'field':field,
                        'base':item.base,
                        'defn':val_reader,
                    })

        # Create the writer definition for this member
        defn = []
        for i in xrange(item.multiple):
            if i == index:
                # `foo ^ ((1 << item.base) - 1)` acts as a bitwise-not
                # substitute here because Python doesn't have a native
                # understanding of the width of this type.
                mask_out = (((1 << size) - 1) << (offset % item.base)) ^ ((1 << item.base) - 1)
                defn.append('%(index)d \\<Rightarrow> ((index ' \
                                '(%(struct_type)s.words_C y\'\') %(index)d) ' \
                                '&& 0x%(mask_out)x) || %(value)s' % {
                                    'struct_type':struct_type,
                                    'index':i,
                                    'mask_out':mask_out,
                                    'value':val_new,
                                })
            else:
                defn.append('%(index)d \\<Rightarrow> index ' \
                                '(%(struct_type)s.words_C y\'\') %(index)d' % {
                                    'struct_type':struct_type,
                                    'index':i,
                                })
        defs.append('definition\n' \
                    '  %(prefix)s%(name)s_set_%(field)s :: "%(struct_type)s \\<Rightarrow> ' \
                        'word%(base)s \\<Rightarrow> %(struct_type)s"\n' \
                    'where\n' \
                    '  "%(prefix)s%(name)s_set_%(field)s y\'\' %(field)s\' = ' \
                        '%(struct_type)s.%(struct_type)s (FCP (\\<lambda>x\'\'. case ' \
                        'x\'\' of\n'
                    '    %(defn)s))"' % {
                        'prefix':prefix,
                        'name':item.name,
                        'struct_type':struct_type,
                        'field':field,
                        'base':item.base,
                        'defn':'\n  | '.join(defn),
                    })

    # We're now ready to form the definition of the constructor
    body = []
    for i, v in enumerate(members):
        body.append('%s \\<Rightarrow> (%s)'
            % (i, '0' if len(v) == 0 else ' || '.join(v)))

    # In the following definition, note that `FCP (\<lambda>x. ...` is an idiom
    # for defining an array.
    defs.append('definition\n' \
           '  %(prefix)s%(name)s_new :: "%(typesig)s"\n' \
           'where\n' \
           '  "%(prefix)s%(name)s_new %(argv)s = %(struct_type)s.%(struct_type)s (FCP (\\<lambda>x\'\'. case x\'\' of\n' \
           '    %(body)s))"' % {
        'prefix':prefix,
        'name':item.name,
        'struct_type':struct_type,
        'typesig':typesig,
        'argv':' '.join(map(lambda x: '%s\'' % x, argv)),
        'body':'\n  | '.join(body), # body as a case distinction
    })

    return '\n\n'.join(defs)

def generate_defs(symtab, output=sys.stdout):
    for v in symtab.values():
        if is_tagged_union(v):
            # For tagged unions, we need to generate definitions for each
            # subtype but they also need to be aware of their container.
            for block in v.tags:
                print >>output, generate_def(block[2], v.name)
                print >>output, '\n'
        else:
            if v.tagged:
                continue
            print >>output, generate_def(v)
            print >>output, '\n'

def generate_proof_helpers(item):
    '''Generate minor lemmas that will be needed in the course of other
    generated proofs below'''

    # FIXME: Ideally this lemma would already be generated for us
    return 'lemma %(name)s_contents_eq: "%(name)s_C.words_C x = ' \
               '%(name)s_C.words_C y \\<Longrightarrow> x = y"\n' \
           '  by (metis %(name)s_C_idupdates(1))' % {
               'name':item.name,
           }

def generate_proof(item, group=None):
    '''Generate WP proofs for the generated functions of the given structure'''

    prefix = function_prefix(group)
    struct_type = struct(group, item.name)
    argv = get_args(item)

    lemmas = []

    # First, a WP lemma for `new`.
    to_unfold = ['%s%s_new\'_def' % (prefix, item.name),
                 '%s%s_new_def' % (prefix, item.name)]
    if group is not None:
        # This block is part of a tagged union and we'll also need to unfold
        # the definition of its tag
        to_unfold.append('%s_%s_def' % (group, item.name))
        # The helper lemma we've previously created
        contents_eq = '%s_contents_eq' % group
    else:
        contents_eq = '%s_contents_eq' % item.name
    lemmas.append('lemma %(prefix)s%(name)s_new_wp[THEN validNF_make_schematic_post, simplified]:\n' \
           '  "\\<forall>s. \\<lbrace>\\<lambda>s\'. s = s\'\\<rbrace>\n' \
           '        %(prefix)s%(name)s_new\' %(fields)s\n' \
           '      \\<lbrace>\\<lambda>r s\'. s = s\' \\<and> r = %(prefix)s%(name)s_new ' \
               '%(fields)s\\<rbrace>!"\n' \
           '  apply (rule allI)\n' \
           '  unfolding %(to_unfold)s apply wp\n' \
           '  apply clarsimp\n' \
           '  apply (rule %(contents_eq)s)\n' \
           '  apply (clarsimp simp:cart_eq fcp_beta)\n' \
           '  apply word_bitwise?\n' \
           '  done' % {
                   'prefix':prefix,
                   'name':item.name,
                   'fields':' '.join(map(lambda x: '%s\'' % x, argv)),
                   'to_unfold':' '.join(to_unfold),
                   'contents_eq':contents_eq,
               })

    for field_index, (field, offset, size, high) in enumerate(item.fields):
        if item.tagged and field_index == 0:
            # Skip the tag
            continue
        preconditions = ['s = s0']
        postconditions = ['s = s0']
        to_unfold = []
        for f in argv:
            if f != field:
                accessor = '%s%s_get_%s' % (prefix, item.name, f)
                postconditions.append('%(acc)s r = %(acc)s x' % {'acc':accessor})
                to_unfold.append('%s_def' % accessor)
        if size < item.base:
            # We need a precondition on the input stating that the mask is a
            # no-op.
            if high:
                mask = ((1 << size) - 1) << (item.base - size)
            else:
                mask = (1 << size) - 1
            preconditions.append('%(field)s\' && 0x%(mask)x = %(field)s\'' % {
                'field':field,
                'mask':mask,
            })

        # Emit a WP lemma for the writer function for this field.
        lemmas.append('lemma %(prefix)s%(name)s_set_%(field)s_wp[THEN validNF_make_schematic_post, simplified]:\n' \
                      '  "\\<forall>s0. \\<lbrace>\\<lambda>s. %(preconditions)s\\<rbrace>\n' \
                      '         %(prefix)s%(name)s_set_%(field)s\' x %(field)s\'\n' \
                      '       \\<lbrace>\\<lambda>r s. %(postconditions)s\\<rbrace>!"\n' \
                      '  apply (rule allI)\n' \
                      '  unfolding %(prefix)s%(name)s_set_%(field)s\'_def apply wp\n' \
                      '  %(to_unfold)sapply (word_bitwise, clarsimp)\n' \
                      '  done' % {
                          'name':item.name,
                          'prefix':prefix,
                          'field':field,
                          'preconditions':' \\<and>\n             '.join(preconditions),
                          'postconditions':' \\<and>\n                 '.join(postconditions),
                          'to_unfold':'' if len(to_unfold) == 0 else ('unfolding %s ' % ' '.join(to_unfold)),
                      })

        # Emit a WP lemma for the reader function for this field.
        lemmas.append('lemma %(prefix)s%(name)s_get_%(field)s_wp[THEN validNF_make_schematic_post, simplified]:\n' \
                      '  "\\<forall>s0. \\<lbrace>\\<lambda>s. s = s0\\<rbrace>\n' \
                      '         %(prefix)s%(name)s_get_%(field)s\' x\n' \
                      '       \\<lbrace>\\<lambda>r s. r = %(prefix)s%(name)s_get_%(field)s x \\<and> s = s0\\<rbrace>!"\n' \
                      '  apply (rule allI)\n' \
                      '  unfolding %(prefix)s%(name)s_get_%(field)s\'_def apply wp\n' \
                      '  apply (clarsimp simp:%(prefix)s%(name)s_get_%(field)s_def)\n' \
                      '  done' % {
                          'name':item.name,
                          'prefix':prefix,
                          'field':field,
                      })

        # Emit a simp lemma for `set (set x a) a`.
        mask_bits = size
        if high:
            shift = item.base - size - (offset % item.base)
        else:
            shift = offset % item.base
        lemmas.append('lemma %(prefix)s%(name)s_set_%(field)s_twice[simp]:\n' \
                      '  "%(prefix)s%(name)s_set_%(field)s (%(prefix)s%(name)s_set_%(field)s x a) a = %(prefix)s%(name)s_set_%(field)s x a"\n' \
                      '  apply (clarsimp simp:%(prefix)s%(name)s_set_%(field)s_def fcp_beta)\n' \
                      '  apply (clarsimp simp:bitfield_op_twice[unfolded mask_def, where n=%(mask)d and m=%(shift)d, simplified])\n' \
                      '  done' % {
                          'name':item.name,
                          'prefix':prefix,
                          'field':field,
                          'mask':mask_bits,
                          'shift':shift,
                      })

        # Emit a simp lemma for `get (set x a)`.
        lemmas.append('lemma %(prefix)s%(name)s_get_%(field)s_set[simp]:\n' \
                      '  "a \\<le> 0x%(mask)x \\<Longrightarrow> %(prefix)s%(name)s_get_%(field)s (%(prefix)s%(name)s_set_%(field)s x a) = a"\n' \
                      '  apply (simp add:%(prefix)s%(name)s_get_%(field)s_def %(prefix)s%(name)s_set_%(field)s_def)\n' \
                      '  apply (simp add:fcp_beta)\n' \
                      '  apply (word_bitwise, clarsimp)\n' \
                      '  done' % {
                          'prefix':prefix,
                          'name':item.name,
                          'field':field,
                          'mask':mask,
                      })

        # Emit a simp lemma for `get (new x)`.
        if item.tagged:
            current = field_index - 1
            num_fields = len(item.fields) - 1
            tag_def = ' %s_%s_def' % (group, item.name)
        else:
            current = field_index
            num_fields = len(item.fields)
            tag_def = ''
        fields = ' '.join(['f%d' % i for i in xrange(num_fields)])
        lemmas.append('lemma %(prefix)s%(name)s_get_%(field)s_new[simp]:\n' \
                      '  "f%(current)d \\<le> 0x%(mask)x \\<Longrightarrow> ' \
                          '%(prefix)s%(name)s_get_%(field)s ' \
                          '(%(prefix)s%(name)s_new %(fields)s) = f%(current)d"\n' \
                      '  unfolding %(prefix)s%(name)s_get_%(field)s_def ' \
                          '%(prefix)s%(name)s_new_def%(tag_def)s\n' \
                      '  apply (clarsimp simp:fcp_beta)\n' \
                      '  apply word_bitwise\n' \
                      '  apply clarsimp\n' \
                      '  done' % {
                          'prefix':prefix,
                          'name':item.name,
                          'tag_def':tag_def,
                          'field':field,
                          'current':current,
                          'mask':mask,
                          'fields':fields,
                      })

        # Emit reordering simp lemmas for each combination of following fields.
        # The basic idea here is that we provide the simplifier with enough
        # information to canonicalise any series of `set` operations.
        for other, _, _, _ in item.fields[field_index + 1:]:
            lemmas.append('lemma %(prefix)s%(name)s_set_%(field)s_%(other)s_reorder[simp]:\n' \
                          '  "%(prefix)s%(name)s_set_%(other)s ' \
                              '(%(prefix)s%(name)s_set_%(field)s x a) b = ' \
                              '%(prefix)s%(name)s_set_%(field)s ' \
                              '(%(prefix)s%(name)s_set_%(other)s x b) a"\n' \
                          '  unfolding %(prefix)s%(name)s_set_%(field)s_def ' \
                              '%(prefix)s%(name)s_set_%(other)s_def\n' \
                          '  apply (clarsimp simp:fcp_beta)\n' \
                          '  apply (rule FCP_arg_cong)\n' \
                          '  apply (rule arg_cong)\n' \
                          '  apply word_bitwise\n' \
                          '  done' % {
                              'prefix':prefix,
                              'name':item.name,
                              'field':field,
                              'other':other,
                          })

    # For non-tagged structures, emit a simp lemma that says setting all its
    # fields has the same effect as creating a new structure.
    if not item.tagged:
        set_all = ' ('.join(['%(prefix)s%(name)s_set_%(field)s' % {
                'prefix':prefix,
                'name':item.name,
                'field':f[0],
            } for f in reversed(item.fields)])
        inputs = ['f%d' % i for i in xrange(len(item.fields))]
        unfolded = ' '.join(['%(prefix)s%(name)s_set_%(field)s_def' % {
                'prefix':prefix,
                'name':item.name,
                'field':f[0],
            } for f in item.fields])
        lemmas.append('lemma %(prefix)s%(name)s_set_all[simp]:\n' \
                      '  "%(set_all)s x %(trailer)s = %(prefix)s%(name)s_new ' \
                          '%(parameters)s"\n' \
                      '  unfolding %(unfolded)s %(prefix)s%(name)s_new_def\n' \
                      '  apply (clarsimp simp:fcp_beta)\n' \
                      '  apply (rule FCP_arg_cong)\n' \
                      '  apply (rule arg_cong)\n' \
                      '  apply word_bitwise\n' \
                      '  done' % {
                          'prefix':prefix,
                          'name':item.name,
                          'set_all':set_all,
                          'trailer':') '.join(inputs),
                          'parameters':' '.join(inputs),
                          'unfolded':unfolded,
                      })

    return '\n\n'.join(lemmas)

def generate_proofs(symtab, output=sys.stdout):
    for v in symtab.values():
        if is_tagged_union(v):
            print >>output, generate_proof_helpers(v)
            print >>output, '\n'
            # For tagged unions, we need to generate proofs for each
            # subtype but they also need to be aware of their container.
            for block in v.tags:
                print >>output, generate_proof(block[2], v.name)
                print >>output, '\n'
        else:
            if v.tagged:
                continue
            print >>output, generate_proof_helpers(v)
            print >>output, '\n'
            print >>output, generate_proof(v)
            print >>output, '\n'
