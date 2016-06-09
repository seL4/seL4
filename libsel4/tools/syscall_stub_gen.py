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

#
# seL4 System Call Stub Generator
# ===============================
#
# 2009 David Greenaway
#
# This script generates system call stubs based on an XML specification of the
# objects that the kernel exports (and the methods those objects export).
#
# Previously, Magpie (an IDL compiler) was used to generate these stubs. As
# Magpie development progressed, support for a fixed ABI (i.e., the ABI
# implemented by the seL4 kernel) was lost, and support for generating
# alignment-safe code (required by platforms such as ARM) was also removed.
#
# This script is a stop-gap until these features can be restored in Magpie
# once again.
#
# The script has certain limitations:
#
#   * It must be told the size of all types. This includes complex types
#     such as structures.
#
#     We generate code that will cause compilation to fail if we get any
#     object's size wrong, which should help mitigate the number of bugs caused
#     because of this script becoming out of date compared to the source files.
#
#   * The script has only been tested on the actual seL4 API XML description.
#
#     No stress testing has taken place; there may be bugs if new and wonderful
#     XML method descriptions are added.
#

import xml.dom.minidom
from argparse import ArgumentParser
import sys

# Number of bits in a standard word
WORD_SIZE_BITS_ARCH = {
    "aarch32": 32,
    "ia32": 32,
    "aarch64": 64,
    "ia64": 64,
    "x86_64": 64,
    "arm_hyp": 32,
}

MESSAGE_REGISTERS_FOR_ARCH = {
    "aarch32": 4,
    "ia32": 2,
    "arm_hyp": 4,
}

WORD_CONST_SUFFIX_BITS = {
    32: "ul",
    64: "ull",
}

# Maximum number of words that will be in a message.
MAX_MESSAGE_LENGTH = 32

# Headers to include
INCLUDES = [
    'autoconf.h', 'sel4/types.h'
]

TYPES = {
    8:  "seL4_Uint8",
    16: "seL4_Uint16",
    32: "seL4_Uint32",
    64: "seL4_Uint64"
}

class Type(object):
    """
    This class represents a C type (such as an 'int', structure or
    pointer.
    """

    def __init__(self, name, size_bits, wordsize, double_word=False, native_size_bits=None):
        """
        Define a new type, named 'name' that is 'size_bits' bits
        long.
        """

        self.name = name
        self.size_bits = size_bits
        self.wordsize = wordsize
        self.double_word = double_word

        #
        # Store the number of bits C will use for this type
        # in its native unpacked form.
        #
        # Required for 'bool', for example, which only uses 1
        # bit when packed, but 32 bits when unpacked.
        #
        if native_size_bits:
            self.native_size_bits = native_size_bits
        else:
            self.native_size_bits = size_bits

    def pass_by_reference(self):
        return self.size_bits > self.wordsize and not self.double_word

    def render_parameter_name(self, name):
        """
        Return a string of C code that would be used in a function
        parameter declaration.
        """
        return "%s %s" % (self.name, name)

    def pointer(self):
        """
        Return a new Type class representing a pointer to this
        object.
        """
        return PointerType(self, self.wordsize)

    def c_expression(self, var_name, word_num=0):
        """
        Return code for a C expression that gets word 'word_num'
        of this type.
        """
        assert word_num == 0
        return "%s" % var_name

    def double_word_expression(self, var_name, word_num, word_size):

        assert word_num == 0 or word_num == 1

        if word_num == 0:
            return "({0}) {1}".format(TYPES[self.size_bits], var_name)
        elif word_num == 1:
            return "({0}) ({1} >> {2})".format(TYPES[self.size_bits], var_name,
                                               word_size)


class PointerType(Type):
    """
    A pointer to a standard type.
    """
    def __init__(self, base_type, wordsize):
        Type.__init__(self, base_type.name, wordsize, wordsize)
        self.base_type = base_type

    def render_parameter_name(self, name):
        return "%s *%s" % (self.name, name)

    def c_expression(self, var_name, word_num=0):
        assert word_num == 0
        return "*%s" % var_name

    def pointer(self):
        raise NotImplementedError()

class CapType(Type):
    """
    A type that is just a typedef of seL4_CPtr.
    """
    def __init__(self, name, wordsize):
        Type.__init__(self, name, wordsize, wordsize)

class StructType(Type):
    """
    A C 'struct' definition.
    """
    def __init__(self, name, size_bits, wordsize):
        Type.__init__(self, name, size_bits, wordsize)

    def c_expression(self, var_name, word_num, member_name):
        assert word_num < self.size_bits / self.wordsize

        # Multiword structure.
        assert self.pass_by_reference()
        return "%s->%s" % (var_name, member_name[word_num])

class BitFieldType(Type):
    """
    A special C 'struct' generated by the bitfield generator
    """
    def __init__(self, name, size_bits, wordsize):
        Type.__init__(self, name, size_bits, wordsize)

    def c_expression(self, var_name, word_num=0):

        return "%s.words[%d]" % (var_name, word_num)

class Parameter(object):
    def __init__(self, name, type):
        self.name = name
        self.type = type

#
# Types
#
def init_data_types(wordsize):
    types = [
        # Simple Types
        Type("int", wordsize, wordsize),

        Type("seL4_Uint8", 8, wordsize),
        Type("seL4_Uint16", 16, wordsize),
        Type("seL4_Uint32", 32, wordsize),
        Type("seL4_Uint64", 64, wordsize, double_word=(wordsize == 32)),
        Type("seL4_Word", wordsize, wordsize),
        Type("seL4_Bool", 1, wordsize, native_size_bits=8),
        Type("seL4_CapRights", wordsize, wordsize),

        # seL4 Structures
        BitFieldType("seL4_CapData_t", wordsize, wordsize),
        BitFieldType("seL4_PrioProps_t", wordsize, wordsize),

        # Object types
        CapType("seL4_CPtr", wordsize),
        CapType("seL4_CNode", wordsize),
        CapType("seL4_IRQHandler", wordsize),
        CapType("seL4_IRQControl", wordsize),
        CapType("seL4_TCB", wordsize),
        CapType("seL4_Untyped", wordsize),
        CapType("seL4_DomainSet", wordsize),
    ]

    return types

def init_arch_types(wordsize):
    arch_types = {
        "aarch32" : [
            Type("seL4_ARM_VMAttributes", wordsize, wordsize),
            CapType("seL4_ARM_Page", wordsize),
            CapType("seL4_ARM_PageTable", wordsize),
            CapType("seL4_ARM_PageDirectory", wordsize),
            CapType("seL4_ARM_ASIDControl", wordsize),
            CapType("seL4_ARM_ASIDPool", wordsize),
            CapType("seL4_ARM_VCPU", wordsize),
            CapType("seL4_ARM_IOSpace", wordsize),
            CapType("seL4_ARM_IOPageTable", wordsize),
            StructType("seL4_UserContext", wordsize * 17, wordsize),
        ],

        "arm_hyp" : [
            Type("seL4_ARM_VMAttributes", wordsize, wordsize),
            CapType("seL4_ARM_Page", wordsize),
            CapType("seL4_ARM_PageTable", wordsize),
            CapType("seL4_ARM_PageDirectory", wordsize),
            CapType("seL4_ARM_ASIDControl", wordsize),
            CapType("seL4_ARM_ASIDPool", wordsize),
            CapType("seL4_ARM_VCPU", wordsize),
            CapType("seL4_ARM_IOSpace", wordsize),
            CapType("seL4_ARM_IOPageTable", wordsize),
            StructType("seL4_UserContext", wordsize * 17, wordsize),
        ],

        "ia32" : [
            Type("seL4_X86_VMAttributes", wordsize, wordsize),
            CapType("seL4_X86_IOPort", wordsize),
            CapType("seL4_X86_ASIDControl", wordsize),
            CapType("seL4_X86_ASIDPool", wordsize),
            CapType("seL4_X86_IOSpace", wordsize),
            CapType("seL4_X86_Page", wordsize),
            CapType("seL4_X86_PageDirectory", wordsize),
            CapType("seL4_X86_PageTable", wordsize),
            CapType("seL4_X86_IOPageTable", wordsize),
            StructType("seL4_UserContext", wordsize * 13, wordsize),
        ]
    }

    return arch_types

# Retrieve a member list for a given struct type
def struct_members(typ, structs):
    members = [member for struct_name, member in structs if struct_name == typ.name]
    assert len(members) == 1
    return members[0]

# Keep increasing the given number 'x' until 'x % a == 0'.
def align_up(x, a):
    if x % a == 0:
        return x
    return x + a - (x % a)

def get_parameter_positions(parameters, wordsize):
    """
    Determine where each parameter should be packed in the generated message.
    We generate a list of:

        (param_name, param_type, first_bit, num_bits)

    tuples.

    We guarantee that either (num_words == 1) or (bit_offset == 0).
    """
    bits_used = 0
    results = []

    for param in parameters:
        # How big are we?
        type_size = param.type.size_bits

        # We need everything to be a power of two, or word sized.
        assert ((type_size & (type_size - 1)) == 0) or (type_size % wordsize == 0)

        # Align up to our own size, or the next word. (Whichever is smaller)
        bits_used = align_up(bits_used, min(type_size, wordsize))

        # Place ourself.
        results.append((param, bits_used, type_size))
        bits_used += type_size

    return results

def generate_param_list(input_params, output_params):
    # Generate parameters
    params = []
    for param in input_params:
        if not param.type.pass_by_reference():
            params.append(param.type.render_parameter_name(param.name))
        else:
            params.append(param.type.pointer().render_parameter_name(param.name))
    for param in output_params:
        if param.type.pass_by_reference():
            params.append(param.type.pointer().render_parameter_name(param.name))

    return ", ".join(params)


def generate_marshal_expressions(params, num_mrs, structs, wordsize):
    """
    Generate marshalling expressions for the given set of inputs.

    We return a list of expressions; one expression per word required
    to marshal all the inputs.
    """

    def generate_param_code(param, first_bit, num_bits, word_array, wordsize):
        """
        Generate code to marshal the given parameter into the correct
        location in the message.

        'word_array' is an array of the final contents of the message.
        word_array[k] contains what should be placed in the k'th message
        register, and is an array of expressions that will (eventually)
        be bitwise-or'ed into it.
        """

        target_word = first_bit / wordsize
        target_offset = first_bit % wordsize

        # double word type
        if param.type.double_word:
            word_array[target_word].append(param.type.double_word_expression(param.name, 0, wordsize))
            word_array[target_word + 1].append(param.type.double_word_expression(param.name, 1, wordsize))
            return

        # Single full word?
        if num_bits == wordsize:
            assert target_offset == 0
            expr = param.type.c_expression(param.name)
            word_array[target_word].append(expr)
            return

        # Part of a word?
        if num_bits < wordsize:
            expr = param.type.c_expression(param.name)
            expr = "(%s & %#x%s)" % (expr, (1 << num_bits) - 1,
                                     WORD_CONST_SUFFIX_BITS[wordsize])
            if target_offset:
                expr = "(%s << %d)" % (expr, target_offset)
            word_array[target_word].append(expr)
            return

        # Multiword array
        assert target_offset == 0
        num_words = num_bits / wordsize
        for i in range(num_words):
            expr = param.type.c_expression(param.name, i, struct_members(param.type, structs))
            word_array[target_word + i].append(expr)


    # Get their marshalling positions
    positions = get_parameter_positions(params, wordsize)

    # Generate marshal code.
    words = [[] for _ in range(num_mrs, MAX_MESSAGE_LENGTH)]
    for (param, first_bit, num_bits) in positions:
        generate_param_code(param, first_bit, num_bits, words, wordsize)

    # Return list of expressions.
    return [" | ".join(x) for x in words if len(x) > 0]

def generate_unmarshal_expressions(params, wordsize):
    """
    Generate unmarshalling expressions for the given set of outputs.

    We return a list of list of expressions; one list per variable, containing
    expressions for the words in it that must be unmarshalled. The expressions
    will have tokens of the form:
        "%(w0)s"
    in them, indicating a read from a word in the message.
    """

    def unmarshal_single_param(first_bit, num_bits, wordsize):
        """
        Unmarshal a single parameter.
        """
        first_word = first_bit / wordsize
        bit_offset = first_bit % wordsize

        # Multiword type?
        if num_bits > wordsize:
            result = []
            for x in range(num_bits / wordsize):
                result.append("%%(w%d)s" % (x + first_word))
            return result

        # Otherwise, bit packed.
        if num_bits == wordsize:
            return ["%%(w%d)s" % first_word]
        elif bit_offset == 0:
            return ["(%%(w%d)s & %#x)" % (
                first_word, (1 << num_bits) - 1)]
        else:
            return ["(%%(w%d)s >> %d) & %#x" % (
                first_word, bit_offset, (1 << num_bits) - 1)]

    # Get their marshalling positions
    positions = get_parameter_positions(params, wordsize)

    # Generate the unmarshal code.
    results = []
    for (param, first_bit, num_bits) in positions:
        results.append((param, unmarshal_single_param(first_bit, num_bits, wordsize)))
    return results

def generate_result_struct(interface_name, method_name, output_params):
    """
    Generate a structure definition to be returned by the system call stubs to
    the user.

    We have a few constraints:

        * We always need an 'error' output parameter, even though it won't
          appear in the list 'output_params' given to us.

        * Output parameters may be marked as 'pass_by_reference', indicating
          that we only ever see pointers to the item.

    If no structure is needed (i.e., we just return an error code), we return
    'None'.
    """

    # Do we actually need a structure?
    if len([x for x in output_params if not x.type.pass_by_reference()]) == 0:
        return None

    #
    # Generate the structure:
    #
    #   struct seL4_CNode_Copy {
    #       int error;
    #       seL4_Word foo;
    #   };
    #   typedef struct seL4_CNode_Copy seL4_CNode_Copy_t;
    #
    result = []
    result.append("struct %s_%s {" % (interface_name, method_name))
    result.append("\tint error;")
    for i in output_params:
        if not i.type.pass_by_reference():
            result.append("\t%s;" % i.type.render_parameter_name(i.name))
    result.append("};")
    result.append("typedef struct %s_%s %s_%s_t;" % (
        (interface_name, method_name, interface_name, method_name)))
    result.append("")

    return "\n".join(result)

def generate_stub(arch, wordsize, interface_name, method_name, method_id, input_params, output_params, structs, use_only_ipc_buffer):
    result = []

    if use_only_ipc_buffer:
        num_mrs = 0
    else:
        num_mrs = MESSAGE_REGISTERS_FOR_ARCH[arch]

    # Split out cap parameters and standard parameters
    standard_params = []
    cap_params = []
    for x in input_params:
        if isinstance(x.type, CapType):
            cap_params.append(x)
        else:
            standard_params.append(x)

    # Determine if we are returning a structure, or just the error code.
    returning_struct = False
    results_structure = generate_result_struct(interface_name, method_name, output_params)
    if results_structure:
        return_type = "%s_%s_t" % (interface_name, method_name)
        returning_struct = True
    else:
        return_type = "int"

    #
    # Print function header.
    #
    #   static inline int
    #   seL4_Untyped_Retype(...)
    #   {
    #
    result.append("LIBSEL4_INLINE %s" % return_type)
    result.append("%s_%s(%s)" % (interface_name, method_name,
                                 generate_param_list(input_params, output_params)))
    result.append("{")

    #
    # Get a list of expressions for our caps and inputs.
    #
    input_expressions = generate_marshal_expressions(standard_params, num_mrs,
                                                     structs, wordsize)
    cap_expressions = [x.name for x in cap_params]
    service_cap = cap_expressions[0]
    cap_expressions = cap_expressions[1:]

    #
    # Compute how many words the inputs and output will require.
    #
    input_param_words = len(input_expressions)
    output_param_words = sum([p.type.size_bits for p in output_params]) / wordsize

    #
    # Setup variables we will need.
    #
    if returning_struct:
        result.append("\t%s result;" % return_type)
    result.append("\tseL4_MessageInfo_t tag = seL4_MessageInfo_new(%s, 0, %d, %d);"  % (method_id, len(cap_expressions), len(input_expressions)))
    result.append("\tseL4_MessageInfo_t output_tag;")
    for i in range(min(num_mrs, max(input_param_words, output_param_words))):
        result.append("\tseL4_Word mr%d;" % i)
    result.append("")

    #
    # Copy capabilities.
    #
    #   /* Setup input capabilities. */
    #   seL4_SetCap(i, cap);
    #
    if len(cap_expressions) > 0:
        result.append("\t/* Setup input capabilities. */")
        for i in range(len(cap_expressions)):
            result.append("\tseL4_SetCap(%d, %s);" % (i, cap_expressions[i]))
        result.append("")

    #
    # Copy in the inputs.
    #
    #   /* Marshal input parameters. */
    #   seL4_SetMR(i, v);
    #   ...
    #
    if len(input_expressions) > 0:
        result.append("\t/* Marshal input parameters. */")
        for i in range(len(input_expressions)):
            if i < num_mrs:
                result.append("\tmr%d = %s;" % (i, input_expressions[i]))
            else:
                result.append("\tseL4_SetMR(%d, %s);" % (i, input_expressions[i]))
        result.append("")

    #
    # Generate the call.
    #
    call_arguments = []
    for i in range(num_mrs):
        if i < max(input_param_words, output_param_words):
            call_arguments.append("&mr%d" % i)
        else:
            call_arguments.append("seL4_Null")
    if use_only_ipc_buffer:
        result.append("\t/* Perform the call. */")
        result.append("\toutput_tag = seL4_Call(%s, tag);" % service_cap)
    else:
        result.append("\t/* Perform the call, passing in-register arguments directly. */")
        result.append("\toutput_tag = seL4_CallWithMRs(%s, tag," % (service_cap))
        result.append("\t\t%s);" % ', '.join(
            [call_arguments[i] for i in range(num_mrs)]))
    result.append("")

    #
    # Generate unmarshalling code.
    #
    if len(output_params) > 0:
        result.append("\t/* Unmarshal result. */")
        source_words = {}
        for i in range(MAX_MESSAGE_LENGTH):
            if i < num_mrs:
                source_words["w%d" % i] = "mr%d" % i
            else:
                source_words["w%d" % i] = "seL4_GetMR(%d)" % i
        unmashalled_params = generate_unmarshal_expressions(output_params, wordsize)
        for (param, words) in unmashalled_params:
            if param.type.pass_by_reference():
                members = struct_members(param.type, structs)
                for i in range(len(words)):
                    result.append("\t%s->%s = %s;" %
                                  (param.name, members[i], words[i] % source_words))
            else:
                if param.type.double_word:
                    result.append("\tresult.%s = ((%s)%s + ((%s)%s << 32));" %
                                  (param.name, TYPES[64], words[0] % source_words,
                                   TYPES[64], words[1] % source_words))
                else:
                    for word in words:
                        result.append("\tresult.%s = %s;" % (param.name, word % source_words))

        result.append("")

    # Return result
    if returning_struct:
        result.append("\tresult.error = seL4_MessageInfo_get_label(output_tag);")
        result.append("\treturn result;")
    else:
        result.append("\treturn seL4_MessageInfo_get_label(output_tag);")

    #
    # }
    #
    result.append("}")

    return "\n".join(result) + "\n"

def parse_xml_file(input_file, valid_types):
    """
    Parse an XML file containing method definitions.
    """

    # Create a dictionary of type name to type.
    type_names = {}
    for i in valid_types:
        type_names[i.name] = i

    # Parse the XML to generate method structures.
    methods = []
    structs = []
    doc = xml.dom.minidom.parse(input_file)

    for struct in doc.getElementsByTagName("struct"):
        _struct_members = []
        struct_name = struct.getAttribute("name")
        for members in struct.getElementsByTagName("member"):
            member_name = members.getAttribute("name")
            _struct_members.append(member_name)
        structs.append((struct_name, _struct_members))

    for interface in doc.getElementsByTagName("interface"):
        interface_name = interface.getAttribute("name")
        for method in interface.getElementsByTagName("method"):
            method_name = method.getAttribute("name")
            method_id = method.getAttribute("id")
            method_config = method.getAttribute("config")

            #
            # Get parameters.
            #
            # We always have an implicit cap parameter.
            #
            input_params = [Parameter("service", type_names[interface_name])]
            output_params = []
            for param in method.getElementsByTagName("param"):
                param_name = param.getAttribute("name")
                param_type = type_names.get(param.getAttribute("type"))
                if not param_type:
                    raise Exception("Unknown type '%s'." % (param.getAttribute("type")))
                param_dir = param.getAttribute("dir")
                assert (param_dir == "in") or (param_dir == "out")
                if param_dir == "in":
                    input_params.append(Parameter(param_name, param_type))
                else:
                    output_params.append(Parameter(param_name, param_type))
            methods.append((interface_name, method_name, method_id, input_params, output_params, method_config))

    return (methods, structs)

def generate_stub_file(arch, wordsize, input_files, output_file, use_only_ipc_buffer):
    """
    Generate a header file containing system call stubs for seL4.
    """
    result = []

    # Ensure architecture looks sane.
    if arch not in WORD_SIZE_BITS_ARCH.keys():
        raise Exception("Invalid architecture.")

    data_types = init_data_types(wordsize)
    arch_types = init_arch_types(wordsize)

    # Parse XML
    methods = []
    structs = []
    for infile in input_files:
        method, struct = parse_xml_file(infile, data_types + arch_types[arch])
        methods += method
        structs += struct

    # Print header.
    result.append("""
/*
 * Automatically generated system call stubs.
 */

#ifndef __LIBSEL4_SEL4_CLIENT_H
#define __LIBSEL4_SEL4_CLIENT_H
""")

    # Emit the includes
    result.append('\n'.join(['#include <%s>' % include for include in INCLUDES]))

    #
    # Emit code to ensure that all of our type sizes are consistent with
    # the compiler's.
    #
    result.append("""
/*
 * The following code generates a compile-time error if the system call
 * stub generator has an incorrect understanding of how large a type is.
 *
 * If you receive a compile-time error here, you will need to adjust
 * the type information in the stub generator.
 */
#define assert_size_correct(type, expected_bytes) \\
        typedef unsigned long __type_##type##_size_incorrect[ \\
                (sizeof(type) == expected_bytes) ? 1 : -1]
""")
    for x in data_types + arch_types[arch]:
        result.append("assert_size_correct(%s, %d);" % (x.name, x.native_size_bits / 8))
    result.append("")

    #
    # Generate structures needed to return results back to the user.
    #
    # We can not use pass-by-reference (except for really large objects), as
    # the verification framework does not support them.
    #
    result.append("/*")
    result.append(" * Return types for generated methods.")
    result.append(" */")
    for (interface_name, method_name, _, _, output_params, _) in methods:
        results_structure = generate_result_struct(interface_name, method_name, output_params)
        if results_structure:
            result.append(results_structure)

    #
    # Generate the actual stub code.
    #
    result.append("/*")
    result.append(" * Generated stubs.")
    result.append(" */")
    for (interface_name, method_name, method_id, inputs, outputs, config) in methods:
        if config != "":
            result.append("#ifdef %s" % config)
        result.append(generate_stub(arch, wordsize, interface_name, method_name,
                                    method_id, inputs, outputs, structs, use_only_ipc_buffer))
        if config != "":
            result.append("#endif")

    # Print footer.
    result.append("#endif /* __LIBSEL4_SEL4_CLIENT_H */")
    result.append("")

    # Write the output
    output = open(output_file, "w")
    output.write("\n".join(result))
    output.close()

def process_args():
    usage_str = """
    %(prog)s [OPTIONS] [FILES] """
    epilog_str = """

    """
    parser = ArgumentParser(description='seL4 System Call Stub Generator.',
                            usage=usage_str,
                            epilog=epilog_str)
    parser.add_argument("-o", "--output", dest="output", default="/dev/stdout",
                        help="Output file to write stub to. (default: %(default)s).")
    parser.add_argument("-b", "--buffer", dest="buffer", action="store_true", default=False,
                        help="Use IPC buffer exclusively, i.e. do not pass syscall arguments by registers. (default: %(default)s)")
    parser.add_argument("-a", "--arch", dest="arch", required=True, choices=WORD_SIZE_BITS_ARCH,
                        help="Architecture to generate stubs for.")

    wsizegroup = parser.add_mutually_exclusive_group()
    wsizegroup.add_argument("-w", "--word-size", dest="wsize",
                            help="Word size(in bits), for the platform.")
    wsizegroup.add_argument("-c", "--cfile", dest="cfile",
                            help="Config file for Kbuild, used to get Word size.")

    parser.add_argument("files", metavar="FILES", nargs="+",
                        help="Input XML files.")

    return parser

def main():

    parser = process_args()
    args = parser.parse_args()

    if not (args.wsize or args.cfile):
        parser.error("Require either -w/--word-size or -c/--cfile argument.")
        sys.exit(2)

    # Get word size
    wordsize = -1

    if args.cfile:
        try:
            with open(args.cfile) as conffile:
                for line in conffile:
                    if line.startswith('CONFIG_WORD_SIZE'):
                        wordsize = int(line.split('=')[1].strip())
        except IndexError:
            print "Invalid word size in configuration file."
            sys.exit(2)
    else:
        wordsize = int(args.wsize)

    if wordsize is -1:
        print "Invalid word size."
        sys.exit(2)

    # Generate the stubs.
    generate_stub_file(args.arch, wordsize, args.files, args.output, args.buffer)


if __name__ == "__main__":
    sys.exit(main())

