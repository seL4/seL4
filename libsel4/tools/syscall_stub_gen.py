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
#   * The word-size is fixed at 32 bits, and we may implicitly assume that
#     sizeof(int) == sizeof(long) == 32.
#
#     Though the constant 'WORD_SIZE_BITS' has been used throughout, there
#     may be implicit assumptions hanging around causing things to fail.
#
#   * The script has only been tested on the actual seL4 API XML description.
#
#     No stress testing has taken place; there may be bugs if new and wonderful
#     XML method descriptions are added.
#

import xml.dom.minidom
import optparse

# Number of bits in a standard word
WORD_SIZE_BITS = 32

# Maximum number of words that will be in a message.
MAX_MESSAGE_LENGTH = 32

MESSAGE_REGISTERS_FOR_ARCH = {
    "arm": 4,
    "ia32": 2,
}

class Type(object):
    """
    This class represents a C type (such as an 'int', structure or
    pointer.
    """

    def __init__(self, name, size_bits, double_word=False, native_size_bits=None):
        """
        Define a new type, named 'name' that is 'size_bits' bits
        long.
        """

        self.name = name
        self.size_bits = size_bits
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
        return self.size_bits > WORD_SIZE_BITS and not self.double_word

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
        return PointerType(self)

    def c_expression(self, var_name, word_num=0):
        """
        Return code for a C expression that gets word 'word_num'
        of this type.
        """
        assert word_num == 0
        return "%s" % var_name

    def double_word_expression(self, var_name, word_num):

        assert(word_num == 0 or word_num == 1)
        
        if word_num == 0:
            return "(uint{0}_t) {1}".format(WORD_SIZE_BITS, var_name)
        elif word_num == 1:
            return "(uint{0}_t) ({1} >> {0})".format(WORD_SIZE_BITS, var_name)
        

class PointerType(Type):
    """
    A pointer to a standard type.
    """
    def __init__(self, base_type):
        Type.__init__(self, base_type.name, WORD_SIZE_BITS)
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
    def __init__(self, name):
        Type.__init__(self, name, WORD_SIZE_BITS)

class StructType(Type):
    """
    A C 'struct' definition.
    """
    def __init__(self, name, size_bits):
        Type.__init__(self, name, size_bits)

    def c_expression(self, var_name, word_num, member_name):
        assert word_num < self.size_bits / WORD_SIZE_BITS

        # Multiword structure.
        assert self.pass_by_reference()
        return "%s->%s" % (var_name, member_name[word_num])

class CapDataType(Type):
    """
    A special C 'struct' definition for seL4_CapData
    """
    def __init__(self):
        Type.__init__(self,"seL4_CapData_t", WORD_SIZE_BITS)
    
    def c_expression(self, var_name, word_num=0):
        assert word_num == 0
        
        return "%s.words[0]" % var_name
        
class Parameter(object):
    def __init__(self, name, type):
        self.name = name
        self.type = type

#
# Return the size (in bits) of a particular type.
#
types = [
        # Simple Types
        Type("uint8_t", 8),
        Type("uint16_t", 16),
        Type("uint32_t", 32),
        Type("uint64_t", 64, double_word=True),
        Type("int", WORD_SIZE_BITS),
        Type("bool", 1, native_size_bits=8),
        Type("seL4_Word", WORD_SIZE_BITS),
        Type("seL4_CapRights", WORD_SIZE_BITS),

        # seL4 Structures
        CapDataType(),

        # Object types
        CapType("seL4_CPtr"),
        CapType("seL4_CNode"),
        CapType("seL4_IRQHandler"),
        CapType("seL4_IRQControl"),
        CapType("seL4_TCB"),
        CapType("seL4_Untyped"),
        CapType("seL4_DomainSet"),
        ]

#
# Arch-specific types.
#
arch_types = {
    "arm" : [
        Type("seL4_ARM_VMAttributes", WORD_SIZE_BITS),
        CapType("seL4_ARM_Page"),
        CapType("seL4_ARM_PageTable"),
        CapType("seL4_ARM_PageDirectory"),
        CapType("seL4_ARM_ASIDControl"),
        CapType("seL4_ARM_ASIDPool"),
        StructType("seL4_UserContext", WORD_SIZE_BITS * 17),
        ],

    "ia32" : [
        Type("seL4_IA32_VMAttributes", WORD_SIZE_BITS),
        CapType("seL4_IA32_ASIDControl"),
        CapType("seL4_IA32_ASIDPool"),
        CapType("seL4_IA32_IOSpace"),
        CapType("seL4_IA32_IOPort"),
        CapType("seL4_IA32_Page"),
        CapType("seL4_IA32_PageDirectory"),
        CapType("seL4_IA32_PageTable"),
        CapType("seL4_IA32_IOPageTable"),
        StructType("seL4_UserContext", WORD_SIZE_BITS * 13),
        ]
    }

# Keep increasing the given number 'x' until 'x % a == 0'.
def align_up(x, a):
    if x % a == 0:
        return x
    return x + a - (x % a)

def get_parameter_positions(parameters):
    """
    Determine where each parameter should be packed in the generated message.
    We generate a list of:

        (param_name, param_type, first_bit, num_bits)

    tuples.

    We guarantee that either (num_words == 1) or (bit_offset == 0).
    """
    words_used = 0
    bits_used = 0
    results = []

    for param in parameters:
        # How big are we?
        type_size = param.type.size_bits

        # We need everything to be a power of two, or word sized.
        assert ((type_size & (type_size - 1)) == 0) or (type_size % WORD_SIZE_BITS == 0)

        # Align up to our own size, or the next word. (Whichever is smaller)
        bits_used = align_up(bits_used, min(type_size, WORD_SIZE_BITS))

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


def generate_marshal_expressions(params, num_mrs, registers):
    """
    Generate marshalling expressions for the given set of inputs.

    We return a list of expressions; one expression per word required
    to marshal all the inputs.
    """

    def generate_param_code(param, first_bit, num_bits, word_array):
        """
        Generate code to marshal the given parameter into the correct
        location in the message.

        'word_array' is an array of the final contents of the message.
        word_array[k] contains what should be placed in the k'th message
        register, and is an array of expressions that will (eventually)
        be bitwise-or'ed into it.
        """

        target_word = first_bit / WORD_SIZE_BITS
        target_offset = first_bit % WORD_SIZE_BITS

        # double word type
        if param.type.double_word:
            word_array[target_word].append(param.type.double_word_expression(param.name, 0))
            word_array[target_word + 1].append(param.type.double_word_expression(param.name, 1))
            return

        # Single full word?
        if num_bits == WORD_SIZE_BITS:
            assert target_offset == 0
            expr = param.type.c_expression(param.name);
            word_array[target_word].append(expr)
            return

        # Part of a word?
        if num_bits < WORD_SIZE_BITS:
            expr = param.type.c_expression(param.name);
            expr = "(%s & %#x)" % (expr, (1 << num_bits) - 1)
            if target_offset:
                expr = "(%s << %d)" % (expr, target_offset)
            word_array[target_word].append(expr)
            return

        # Multiword array
        assert target_offset == 0
        num_words = num_bits / WORD_SIZE_BITS
        for i in range(num_words):
            expr = param.type.c_expression(param.name, i, registers);
            word_array[target_word + i].append(expr)


    # Get their mashalling positions
    positions = get_parameter_positions(params)

    # Generate marshal code.
    words = [[] for _ in range(num_mrs, MAX_MESSAGE_LENGTH)]
    for (param, first_bit, num_bits) in positions:
        generate_param_code(param, first_bit, num_bits, words)

    # Return list of expressions.
    return [" | ".join(x) for x in words if len(x) > 0]

def generate_unmarshal_expressions(params):
    """
    Generate unmarshalling expressions for the given set of outputs.

    We return a list of list of expressions; one list per variable, containing
    expressions for the words in it that must be unmarshalled. The expressions
    will have tokens of the form:
        "%(w0)s"
    in them, indicating a read from a word in the message.
    """

    def unmarshal_single_param(first_bit, num_bits):
        """
        Unmarshal a single parameter.
        """
        first_word = first_bit / WORD_SIZE_BITS
        bit_offset = first_bit % WORD_SIZE_BITS

        # Multiword type?
        if num_bits > WORD_SIZE_BITS:
            result = []
            for x in range(num_bits / WORD_SIZE_BITS):
                result.append("%%(w%d)s" % (x + first_word))
            return result

        # Otherwise, bit packed.
        if num_bits == WORD_SIZE_BITS:
            return ["%%(w%d)s" % first_word]
        elif bit_offset == 0:
            return ["(%%(w%d)s & %#x)" % (
                    first_word, (1 << num_bits) - 1)]
        else:
            return ["(%%(w%d)s >> %d) & %#x" % (
                    first_word, bit_offset, (1 << num_bits) - 1)]

    # Get their mashalling positions
    positions = get_parameter_positions(params)

    # Generate the unmarshal code.
    results = []
    for (param, first_bit, num_bits) in positions:
        results.append((param, unmarshal_single_param(first_bit, num_bits)))
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

def generate_stub(arch, interface_name, method_name, method_id, input_params, output_params, registers, use_only_ipc_buffer):
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
    result.append("static inline %s" % return_type)
    result.append("%s_%s(%s)" % (interface_name, method_name,
        generate_param_list(input_params, output_params)))
    result.append("{")

    #
    # Get a list of expressions for our caps and inputs.
    #
    input_expressions = generate_marshal_expressions(standard_params, num_mrs, registers)
    cap_expressions = [x.name for x in cap_params]
    service_cap = cap_expressions[0]
    cap_expressions = cap_expressions[1:]

    #
    # Compute how many words the inputs and output will require.
    #
    input_param_words = len(input_expressions)
    output_param_words = sum([p.type.size_bits for p in output_params]) / WORD_SIZE_BITS

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
            call_arguments.append("NULL")
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
    # Generate unmarshaling code.
    #
    if len(output_params) > 0:
        result.append("\t/* Unmarshal result. */")
        source_words = {}
        for i in range(MAX_MESSAGE_LENGTH):
            if i < num_mrs:
                source_words["w%d" % i] = "mr%d" % i;
            else:
                source_words["w%d" % i] = "seL4_GetMR(%d)" % i;
        unmashalled_params = generate_unmarshal_expressions(output_params)
        for (param, words) in unmashalled_params:
            if param.type.pass_by_reference():
                for i in range(len(words)):
                    result.append("\t%s->%s = %s;" % (param.name, registers[i], words[i] % source_words))
            else:
                if param.type.double_word:
                    result.append("\tresult.%s = ((uint64_t)%s + ((uint64_t)%s << 32));" % (param.name, words[0] % source_words, words[1] % source_words))
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
    Parse an XML file containing method defintions.
    """

    # Create a dictionary of type name to type.
    type_names = {}
    for i in valid_types:
        type_names[i.name] = i

    # Parse the XML to generate method structures.
    methods = []
    registers = []
    doc = xml.dom.minidom.parse(input_file)

    for register in doc.getElementsByTagName("register"):
        register_name = (register.getAttribute("name")).lower()
        registers.append(register_name)

    for interface in doc.getElementsByTagName("interface"):
        interface_name = interface.getAttribute("name")
        for method in interface.getElementsByTagName("method"):
            method_name = method.getAttribute("name")
            method_id = method.getAttribute("id")

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
                if (param_dir == "in"):
                    input_params.append(Parameter(param_name, param_type))
                else:
                    output_params.append(Parameter(param_name, param_type))
            methods.append((interface_name, method_name, method_id, input_params, output_params))

    return (methods, registers)

def generate_stub_file(arch, input_files, output_file, use_only_ipc_buffer):
    """
    Generate a header file containing system call stubs for seL4.
    """
    result = []

    # Ensure architecture looks sane.
    if not arch in arch_types.keys():
        raise Exception("Invalid architecture. Expected %s.",
                " or ".join(arch_types.keys()))

    # Parse XML
    methods = []
    registers = []
    for file in input_files:
        method, register = parse_xml_file(file, types + arch_types[arch])
        methods += method
        registers += register

    # Print header.
    result.append("""
/*
 * Automatically generated system call stubs.
 */

#ifndef __LIBSEL4_SEL4_CLIENT_H
#define __LIBSEL4_SEL4_CLIENT_H

#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>
#include "sel4/types.h"
#include "sel4/invocation.h"
#include "sel4/arch/functions.h"
#include "sel4/arch/syscalls.h"
""");

    #
    # Emit code to ensure that all of our type sizes are consistant with
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
    for x in types + arch_types[arch]:
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
    for (interface_name, method_name, _, _, output_params) in methods:
        results_structure = generate_result_struct(interface_name, method_name, output_params)
        if results_structure:
            result.append(results_structure)

    #
    # Generate the actual stub code.
    #
    result.append("/*")
    result.append(" * Generated stubs.")
    result.append(" */")
    for (interface_name, method_name, method_id, inputs, outputs) in methods:
        result.append(generate_stub(arch, interface_name, method_name,
                method_id, inputs, outputs, registers, use_only_ipc_buffer))

    # Print footer.
    result.append("#endif /* __LIBSEL4_SEL4_CLIENT_H */")
    result.append("")

    # Write the output
    output = open(output_file, "w")
    output.write("\n".join(result))
    output.close()

def main():
    #
    # Read command line arguments.
    #
    parser = optparse.OptionParser(
            usage = "usage: %prog -a <arch> [-o <ouput file] <input XML> [<input XML> ...]")
    parser.add_option("-a", "--arch",
            dest="arch", help="Architecture to generate stubs for.")
    parser.add_option("-o", "--output",
            dest="output", help="Output file to write stub to.")
    parser.add_option("-b", "--buffer", action="store_true",
            help="Use IPC buffer exclusively (i.e. do not pass syscall "
            "arguments by registers).")
    (options, args) = parser.parse_args()

    # Validate arguments
    if len(args) < 1:
        parser.error("Require at least one input file.")
    if not options.arch:
        parser.error("Require an architecture to be specified.")
    if not options.output:
        options.output = "/dev/stdout"
    input_files = args

    # Generate the stubs.
    generate_stub_file(options.arch, input_files, options.output, options.buffer)

main()

