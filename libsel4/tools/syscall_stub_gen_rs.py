#!/usr/bin/env python3
#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
# Copyright 2017, Ember Arlynx
# Copyright (c) 2022 Google LLC
# Copyright (c) 2022 Antmicro
#
# SPDX-License-Identifier: BSD-2-Clause
#

# This script is intended to work as a Rust-specific copy of syscall_stub_gen.py
# Use it with sel4-sys crate to allow Rust applications inteface with the seL4 API.

from argparse import ArgumentParser
import sys
import syscall_stub_gen


TYPES = {
    8:  "u8",
    16: "u16",
    32: "u32",
    64: "u64"
}


def translate_expr(name):
    if name == "type":
        return "type_"
    return name


def translate_type(name):
    type_trans = {
        "int": "isize",
        "seL4_Uint8": "u8",
        "seL4_Uint16": "u16",
        "seL4_Uint32": "u32",
        "seL4_Uint64": "u64",
        "seL4_Bool": "u8",
        "seL4_CapData_t": "seL4_CapData",
        "seL4_PrioProps_t": "seL4_PrioProps",
        "seL4_CapRights_t": "seL4_CapRights",
        "seL4_RISCV_Page_GetAddress_t": "seL4_RISCV_Page_GetAddress",
    }
    if name in type_trans:
        return type_trans[name]
    else:
        return name


def rust_render_parameter_name(param_type, name, mutable_ref=False):
    """
    Return a string of Rust code that would be used in a function
    parameter declaration.
    """
    if type(param_type) in [syscall_stub_gen.Type, syscall_stub_gen.CapType, syscall_stub_gen.StructType, syscall_stub_gen.BitFieldType]:
        name = translate_expr(name)
        return "%s: %s" % (name, translate_type(param_type.name))
    elif type(param_type) is syscall_stub_gen.PointerType:
        return "%s: *%s %s" % (
            translate_expr(name),
            "mut" if mutable_ref else "const",
            translate_type(param_type.name))
    else:
        raise Exception("Unsupported type")


def rust_double_word_expression(param_type, var_name, word_num, word_size):

    assert word_num == 0 or word_num == 1

    if word_num == 0:
        return "{1} as {0}".format(TYPES[param_type.size_bits], var_name)
    elif word_num == 1:
        return "{1}.wrapping_shr({2}) as {0}".format(TYPES[param_type.size_bits], var_name,
                                                     word_size)


def rust_expression(param_type, var_name, word_num=0, member_name=None):
    """
    Return code for an expression that gets word 'word_num'
    of the provided type.
    """
    if type(param_type) is syscall_stub_gen.BitFieldType:
        return "%s.words[%d]" % (var_name, word_num)
    elif type(param_type) is syscall_stub_gen.StructType:
        assert word_num < param_type.size_bits // param_type.wordsize

        # Multiword structure.
        assert param_type.pass_by_reference()
        return "(*%s).%s" % (var_name, member_name[word_num])
    elif type(param_type) is syscall_stub_gen.PointerType:
        assert word_num == 0
        return "unsafe { *%s }" % var_name
    elif type(param_type) in [syscall_stub_gen.Type, syscall_stub_gen.CapType]:
        assert word_num == 0
        return "%s" % var_name
    else:
        raise Exception("Unsupported type")


def generate_param_list(input_params, output_params):
    # Generate parameters
    params = []
    for param in input_params:
        t = param.type
        if not t.pass_by_reference():
            params.append(rust_render_parameter_name(t, param.name))
        else:
            params.append(rust_render_parameter_name(t.pointer(), param.name, mutable_ref=False))
    for param in output_params:
        # We drop params that aren't pass_by_reference deliberately
        if param.type.pass_by_reference():
            params.append(rust_render_parameter_name(
                param.type.pointer(), param.name, mutable_ref=True))

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

        target_word = first_bit // wordsize
        target_offset = first_bit % wordsize

        # double word type
        if param.type.double_word:
            word_array[target_word].append(
                rust_double_word_expression(param.type, param.name, 0, wordsize))
            word_array[target_word +
                       1].append(rust_double_word_expression(param.type, param.name, 1, wordsize))
            return

        # Single full word?
        if num_bits == wordsize:
            assert target_offset == 0
            expr = rust_expression(param.type, param.name)
            word_array[target_word].append(expr)
            return

        # Part of a word?
        if num_bits < wordsize:
            expr = rust_expression(param.type, param.name)
            expr = "(%s & %#x)" % (expr, (1 << num_bits) - 1)
            if target_offset:
                expr = "(%s as seL4_Word).wrapping_shl(%d)" % (expr, target_offset)
            word_array[target_word].append(expr)
            return

        # Multiword array
        assert target_offset == 0
        num_words = num_bits // wordsize
        for i in range(num_words):
            expr = rust_expression(param.type, param.name, i,
                                   syscall_stub_gen.struct_members(param.type, structs))
            word_array[target_word + i].append(expr)

    # Get their marshalling positions
    positions = syscall_stub_gen.get_parameter_positions(params, wordsize)

    # Generate marshal code.
    words = [[] for _ in range(num_mrs, syscall_stub_gen.MAX_MESSAGE_LENGTH)]
    for (param, first_bit, num_bits) in positions:
        generate_param_code(param, first_bit, num_bits, words, wordsize)

    # Return list of expressions.
    return [" | ".join(map(lambda x: "(" + translate_expr(x) + " as seL4_Word)", x))
            for x in words if len(x) > 0]


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
        first_word = first_bit // wordsize
        bit_offset = first_bit % wordsize

        # Multiword type?
        if num_bits > wordsize:
            result = []
            for x in range(num_bits // wordsize):
                result.append("%%(w%d)s" % (x + first_word))
            return result

        # Otherwise, bit packed.
        if num_bits == wordsize:
            return ["%%(w%d)s" % first_word]
        elif bit_offset == 0:
            return ["(%%(w%d)s & %#x)" % (
                first_word, (1 << num_bits) - 1)]
        else:
            return ["(%%(w%d)s.wrapping_shr(%d)) & %#x" % (
                first_word, bit_offset, (1 << num_bits) - 1)]

    # Get their marshalling positions
    positions = syscall_stub_gen.get_parameter_positions(params, wordsize)

    # Generate the unmarshal code.
    results = []
    for (param, first_bit, num_bits) in positions:
        results.append((param, unmarshal_single_param(first_bit, num_bits, wordsize)))
    return results


def generate_result_struct(interface_name, method_name, output_params):
    # Do we actually need a structure?
    if not syscall_stub_gen.is_result_struct_required(output_params):
        return None

    # Generate the structure:
    #
    #   #[repr(C)]
    #   #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    #   pub struct seL4_SchedContext_Consumed {
    #       pub error: isize,
    #       pub consumed: seL4_Time,
    result = []
    result.append("#[repr(C)]")
    result.append("#[derive(Debug, Clone, Copy, PartialEq, Eq)]")
    result.append("pub struct %s_%s {" % (interface_name, method_name))
    result.append("\tpub error: isize,")
    for i in output_params:
        if not i.type.pass_by_reference():
            result.append("\tpub %s," % rust_render_parameter_name(i.type, i.name))
    result.append("}")
    result.append("")
    return "\n".join(result)


def generate_stub(arch, wordsize, interface_name, method_name, method_id, input_params, output_params, structs, use_only_ipc_buffer, comment, mcs):
    result = []

    if use_only_ipc_buffer:
        num_mrs = 0
    else:
        if mcs and "%s-mcs" % arch in syscall_stub_gen.MESSAGE_REGISTERS_FOR_ARCH:
            num_mrs = syscall_stub_gen.MESSAGE_REGISTERS_FOR_ARCH["%s-mcs" % arch]
        else:
            num_mrs = syscall_stub_gen.MESSAGE_REGISTERS_FOR_ARCH[arch]

    # Split out cap parameters and standard parameters
    standard_params = []
    cap_params = []
    for x in input_params:
        if isinstance(x.type, syscall_stub_gen.CapType):
            cap_params.append(x)
        else:
            standard_params.append(x)

    # Determine if we are returning a structure, or just the error code.
    returning_struct = False
    results_structure = generate_result_struct(interface_name, method_name, output_params)
    if results_structure:
        return_type = "%s_%s" % (interface_name, method_name)
        returning_struct = True
    else:
        return_type = "seL4_Result"

    #
    # Print doxygen comment.
    #
    result.append(comment)

    #
    # Print function header.
    #
    #   #[inline(always)]
    #   #[macros::derive_test_wrapper]
    #   pub unsafe fn seL4_Untyped_Retype(...) -> seL4_Result
    #   {
    #
    result.append("#[inline(always)]")
    result.append("pub unsafe fn %s_%s(%s) -> %s" % (interface_name, method_name,
                                                     generate_param_list(input_params, output_params), return_type))
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
    # Setup variables we will need.
    #
    if returning_struct:
        result.append("\tlet mut result: %s = ::core::mem::zeroed();" % return_type)
    result.append("\tlet tag = seL4_MessageInfo::new(InvocationLabel::%s as seL4_Word, 0, %d, %d);" % (
        method_id, len(cap_expressions), len(input_expressions)))

    for i in range(num_mrs):
        result.append("\tlet mut mr%d: seL4_Word = 0;" % i)
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
    #   mr0 = (start_offset as seL4_Word) as seL4_Word;
    #   ...
    #   seL4_SetMR(i, v);
    #   ...
    #
    if max(num_mrs, len(input_expressions)) > 0:
        result.append("\t/* Marshal and initialize parameters. */")
        # Initialise in-register parameters
        for i in range(num_mrs):
            if i < len(input_expressions):
                result.append("\tmr%d = %s as seL4_Word;" % (i, input_expressions[i]))
            else:
                result.append("\tmr%d = 0 as seL4_Word;" % i)
        # Initialise buffered parameters
        for i in range(num_mrs, len(input_expressions)):
            expression = translate_expr(input_expressions[i])
            result.append("\tseL4_SetMR(%d, %s);" % (i, expression))
        result.append("")

    #
    # Generate the call.
    #
    if use_only_ipc_buffer:
        result.append("\t/* Perform the call. */")
        result.append("\tlet output_tag = seL4_Call(%s, tag);" % service_cap)
    else:
        result.append("\t/* Perform the call, passing in-register arguments directly. */")
        result.append("\tlet output_tag = seL4_CallWithMRs(%s, tag," % (service_cap))
        result.append("\t\t%s);" % ', '.join(
            ("&mut mr%d" % i) for i in range(num_mrs)))

    #
    # Prepare the result.
    #

    if returning_struct:
        result.append("\tresult.error = output_tag.get_label() as _;")
    else:
        result.append("\tlet error: seL4_Error = output_tag.get_label().into();")
    result.append("")

    if not use_only_ipc_buffer:
        result.append("\t/* Unmarshal registers into IPC buffer on error. */")
        result.append("\tif error != seL4_Error::seL4_NoError {")
        for i in range(num_mrs):
            result.append("\t\tseL4_SetMR(%d, mr%d);" % (i, i))
        if returning_struct:
            result.append("\t\treturn result;")
        result.append("\t}")
        result.append("")

    #
    # Generate unmarshalling code.
    #
    if len(output_params) > 0:
        result.append("\t/* Unmarshal result. */")
        source_words = {}
        for i in range(syscall_stub_gen.MAX_MESSAGE_LENGTH):
            if i < num_mrs:
                source_words["w%d" % i] = "mr%d" % i
            else:
                source_words["w%d" % i] = "seL4_GetMR(%d)" % i
        unmashalled_params = generate_unmarshal_expressions(output_params, wordsize)
        for (param, words) in unmashalled_params:
            param.name = translate_expr(param.name)
            if param.type.pass_by_reference():
                members = syscall_stub_gen.struct_members(param.type, structs)
                for i in range(len(words)):
                    result.append("\t(*%s).%s = %s;" %
                                  (param.name, members[i], words[i] % source_words))
            else:
                if param.type.double_word:
                    result.append("\tresult.%s = (%s as %s) + ((%s as %s).wrapping_shr(32));" %
                                  (param.name, words[0] % source_words, TYPES[64],
                                   words[1] % source_words, TYPES[64]))
                else:
                    for word in words:
                        if type(param.type) is syscall_stub_gen.StructType:
                            result.append("\tresult.%s = %s { words: [%s] };" % (
                                param.name, param.type.name, word % source_words))
                        else:
                            result.append("\tresult.%s = %s as %s;" % (param.name, word %
                                                                       source_words, translate_type(param.type.name)))

    #
    # }
    #
    result.append("\tresult" if returning_struct else "\terror.into()")
    result.append("}")

    return "\n".join(result) + "\n"


def generate_stub_file(arch, input_files, output_file, use_only_ipc_buffer, mcs, args):
    """
    Generate a header file containing system call stubs for seL4.
    """
    result = []

    # Ensure architecture looks sane.
    if arch not in syscall_stub_gen.WORD_SIZE_BITS_ARCH.keys():
        raise Exception(f"Invalid architecture: {arch}")

    wordsize = syscall_stub_gen.WORD_SIZE_BITS_ARCH[arch]

    # cheriTODO: document CHERI support
    data_types = syscall_stub_gen.init_data_types(wordsize, None)
    arch_types = syscall_stub_gen.init_arch_types(wordsize, args, False)

    # Parse XML
    methods = []
    structs = []
    for infile in input_files:
        method, struct, _ = syscall_stub_gen.parse_xml_file(infile, data_types + arch_types[arch])
        methods += method
        structs += struct

    # Print header.
    result.append("""
/*
 * Automatically generated system call stubs.
 */

""")

    #
    # Generate structures needed to return results back to the user.
    #
    # We can not use pass-by-reference (except for really large objects), as
    # the verification framework does not support them.
    #
    result.append("/*")
    result.append(" * Return types for generated methods.")
    result.append(" */")
    for (interface_name, method_name, _, _, output_params, _, _) in methods:
        results_structure = generate_result_struct(interface_name, method_name, output_params)
        if results_structure:
            result.append(results_structure)

    #
    # Generate the actual stub code.
    #
    result.append("/*")
    result.append(" * Generated stubs.")
    result.append(" */")
    for (interface_name, method_name, method_id, inputs, outputs, condition, comment) in methods:
        if condition != "":
            if condition == "(!defined(CONFIG_KERNEL_MCS) && defined(CONFIG_ENABLE_SMP_SUPPORT))":
                condition = 'all(not(feature = "CONFIG_KERNEL_MCS"), feature = "CONFIG_ENABLE_SMP_SUPPORT")'
            elif condition:
                condition = condition.replace('defined', '')
                condition = condition.replace('(', '')
                condition = condition.replace(')', '')
                if 'CONFIG_' in condition:
                    condition = 'feature = "' + condition + '"'
                if '!' in condition:
                    condition = 'not(%s)' % condition.replace('!', '')
            if condition:
                result.append("#[cfg(%s)]" % condition)
        result.append(generate_stub(arch, wordsize, interface_name, method_name,
                                    method_id, inputs, outputs, structs, use_only_ipc_buffer, comment, mcs))

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
    parser.add_argument("-a", "--arch", dest="arch", required=True, choices=syscall_stub_gen.WORD_SIZE_BITS_ARCH,
                        help="Architecture to generate stubs for.")
    parser.add_argument("--mcs", dest="mcs", action="store_true",
                        help="Generate MCS api.")
    parser.add_argument("--x86-vtx-64-bit-guests", dest="x86_vtx_64bit", action="store_true", default=False,
                        help="Whether the vtx VCPU objects need to be large enough for 64-bit guests.")

    parser.add_argument("files", metavar="FILES", nargs="+",
                        help="Input XML files.")

    return parser


def main():
    parser = process_args()
    args = parser.parse_args()
    # Generate the stubs.
    generate_stub_file(args.arch, args.files, args.output, args.buffer, args.mcs, args)


if __name__ == "__main__":
    sys.exit(main())
